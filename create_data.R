
## load libraries
library(cansim); library(tidyverse); library(janitor); library(rgeos); library(rgdal)
library(stringi); library(magrittr); library(tools)

## create dictionary
names_of_sets = c('grain_area', 'can_snd', 'farm_snd')
location_of_sets = c('32100002', '32100013', '32100015')

sad_conversion = read_csv('sad_conversion.csv')
sad_name_to_code = read_csv('sad_name_to_code.csv')

## create list of ndm sets
list_of_sets = map(location_of_sets, 
                   function(x) get_cansim_ndm(x) %>% 
                     clean_names %>%
                     mutate(geo=stri_trans_general(geo,"Latin-ASCII")) )

names(list_of_sets) = names_of_sets

## create sad map
sad_map = readOGR('CropsSADRegions_2017_Gen/CropsSADRegions_2017_Gen.shp',stringsAsFactors = FALSE)

## create prov map
prov_map = readOGR('Canada/Canada.shp',stringsAsFactors = FALSE)
prov_map = spTransform(prov_map, CRS("+proj=longlat +datum=WGS84"))

## convert new sasksatchewan regions
rbind(
  
  # create set containing all observations which do not need modification
  list_of_sets[['grain_area']] %>% select(ref_date,geo,type_of_crop,harvest_disposition,value) %>%
    filter(!(grepl('Small Area Data',geo) & grepl('Saskatchewan',geo) & ref_date %in% 1976:2016)) %>%
    #mutate(geo=iconv(geo,from='UTF-8',to='ASCII//TRANSLIT')) %>%
    data.frame() ,
  
  # create set containg all observations which need modification
  list_of_sets[['grain_area']] %>% select(ref_date,geo,type_of_crop,harvest_disposition,value) %>%
    filter(grepl('Small Area Data',geo) & grepl('Saskatchewan',geo) & ref_date %in% 1976:2016) %>%
    # group by crop, year and harvest disposition
    group_by(type_of_crop,ref_date,harvest_disposition) %>%
    # convert SAD regions
    group_modify(~ {
      colSums(.$value*sad_conversion,na.rm=TRUE) %>%
        enframe(name='geo', value='value')
      } ) %>%
    # modify geo names
    mutate(geo=paste0('Small Area Data ',geo)
           #,geo=iconv(geo,from='UTF-8',to='ASCII//TRANSLIT')
           ) %>%
    data.frame() ) %>%
  left_join(sad_name_to_code, by=c('geo'='sad_name')) %>%
  mutate(sad_code = ifelse(is.na(sad_code),parse_number(geo),sad_code)) %>%
  filter(
    harvest_disposition %in% c("Seeded area (acres)", "Harvested area (acres)", "Average yield (bushels per acre)", "Production (metric tonnes)") &
    type_of_crop %in% c('Barley','Oats','Wheat, all','Canola','Peas, dry','Rye, fall remaining','Lentils','Flaxseed')
    ) %>%
  mutate(
    value=replace_na(value,0),
    type_of_crop=gsub("(.*),.*", "\\1", tolower(type_of_crop)),
    harvest_disposition = str_replace(harvest_disposition, " \\(.*\\)", ""),
    harvest_disposition = gsub(" ","_",tolower(harvest_disposition))
    )  -> list_of_sets[['grain_area']]


list_of_sets[['farm_snd']] %<>%
  filter(
    farm_supply_and_disposition_of_grains %in% c("Production", "Deliveries", "Total supplies",'Beginning stocks','Seed requirements') &
      type_of_crop %in% c('Barley','Oats','Wheat, all','Canola','Peas, dry','Rye, fall remaining','Lentils','Flaxseed')
  ) %>%
  mutate(
    type_of_crop=gsub("(.*),.*", "\\1", tolower(type_of_crop)),
    farm_supply_and_disposition_of_grains = str_replace(farm_supply_and_disposition_of_grains, " \\(.*\\)", ""),
    farm_supply_and_disposition_of_grains = gsub(" ","_",tolower(farm_supply_and_disposition_of_grains))
  )

grain_area_disp = unique(list_of_sets[['grain_area']]$harvest_disposition)
grain_area_disp_names = toTitleCase(gsub('_', ' ',grain_area_disp))

grain_area_crop = unique(list_of_sets[['grain_area']]$type_of_crop)
grain_area_crop_names = toTitleCase(gsub('_', ' ',grain_area_crop))

farm_snd_disp = unique(list_of_sets[['farm_snd']]$farm_supply_and_disposition_of_grains)
farm_snd_disp_names = toTitleCase(gsub('_', ' ',farm_snd_disp))

farm_snd_crop = unique(list_of_sets[['farm_snd']]$type_of_crop)
farm_snd_crop_names = toTitleCase(gsub('_', ' ',farm_snd_crop))


colSums(is.na(list_of_sets[['grain_area']]))
