
## load libraries
library(cansim); library(tidyverse); library(janitor); library(rgeos); library(rgdal)
library(stringi); library(magrittr)

## create dictionary
names_of_sets = c('grain_area', 'grain_prod', 'can_snd','corn_snd', 'farm_snd')
location_of_sets = c('32100002', '32100359', '32100013', '32100014', '32100015')

sad_conversion = read_csv('sad_conversion.csv')
sad_map = readOGR('CropsSADRegions_2017_Gen/CropsSADRegions_2017_Gen.shp',stringsAsFactors = FALSE)
sad_name_to_code = read_csv('sad_name_to_code.csv')

## create list of ndm sets
list_of_sets = map(location_of_sets, 
                   function(x) get_cansim_ndm(x) %>% 
                     clean_names %>%
                     mutate(geo=stri_trans_general(geo,"Latin-ASCII")) )

names(list_of_sets) = names_of_sets




## convert new sasksatchewan regions
list_of_sets[['grain_area']] = rbind(
  
  # create set containing all observations which do not need modification
  list_of_sets[['grain_area']] %>% select(ref_date,geo,type_of_crop,harvest_disposition,value) %>%
    filter(!(grepl('Small Area Data',geo) & grepl('Saskatchewan',geo) & ref_date %in% 1976:2016)) %>%
    mutate(geo=iconv(geo,from='UTF-8',to='ASCII//TRANSLIT')) %>%
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
    mutate(geo=paste0('Small Area Data ',geo),
           geo=iconv(geo,from='UTF-8',to='ASCII//TRANSLIT')) %>%
    data.frame() ) %>%
  left_join(sad_name_to_code, by=c('geo'='sad_name'))


list_of_sets[['grain_area']] %>% count(sad_code, sort=TRUE)
no = list_of_sets[['grain_area']]

sad_map@data %<>% left_join(list_of_sets[['grain_area']],by=c('PRSADReg'='sad_code'))
