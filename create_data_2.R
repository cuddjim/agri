
## load libraries
library(cansim); library(tidyverse); library(janitor)

## create dictionary
names_of_sets = c('grain_area', 'grain_prod', 'can_snd','corn_snd', 'farm_snd')
location_of_sets = c('32100002', '32100359', '321000013', '321000014', '321000015')
sad_conversion = read_csv('sad_conversion.csv')

## create list of ndm sets
list_of_sets = map(location_of_sets, function(x) get_cansim_ndm(x) %>% clean_names())
names(list_of_sets) = names_of_sets

## convert new sasksatchewan regions
list_of_sets[['grain_area']] = rbind(
  
  # create set containing all observations which do not need modification
  list_of_sets[['grain_area']] %>% select(ref_date,geo,type_of_crop,harvest_disposition,value) %>%
    filter(!(grepl('Small Area Data',geo) & grepl('Saskatchewan',geo) & ref_date %in% 2014:2016)) %>%
    data.frame() ,
  
  # create set containg all observations which need modification
  list_of_sets[['grain_area']] %>% select(ref_date,geo,type_of_crop,harvest_disposition,value) %>%
    filter(grepl('Small Area Data',geo) & grepl('Saskatchewan',geo) & ref_date %in% 2014:2016) %>%
    # group by crop, year and harvest disposition
    group_by(type_of_crop,ref_date,harvest_disposition) %>%
    # convert SAD regions
    group_modify(~ {
      colSums(.$value*sad_conversion,na.rm=TRUE) %>%
        enframe(name='geo', value='value')
      } ) %>%
    # modify geo names
    mutate(geo=paste0('Small Area Data ',geo)) %>%
    data.frame()

)
