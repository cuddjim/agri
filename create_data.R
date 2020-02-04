library(cansim);library(tidyverse);library(janitor)
pids <- c('32100002','32100013','32100014','32100015','32100359')
sad_conversion <- read.csv("C:/Users/lab/Documents/GitHub/agri/sad_conversion.csv")
for (pid in pids) {
  temp = get_cansim_ndm(paste0(pid)) %>% clean_names()
  assign(paste0('T',str_sub(pid,6,8)),temp)
}

# create saskatchewan set
sk_sad = T002 %>% 
  filter(grepl('Small Area Data',geo) & grepl('Saskatchewan',geo) & ref_date %in% 2014:2016) %>%
  add_count(harvest_disposition, type_of_crop) %>%
  filter(n>51) %>% select(-n) %>%
  data.frame()

# create shell for input
sk_shell = data.frame(matrix(NA,0,0))

# convert old SADs to new with sad_conversion table
for (crop in unique(sk_sad$type_of_crop)) {
  
  for (harvest in unique(sk_sad$harvest_disposition)) {
    
    for (year in 2014:2016) {
      
      temp = sk_sad %>% filter(type_of_crop == crop & harvest_disposition == harvest & ref_date == year) %>% data.frame()
      
      if (nrow(temp)==37) {
        
        temp[21:37,'value'] = unname(colSums(sad_conversion*temp[1:20,'value'],na.rm=TRUE))
        temp %<>% filter(grepl('[(]',geo)) %>% data.frame()
        sk_shell = rbind(sk_shell,temp)
        
      }
    }
  }
}


# create new set
T002 %<>% filter(!(grepl('Small Area Data',geo) & grepl('Saskatchewan',geo) & ref_date %in% 2014:2016)) %>% rbind(.,sk_shell) %>% 
  filter(harvest_disposition %in% c("Seeded area (acres)", "harvested area (acres)", "Average yield (bushels per acre)", "Production (metric tonnes)") 
         & type_of_crop %in% c("Barley", "Oats", "Soybeans", "Canola", "Wheat, all"))