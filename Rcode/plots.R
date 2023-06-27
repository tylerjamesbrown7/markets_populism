# plots
library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(broom)    # assists with tidy data
library(ggplot2)  # graphics package
library(leaflet)  # interactive graphics (output does not show in RMD files)
library(dplyr)    # joining data frames
library(readr)    # quickly reads files into R
library(sf)

#####
nuts2_map <- st_read("output/nuts2shp/nuts2_map.shp")
nuts3_map <- st_read("output/nuts3shp/nuts3_map.shp")





##### END OF FILE #####


###### WASTELAND ######


eu %>% filter(LEVL_CODE == 0) %>% 
  group_by(CNTR_CODE) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ggplot()+
  geom_sf()


nuts2 <- map_merge %>%
  group_by(nuts2, year) %>% 
  summarize(geometry = st_union(geometry))


countries <- map_merge %>% select(CNTR_CODE, geometry) %>% 
  group_by(CNTR_CODE) %>% 
  summarize(geometry = st_union(geometry))



map_merge %>% filter(non_democ == 1) %>% ggplot(aes(x = educ_uni, y = voteshare))+
  stat_summary_bin(geom = 'line')

merged %>% filter(non_democ == 1) %>%  select_if(is.numeric) %>% select(!c(educ_voc, non_democ, nutslevel)) %>% 
  as.matrix %>% 
  cor(use = 'pairwise.complete.obs') %>% 
  corrplot::corrplot()
merged$non_democ

merged %>% names