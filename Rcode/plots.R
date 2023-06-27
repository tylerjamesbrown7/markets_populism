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
nuts3_final <- read_csv("output/nuts3_final.csv")
nuts2_final <- read_csv("output/nuts2_final.csv")

eu <- st_read("input/eu_shapes/eu_shapefile.shp")
eu <- eu %>% rename(nutslevel = LEVL_CODE, nutscode = NUTS_ID, country_code = CNTR_CODE, mount_type = MOUNT_TYPE, urban_type = URBN_TYPE, coast_type = COAST_TYPE)

eu2 <- eu %>% filter(nutslevel == 2)
eu3 <- eu %>% filter(nutslevel == 3)

nuts2_map <- merge(eu2, nuts2_final, by.y = 'nuts2', by.x = 'nutscode', all=TRUE)
nuts3_map <- merge(eu3, nuts3_final, by.y = 'nuts3', by.x = 'nutscode', all=TRUE)

eu_coord <-  coord_sf(xlim = c(-2500000, 5500000), ylim = c(3000000, 12000000), expand = TRUE)
eu_aes <- scale_fill_gradientn(colors = c('white','black'), na.value = '#d3d3d3')

#####

nuts2_map

# basic plots

nuts3_map <- nuts3_map %>% group_by(country, year, ideology, ideo01) %>% mutate(demeaned = voteshare - mean(voteshare, na.rm=TRUE)) 

nuts3_map %>% View

nuts3_map %>% filter(voteshare < 1 | is.na(voteshare)) %>% group_by(nutscode) %>% arrange(-year) %>% slice(1) %>% ggplot() +
  geom_sf(aes(fill = demeaned))+
  eu_coord+
  eu_aes



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