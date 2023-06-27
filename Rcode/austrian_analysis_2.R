##

library(tidyverse)
library(stargazer)

eu_coord <-  coord_sf(xlim = c(-2500000, 5500000), ylim = c(3000000, 12000000), expand = TRUE)
eu_aes <- scale_fill_gradientn(colors = c('white', 'red'), na.value = '#d3d3d3')

library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(broom)    # assists with tidy data
library(ggplot2)  # graphics package
library(leaflet)  # interactive graphics (output does not show in RMD files)
library(dplyr)    # joining data frames
library(readr)    # quickly reads files into R
library(sf)

eu <- st_read("/Users/tylerbrown/R/markets_populism/NUTS_RG_60M_2016_3857.shp/NUTS_RG_60M_2016_3857.shp")




ggplot(eu) +
  geom_sf()

eu %>% filter(LEVL_CODE == 0) %>% 
  group_by(CNTR_CODE) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ggplot()+
  geom_sf()

eu %>% View




names(eu)
eu$NUTS_ID


m1
map_merge <- merge(eu, merged, by.x = c('NUTS_ID'), by.y = 'nuts3', all.x = TRUE)

map_merge %>% names



nuts2 <- map_merge %>%
  group_by(nuts2, year) %>% 
  summarize(geometry = st_union(geometry))


countries <- map_merge %>% select(CNTR_CODE, geometry) %>% 
  group_by(CNTR_CODE) %>% 
  summarize(geometry = st_union(geometry))


map_merge %>% names
map_merge %>% names
map_merge %>% View

map_merge  %>% filter(non_democ == 1 & voteshare <=1) %>% 
  ggplot() +
  geom_sf(aes(fill = voteshare))+
  eu_coord + 
  eu_aes
  

map_merge %>% names
countries


geom_sf(data = map_merge %>% filter(non_democ == 1 & voteshare <= 1), aes(fill = voteshare))+
  eu_coord+
  eu_aes



map_merge %>% names

eu_cropped

st_crop(eu, xmin = -20, xmax = 45,
        ymin = 30, ymax = 73)


# write.csv(merged, 'vote_data_20jun.csv')



map_merge %>% filter(non_democ == 1) %>% ggplot(aes(x = educ_uni, y = voteshare))+
  stat_summary_bin(geom = 'line')

merged %>% filter(non_democ == 1) %>%  select_if(is.numeric) %>% select(!c(educ_voc, non_democ, nutslevel)) %>% 
  as.matrix %>% 
  cor(use = 'pairwise.complete.obs') %>% 
  corrplot::corrplot()
merged$non_democ

merged %>% names









