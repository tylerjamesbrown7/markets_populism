##

library(tidyverse)
library(stargazer)



aust <- merge(aust_dat, nuts_codes, all.x = TRUE) 
freight <- road_freight_data %>% rename(nuts3 = dist_id)
aust <- merge(freight, aust) 


aust



library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(broom)    # assists with tidy data
library(ggplot2)  # graphics package
library(leaflet)  # interactive graphics (output does not show in RMD files)
library(dplyr)    # joining data frames
library(readr)    # quickly reads files into R
library(sf)

eu <- st_read("/Users/tylerbrown/R/markets_populism/NUTS_RG_01M_2021_3857.shp/NUTS_RG_01M_2021_3857.shp")




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



map_merge <- merge(eu, elect, by.x = c('NUTS_ID'), by.y = 'nuts_code', all.x=TRUE)

map_merge %>% View

map_merge %>% ggplot() +
  geom_sf(aes(fill = validvote))+
  coord_sf(xlim = c(-2000000, 6000000), ylim = c(3000000, 730000), expand = FALSE)
    scale_fill_gradientn(colors = c('white', 'red'), na.value = '#d3d3d3')

map_merge %>% names

eu_cropped

st_crop(eu, xmin = -20, xmax = 45,
                      ymin = 30, ymax = 73)




