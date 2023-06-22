library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


world_map <- ne_countries(scale = 50, returnclass = 'sf')

european_union <- unique(ess_model$cntry_name)
european_union <- append(european_union, 'Czech Rep.')

european_union_map <- 
  world_map %>% 
  filter(name %in% european_union)

european_union_map %>% pull(formal_en) %>% unique()
bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 80), crs = st_crs(european_union_map))
european_union_map_cropped <- st_crop(european_union_map, bbox_europe)

european_union_map_cropped %>% View

df <- comm_merged %>% group_by(cntry_name) %>% summarise(populist = mean(populist,na.rm=TRUE))
df <- df %>% mutate(cntry_name = case_when(cntry_name == 'Czechia' ~'Czech Rep.',
                                           TRUE ~ paste(cntry_name)))

df

map <- 
  european_union_map_cropped %>% 
  left_join(df, by = c("name" = "cntry_name"))
map %>% View
map %>% View
?coord_sf
ggplot(data = map) +
  geom_sf(mapping = aes(fill = populist)) +
  scale_fill_gradient(name = "% voting for a populist party", low = "blue", high = "white", na.value = "grey50") +
  labs(title = "Incidence of populist voting") +
  theme(plot.title.position = "plot", legend.position = 'bottom')


