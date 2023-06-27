### area data analysis


library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(broom)    # assists with tidy data
library(ggplot2)  # graphics package
library(leaflet)  # interactive graphics (output does not show in RMD files)
library(dplyr)    # joining data frames
library(readr)    # quickly reads files into R
library(sf)

getwd()



###### color prims
pal  <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
          "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


oe_red <- "#C8102E"
rot_lab <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##########

okres <- st_read(
  "/Users/tylerbrown/R/markets_populism/ah_shp_3/okres_3.shp")
okres

okres

bundeslaender <- oes %>%
  group_by(bundesland) %>% 
  summarize(geometry = st_union(geometry))


okres %>% ggplot() +
  geom_sf()

okresy <- merge(okres, sk, by.x = 'NM3', by.y = 'okres') 



okresy %>% ggplot()+
  geom_sf(aes(fill = private_foreign/business_total))+
  scale_fill_gradientn(colors = c('white', oe_red), na.value = 'blue')

okresy %>% ggplot()+
  geom_sf(aes(fill = hungarian/total_pop))+
  facet_wrap(~year)+
  scale_fill_gradientn(colors = c('white', 'steelblue'), na.value = 'blue')



okresy %>% ggplot()+
  geom_sf(aes(fill = total_populist))+
  facet_wrap(~year)+
  scale_fill_gradientn(colors = c('white', oe_red), na.value = 'blue')
  
okresy %>% group_by(NM3) %>% summarise(m=mean(private_foreign/business_total,na.rm=TRUE)) %>% arrange(-m)

okresy %>% filter(NM3 == 'Komárno')

### vote by period
bundeslaender  %>%  ggplot(
  aes(label = bundesland))+
  geom_sf(linewidth = 0, alpha = 0.5, fill = 'grey')+
  geom_sf(data = map_merge %>% filter(!is.na(years_to_elect !=0) & elect_year == 1) %>% mutate(t_label = case_when(years_to_elect == 0 ~ 't1',TRUE ~ 't0')), 
          aes(fill = fpo), linewidth = 0)+
  scale_fill_gradientn(colors = c('white', oe_red), na.value = 'blue')+
  geom_sf(linewidth = 0.5, alpha = 0, fill = 'blue')+
  facet_wrap(~t_label, nrow = 2)+
  guides(fill = guide_legend(title = "FPÖ Vote share"))+
  theme_classic()


### migration
bundeslaender %>% ggplot()+
  geom_sf()+
  geom_sf(data = map_merge %>% filter(!is.na(dVS)), aes(fill = as.factor(Tx2)), linewidth = 0)+
  geom_sf(data = dist_map, alpha = 0, linewidth = 0.3, color = pal[6])+
  scale_fill_manual(values = c('#FFFFFE', pal[4]), labels = c('No','Yes'))+
  guides(fill = guide_legend(title = "Municipalities receiving migrants"))+
  theme_classic()+
  theme(legend.position = 'bottom',legend.key=element_rect(colour="black"))


### change in vote share
bundeslaender %>% ggplot()+
  geom_sf(data = map_merge %>% filter(!is.na(dVS)), aes(fill = dVS), linewidth = 0)+
  geom_sf(linewidth = 0.3, alpha = 0.2, fill = 'black')+
  scale_fill_gradientn(colors = c(pal[11],'white',oe_red), na.value = '#d3d3d3')+
  guides(fill = guide_legend(title = "Change in FPÖ vote share"))+
  facet_wrap(~year)+
  theme_classic()


### change in vote share
bundeslaender %>% ggplot()+
  geom_sf(data = map_merge %>% filter(!is.na(dVS)), aes(fill = dVS), linewidth = 0)+
  geom_sf(linewidth = 0.3, alpha = 0.2, fill = 'black')+
  scale_fill_gradientn(colors = c(pal[11],'white',oe_red), na.value = 'black')+
  guides(fill = guide_legend(title = "Change in FPÖ vote share"))+
  theme_classic()

## unemployment
bundeslaender %>% ggplot()+
  geom_sf(data = unemp_df_map, aes(fill = ruemp), linewidth = 0)+
  geom_sf(linewidth = 0.3, alpha = 0.2, fill = 'black')+
  scale_fill_continuous(low = 'white', high = pal[6], na.value = 'black')+
  guides(fill = guide_legend(title = "Unemployment rate"))+
  theme_classic()+
  theme(legend.position = 'bottom')

dist_map %>%  ggplot()+
  geom_sf(data = map_merge %>% filter(!is.na(dVS)), aes(fill = dist_exposure), linewidth = 0)



dist_totss <- incl_df %>% filter(!is.na(dVS)) %>% group_by(dist_id) %>% summarise(dist_exposure = mean(dist_exposure))


incl_df %>% filter(!is.na(fpo)) %>% View


map_merge %>% mutate(fpo2 = case_when(years_to_elect != 0 ~ fpo)) %>% 
  ggplot()+
  geom_sf(aes(fill = fpo))+ 
  scale_fill_gradientn(colors = c('white', 'red'), na.value = '#d3d3d3')+
  geom_sf(data = bundeslaender, linewidth = 0.5, fill = NA, color = 'green')+
  geom_sf_label(fill = "white",  
                fun.geometry = sf::st_centroid)

map_merge


map_merge %>% filter() %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data = dist_map,  linewidth = 0.5, fill = NA, color = 'green')+
  geom_sf_label(fill = "white",  
                fun.geometry = sf::st_centroid)


dist_map



incl_df %>% filter(!is.na(dVS)) %>% group_by(dist_id) %>% summarise(sum(area,na.rm=TRUE)) %>% View
plot(st_geometry(oes))

scale_fill_gradientn(colors = c('white', 'red'))

oes %>% ggplot()+
  geom_sf()





fortify(geo_df)

droughtfort <- fortify(oes, region="NAME_1")
droughtfort



utils::unzip(fn, exdir = tempdir())
shp <- readOGR(dsn = file.path(tempdir(), oes), stringsAsFactors = F)


oes$geom


oes <- oes %>% mutate(area = st_area(oes))
oes
centroids <- st_centroid(oes)

centroids
library(rgeos)

centroids


capital_centroids <- centroids %>% filter(NAME %in% capitals)
capital_centroids

st_distance(centroids$geometry[centroids$ID == '10101'], capital_centroids %>% filter(NAME == 'Eisenstadt')) 

capital_centroids %>% filter(NAME == 'Eisenstadt')


capital_centroids$geometry[2]

centroids


centroids %>% group_by(NAME) %>% summarise(dist = st_distance(NAME, capital_centroids %>% filter(NAME == dist_capital)))

cap_dist <- centroids %>% mutate(dist_capital = case_when(
  as.numeric(ID) < 20000 ~ st_distance(geometry, capital_centroids %>% filter(NAME == 'Eisenstadt')),
  as.numeric(ID) >= 20000 & as.numeric(ID) <30000 ~ st_distance(centroids$geometry[centroids$ID == ID], capital_centroids$geometry[1]),
  as.numeric(ID) >= 30000 & as.numeric(ID) <40000 ~ st_distance(centroids$geometry[centroids$ID == ID], capital_centroids$geometry[2]),
  as.numeric(ID) >= 40000 & as.numeric(ID) <50000 ~ st_distance(centroids$geometry[centroids$ID == ID], capital_centroids$geometry[3]),
  as.numeric(ID) >= 50000 & as.numeric(ID) <60000 ~ st_distance(centroids$geometry[centroids$ID == ID], capital_centroids$geometry[4]),
  as.numeric(ID) >= 60000 & as.numeric(ID) <70000 ~ st_distance(centroids$geometry[centroids$ID == ID], capital_centroids$geometry[5]),
  as.numeric(ID) >= 70000 & as.numeric(ID) <80000 ~ st_distance(centroids$geometry[centroids$ID == ID], capital_centroids$geometry[6]),
  as.numeric(ID) >= 80000 & as.numeric(ID) <90000 ~ st_distance(centroids$geometry[centroids$ID == ID], capital_centroids$geometry[7])
))

centroids

cap_dist %>% View


merge(centroids, capital_centroids)

centroids %>% View
flows %>% filter(comm_name == 'Wien')

capitals <- c('Linz', 'Klagenfurt am Wörthersee', 'Eisenstadt','St. Pölten','Salzburg','Graz', 'Innsbruck','Wien', 'Bregenz')

centroids %>% filter(ID == '80207') %>% pull(geometry)

st_distance(centroids$geometry[3], centroids$geometry[centroids])

flows %>% View
oes$comm_id <- flows %>% pull(comm_id) %>% unique
oes$dist_id <- area_data$dist_id
area_data

oes



centroids <- centroids %>% mutate(bundesland = case_when(
  dist_id < 200 ~ 'Burgenland',
  dist_id >= 200 & dist_id <300 ~ 'Kaernten',
  dist_id >= 300 & dist_id <400 ~ 'Niederoesterreich',
  dist_id >= 400 & dist_id <500 ~ 'Oberoesterreich',
  dist_id >= 500 & dist_id <600 ~ 'Salzburg',
  dist_id >= 600 & dist_id <700 ~ 'Steiermark',
  dist_id >= 700 & dist_id <800 ~ 'Tirol',
  dist_id >= 800 & dist_id <900 ~ 'Vorarlberg',
  dist_id >= 900 ~ 'Wien'
))


centroids

# Burgenland ~ Linz
# Karnten Klagenfurt
# NOE Sankt Poelten
# Salz Salz
# Steiermark Graz
# Tirol INnsbruck
# OOE Linz
# Wien Wien
# VBG Bregenz


centroids %>% mutate(capital_id = case_when(
  dist_id < 200 ~ 10101,
  dist_id >= 200 & dist_id <300 ~ 20101,
  dist_id >= 300 & dist_id <400 ~ 30201,
  dist_id >= 400 & dist_id <500 ~ 40101,
  dist_id >= 500 & dist_id <600 ~ 50101,
  dist_id >= 600 & dist_id <700 ~ 60101,
  dist_id >= 700 & dist_id <800 ~ 70101,
  dist_id >= 800 & dist_id <900 ~ 'Vorarlberg'
))



write_csv(incl_df, 'data_28.03.2023.csv')

unique(incl_df$ruemp) %>% length

library(stargazer)
model.frame(m4) %>% as.data.frame %>% summarise_all(funs(mean(.,na.rm=TRUE))) %>% t %>% stargazer()



incl_df$ruemp_quart <- cut(incl_df$ruemp,quantile(incl_df$ruemp, na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
incl_df$dist_quart <- cut(incl_df$dist_exposure,quantile(incl_df$dist_exposure, na.rm=TRUE),include.lowest=TRUE,labels=FALSE)


incl_df %>% filter(!is.na(dVS)) %>% ggplot(aes(x = ruemp, y = dVS, color = as.factor(Tx2)))+
  geom_point()+
  geom_smooth(method= 'lm')+
  facet_wrap(~dist_quart)


