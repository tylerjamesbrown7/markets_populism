## analysis and mapping of Slovak data
library(tidyverse)
library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(leaflet)  # interactive graphics (output does not show in RMD files)
library(sf)
devtools::install_github('tylerjamesbrown7/tbtools', force = TRUE)
library(tbtools)


slov <- slovak_data_11may
slovak_unemployment <- slovak_unemployment %>% rename(okres = name)
slov <- merge(slov, slovak_unemployment, by = 'okres', all.x = TRUE)

###### color prims
pal  <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
          "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


oe_red <- "#C8102E"
rot_lab <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##########

slov_map <- st_read("input/slovakia_shapes/okres_3.shp")

slov_map <- slov_map %>% rename(lau1 = LAU1_CODE)
slov_map <- merge(slov_map, slov, by = 'lau1', all = TRUE)
slov_map %>% names

ggplot(slov_map)+
  geom_sf(aes(fill = hungarian/total_pop))+
  scale_fill_gradient(low = 'white', high = pal[10])

slov_map %>% group_by(okres) %>% summarise(m=mean(hungarian/total_pop, na.rm=TRUE)) %>% arrange(m)

ggplot(slov_map)+
  geom_sf(aes(fill = hungarian/total_pop))+
  scale_fill_gradient(low = 'white', high = pal[10])+
  theme(legend.position = 'none')

slov_map <- slov_map %>% mutate(min_eth = case_when(hung_pop >= quantile(a)[4] ~ 1,
                                        TRUE ~ 0))

slov_map %>% 
  select(hung_pop, min_eth, geometry) %>% pivot_longer(!geometry) %>% 
  ggplot()+
  geom_sf(aes(fill = value))+
  scale_fill_gradient(low = 'white', high = pal[10])+
  theme(legend.position = 'none')+
  facet_wrap(~name)


a <- slov %>% group_by(okres) %>% summarise(hung_pop = mean(hung_pop,na.rm=TRUE)) %>% pull(hung_pop)
quantile(a)[4]

ggplot(slov_map)+
  geom_sf(aes(fill = as.factor(hungarian/total_pop > 0.01)))+
  theme(legend.position = 'none')+
  scale_fill_manual(values = c('white',pal[10]))

slov_map <- slov_map %>% mutate(min_eth = case_when(hungarian/total_pop > 0.01 ~ 1,
                                                    hungarian/total_pop <= 0.01 ~ 0))

slov <- slov %>% mutate(min_eth = case_when(hungarian/total_pop > 0.01 ~ 1,
                                                   hungarian/total_pop <= 0.01 ~ 0))
slov <- slov %>% mutate(hung_pop = hungarian/total_pop)
slov %>% select_if(is.numeric) %>% cor %>% corrplot::corrplot()

slov %>% names
model <- AER::ivreg(total_populist ~ I((private-private_foreign-private_foreign_intl)/business_total) + log(total_pop), data = slov %>% filter(min_eth == 0), weights = log(total_pop))
se_robust(model)

model <- AER::ivreg(total_populist ~ I((private-private_foreign-private_foreign_intl)/business_total) + log(total_pop), data = slov %>% filter(min_eth == 1), weights = log(total_pop))
se_robust(model)

(private-private_foreign-private_foreign_intl)/business_total)

ggplot(slov_map, aes(x = (private-private_foreign-private_foreign_intl)/business_total, y = total_populist))+
  geom_point(aes(color = as.factor(min_eth), size = log(total_pop)))+
  geom_smooth(method = 'lm', aes(color = as.factor(min_eth)))


ggplot(slov, aes(x = hung_pop))+
  geom_histogram()+
  geom_vline(xintercept = quantile(a)[4], color = 'red')
  

slov %>% group_by(okres) %>% summarise(sum(hungarian)/sum(total_pop)) %>% View

regtable(model)
