##### generate data from scratch
library(tidyverse)
library(readxl)
library(stargazer)
library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(broom)    # assists with tidy data
library(ggplot2)  # graphics package
library(leaflet)  # interactive graphics (output does not show in RMD files)
library(dplyr)    # joining data frames
library(readr)    # quickly reads files into R
library(sf)

### importing national level election data
eu_ned_national <- read_excel("input/eu_ned_national.xlsx")
party_codings <- read_excel("input/party_codings.xlsx")
names(eu_ned_national)
names(party_codings)

votes_merged <- merge(eu_ned_national, party_codings, by = 'partyfacts_id', all.x = TRUE)


votes_merged %>% names
votes_ideolog <- votes_merged %>% mutate(non_democ = case_when(!is.na(populist) ~ 1,
                                              !is.na(partyfacts_id) ~ 0)) %>%  
  pivot_longer(c(populist, populist_bl, farright, farright_bl, farleft, farleft_bl, eurosceptic, eurosceptic_bl, non_democ)) %>% 
  group_by(country.x, country_code, regionname, nutslevel, nuts3, year, name, value) %>% summarise(num_parties = length(unique(partyfacts_id)),
                                                   voteshare = sum(partyvote/totalvote)) %>% filter(value == 1 | name == 'non_democ') %>% 
  rename(ideology = name, ideo01 = value, country = country.x)
             



votes_nuts3 <- votes_ideolog %>% filter(nutslevel == 3)
votes_nuts2 <- votes_ideolog %>% filter(nutslevel == 2)

votes_nuts2 <- votes_nuts2 %>% rename(nuts2 = nuts3)
votes_nuts3 <- votes_nuts3 %>% mutate(nuts2 = substr(nuts3, 1, 4))

votes_nuts2 <- as.data.frame(votes_nuts2)
votes_nuts3 <- as.data.frame(votes_nuts3)


write.csv(votes_nuts2, 'output/votes_nuts2.csv')
write.csv(votes_nuts3, 'output/votes_nuts3.csv')


### merging with covariates

prop_uni_edu_EU <- read_delim("input/prop_uni_edu_EU.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

business_demog_EU <- read_delim("input/business_demog_EU.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

freigh_unload_EU <- read_delim("input/freigh_unload_EU.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

pop_tot_EU <- read_delim("input/pop_tot_EU.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

pop_density_EU <- read_delim("input/pop_density_EU.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)


prop_uni_edu_EU <- prop_uni_edu_EU[,1:5]
prop_uni_edu_EU <- prop_uni_edu_EU %>% rename(nuts2 = nuts3)
prop_uni_edu_EU <- prop_uni_edu_EU %>% filter(age == 'Y25-64')

prop_uni_edu_EU <- prop_uni_edu_EU %>% mutate(edu_level = case_when(
  edu_level == 'Vocational_edu' ~ 'voc',
  edu_level == 'Uni_edu' ~ 'uni'
))

prop_uni_edu_EU <- prop_uni_edu_EU %>% pivot_wider(id_cols = c('nuts2','year'), names_from = c('edu_level'), names_prefix = 'educ_', values_from = 'prop')

business_demog_EU <- business_demog_EU %>% 
  pivot_wider(id_cols = c('nuts3','year'), names_from = 'variable', values_from = 'value')

business_demog_EU <- business_demog_EU %>% mutate(nuts2 = substr(nuts3, 1, 4)) %>% 
  mutate(nutslevel = case_when(nchar(nuts3) == 5 ~ 3,
                               nchar(nuts3) == 4 ~ 2,
                               nchar(nuts3) == 3 ~ 1,
                               nchar(nuts3) == 2 ~ 0))

m1 <- merge(pop_tot_EU, pop_density_EU, by = c('nuts3','year'), all = TRUE)
m1 <- merge(m1, business_demog_EU, by = c('nuts3','year'), all = TRUE)
m1 <- merge(m1, freigh_unload_EU, by = c('nuts3','year'), all = TRUE)
m1 <- m1 %>% mutate(nuts2 = substr(nuts3, 1, 4)) %>% 
  mutate(nutslevel = case_when(nchar(nuts3) == 5 ~ 3,
                               nchar(nuts3) == 4 ~ 2,
                               nchar(nuts3) == 3 ~ 1,
                               nchar(nuts3) == 2 ~ 0))

m1 <- merge(m1, prop_uni_edu_EU, by = c('nuts2', 'year'), all = TRUE)

nuts2_final <- merge(votes_nuts2 %>% filter(nutslevel == 2), m1 %>% filter(nutslevel == 2), by = c('nuts2', 'year'), all.x = TRUE)
nuts3_final <- merge(votes_nuts3 %>% filter(nutslevel == 3), m1 %>% filter(nutslevel == 3), by = c('nuts3', 'year'), all.x = TRUE)

nuts2_final <- nuts2_final %>% rename(nutslevel = nutslevel.x) %>% mutate(nutslevel.y = NULL) %>% as.data.frame()
nuts3_final <- nuts3_final %>% rename(nutslevel = nutslevel.x) %>% mutate(nutslevel.y = NULL) %>% as.data.frame()

write.csv(nuts2_final, 'output/nuts2_final.csv')
write.csv(nuts3_final, 'output/nuts3_final.csv')


##### generating map dataset
nuts3_final <- read_csv("output/nuts3_final.csv")
nuts2_final <- read_csv("output/nuts2_final.csv")

eu <- st_read("input/eu_shapes/eu_shapefile.shp")
eu <- eu %>% rename(nutslevel = LEVL_CODE, nutscode = NUTS_ID, country_code = CNTR_CODE, mount_type = MOUNT_TYPE, urban_type = URBN_TYPE, coast_type = COAST_TYPE)

eu2 <- eu %>% filter(nutslevel == 2)
eu3 <- eu %>% filter(nutslevel == 3)

nuts2_map <- merge(nuts2_final, eu2, by.x = 'nuts2', by.y = 'nutscode', all.y=TRUE)
nuts3_map <- merge(nuts3_final, eu3, by.x = 'nuts3', by.y = 'nutscode', all.y=TRUE)



# write_sf(nuts2_map, 'output/nuts2shp/nuts2_map.shp')
# write_sf(nuts3_map, 'output/nuts3shp/nuts3_map.shp')


##### END OF FILE #####

