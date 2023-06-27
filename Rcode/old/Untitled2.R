# new data cleaning
library(tidyverse)
library(readxl)

eu_ned_national <- read_excel("final data/eu_ned_national.xlsx", 
                              sheet = "Sheet1")

elect_eu <- eu_ned_national

elect_eu %>% View



elect <- merge(elect_eu, party_codings %>% select(!country), by = 'partyfacts_id', all.x=TRUE)

elect <- elect %>% mutate(non_democ = case_when(
  !is.na(populist) ~ 1,
  is.na(populist) ~ 0
))


elect <- elect %>% mutate(nuts2 = substr(nuts_code, 1, 4))

elect <- elect %>% mutate(nuts3 = case_when(
  nutslevel == 3 & str_length(nuts_code) == 5 ~ nuts_code
))


trucks <- elect_1 %>% filter(!is.na(nuts3) & !is.na(freight_kt_unload))

trucks %>% filter(country='France')
length(unique(elect$nuts3))




###### creating covariate data

library(readr)
business_demog_EU <- read_delim("final data/business_demog_EU.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

pop_density_EU <- read_delim("final data/pop_density_EU.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

pop_tot_EU <- read_delim("final data/pop_tot_EU.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

prop_uni_edu_EU <- read_delim("final data/prop_uni_edu_EU.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
names(prop_uni_edu_EU) <- c('nuts3','year','prop_unied')

business_demog_EU <- business_demog_EU %>% pivot_wider(id_cols = c(nuts3, year), names_from = variable, values_from = value)



pop_density_EU %>% dim
pop_density_EU %>% View
pop_tot_EU %>% dim
prop_uni_edu_EU %>% dim

prop_uni_edu_EU


### aggregating voting to NUTS 3 level

names(elect)

elect %>% filter(nuts_code == 'AT111') %>% View

elect_short <- elect %>% select(nuts_code, year, nuts2, nutslevel, country, country_code, regionname, partyvote, electorate, totalvote, validvote, populist, populist_bl, farright, farright_bl, farleft, farleft_bl, eurosceptic, eurosceptic_bl, non_democ)

elect_short <- elect_short %>% 
  group_by(nuts_code, year, nuts2, nutslevel, country, country_code, regionname, non_democ) %>% 
  summarise(vote_prop = sum(partyvote)/validvote)


elect_short %>% ggplot(aes(x = year, y = vote_prop))+
  stat_summary_bin(geom = 'line') +
  facet_wrap(~country)


elect %>% pull(nuts_code) %>% unique %>% length





merge(elect, pop_density_EU)
