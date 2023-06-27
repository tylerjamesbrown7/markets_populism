## generates a clean data set

library(tidyverse)

elect %>% names

elect_short <- elect %>% select(nuts_code, year, nutslevel, country_code, country, regionname, partyvote, totalvote, validvote, non_democ)


elect_short <- elect_short %>% 
  group_by(nuts_code, year, nutslevel, country_code, country, regionname, non_democ) %>% 
  summarise(voteshare = sum(partyvote)/totalvote) %>% 
  unique

# write.csv(elect_short, 'nuts3_nondemoc_13.jun.2023.csv')


# here I begin 
prop_uni_edu_EU <- read_delim("final data/prop_uni_edu_EU.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

business_demog_EU <- read_delim("final data/business_demog_EU.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

freigh_unload_EU <- read_delim("final data/freigh_unload_EU.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

pop_tot_EU <- read_delim("final data/pop_tot_EU.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

pop_density_EU <- read_delim("final data/pop_density_EU.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)


prop_uni_edu_EU <- prop_uni_edu_EU[,1:5]
prop_uni_edu_EU <- prop_uni_edu_EU %>% rename(nuts2 = nuts3)
prop_uni_edu_EU <- prop_uni_edu_EU %>% filter(age == 'Y25-64')
prop_uni_edu_EU %>% pull(edu_level) %>% unique

prop_uni_edu_EU <- prop_uni_edu_EU %>% mutate(edu_level = case_when(
  edu_level == 'Vocational_edu' ~ 'voc',
  edu_level == 'Uni_edu' ~ 'uni'
))
prop_uni_edu_EU
prop_uni_edu_EU <- prop_uni_edu_EU %>% pivot_wider(id_cols = c('nuts2','year'), names_from = c('edu_level'), names_prefix = 'educ_', values_from = 'prop')
business_demog_EU <- business_demog_EU %>% pivot_wider(id_cols = c('nuts3','year'), names_from = 'variable', values_from = 'value')



freigh_unload_EU %>% pull(nuts3) %>% unique
pop_tot_EU %>% pull(nuts3) %>% unique
pop_density_EU %>% pull(nuts3) %>% unique


m1 <- merge(pop_tot_EU, pop_density_EU, by = c('nuts3','year'), all = TRUE)
m1 <- merge(m1, business_demog_EU, by = c('nuts3','year'), all = TRUE)
m1 <- merge(m1, freigh_unload_EU, by = c('nuts3','year'), all = TRUE)
m1 <- m1 %>% mutate(nuts2 = substr(nuts3, 1, 4))
m1 <- merge(m1, prop_uni_edu_EU, by = c('nuts2', 'year'), all = TRUE)

complete_obs <- m1 %>% select(!educ_voc) %>% filter(complete.cases(.)) %>% pull(nuts2)

m1 <- m1 %>% mutate(complete = case_when(
  nuts2 %in% complete_obs ~ 1,
  !(nuts2 %in% complete_obs) ~ 0
))

nuts3_nondemoc_13_jun_2023 <- nuts3_nondemoc_13_jun_2023 %>% mutate(nuts2 = substr(nuts_code, 1, 4))
nuts3_nondemoc_13_jun_2023 <- nuts3_nondemoc_13_jun_2023 %>% mutate(nuts3 = case_when(
  nuts2 != nuts_code ~ nuts_code
))


elect <- merge(m1, nuts3_nondemoc_13_jun_2023, by = c('nuts3','year'), all.y=TRUE) %>% rename(nuts2 = nuts2.y)
#write.csv(elect, 'panel_covs_18jun.csv')

elect %>% filter(country == 'Poland') %>% View
