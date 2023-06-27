# panel analysis with covariates

library(tidyverse)
panel_covs_18jun <- read_csv("panel_covs_18jun.csv")

panel <- panel_covs_18jun
panel <- panel %>% filter(country != 'Luxembourg')

panel <- panel %>% mutate(nuts_level = case_when(
  nuts3 == nuts2 ~ 2,
  nuts3 != nuts2 ~ 3
))


elect <- elect %>% mutate(nuts_level = case_when(
  nuts3 == nuts2 ~ 2,
  nuts3 != nuts2 ~ 3
))


elect %>% View
elect %>% filter(nuts_level == 3 & non_democ == 1 & Business_growth_rate < 500) %>% 
  ggplot(aes(x = educ_uni, y = voteshare))+
  geom_point()+
  geom_smooth(method = 'lm')

