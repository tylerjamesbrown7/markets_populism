# panel analysis with covariates
library(tidyverse)
library(readr)
devtools::install_github('tylerjamesbrown7/tbtools', force = TRUE)
library(tbtools)
library(stargazer)


#####
nuts3_final <- read_csv("output/nuts3_final.csv")
nuts2_final <- read_csv("output/nuts2_final.csv")

nuts3_final <- nuts3_final %>% group_by(country, year, ideology, ideo01) %>% mutate(demeaned = voteshare - mean(voteshare, na.rm=TRUE)) %>% as.data.frame

nuts3_final <- nuts3_final %>% group_by(nuts3) %>% mutate(l.p = lag(Enterprise_population)/lag(pop_tot))
nuts3_final <- nuts3_final %>% group_by(nuts3) %>% mutate(d.p = Enterprise_population/pop_tot - l.p)
nuts3_final <- nuts3_final %>% filter(Business_growth_rate < 500)
## base model
model11 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 1), weights = log(pop_tot))
model12 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist' & ideo01 == 1), weights = log(pop_tot))
model13 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farright' & ideo01 == 1), weights = log(pop_tot))
model14 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farleft' & ideo01 == 1), weights = log(pop_tot))
model15 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'eurosceptic' & ideo01 == 1), weights = log(pop_tot))
model16 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist_bl' & ideo01 == 1), weights = log(pop_tot))
model17 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 0), weights = log(pop_tot))

## with lag
model11 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 1), weights = log(pop_tot))
model12 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist' & ideo01 == 1), weights = log(pop_tot))
model13 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farright' & ideo01 == 1), weights = log(pop_tot))
model14 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farleft' & ideo01 == 1), weights = log(pop_tot))
model15 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'eurosceptic' & ideo01 == 1), weights = log(pop_tot))
model16 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist_bl' & ideo01 == 1), weights = log(pop_tot))
model17 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 0), weights = log(pop_tot))

## lag only
model11 <- lm(voteshare ~ l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 1), weights = log(pop_tot))
model12 <- lm(voteshare ~ l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist' & ideo01 == 1), weights = log(pop_tot))
model13 <- lm(voteshare ~ l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farright' & ideo01 == 1), weights = log(pop_tot))
model14 <- lm(voteshare ~ l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farleft' & ideo01 == 1), weights = log(pop_tot))
model15 <- lm(voteshare ~ l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'eurosceptic' & ideo01 == 1), weights = log(pop_tot))
model16 <- lm(voteshare ~ l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist_bl' & ideo01 == 1), weights = log(pop_tot))
model17 <- lm(voteshare ~ l.p + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 0), weights = log(pop_tot))

## difference
model11 <- lm(voteshare ~ d.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 1), weights = log(pop_tot))
model12 <- lm(voteshare ~ d.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist' & ideo01 == 1), weights = log(pop_tot))
model13 <- lm(voteshare ~ d.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farright' & ideo01 == 1), weights = log(pop_tot))
model14 <- lm(voteshare ~ d.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farleft' & ideo01 == 1), weights = log(pop_tot))
model15 <- lm(voteshare ~ d.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'eurosceptic' & ideo01 == 1), weights = log(pop_tot))
model16 <- lm(voteshare ~ d.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist_bl' & ideo01 == 1), weights = log(pop_tot))
model17 <- lm(voteshare ~ d.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 0), weights = log(pop_tot))


## difference
model11 <- lm(voteshare ~ d.p + l.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 1), weights = log(pop_tot))
model12 <- lm(voteshare ~ d.p + l.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist' & ideo01 == 1), weights = log(pop_tot))
model13 <- lm(voteshare ~ d.p + l.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farright' & ideo01 == 1), weights = log(pop_tot))
model14 <- lm(voteshare ~ d.p + l.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farleft' & ideo01 == 1), weights = log(pop_tot))
model15 <- lm(voteshare ~ d.p + l.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'eurosceptic' & ideo01 == 1), weights = log(pop_tot))
model16 <- lm(voteshare ~ d.p + l.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist_bl' & ideo01 == 1), weights = log(pop_tot))
model17 <- lm(voteshare ~ d.p + l.p + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 0), weights = log(pop_tot))

## no weights
model21 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 1))
model22 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist' & ideo01 == 1))
model23 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farright' & ideo01 == 1))
model24 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'farleft' & ideo01 == 1))
model25 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'eurosceptic' & ideo01 == 1))
model26 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'populist_bl' & ideo01 == 1))
model27 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = nuts3_final %>% filter(ideology == 'non_democ' & ideo01 == 0))


stargazer(se_robust(model11), se_robust(model12), se_robust(model13), se_robust(model14), se_robust(model15), se_robust(model16), se_robust(model17),  type = 'text')

stargazer(se_robust(model21), se_robust(model22), se_robust(model23), se_robust(model24), se_robust(model25), se_robust(model26), se_robust(model27),  type = 'text')


nuts3_final %>% filter(ideology == 'farright' & ideo01 == 1) %>% 
  ggplot(aes(x = educ_uni, y = voteshare))+
  geom_point(aes(color = country))+
  geom_smooth(aes(color = country), method = 'lm')


#####