## analysis

library(tidyverse)
vote <- vote_data_21jun

write.csv(votes, 'vote_data_21jun.csv')

vote_panel <- merge(panel_covs_18jun, elect_short, by.x = c('nuts3','year'), by.y = c('nuts_code','year'), all.x=TRUE)





votes <- merged

votes %>% View
votes <- votes %>% mutate(country_code = case_when(
  is.na(country_code) ~ substr(nuts3, 1, 2),
  TRUE ~ country_code
))

#countries_list <- unique(votes$country_code)[14:55]

votes <- votes %>% filter(country_code %in% countries_list)


votes %>% filter(!is.na(non_democ)) %>% group_by(country_code) %>%   summarise_all(funs(round(sum(is.na(.))/n(),2))) %>% View


model1 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + year, data = merged %>% filter(non_democ == 1))

model1 %>% summary


model2_weighted <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), weights = pop_tot, data = votes %>% filter(non_democ == 1 & nutslevel == 3 & year >= 2014))
model2_weighted %>% summary

model2_log_weighted <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), weights = log(pop_tot), data = votes %>% filter(non_democ == 1 & nutslevel == 3 & year >= 2014))
model2_log_weighted %>% summary

model2 <- lm(voteshare ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = votes %>% filter(non_democ == 1 & nutslevel == 3 & year >= 2014))
model2 %>% summary

stargazer::stargazer(model2, model2_weighted, model2_log_weighted, type = 'text')





cor(votes$freight_kt_unload, votes$Enterprise_population, use = 'pairwise.complete.obs')

cor(ep, fr, use = 'pairwise.complete.obs')



votes %>% View

plot(model2$fitted.values, model2$residuals)





library(AER)
library(lmtest)

coeftest(model2_log_weighted, vcov = vcovHC(model2, 'HC1'))

stargazer::stargazer(model2, type = 'text')

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

model2 %>% summary


votes <- votes %>% arrange(nuts3, year)

votes <- votes %>% group_by(nuts3) %>% mutate(lead_vs = lead(voteshare)) 

votes <- votes %>% mutate(nutslevel = case_when(nuts3 == nuts2 & nuts3 != country_code ~ 2,
                                       nuts3 != country_code ~ 3)) 


model_lagged <- lm(lead_vs ~ I(Enterprise_population/pop_tot) + Business_growth_rate + log(pop_tot) + pop_density + educ_uni + country + as.factor(year), data = votes %>% filter(non_democ == 1 & nuts3 == nuts2))


votes %>% group_by(year) %>% 

votes %>% View














