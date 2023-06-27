library(tidyverse)
library(stats)
library(readxl)
library(corrplot)
library(stargazer)

setwd('/Users/tylerbrown/R/ESS project/ESS Project SUmmer 2022')
SK_voting_final <- read_excel("ESS.datafiles/SK_voting_final.xlsx")
SK_covariates <- read_excel("ESS.datafiles/SK_covariates.xlsx")
SK_uni_prop <- read_excel("ESS.datafiles/SK_uni_prop.xlsx")
SK_relig <- read_excel("ESS.datafiles/SK_relig.xlsx", 
                       sheet = "Sheet1")
SK_indust <- read_excel("ESS.datafiles/SK_industry_makeup.xlsx", 
                        sheet = "Sheet1")
SK_indust_full <- read_excel("ESS.datafiles/SK_industry_makeup.xlsx")
SK_agro_land <- read_excel("ESS.datafiles/SK_agro_land.xlsx")

SK_agro_land <- SK_agro_land %>% pivot_longer(!c(okres, land_type), names_to = 'year') %>% pivot_wider(names_from = land_type, values_from = value) %>% 
  mutate(agro_prop_land = agro_land/tot_land)

  

SK_indust <- SK_indust %>% pivot_longer(!c(okres, industry)) %>% pivot_wider(names_from = industry, values_from = value)
SK_indust <- SK_indust %>% rename(year = name) %>% mutate(year = case_when(
  year == 'y20' ~ 2020,
  year == 'y16' ~ 2016,
  year == 'y12' ~ 2012,
  year == 'y10' ~ 2010
))

SK_indust <- SK_indust %>% as.data.frame

SK_indust <- SK_indust %>% group_by(okres) %>% 
  summarise_all(funs(mean(., na.rm=TRUE)))

SK_indust_full <- SK_indust_full %>% pivot_longer(!c(okres, industry)) %>% pivot_wider(names_from = industry, values_from = value)
SK_indust_full <- SK_indust_full %>% rename(year = name)
SK_indust_full <- SK_indust_full %>% as.data.frame


SK_indust_full_final <- SK_indust_full %>% mutate_at(vars(-("okres")),as.numeric) %>% group_by(okres) %>% 
  summarise_all(funs(mean(., na.rm=TRUE))) %>% 
  mutate(year = NULL) %>% 
  mutate_at(vars(-c('okres','Total')), funs(./Total))


SK_indust <- SK_indust %>% mutate(year = NULL) %>% 
  mutate(prop_agro = agro/tot_indust) %>% 
  mutate(prop_manuf = manuf/tot_indust) %>% 
  mutate(prop_prof = profscitech/tot_indust)

indust_addon <- SK_indust %>% select(okres, prop_agro, prop_manuf, prop_prof)

sk1 <- merge(SK_voting_final, SK_covariates, by = c('okres','year'))
sk1 <- merge(sk1, SK_uni_prop, by = c('okres'))
sk1 <- merge(sk1, SK_relig)
sk1 <- merge(sk1, indust_addon)
sk1 <- merge(sk1, SK_agro_land)

sk <- sk1
sk_big <- merge(sk1, SK_indust_full_final) 



sk <- sk %>%
  mutate_at(vars(-("okres")),as.numeric)

sk <- sk %>% mutate(foreign_biz_prop = private_foreign/business_total)
sk <- sk %>% mutate(turnout = total_voters/pop_tot)


sk <- merge(sk, slovak_demography)

pc <- princomp(x = sk[, -1],
             center = TRUE,
             scale. = TRUE)
pc
print(summary(pc), digits = 2, loadings = pc$loadings, cutoff = 1/14)	




screeplot(x= pc, type = "line", main = "Scree plot", cex.main = 1.8)

plot(pc, main = "Variance bar plot", cex.main = 1.8)

corrplot(cor(sk[, -1]), method = 'ellipse', order = 'FPC')


#try 3 factor solution	
?factanal
fact1 <- factanal(sk[,-1], factors = 4, rotation = "varimax")	
fact1


# Get loading plots for 3 factor model	
#factors 1 and 3	
plot(fact1$loadings[, c(1,2)], pch = 18, col = 'red')	
abline(h = 0)	
abline(v = 0)	
text(fact1$loadings[, c(1,2)], labels = names(sk), cex = 0.8)	


names(sk)

sk

model1 <- lm(total_populist ~  I(business_total/pop_tot) +         pop_tot + pop_density + age_dep_ratio + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)
model2 <- lm(total_populist ~  I(private/business_total) +         pop_tot + pop_density + age_dep_ratio + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)
model3 <- lm(total_populist ~  I(private_foreign/business_total) + pop_tot + pop_density + age_dep_ratio + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)
model4 <- lm(total_populist ~  I(business_total/pop_tot) + I(private/business_total) + I(private_foreign/business_total) + pop_tot + pop_density + age_dep_ratio + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)

model1 <- lm(total_populist ~  I(business_total/pop_tot) +         pop_tot + pop_density + age_dep_ratio + I(hungarian/total_pop) + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)
model2 <- lm(total_populist ~  I(private/business_total) +         pop_tot + pop_density + age_dep_ratio + I(hungarian/total_pop) + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)
model3 <- lm(total_populist ~  I(private_foreign/business_total) + pop_tot + pop_density + age_dep_ratio + I(hungarian/total_pop) + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)
model4 <- lm(total_populist ~  I(business_total/pop_tot) + I(private/business_total) + I(private_foreign/business_total) + pop_tot + pop_density + age_dep_ratio + I(hungarian/total_pop) + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)


estimatr::lm_robust(total_populist ~  I(business_total/pop_tot) + I(private/business_total) + I(private_foreign/business_total) + pop_tot + pop_density + age_dep_ratio + foreign_prop + uni_prop + relig + prop_manuf + agro_prop_land + turnout + as.factor(year), sk)

??lm_robust


?stargazer
stargazer::stargazer(model1, model2, model3, model4, type='text')


jtools::export_summs(model1, model2, model3, model4)

plot(model1$fitted.values, model1$residuals)



ggplot(model4, aes(x = .resid)) +
  geom_histogram(color = "white")

ggplot(sk, aes(x = private_foreign/business_total))+
  geom_histogram()

unique(sk$year)


library(ggpmisc)

names(sk_big)

### analyzing correlation between industry composition and populist voting
sk_big %>% pivot_longer(!names(sk_big)[1:21], names_to = 'industry', values_to = 'industry_prop') %>% 
  ggplot(aes(x = as.numeric(private_foreign), y = industry_prop))+
  stat_summary_bin()+
  geom_smooth(method = 'lm')+
  facet_wrap(~industry, scales = 'free')


names(sk_big %>% pivot_longer(!names(sk_big)[1:21], names_to = 'industry', values_to = 'industry_prop'))

sk_big_cor <- sk_big[,c(5,6,7,8,4,22:41)] %>% 
  mutate_all(funs(as.numeric)) %>% 
  cor(use = 'pairwise.complete.obs')

sk_big %>% View

sk_big %>% names
sk_num <- sk_big[,c(22:35)] %>% 
  mutate_all(funs(as.numeric))


sk_num <- sk_num %>% filter(complete.cases(sk_num))

sk_num
#try 3 factor solution	
fact1 <- factanal(sk_num, factors = 2, rotation = "varimax")	
fact1	


# Get loading plots for 3 factor model	


#first two factors	
plot(fact1$loadings, pch = 18, col = 'red')	
abline(h = 0)	
abline(v = 0)	
text(fact1$loadings, labels = names(WB2), cex = 0.8)	

#factors 1 and 3	
plot(fact1$loadings[, c(1,3)], pch = 18, col = 'red')	
abline(h = 0)	
abline(v = 0)	
text(fact1$loadings[, c(1,3)], labels = names(WB2), cex = 0.8)	

#loading plot for 2 factor model	
plot(fact0$loadings, pch = 18, col = 'red')	
abline(h = 0)	
abline(v = 0)	
text(fact0$loadings, labels = names(WB2), cex = 0.8)	


sk_big %>% pivot_longer(!names(sk_big)[1:18], names_to = 'industry', values_to = 'industry_prop') %>% 
  ggplot(aes(x = industry_prop, y = total_populist))+
  #stat_summary_bin()+
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  facet_wrap(~industry, scales = 'free')

sk %>% filter(private_foreign/business_total < 0.3) %>% ggplot(aes(x = private_foreign/business_total, y = total_populist))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~year)

sk %>% select(total_populist, total_pop, slovak, hungarian, polish, ukrainian) %>% pivot_longer(!c(total_populist, total_pop)) %>% 
  ggplot(aes(x = value/total_pop, y = total_populist))+
  geom_point()+
  facet_wrap(~name, scales = 'free')
  


ggplot(sk, aes(x = slovak/total_pop, y = total_populist))+
  geom_point()



### CONSIDER TAX EVASION 

sk %>% pull(year) %>% (unique)

