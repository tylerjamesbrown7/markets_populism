## analysis and mapping of Slovak data
library(tidyverse)
library(rgdal)    # Updates deprecated maptools functions
library(maptools) # creates maps and work with spatial files
library(leaflet)  # interactive graphics (output does not show in RMD files)
library(sf)
devtools::install_github('tylerjamesbrown7/tbtools', force = TRUE)
library(tbtools)
library(readxl)


slov <- read_csv("input/slovakia/slovak_data_11may.csv")
slovak_unemployment <- read_excel("input/slovak_unemployment.xlsx") %>% rename(okres = name)
slov <- merge(slov, slovak_unemployment, by = 'okres', all.x = TRUE)
slov <- slov %>% mutate(hung_pop = hungarian/total_pop)
a <- slov %>% group_by(okres) %>% summarise(hung_pop = mean(hung_pop,na.rm=TRUE)) %>% pull(hung_pop)
slov <- slov %>% mutate(min_eth = case_when(hung_pop >= quantile(a)[4] ~ 1,
                                            T ~ 0))
    

slov <- slov %>% mutate(private_domestic_prop = (private-private_foreign-private_foreign_intl)/business_total,
                        private_prop = private/business_total)

# construction of the Bartik instrument
slov <- slov %>% group_by(okres) %>% mutate(base_private = private[year == 2002]/business_total[year == 2002],
                                            base_private_domestic = (private[year == 2002]-private_foreign[year == 2002]-private_foreign_intl[year == 2002])/business_total[year == 2002],
                                            base_private_foreign_intl = private_foreign_intl[year == 2002]/business_total[year == 2002]) %>% 
  mutate(private_domestic = private - private_foreign)


slov <- slov %>% group_by(okres) %>% mutate(real_growth = (private - lag(private))/(business_total - (private - lag(private)))) %>%  
  ungroup %>% 
  group_by(year) %>% mutate(nat_priv = sum(private),
                            nat_biz = sum(business_total)) %>% 
  ungroup %>% 
  group_by(okres) %>% 
  mutate(nat_growth_private = (nat_priv - lag(nat_priv))/(nat_biz - (nat_priv - lag(nat_priv)))) %>% 
  mutate(inst_growth = base_private * nat_growth_private) %>% 
  ungroup %>% 
  group_by(okres) %>% mutate(real_growth_dom = (private_domestic - lag(private_domestic))/(business_total - (private_domestic - lag(private_domestic)))) %>%  
  ungroup %>% 
  group_by(year) %>% mutate(nat_priv_dom = sum(private_domestic),
                            nat_biz_dom = sum(business_total)) %>% 
  ungroup %>% 
  group_by(okres) %>% 
  mutate(nat_growth_private_dom = (nat_priv_dom - lag(nat_priv_dom))/(nat_biz_dom - (nat_priv_dom - lag(nat_priv_dom)))) %>% 
  mutate(inst_growth_dom = base_private_domestic * nat_growth_private_dom) %>% 
  ungroup %>% 
  group_by(okres) %>% mutate(real_growth_for = (private_foreign_intl - lag(private_foreign_intl))/(business_total - (private_foreign_intl - lag(private_foreign_intl)))) %>%  
  ungroup %>% 
  group_by(year) %>% mutate(nat_priv_for = sum(private_foreign_intl),
                            nat_biz_for = sum(business_total)) %>% 
  ungroup %>% 
  group_by(okres) %>% 
  mutate(nat_growth_private_for = (nat_priv_for - lag(nat_priv_for))/(nat_biz_for - (nat_priv_for - lag(nat_priv_for)))) %>% 
  mutate(inst_growth_for = base_private_foreign_intl * nat_growth_private_for) 


# slov with LAU1 instrument

slov <- slov %>% mutate(nuts1 = substr(lau1,1, 4)) %>% 
  group_by(okres) %>% mutate(real_growth_lau = (private - lag(private))/(business_total - (private - lag(private)))) %>%  
  ungroup %>% 
  group_by(year, nuts1) %>% mutate(lau_priv = sum(private),
                                   lau_biz = sum(business_total)) %>% 
  ungroup %>% 
  group_by(okres) %>% 
  mutate(lau_growth_private = (lau_priv - lag(lau_priv))/(lau_biz - (lau_priv - lag(lau_priv)))) %>% 
  mutate(inst_growth_lau = base_private * lau_growth_private) %>% 
  ungroup %>% 
  group_by(okres) %>% mutate(real_growth_dom_lau = (private_domestic - lag(private_domestic))/(business_total - (private_domestic - lag(private_domestic)))) %>%  
  ungroup %>% 
  group_by(year, nuts1) %>% mutate(lau_priv_dom = sum(private_domestic),
                                   lau_biz_dom = sum(business_total)) %>% 
  ungroup %>% 
  group_by(okres) %>% 
  mutate(lau_growth_private_dom = (lau_priv_dom - lag(lau_priv_dom))/(lau_biz_dom - (lau_priv_dom - lag(lau_priv_dom)))) %>% 
  mutate(inst_growth_dom_lau = base_private_domestic * lau_growth_private_dom) %>% 
  ungroup %>% 
  group_by(okres) %>% mutate(real_growth_for_lau = (private_foreign_intl - lag(private_foreign_intl))/(business_total - (private_foreign_intl - lag(private_foreign_intl)))) %>%  
  ungroup %>% 
  group_by(year, nuts1) %>% mutate(lau_priv_for = sum(private_foreign_intl),
                                   lau_biz_for = sum(business_total)) %>% 
  ungroup %>% 
  group_by(okres) %>% 
  mutate(lau_growth_private_for = (lau_priv_for - lag(lau_priv_for))/(lau_biz_for - (lau_priv_for - lag(lau_priv_for)))) %>% 
  mutate(inst_growth_for_lau = base_private_foreign_intl * lau_growth_private_for)

slov <- slov %>% group_by(okres) %>% mutate(dVS = total_populist - lag(total_populist))

names(slov)

# slov_base <- slov

slov <- slov_base %>% select(lau1, okres, year, nuts1, total_populist, dVS, business_total, private, private_foreign, private_foreign_intl, pop_density, age_dep_ratio, pop_tot, foreign_prop, uni_prop, relig, prop_agro, prop_manuf, foreign_biz_prop, hung_pop, min_eth, long_unemp_rate, real_growth, real_growth_dom, real_growth_for, real_growth_lau, real_growth_dom_lau, real_growth_for_lau, inst_growth, inst_growth_dom, inst_growth_for, inst_growth_lau, inst_growth_dom_lau, inst_growth_for_lau)

# use of a Bartik instrument to measure influence of changing business conditions on populist voting

m1 <- AER::ivreg(dVS ~ real_growth + foreign_prop + age_dep_ratio + log(pop_tot), data = slov) 

m2 <- AER::ivreg(dVS ~ real_growth + foreign_prop + age_dep_ratio + log(pop_tot) | inst_growth + foreign_prop +  age_dep_ratio + log(pop_tot) , data = slov) 


stargazer::stargazer(se_robust(m1), se_robust(m2), type = 'text')

m1 <- AER::ivreg(dVS ~ real_growth_lau + log(pop_tot), data = slov, weights = log(pop_tot)) 

m2 <- AER::ivreg(dVS ~ real_growth_lau + log(pop_tot) | inst_growth_for_lau + log(pop_tot), data = slov) 


lm(real_growth ~ inst_growth + foreign_prop + age_dep_ratio + log(pop_tot), slov) %>% summary

stargazer::stargazer(se_robust(m1), se_robust(m2), type = 'text')

modelplot(list(OLS = m1, IV = m2), coef_omit = '(Intercept)')+
  geom_vline(xintercept = 0, color = 'chartreuse4', linetype = 'dashed')
?modelplot

slov %>% as.data.frame() %>%  select_if(is.numeric)

slov %>% pull(year) %>% unique

corrplot::corrplot.mixed(cor(slov%>% as.data.frame() %>%  select_if(is.numeric), use='pairwise.complete.obs'))


cor(slov_base %>% as.data.frame() %>%  select_if(is.numeric), use='pairwise.complete.obs')[,1] 

cor(slov_base$real_growth_for_lau, slov_base$inst_growth_for_lau, use = 'pairwise.complete.obs')

slov$lau1

summary(slov %>% select(private, private_foreign, private_domestic_prop, private_prop, business_total, pop_tot))


slov %>% names
slov %>% ggplot(aes(x = real_growth_for_lau, y = inst_growth_for_lau))+
  geom_point()+
  geom_smooth(method = 'lm')


cor(slov$real_growth_dom, slov$inst_growth_dom, use = 'pairwise.complete.obs')
cor(slov$real_growth, slov$inst_growth, use = 'pairwise.complete.obs')




slov_map %>% ggplot()+
  geom_sf(aes(fill = total_populist))+
  scale_fill_gradient(low = 'white', high = pal[4])





slov_lau %>% View
######## WASTELAND



###### color prims
pal  <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
          "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


oe_red <- "#C8102E"
rot_lab <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##########


TRUE ~ 0))
slov_map <- st_read("input/slovakia_shapes/okres_3.shp")

slov_map <- slov_map %>% rename(lau1 = LAU1_CODE)
slov_map <- merge(slov_map, slov, by = 'lau1', all = TRUE)

# proportion of hungarians nationally

ggplot(slov_map)+
  geom_sf(aes(fill = hungarian/total_pop))+
  scale_fill_gradient(low = 'white', high = pal[10])

ggplot(slov_map)+
  geom_sf(aes(fill = hungarian/total_pop))+
  scale_fill_gradient(low = 'white', high = pal[10])+
  theme(legend.position = 'none')


slov_map %>% 
  select(hung_pop, min_eth, geometry) %>% pivot_longer(!geometry) %>% 
  ggplot()+
  geom_sf(aes(fill = value))+
  scale_fill_gradient(low = 'white', high = pal[10])+
  theme(legend.position = 'none')+
  facet_wrap(~name)


ggplot(slov_map)+
  geom_sf(aes(fill = as.factor(hungarian/total_pop > 0.01)))+
  theme(legend.position = 'none')+
  scale_fill_manual(values = c('white',pal[10]))


slov %>% select_if(is.numeric) %>% cor %>% corrplot::corrplot()



# looking at descriptives on market allocation

names(slov)


# role of market structure

# role of competition (domestic vs. international firms)
