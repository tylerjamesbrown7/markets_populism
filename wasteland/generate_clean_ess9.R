library(tidyverse)
library(lme4)
library(lmerTest)

# analysis of ESS9 dataset

# working on research design

# data cleaning

ess9 <- ESS9
which(colnames(ess9) == 'vote')

uempla
uempli
ess9$pdwrk

vars <- c('cntry',
          'lrscale',
          'emplrel',
          'emplno',
          'estsz',
          'hinctnta',
          'isco08',
          'jbspv',
          'njbspv',
          'vote',
          'prtvtcat','prtvtdbe','prtvtdbg','prtvtgch','prtvtbcy','prtvtecz','prtvede1','prtvtddk','prtvtgee','prtvtees','prtvtdfi','prtvtdfr','prtvtcgb','prtvtahr','prtvtfhu','prtvtcie','prtvtcis','prtvtcit','prtvblt1','prtvtalv','prtvtme','prtvtgnl','prtvtbno','prtvtdpl','prtvtcpt','prtvtrs','prtvtcse','prtvtfsi','prtvtdsk',
          'sclmeet', 
          'inprdsc', 
          'sclact',
          'agea',
          'rlgdgr',
          'eduyrs',
          'gndr',
          'blgetmg',
          'uemp3m',
          'uemp12m',
          'uemp5yr',
          'mbtru',
          'tporgwk',
          'wrkac6m',
          'anweight',
          'domicil',
          'region',
          'uempla',
          'uempli',
          'pdwrk',
          'nacer2')  

ess <- ess9[vars]
vote_vars <- c('prtvtcat','prtvtdbe','prtvtdbg','prtvtgch','prtvtbcy','prtvtecz','prtvede1','prtvtddk','prtvtgee','prtvtees','prtvtdfi','prtvtdfr','prtvtcgb','prtvtahr','prtvtfhu','prtvtcie','prtvtcis','prtvtcit','prtvblt1','prtvtalv','prtvtme','prtvtgnl','prtvtbno','prtvtdpl','prtvtcpt','prtvtrs','prtvtcse','prtvtfsi','prtvtdsk')

ess[vote_vars] <- mutate_all(ess[vote_vars], function(x) as.numeric(as.character(x)))
# ess <- ess[rowSums(is.na(ess[vote_vars])) != ncol(ess[vote_vars]),]

ess <- unite(ess, vote_code, vote_vars, na.rm=TRUE) 
ess <- ess %>% mutate(vote_code = as.numeric(vote_code))

ess <- ess %>% mutate(vote_code = case_when(
  vote == 2 ~ 00,
  TRUE ~ vote_code
))


ess <- ess %>% filter(!is.na(vote_code)) 

relnames <- c('employee','selfemployed','fambiz')
# tests comparing employed vs. self-employed on political spectrum
empl.ess <- ess %>% 
  filter(emplrel == 1 | emplrel == 2 | emplrel == 3)
  
empl.ess$lrscale <- as.integer(empl.ess$lrscale)
empl.ess['empName'] <- NA

empl.ess <- empl.ess %>% mutate(empName = case_when(
  emplrel == 1 ~ 'employee',
  emplrel == 2 ~ 'selfemployed',
  emplrel == 3 ~ 'fambiz'
  
))

# also former communist countries
# Bulgaria, Czechia, Estonia*, Croatia, Hungary, Latvia, Lithuania, Montenegro, POland, Serbia, SLovenia, SLovakia 
communist <- c('BG','CZ','EE','HR','HU','LT','LV','ME','PL','RS','SI','SK')
empl.ess <- empl.ess %>% mutate(communist10 = case_when(
  cntry %in% communist ~ 1,
  !(cntry %in% communist) ~ 0,
))

# recoding gender male = 1
empl.ess <- empl.ess %>% mutate(gndr = case_when(
  gndr == 1 ~ 1, # male
  gndr == 2 ~ 0  # female
)) 

# recoding ethnic minority yes = 1
empl.ess <- empl.ess %>% mutate(blgetmg = case_when(
  blgetmg == 1 ~ 1, # yes, belong to ethnic minor.
  blgetmg == 2 ~ 0  # no, does not belong to ethnic minor
)) 

# recording uemp3m scale
empl.ess <- empl.ess %>% mutate(uemp3m = case_when(
  uemp3m == 1 ~ 1, 
  uemp3m == 2 ~ 0  # 
)) 

empl.ess <- empl.ess %>% mutate(occup = case_when(
  between(isco08,1000,1999) ~ '1 managers',
  between(isco08,2000,2999) ~ '2 professionals',
  between(isco08,3000,3999) ~ '3 technicians and assoc. profs.',
  between(isco08,4000,4999) ~ '4 clerical',
  between(isco08,5000,5999) ~ '5 service & sales',
  between(isco08,6000,6999) ~ '6 skilled agro',
  between(isco08,7000,7999) ~ '7 craft & trade',
  between(isco08,8000,8999) ~ '8 plant & machine op.',
  between(isco08,9000,9999) ~ '9 elementary',
  between(isco08,0,999) ~ '0 armed forces',
))

empl.ess <- empl.ess %>% mutate(jbspv = case_when(
  jbspv == 1 ~ 1, # yes, supervise others
  jbspv == 2 ~ 0  # no, don't supervise others
)) 

empl.ess <- empl.ess %>% mutate(mbtru = case_when(
  mbtru == 1 ~ 2, # yes, currently
  mbtru == 2 ~ 1, # yes, formerly
  mbtru == 3 ~ 0  # no, never 
)) 



empl.ess <- empl.ess %>% mutate(org_name = case_when(
  tporgwk == 1 ~ 'Central, local government', # 
  tporgwk == 2 ~ 'Other public sector', #
  tporgwk == 3 ~ 'State-owned enterprise',
  tporgwk == 4 ~ 'Private firm',
  tporgwk == 5 ~ 'Self-employed',
  tporgwk == 6 ~ 'Other' # no, never 
)) 


empl.ess <- empl.ess %>% mutate(domicil = case_when(
  domicil == 1 ~ 5, # big city
  domicil == 2 ~ 4, # suburb
  domicil == 3 ~ 3, # town, small city
  domicil == 4 ~ 2, # village
  domicil == 5 ~ 1  # farm, home in countryside
)) 


empl.ess <- empl.ess %>% mutate(domicil_name = case_when(
  domicil == 1 ~ 'Big city', # 
  domicil == 2 ~ 'Suburbs, outskirts of big city', # 
  domicil == 3 ~ 'Town or small city',
  domicil == 4 ~ 'Country village',
  domicil == 5 ~ 'Farm or home in countryside'
)) 


empl.ess <- empl.ess %>% mutate(self_emp10 = case_when(
  empName == 'selfemployed' ~ 1,
  empName == 'fambiz' ~ 2,
  empName == 'employee' ~ 0)) %>% 
  mutate(occup_level = trunc(isco08/1000)) %>% 
  filter(occup_level != 0) 

empl.ess <- empl.ess %>% 
  filter(rlgdgr < 11) # 10 very religious

# now I need to code the parties by country

popul_coding <- coding_w_antidemocractic

popul_coding %>% View

names(popul_coding)[1] <- 'vote_code'
names(popul_coding)[3] <- 'cntry'
names(popul_coding)
cntry_names <- unique(popul_coding$cntry)
cntry_names
vote_0 <- rep(0,30)
vote_0

null_votes <- cbind(vote_0,0, cntry_names,0,0) %>% as.data.frame
null_votes <- null_votes %>% mutate(vote_0 = as.numeric(vote_0))

names(null_votes) <- names(popul_coding)

popul_coding <- rbind(popul_coding,null_votes) 

popul_coding <- popul_coding %>% mutate(party_name = case_when(vote_code == 0 ~ 'novote',
                                               TRUE ~ party_name))

ess <- merge(empl.ess, popul_coding)

ess <- ess %>% mutate(populist = case_when(
  is.na(populist) | populist == 0 ~ 0,
  TRUE ~ 1
))

empl.ess$cntry %>% unique
# need political info data here
ess <- ess %>% mutate(cntry_name = case_when(cntry == 'AT' ~ 'Austria',
                                        cntry == 'BE' ~ 'Belgium',
                                        cntry == 'BG' ~ 'Bulgaria',
                                        cntry == 'CH' ~ 'Switzerland',
                                        cntry == 'CY' ~ 'Cyprus',
                                        cntry == 'CZ' ~ 'Czechia',
                                        cntry == 'DE' ~ 'Germany',
                                        cntry == 'DK' ~ 'Denmark',
                                        cntry == 'EE' ~ 'Estonia',
                                        cntry == 'ES' ~ 'Spain',
                                        cntry == 'FI' ~ 'Finland',
                                        cntry == 'FR' ~ 'France',
                                        cntry == 'GB' ~ 'United Kingdom',
                                        cntry == 'HR' ~ 'Croatia',
                                        cntry == 'HU' ~ 'Hungary',
                                        cntry == 'IE' ~ 'Ireland',
                                        cntry == 'IS' ~ 'Iceland',
                                        cntry == 'IT' ~ 'Italy',
                                        cntry == 'LT' ~ 'Lithuania',
                                        cntry == 'LV' ~ 'Latvia',
                                        cntry == 'ME' ~ 'Montenegro',
                                        cntry == 'NL' ~ 'Netherlands',
                                        cntry == 'NO' ~ 'Norway',
                                        cntry == 'PL' ~ 'Poland',
                                        cntry == 'PT' ~ 'Portugal',
                                        cntry == 'RS' ~ 'Serbia',
                                        cntry == 'SE' ~ 'Sweden',
                                        cntry == 'SI' ~ 'Slovenia',
                                        cntry == 'SK' ~ 'Slovakia'))






agg_pol <- agg_pol_raw %>% mutate(cntry_name = case_when(
`Country/Territory` == 'Czech Republic' ~ 'Czechia',
  TRUE ~ `Country/Territory`)) %>% filter(cntry_name %in% unique(ess$cntry_name))

agg_pol <- agg_pol %>% filter(Edition %in% c(2008,2018)) %>% select(cntry_name,Edition, PR, CL, Status, Total)

names(agg_pol) <- c('cntry_name','year','polit_rights','civil_lib','freedom_status','total_polit')

agg_pol <- agg_pol %>% pivot_wider(names_from = 'year', values_from = c('civil_lib','polit_rights','freedom_status','total_polit'))

final <- merge(ess, agg_pol)

saveRDS(final, 'clean_ess_22jul22.rds')

4pm-430 pm



ess <- final %>% mutate(gov_type = case_when(
  cntry_name == 'Switzerland' ~ 'federal.republic',
  cntry_name %in% c('France', 'Portugal', 'Lithuania') ~ 'semi.presidential.republic',
  cntry_name %in% c('Spain','Netherlands','Belgium','Sweden','Denmark','Norway','United Kingdom') ~ 'parliamentary.monarchy',
  cntry_name %in% c('Germany',
                    'Iceland',
                    'Poland',
                    'Czechia',
                    'Slovakia',
                    'Slovenia',
                    'Finland',
                    'Croatia',
                    'Estonia',
                    'Latvia',
                    'Bulgaria',
                    'Italy',
                    'Austria',
                    'Ireland',
                    'Hungary',
                    'Serbia') ~ 'parliamentary.republic',
  cntry_name == 'Cyprus' ~ 'presidential.republic'))

library(haven)

gdp_df <- read.csv('gdp-per-capita-growth.csv')
gdp_df <- gdp_df %>% filter(Year == 2009 & Entity %in% unique(ess$cntry_name))

names(gdp_df) <- c('cntry_name','cntry_code','year','gdp_growth')

ess <- merge(ess, gdp_df[,c('cntry_name','gdp_growth')], all = TRUE)

saveRDS(ess, 'clean_ess_22jul22.rds')


