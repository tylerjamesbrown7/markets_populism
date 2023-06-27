## merging all the ESS documents


library(haven)
library(tidyverse)

ess4_raw <- read_sav("ESS.datafiles/ess_files/ess4_raw.sav")
ess5_raw <- read_sav("ESS.datafiles/ess_files/ess5_raw.sav")
ess6_raw <- read_sav("ESS.datafiles/ess_files/ess6_raw.sav")
ess7_raw <- read_sav("ESS.datafiles/ess_files/ess7_raw.sav")
ess8_raw <- read_sav("ESS.datafiles/ess_files/ess8_raw.sav")
ess9_raw <- read_sav("ESS.datafiles/ess_files/ess9_raw.sav")

vars4 <- c('cntry',
          'lrscale',
          'emplrel',
          'emplno',
          'estsz',
          'hinctnta',
          'iscoco',
          'jbspv',
          'njbspv',
          'vote',
          'prtvtabg','prtvtacz','prtvtbee','prtvthr','prtvtbhu','prtvlt1','prtvtlv','prtvtbpl','prtvtaro','prtvtaru','prtvtask','prtvtcsi',
          'sclmeet', 
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
          'domicil',
          'uempla',
          'uempli',
          'pdwrk',
          'nacer11')  

c(Bulgaria, Czechia, Estonia, Croatia, Hungary, Lithuania, Latvia, Poland, Romania, Russia, Slovenia, Slovakia, Ukraine)

ess4 <- ess4_raw[vars4]

ess4 <- ess4 %>% mutate(cntry_name = case_when(cntry == 'AT' ~ 'Austria',
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

comm_4 <- ess4 %>% filter(cntry_name %in% c('Bulgaria', 'Czechia', 'Estonia', 'Croatia', 'Hungary', 'Lithuania', 'Latvia', 'Poland', 'Romania', 'Russia', 'Slovenia', 'Slovakia', 'Ukraine'))

comm_4 <- comm_4 %>% as.data.frame
# 2008 populist parties

comm_4 <- comm_4 %>% unite('party_id', c('prtvtabg','prtvtacz','prtvtbee','prtvthr','prtvtbhu','prtvlt1','prtvtlv','prtvtbpl','prtvtaro','prtvtaru','prtvtask','prtvtcsi'), na.rm=TRUE) 

pop_ess4 <- pop_ess4 %>% as.data.frame

comm_pop4_raw <- merge(comm_4, pop_ess4, by = c('cntry_name', 'party_id'))

comm_pop4 <- comm_pop4_raw %>% 
  filter(emplrel == 1 | emplrel == 2 | emplrel == 3)

comm_pop4$lrscale <- as.integer(comm_pop4$lrscale)
comm_pop4['empName'] <- NA

comm_pop4 <- comm_pop4 %>% mutate(empName = case_when(
  emplrel == 1 ~ 'employee',
  emplrel == 2 ~ 'selfemployed',
  emplrel == 3 ~ 'fambiz'
  
))

# also former communist countries
# Bulgaria, Czechia, Estonia*, Croatia, Hungary, Latvia, Lithuania, Montenegro, POland, Serbia, SLovenia, SLovakia 
communist <- c('BG','CZ','EE','HR','HU','LT','LV','ME','PL','RS','SI','SK')
comm_pop4 <- comm_pop4 %>% mutate(communist10 = case_when(
  cntry %in% communist ~ 1,
  !(cntry %in% communist) ~ 0,
))

# recoding gender male = 1
comm_pop4 <- comm_pop4 %>% mutate(gndr = case_when(
  gndr == 1 ~ 1, # male
  gndr == 2 ~ 0  # female
)) 

# recoding ethnic minority yes = 1
comm_pop4 <- comm_pop4 %>% mutate(blgetmg = case_when(
  blgetmg == 1 ~ 1, # yes, belong to ethnic minor.
  blgetmg == 2 ~ 0  # no, does not belong to ethnic minor
)) 

# recording uemp3m scale
comm_pop4 <- comm_pop4 %>% mutate(uemp3m = case_when(
  uemp3m == 1 ~ 1, 
  uemp3m == 2 ~ 0  # 
)) 

comm_pop4 <- comm_pop4 %>% mutate(occup = case_when(
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

comm_pop4 <- comm_pop4 %>% mutate(jbspv = case_when(
  jbspv == 1 ~ 1, # yes, supervise others
  jbspv == 2 ~ 0  # no, don't supervise others
)) 

comm_pop4 <- comm_pop4 %>% mutate(mbtru = case_when(
  mbtru == 1 ~ 2, # yes, currently
  mbtru == 2 ~ 1, # yes, formerly
  mbtru == 3 ~ 0  # no, never 
)) 



comm_pop4 <- comm_pop4 %>% mutate(org_name = case_when(
  tporgwk == 1 ~ 'Central, local government', # 
  tporgwk == 2 ~ 'Other public sector', #
  tporgwk == 3 ~ 'State-owned enterprise',
  tporgwk == 4 ~ 'Private firm',
  tporgwk == 5 ~ 'Self-employed',
  tporgwk == 6 ~ 'Other' # no, never 
)) 


comm_pop4 <- comm_pop4 %>% mutate(domicil = case_when(
  domicil == 1 ~ 5, # big city
  domicil == 2 ~ 4, # suburb
  domicil == 3 ~ 3, # town, small city
  domicil == 4 ~ 2, # village
  domicil == 5 ~ 1  # farm, home in countryside
)) 


comm_pop4 <- comm_pop4 %>% mutate(domicil_name = case_when(
  domicil == 1 ~ 'Big city', # 
  domicil == 2 ~ 'Suburbs, outskirts of big city', # 
  domicil == 3 ~ 'Town or small city',
  domicil == 4 ~ 'Country village',
  domicil == 5 ~ 'Farm or home in countryside'
)) 


comm_pop4 <- comm_pop4 %>% mutate(self_emp10 = case_when(
  empName == 'selfemployed' ~ 1,
  empName == 'fambiz' ~ 2,
  empName == 'employee' ~ 0)) %>% 
  mutate(occup_level = trunc(isco08/1000)) %>% 
  filter(occup_level != 0) 

comm_pop4 <- comm_pop4 %>% 
  filter(rlgdgr < 11) # 10 very religious
comm_pop5


comm_pop4 <- comm_pop4 %>% mutate(psoe = case_when(
  org_name == 'Private firm' ~ 'private',
  org_name == 'State-owned enterprise' ~ 'SOE',
  org_name == 'Central, local government' | org_name == 'Other public sector' ~ 'public',
  TRUE ~ 'other'
))


comm_pop4 %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin()+
  facet_wrap(~cntry_name, scales = 'free')



vars5 <- c('cntry',
           'lrscale',
           'emplrel',
           'emplno',
           'estsz',
           'hinctnta',
           'iscoco',
           'jbspv',
           'njbspv',
           'vote',
           'prtvtbbg','prtvtbcz','prtvtcee','prtvthr','prtvtchu','prtvlt1','prtvtbpl','prtvtbru','prtvtbsk','prtvtcsi',
           'sclmeet', 
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
           'domicil',
           'uempla',
           'uempli',
           'pdwrk',
           'nacer2')  

ess5 <- ess5_raw[vars5]

ess5 <- ess5 %>% mutate(cntry_name = case_when(cntry == 'AT' ~ 'Austria',
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

comm_5 <- ess5 %>% filter(cntry_name %in% c('Bulgaria', 'Czechia', 'Estonia', 'Croatia', 'Hungary', 'Lithuania', 'Latvia', 'Poland', 'Romania', 'Russia', 'Slovenia', 'Slovakia', 'Ukraine'))

comm_5 <- comm_5 %>% as.data.frame
# 2008 populist parties

comm_5 <- comm_5 %>% unite('party_id', c('prtvtbbg','prtvtbcz','prtvtcee','prtvthr','prtvtchu','prtvlt1','prtvtbpl','prtvtbru','prtvtbsk','prtvtcsi',), na.rm=TRUE) 

pop_ess5 <- pop_ess5 %>% as.data.frame

comm_pop5_raw <- merge(comm_5, pop_ess5, by = c('cntry_name', 'party_id'))

comm_pop5 <- comm_pop5_raw %>% 
  filter(emplrel == 1 | emplrel == 2 | emplrel == 3)

comm_pop5$lrscale <- as.integer(comm_pop5$lrscale)
comm_pop5['empName'] <- NA

comm_pop5 <- comm_pop5 %>% mutate(empName = case_when(
  emplrel == 1 ~ 'employee',
  emplrel == 2 ~ 'selfemployed',
  emplrel == 3 ~ 'fambiz'
  
))

# also former communist countries
# Bulgaria, Czechia, Estonia*, Croatia, Hungary, Latvia, Lithuania, Montenegro, POland, Serbia, SLovenia, SLovakia 
communist <- c('BG','CZ','EE','HR','HU','LT','LV','ME','PL','RS','SI','SK')
comm_pop5 <- comm_pop5 %>% mutate(communist10 = case_when(
  cntry %in% communist ~ 1,
  !(cntry %in% communist) ~ 0,
))

# recoding gender male = 1
comm_pop5 <- comm_pop5 %>% mutate(gndr = case_when(
  gndr == 1 ~ 1, # male
  gndr == 2 ~ 0  # female
)) 

# recoding ethnic minority yes = 1
comm_pop5 <- comm_pop5 %>% mutate(blgetmg = case_when(
  blgetmg == 1 ~ 1, # yes, belong to ethnic minor.
  blgetmg == 2 ~ 0  # no, does not belong to ethnic minor
)) 

# recording uemp3m scale
comm_pop5 <- comm_pop5 %>% mutate(uemp3m = case_when(
  uemp3m == 1 ~ 1, 
  uemp3m == 2 ~ 0  # 
)) 

comm_pop5 <- comm_pop5 %>% mutate(occup = case_when(
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

comm_pop5 <- comm_pop5 %>% mutate(jbspv = case_when(
  jbspv == 1 ~ 1, # yes, supervise others
  jbspv == 2 ~ 0  # no, don't supervise others
)) 

comm_pop5 <- comm_pop5 %>% mutate(mbtru = case_when(
  mbtru == 1 ~ 2, # yes, currently
  mbtru == 2 ~ 1, # yes, formerly
  mbtru == 3 ~ 0  # no, never 
)) 



comm_pop5 <- comm_pop5 %>% mutate(org_name = case_when(
  tporgwk == 1 ~ 'Central, local government', # 
  tporgwk == 2 ~ 'Other public sector', #
  tporgwk == 3 ~ 'State-owned enterprise',
  tporgwk == 4 ~ 'Private firm',
  tporgwk == 5 ~ 'Self-employed',
  tporgwk == 6 ~ 'Other' # no, never 
)) 


comm_pop5 <- comm_pop5 %>% mutate(domicil = case_when(
  domicil == 1 ~ 5, # big city
  domicil == 2 ~ 4, # suburb
  domicil == 3 ~ 3, # town, small city
  domicil == 4 ~ 2, # village
  domicil == 5 ~ 1  # farm, home in countryside
)) 


comm_pop5 <- comm_pop5 %>% mutate(domicil_name = case_when(
  domicil == 1 ~ 'Big city', # 
  domicil == 2 ~ 'Suburbs, outskirts of big city', # 
  domicil == 3 ~ 'Town or small city',
  domicil == 4 ~ 'Country village',
  domicil == 5 ~ 'Farm or home in countryside'
)) 


comm_pop5 <- comm_pop5 %>% mutate(self_emp10 = case_when(
  empName == 'selfemployed' ~ 1,
  empName == 'fambiz' ~ 2,
  empName == 'employee' ~ 0)) %>% 
  mutate(occup_level = trunc(isco08/1000)) %>% 
  filter(occup_level != 0) 

comm_pop5 <- comm_pop5 %>% 
  filter(rlgdgr < 11) # 10 very religious
comm_pop5


comm_pop5 <- comm_pop5 %>% mutate(psoe = case_when(
  org_name == 'Private firm' ~ 'private',
  org_name == 'State-owned enterprise' ~ 'SOE',
  org_name == 'Central, local government' | org_name == 'Other public sector' ~ 'public',
  TRUE ~ 'other'
))


comm_pop5 %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin()+
  facet_wrap(~cntry_name, scales = 'free')



############# 6 

vars6 <- c('cntry',
           'lrscale',
           'emplrel',
           'emplno',
           'estsz',
           'hinctnta',
           'isco08',
           'jbspv',
           'njbspv',
           'vote',
           'prtvtcbg','prtvtccz','prtvtdee','prtvtdhu','prtvalt1','prtvtcpl','prtvtcsk','prtvtdsi',
           'sclmeet', 
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
           'domicil',
           'uempla',
           'uempli',
           'pdwrk',
           'nacer2')  

ess6 <- ess6_raw[vars6]

ess6 <- ess6 %>% mutate(cntry_name = case_when(cntry == 'AT' ~ 'Austria',
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

comm_6 <- ess6 %>% filter(cntry_name %in% c('Bulgaria', 'Czechia', 'Estonia', 'Croatia', 'Hungary', 'Lithuania', 'Latvia', 'Poland', 'Romania', 'Russia', 'Slovenia', 'Slovakia', 'Ukraine'))

comm_6 <- comm_6 %>% as.data.frame
# 2008 populist parties

comm_6 <- comm_6 %>% unite('party_id', c('prtvtcbg','prtvtccz','prtvtdee','prtvtdhu','prtvalt1','prtvtcpl','prtvtcsk','prtvtdsi'), na.rm=TRUE) 

comm_pop6_raw <- merge(comm_6, pop_ess6, by = c('cntry_name', 'party_id'))

comm_pop6 <- comm_pop6_raw %>% 
  filter(emplrel == 1 | emplrel == 2 | emplrel == 3)

comm_pop6$lrscale <- as.integer(comm_pop6$lrscale)
comm_pop6['empName'] <- NA

comm_pop6 <- comm_pop6 %>% mutate(empName = case_when(
  emplrel == 1 ~ 'employee',
  emplrel == 2 ~ 'selfemployed',
  emplrel == 3 ~ 'fambiz'
  
))

# also former communist countries
# Bulgaria, Czechia, Estonia*, Croatia, Hungary, Latvia, Lithuania, Montenegro, POland, Serbia, SLovenia, SLovakia 
communist <- c('BG','CZ','EE','HR','HU','LT','LV','ME','PL','RS','SI','SK')
comm_pop6 <- comm_pop6 %>% mutate(communist10 = case_when(
  cntry %in% communist ~ 1,
  !(cntry %in% communist) ~ 0,
))

# recoding gender male = 1
comm_pop6 <- comm_pop6 %>% mutate(gndr = case_when(
  gndr == 1 ~ 1, # male
  gndr == 2 ~ 0  # female
)) 

# recoding ethnic minority yes = 1
comm_pop6 <- comm_pop6 %>% mutate(blgetmg = case_when(
  blgetmg == 1 ~ 1, # yes, belong to ethnic minor.
  blgetmg == 2 ~ 0  # no, does not belong to ethnic minor
)) 

# recording uemp3m scale
comm_pop6 <- comm_pop6 %>% mutate(uemp3m = case_when(
  uemp3m == 1 ~ 1, 
  uemp3m == 2 ~ 0  # 
)) 

comm_pop6 <- comm_pop6 %>% mutate(occup = case_when(
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

comm_pop6 <- comm_pop6 %>% mutate(jbspv = case_when(
  jbspv == 1 ~ 1, # yes, supervise others
  jbspv == 2 ~ 0  # no, don't supervise others
)) 

comm_pop6 <- comm_pop6 %>% mutate(mbtru = case_when(
  mbtru == 1 ~ 2, # yes, currently
  mbtru == 2 ~ 1, # yes, formerly
  mbtru == 3 ~ 0  # no, never 
)) 



comm_pop6 <- comm_pop6 %>% mutate(org_name = case_when(
  tporgwk == 1 ~ 'Central, local government', # 
  tporgwk == 2 ~ 'Other public sector', #
  tporgwk == 3 ~ 'State-owned enterprise',
  tporgwk == 4 ~ 'Private firm',
  tporgwk == 5 ~ 'Self-employed',
  tporgwk == 6 ~ 'Other' # no, never 
)) 


comm_pop6 <- comm_pop6 %>% mutate(domicil = case_when(
  domicil == 1 ~ 5, # big city
  domicil == 2 ~ 4, # suburb
  domicil == 3 ~ 3, # town, small city
  domicil == 4 ~ 2, # village
  domicil == 5 ~ 1  # farm, home in countryside
)) 


comm_pop6 <- comm_pop6 %>% mutate(domicil_name = case_when(
  domicil == 1 ~ 'Big city', # 
  domicil == 2 ~ 'Suburbs, outskirts of big city', # 
  domicil == 3 ~ 'Town or small city',
  domicil == 4 ~ 'Country village',
  domicil == 5 ~ 'Farm or home in countryside'
)) 


comm_pop6 <- comm_pop6 %>% mutate(self_emp10 = case_when(
  empName == 'selfemployed' ~ 1,
  empName == 'fambiz' ~ 2,
  empName == 'employee' ~ 0)) %>% 
  mutate(occup_level = trunc(isco08/1000)) %>% 
  filter(occup_level != 0) 

comm_pop6 <- comm_pop6 %>% 
  filter(rlgdgr < 11) # 10 very religious
comm_pop6


comm_pop6 <- comm_pop6 %>% mutate(psoe = case_when(
  org_name == 'Private firm' ~ 'private',
  org_name == 'State-owned enterprise' ~ 'SOE',
  org_name == 'Central, local government' | org_name == 'Other public sector' ~ 'public',
  TRUE ~ 'other'
))

comm_pop6 %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin()+
  facet_wrap(~cntry_name)


#################### 7

comm_9 <- ess %>% filter(cntry_name %in% c('Bulgaria', 'Czechia', 'Estonia', 'Croatia', 'Hungary', 'Lithuania', 'Latvia', 'Poland', 'Romania', 'Russia', 'Slovenia', 'Slovakia', 'Ukraine'))

comm_9 <- comm_9 %>% as.data.frame
# 2008 populist parties

comm_pop9 <- comm_pop9_raw %>% 
  filter(emplrel == 1 | emplrel == 2 | emplrel == 3)

comm_pop6$lrscale <- as.integer(comm_pop6$lrscale)
comm_pop6['empName'] <- NA

comm_pop6 <- comm_pop6 %>% mutate(empName = case_when(
  emplrel == 1 ~ 'employee',
  emplrel == 2 ~ 'selfemployed',
  emplrel == 3 ~ 'fambiz'
  
))

# also former communist countries
# Bulgaria, Czechia, Estonia*, Croatia, Hungary, Latvia, Lithuania, Montenegro, POland, Serbia, SLovenia, SLovakia 
communist <- c('BG','CZ','EE','HR','HU','LT','LV','ME','PL','RS','SI','SK')
comm_pop6 <- comm_pop6 %>% mutate(communist10 = case_when(
  cntry %in% communist ~ 1,
  !(cntry %in% communist) ~ 0,
))

# recoding gender male = 1
comm_pop6 <- comm_pop6 %>% mutate(gndr = case_when(
  gndr == 1 ~ 1, # male
  gndr == 2 ~ 0  # female
)) 

# recoding ethnic minority yes = 1
comm_pop6 <- comm_pop6 %>% mutate(blgetmg = case_when(
  blgetmg == 1 ~ 1, # yes, belong to ethnic minor.
  blgetmg == 2 ~ 0  # no, does not belong to ethnic minor
)) 

# recording uemp3m scale
comm_pop6 <- comm_pop6 %>% mutate(uemp3m = case_when(
  uemp3m == 1 ~ 1, 
  uemp3m == 2 ~ 0  # 
)) 

comm_pop6 <- comm_pop6 %>% mutate(occup = case_when(
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

comm_pop6 <- comm_pop6 %>% mutate(jbspv = case_when(
  jbspv == 1 ~ 1, # yes, supervise others
  jbspv == 2 ~ 0  # no, don't supervise others
)) 

comm_pop6 <- comm_pop6 %>% mutate(mbtru = case_when(
  mbtru == 1 ~ 2, # yes, currently
  mbtru == 2 ~ 1, # yes, formerly
  mbtru == 3 ~ 0  # no, never 
)) 



comm_pop6 <- comm_pop6 %>% mutate(org_name = case_when(
  tporgwk == 1 ~ 'Central, local government', # 
  tporgwk == 2 ~ 'Other public sector', #
  tporgwk == 3 ~ 'State-owned enterprise',
  tporgwk == 4 ~ 'Private firm',
  tporgwk == 5 ~ 'Self-employed',
  tporgwk == 6 ~ 'Other' # no, never 
)) 


comm_pop6 <- comm_pop6 %>% mutate(domicil = case_when(
  domicil == 1 ~ 5, # big city
  domicil == 2 ~ 4, # suburb
  domicil == 3 ~ 3, # town, small city
  domicil == 4 ~ 2, # village
  domicil == 5 ~ 1  # farm, home in countryside
)) 


comm_pop6 <- comm_pop6 %>% mutate(domicil_name = case_when(
  domicil == 1 ~ 'Big city', # 
  domicil == 2 ~ 'Suburbs, outskirts of big city', # 
  domicil == 3 ~ 'Town or small city',
  domicil == 4 ~ 'Country village',
  domicil == 5 ~ 'Farm or home in countryside'
)) 


comm_pop6 <- comm_pop6 %>% mutate(self_emp10 = case_when(
  empName == 'selfemployed' ~ 1,
  empName == 'fambiz' ~ 2,
  empName == 'employee' ~ 0)) %>% 
  mutate(occup_level = trunc(isco08/1000)) %>% 
  filter(occup_level != 0) 

comm_pop6 <- comm_pop6 %>% 
  filter(rlgdgr < 11) # 10 very religious
comm_pop6


comm_9 <- comm_9 %>% mutate(psoe = case_when(
  org_name == 'Private firm' ~ 'private',
  org_name == 'State-owned enterprise' ~ 'SOE',
  org_name == 'Central, local government' | org_name == 'Other public sector' ~ 'public',
  TRUE ~ 'other'
))

comm_pop6 %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin()+
  facet_wrap(~cntry_name)





#######################################################
comm_pop4a <- comm_pop4 %>% mutate(version = 4) 
comm_pop5a <- comm_pop5 %>% mutate(version = 5) 
comm_pop6a <- comm_pop6 %>% mutate(version = 6) 


names(comm_pop4a)
names(comm_pop5a)
names(comm_pop6a)


comm45 <- plyr::rbind.fill(comm_pop4, comm_pop5, comm_pop6)

comm45 %>% filter(psoe != 'other') %>% ggplot(aes(x = version, y = populist))+
  stat_summary_bin(aes(color = psoe))+
  facet_wrap(~cntry_name)

comm45 <- comm45 %>% mutate(soe = case_when(psoe == 'SOE' ~ 1, TRUE ~ 0))

comm_9 <- final %>% filter(cntry_name %in% c('Bulgaria', 'Czechia', 'Hungary', 'Poland', 'Slovakia'))


lm(populist ~  soe*I(cntry_name == 'Slovakia') + mbtru + hinctnta + estsz + agea + gndr + eduyrs + blgetmg + rlgdgr + sclmeet + sclact + version + cntry_name, comm45) %>% summary()

comm45 %>% filter(psoe != 'other') %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin(aes(color = cntry_name))+
  facet_wrap(~cntry_name, scales= 'free')

comm_merged %>% filter(psoe != 'other') %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin(aes(color = as.factor(version)))+
  facet_wrap(~cntry_name, scales= 'free')

comm_9 <- comm_9 %>% mutate(version = 9)
comm_merged <- plyr::rbind.fill(comm45, comm_9)

comm_merged <- comm_merged %>% mutate(private = case_when(psoe == 'private' ~ 1,
                                                          TRUE ~ 0))
comm_merged <- comm_merged %>% mutate(soe = case_when(psoe == 'SOE' ~ 1,
                                                          TRUE ~ 0))



#######

comm_merged %>% filter(! psoe %in% c('other', 'public')) %>%  ggplot(aes(x = estsz, y = populist))+
  stat_summary_bin(aes(color = psoe))+
  facet_wrap(~cntry_name)

comm_merged %>% ggplot(aes(x = version, y = populist))+
  stat_summary_bin(aes(color = cntry_name), geom = 'smooth')+
  ylim(0,1)


comm_merged %>% 
  ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin(aes(color = method))+
  #stat_summary_bin(aes(color = as.factor(version)))+
  facet_wrap(~cntry_name,nrow = 1)


comm_merged %>% filter(cntry_name %in% c('Czechia', 'Slovakia', 'Poland','Bulgaria')) %>% 
  group_by(cntry_name) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% View

comm_merged <- comm_merged %>% mutate(method = case_when(cntry_name %in% c('Hungary', 'Poland')~'bargain',
                                                         cntry_name %in% c('Czechia','Slovakia')~'markets'))

model1 <- lm(populist ~  method*soe + mbtru + hinctnta + estsz + agea + gndr + eduyrs + blgetmg + rlgdgr + as.factor(version) + (1|cntry_name), comm_merged)
model1 %>% summary


model4 <- lm(populist ~  method*soe + mbtru + hinctnta + estsz + agea + gndr + eduyrs + blgetmg + rlgdgr, comm_merged %>% filter(version == 4))
model4 %>% summary
model5 <- lm(populist ~  method*soe + mbtru + hinctnta + estsz + agea + gndr + eduyrs + blgetmg + rlgdgr, comm_merged %>% filter(version == 5))
model5 %>% summary
model6 <- lm(populist ~  method*soe + mbtru + hinctnta + estsz + agea + gndr + eduyrs + blgetmg + rlgdgr, comm_merged %>% filter(version == 6))
model6 %>% summary
model9 <- lm(populist ~  method*soe + mbtru + hinctnta + estsz + agea + gndr + eduyrs + blgetmg + rlgdgr, comm_merged %>% filter(version == 9))
model9 %>% summary

stargazer::stargazer(model1, model4, model5, model6, model9, type = 'text')







model.frame(model1) %>% as.vector
jtools::effect_plot(model = model1, pred =soe, data = model.frame(model1) %>% as.vector)


comm_merged %>% select(soe, mbtru, hinctnta, estsz, agea, gndr, eduyrs, blgetmg, rlgdgr, populist) %>% 
  cor(use = 'pairwise.complete.obs') %>% 
  corrplot()


names(comm_merged)

###########################
comm_indus <- comm_merged

industry_unloadings <- industry_unloadings %>% mutate(version = case_when(
  year == 2008 ~ 4,
  year == 2010 ~ 5,
  year == 2012 ~ 6,
  year == 2018 ~ 9
)) %>% as.data.frame

comm_indus %>% dim
comm_indus <- merge(comm_indus, industry_unloadings, by = c('region','version'), all.x=TRUE)

comm_indus <- comm_indus %>% filter(!is.na(region))
comm_indus %>% group_by(cntry_name) %>% 
  summarise_all(funs(mean(.,na.rm=TRUE))) %>% View

test %>% ggplot(aes(x = (tot_indust), y = soe))+
  geom_point()

test <- comm_indus %>% group_by(cntry_name, method, region) %>% 
  summarise_all(funs(mean(., na.rm=TRUE)))
test


cor(test$soe, (test$tot_indust), use = 'pairwise.complete.obs')



test
model1 <- lm(populist ~ method*soe + log(tot_indust) + mbtru + hinctnta + estsz + agea + gndr + eduyrs + blgetmg + rlgdgr, comm_indus)

model1 %>% summary
