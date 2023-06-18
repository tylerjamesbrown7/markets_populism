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

