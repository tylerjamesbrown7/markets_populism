## analysis and mapping of Slovak data
library(tidyverse)
slov <- slovak_data_11may
slov %>% dim

slovak_unemployment <- slovak_unemployment %>% rename(okres = name)

merge(slov, slovak_unemployment, by = 'okres', all.x = TRUE) %>% View





