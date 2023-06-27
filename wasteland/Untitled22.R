comm_new <- ess_model %>% filter(communist10 == 1) %>% 
  mutate(method_oldcode = case_when(
    cntry_name %in% c('Bulgaria', 'Slovakia', 'Lithuania','Serbia','Latvia','Estonia') ~ 'state',
    cntry_name %in% c('Hungary','Poland','Czechia', 'Slovenia','Croatia') ~ 'market',
    TRUE ~ 'other'
  ))


comm_new %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin()+
  facet_wrap(~method_oldcode)

small_comm <- comm_new %>% filter(cntry_name %in% c('Hungary','Poland','Czechia','Slovakia','Lithuania', 'Latvia','Estonia')) %>% 
  mutate(method_newcode = case_when(
    cntry_name %in% c('Hungary','Poland') ~ 'market',
    cntry_name %in% c('Czechia') ~ 'state.Czech',
    cntry_name %in% c('Slovakia') ~ 'state.Slovak',
    cntry_name %in% c('Lithuania', 'Latvia','Estonia') ~ 'soviet.Baltic'
  ))

small_comm %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin(aes(color = method_newcode))+
  facet_wrap(~cntry_name)

small_comm %>% ggplot(aes(x = psoe, y = populist))+
  stat_summary_bin(aes(color = method_newcode))+
  facet_wrap(~method_newcode)









