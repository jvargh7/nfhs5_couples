state_joint <- read_csv(file = "analysis/n5cm04_state summary of joint prevalence.csv")


state_joint %>% 
  dplyr::select(n5_state,variable,est_ci) %>% 
  pivot_wider(names_from=variable,values_from=est_ci) %>% 
  arrange(n5_state) %>% 
  dplyr::select(n5_state,h_dm,w_dm,dm_joint,
                h_htn,w_htn,htn_joint) %>% 
  write_csv(.,file="paper/table_marginal and joint prevalence.csv")
