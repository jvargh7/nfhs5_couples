

read_csv(paste0(path_couples_folder,"/working/nfhs5 concordance/india association of partner status long.csv")) %>% 
  mutate(coef_ci = paste0(round(estimate,2)," (",
                          round(estimate/exp(1.96*std.error),2) , ",",
                          round(estimate*exp(1.96*std.error),2), ")"
  )) %>% 
  mutate(term = case_when(str_detect(term,"(_dm|_htn)") ~ "partner status",
                          TRUE ~ as.character(term))) %>% 
  dplyr::select(term,partner,disease,coef_ci) %>% 
  pivot_wider(names_from=c(partner,disease),values_from=coef_ci) %>% 
  write_csv(.,paste0(path_couples_folder,"/working/nfhs5 concordance/india association of partner status wide.csv"))

read_csv(paste0(path_couples_folder,"/working/nfhs5 concordance/strata specific spousal status associations long.csv")) %>% 
  mutate(coef_ci = paste0(round(estimate,2)," (",
                          round(estimate/exp(1.96*std.error),2) , ",",
                          round(estimate*exp(1.96*std.error),2), ")"
  )) %>% 
  mutate(term = case_when(str_detect(term,
                                     "(_dm|_htn)") ~ "partner status",
                          TRUE ~ as.character(term))) %>% 
  dplyr::select(strata,replacement,term,partner,disease,coef_ci) %>% 
  pivot_wider(names_from=c(partner,disease),
              values_from=coef_ci) %>% 
  write_csv(.,paste0(path_couples_folder,"/working/nfhs5 concordance/strata specific spousal status associations wide.csv"))