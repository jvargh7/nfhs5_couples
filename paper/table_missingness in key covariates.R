table1 <- read_csv("analysis/summary table of analytic sample.csv") 

tab_missing = table1 %>% 
  distinct(variable,n) %>% 
  mutate(
         missing = max(n,na.rm=TRUE) - n,
         prop = n/max(n,na.rm=TRUE)) %>% 
  mutate(missing = paste0(missing," (",round((1-prop)*100,1),"%)")) %>% 
  dplyr::select(-prop,-n) %>%
  mutate(strata = case_when(str_detect(variable,"^h_") ~ "Husbands",
                            TRUE ~ "Wives"),
         variable = str_replace(variable,"^(h|w)_","")) %>% 
  pivot_wider(names_from=c("strata"),values_from=missing)

write_csv(tab_missing,"paper/table_missingness in key covariates.csv")
