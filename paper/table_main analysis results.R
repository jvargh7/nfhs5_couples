
htn <- read_csv("analysis/n5cm01_htn poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension")
dm <- read_csv("analysis/n5cm02_dm poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Diabetes")

table_main <- bind_rows(htn,
                        dm) %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  dplyr::filter(!str_detect(iv,"^factor\\(state\\)")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(table_main,"paper/table_main analysis results.csv")

table_unadjusted <- bind_rows(htn,
                        dm) %>% 
  dplyr::filter(model %in% c("W0","H0")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(table_unadjusted,"paper/table_unadjusted analysis results.csv")
