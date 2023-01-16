
htn <- read_csv("overall/n5cm01_htn poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  mutate(outcome = "Hypertension")
dm <- read_csv("overall/n5cm02_dm poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  mutate(outcome = "Diabetes")

table_main <- bind_rows(htn,
                        dm) %>% 
  dplyr::filter(!str_detect(iv,"^factor\\(state\\)")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(table_main,"paper/table_main analysis results.csv")
