
htn <- read_csv("sensitivity/n5cs01_htn poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  mutate(outcome = "Hypertension")
dm <- read_csv("sensitivity/n5cs02_dm poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  mutate(outcome = "Diabetes")

table_spouse_x <- bind_rows(htn,
                        dm) %>% 
  dplyr::filter(!str_detect(iv,"factor\\(state\\)")) %>% 
  mutate(var_type = case_when(model == "W1" & str_detect(iv,"^h_") ~ "Spouse",
                              model == "H1" & str_detect(iv,"^w_") ~ "Spouse",
                              TRUE ~ "Main")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,var_type,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(table_spouse_x,"paper/table_spouse covariates results.csv")
