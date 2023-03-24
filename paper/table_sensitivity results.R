
RR <- read_csv("sensitivity/n5csensitivity01_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("SW1","SH1")) %>% 
  dplyr::filter(!str_detect(iv,"factor\\(state\\)")) %>% 
  mutate(var_type = case_when(model == "SW1" & str_detect(iv,"^h_") ~ "Spouse",
                              model == "SH1" & str_detect(iv,"^w_") ~ "Spouse",
                              TRUE ~ "Main")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,var_type,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(RR,"paper/table_RR spouse covariates results.csv")


Coef <- read_csv("sensitivity/n5csensitivity01_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("SW2","SH2")) %>% 
  dplyr::filter(!str_detect(iv,"factor\\(state\\)")) %>% 
  mutate(var_type = case_when(model == "SW2" & str_detect(iv,"^h_") ~ "Spouse",
                              model == "SH2" & str_detect(iv,"^w_") ~ "Spouse",
                              TRUE ~ "Main")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,var_type,Coefficient) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="Coefficient")


write_csv(Coef,"paper/table_coefficients for continuous variables.csv")


ImputedOutcome <- read_csv("sensitivity/n5csensitivity02_dm and htn poisson regression with imputed outcomes.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  dplyr::filter(!str_detect(iv,"factor\\(state\\)")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(ImputedOutcome,"paper/table_RR with imputed outcomes.csv")
