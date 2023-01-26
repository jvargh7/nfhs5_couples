

summary_table <- bind_rows(
  read_csv("overall/summary table of analytic sample.csv") %>% mutate(dataset = "Analytic"),
  read_csv("overall/summary table of excluded sample.csv") %>% mutate(dataset = "Excluded")) %>%
  mutate(strata = case_when(str_detect(variable,"^h_") ~ "Husbands",
                            TRUE ~ "Wives"),
         variable = str_replace(variable,"^(h|w)_","")) %>% 
  dplyr::select(strata,dataset,variable,group,est_ci) %>% 
  pivot_wider(names_from=c("strata","dataset"),values_from=est_ci)

write_csv(summary_table,"paper/table_descriptive characteristics.csv")
