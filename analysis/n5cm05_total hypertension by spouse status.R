rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")
# ,"hh_lengthmar
# Husbands --------
couples_svy <- couples  %>% 
  mutate(w_htn_h_diaghtn = case_when(w_htn == 1 & 
                                       h_diagnosed_bp == 1 & h_htn == 1 ~ 1,
                                     TRUE ~ 0),
         w_htn_h_undiaghtn = case_when(w_htn == 1 & 
                                       h_diagnosed_bp == 0 & h_htn == 1 ~ 1,
                                     TRUE ~ 0),
         w_htn_h_nohtn = case_when(w_htn == 1 & h_htn == 0 ~ 1,
                                     TRUE ~ 0),
         
         w_diaghtn_h_diaghtn = case_when(w_diagnosed_bp == 1 & 
                                           h_diagnosed_bp == 1 & h_htn == 1 ~ 1,
                                         TRUE ~ 0),
         w_diaghtn_h_undiaghtn = case_when(w_diagnosed_bp == 1 & 
                                             h_diagnosed_bp == 0 & h_htn == 1 ~ 1,
                                           TRUE ~ 0),
         w_diaghtn_h_nohtn = case_when(w_diagnosed_bp == 1 & 
                                         h_htn == 0 ~ 1,
                                       TRUE ~ 0),
         
         w_undiaghtn_h_diaghtn = case_when(w_diagnosed_bp == 0 & w_htn == 1 & 
                                             h_diagnosed_bp == 1 & h_htn == 1 ~ 1,
                                           TRUE ~ 0),
         w_undiaghtn_h_undiaghtn = case_when(w_diagnosed_bp == 0 & w_htn == 1 & 
                                               h_diagnosed_bp == 0 & h_htn == 1 ~ 1,
                                             TRUE ~ 0),
         w_undiaghtn_h_nohtn = case_when(w_diagnosed_bp == 0 & w_htn == 1 & 
                                           h_htn == 0 ~ 1,
                                         TRUE ~ 0),
         
         
         h_htn_w_diaghtn = case_when(h_htn == 1 & 
                                           w_diagnosed_bp == 1 & w_htn == 1 ~ 1,
                                         TRUE ~ 0),
         h_htn_w_undiaghtn = case_when(h_htn == 1 & 
                                             w_diagnosed_bp == 0 & w_htn == 1 ~ 1,
                                           TRUE ~ 0),
         h_htn_w_nohtn = case_when(h_htn == 1 & 
                                         w_htn == 0 ~ 1,
                                       TRUE ~ 0),
         
         h_diaghtn_w_diaghtn = case_when(h_diagnosed_bp == 1 & 
                                           w_diagnosed_bp == 1 & w_htn == 1 ~ 1,
                                         TRUE ~ 0),
         h_diaghtn_w_undiaghtn = case_when(h_diagnosed_bp == 1 & 
                                             w_diagnosed_bp == 0 & w_htn == 1 ~ 1,
                                           TRUE ~ 0),
         h_diaghtn_w_nohtn = case_when(h_diagnosed_bp == 1 & 
                                         w_htn == 0 ~ 1,
                                       TRUE ~ 0),
         
         h_undiaghtn_w_diaghtn = case_when(h_diagnosed_bp == 0 & h_htn == 1 & 
                                             w_diagnosed_bp == 1 & w_htn == 1 ~ 1,
                                           TRUE ~ 0),
         h_undiaghtn_w_undiaghtn = case_when(h_diagnosed_bp == 0 & h_htn == 1 & 
                                               w_diagnosed_bp == 0 & w_htn == 1 ~ 1,
                                             TRUE ~ 0),
         h_undiaghtn_w_nohtn = case_when(h_diagnosed_bp == 0 & h_htn == 1 & 
                                           w_htn == 0 ~ 1,
                                         TRUE ~ 0)
  ) %>% 
  mutate(w_diagstatus_htn = case_when(w_diagnosed_bp == 1 ~ "Diagnosed",
                                      w_diagnosed_bp == 0 & w_htn == 1 ~ "Undiagnosed",
                                      w_htn == 0 ~ "No Hypertension"),
         h_diagstatus_htn = case_when(h_diagnosed_bp == 1 ~ "Diagnosed",
                                      h_diagnosed_bp == 0 & h_htn == 1 ~ "Undiagnosed",
                                      h_htn == 0 ~ "No Hypertension")
  ) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# What is the burden of hypertension 

couples_svysummary <- svysummary(couples_svy,
                                 # c_vars = continuous_vars,
                                 p_vars = c("w_htn_h_diaghtn","w_htn_h_undiaghtn","w_htn_h_nohtn",
                                            "w_diaghtn_h_diaghtn","w_diaghtn_h_undiaghtn","w_diaghtn_h_nohtn",
                                            "w_undiaghtn_h_diaghtn","w_undiaghtn_h_undiaghtn","w_undiaghtn_h_nohtn",
                                            
                                            "h_htn_w_diaghtn","h_htn_w_undiaghtn","h_htn_w_nohtn",
                                            "h_diaghtn_w_diaghtn","h_diaghtn_w_undiaghtn","h_diaghtn_w_nohtn",
                                            "h_undiaghtn_w_diaghtn","h_undiaghtn_w_undiaghtn","h_undiaghtn_w_nohtn"
                                            
                                 ),
                                 # g_vars = grouped_vars
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

couples_svysummary %>% 
  mutate(sex = case_when(str_detect(variable,"^w") ~ "Women",
                         TRUE ~ "Men"),
         spousesex = case_when(str_detect(variable,"^w") ~ "Men",
                               TRUE ~ "Women"),
         diagnosis_status = case_when(str_detect(variable,"^(w|h)_htn") ~ "Total",
                                      str_detect(variable,"^(w|h)_diaghtn") ~ "Diagnosed",
                                      TRUE ~ "Undiagnosed"),
         
         spouse_diagnosis_status = case_when(str_detect(variable,"_diaghtn$") ~ "Diagnosed",
                                   str_detect(variable,"_undiaghtn$") ~ "Undiagnosed",
                                   TRUE ~ "No Hypertension")
  ) %>% 
  write_csv(.,"analysis/n5cm05_summary proportion of total diagnosed undiagnosed by spouse diagnosis.csv")


# Among women who were undiagnosed, how was it attributed by spouse's status ------
women_undiagnosed_svysummary <- couples_svy %>% 
  group_by(h_undiagnosed_bp,w_diagstatus_htn) %>% 
  summarize(prop = survey_prop(na.rm=TRUE,proportion = TRUE,vartype="ci")) %>% 
  rename(estimate = prop,
         lci = prop_low,
         uci = prop_upp) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.*100,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")")) %>% 
  dplyr::filter(h_undiagnosed_bp == 1) %>% 
    mutate(spouse_diagnosis_status = w_diagstatus_htn,
           spousesex = "Women",
           sex = "Men",
           diagnosis_status = "Undiagnosed")


men_undiagnosed_svysummary <- couples_svy %>% 
  group_by(w_undiagnosed_bp,h_diagstatus_htn) %>% 
  summarize(prop = survey_prop(na.rm=TRUE,proportion = TRUE,vartype="ci")) %>% 
  rename(estimate = prop,
         lci = prop_low,
         uci = prop_upp) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.*100,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")")) %>% 
  dplyr::filter(w_undiagnosed_bp == 1) %>% 
  mutate(spouse_diagnosis_status = h_diagstatus_htn,
         spousesex = "Men",
         sex = "Women",
         diagnosis_status = "Undiagnosed")


bind_rows(women_undiagnosed_svysummary ,
          men_undiagnosed_svysummary) %>% 
  ungroup() %>% 
  dplyr::select(-matches("^(w|h)")) %>% 
  write_csv(.,"analysis/n5cm05_summary undiagnosed hypertension by spousal status.csv")
