rm(list=ls());gc();source(".Rprofile")

require(srvyr)
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")

# Proportion of diagnosed hypertension in Individual by Spouse's status

couples_svy <- couples %>% 
  mutate(w_diaghtn_h_diaghtn = case_when(w_diagnosed_bp == 1 & 
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
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


couples_svysummary <- svysummary(couples_svy,
                                 # c_vars = continuous_vars,
                                 p_vars = c("w_diaghtn_h_diaghtn","w_diaghtn_h_undiaghtn","w_diaghtn_h_nohtn",
                                            "w_undiaghtn_h_diaghtn","w_undiaghtn_h_undiaghtn","w_undiaghtn_h_nohtn",
                                            
                                            "h_diaghtn_w_diaghtn","h_diaghtn_w_undiaghtn","h_diaghtn_w_nohtn",
                                            "h_undiaghtn_w_diaghtn","h_undiaghtn_w_undiaghtn","h_undiaghtn_w_nohtn"
                                            
                                            ),
                                 # g_vars = grouped_vars
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

couples_svysummary %>% 
  mutate(sex = case_when(str_detect(variable,"^w") ~ "Wife",
                         TRUE ~ "Husband"),
         diagnosis_status = case_when(str_detect(variable,"^(w|h)_diaghtn") ~ "Diagnosed",
                                                 TRUE ~ "Undiagnosed"),
         
         spouse_status = case_when(str_detect(variable,"_diaghtn$") ~ "Diagnosed",
                                   str_detect(variable,"_undiaghtn$") ~ "Undiagnosed",
                                   TRUE ~ "No Hypertension")
         ) %>% 
  write_csv(.,"analysis/summary proportion of diagnosed undiagnosed by spouse diagnosis.csv")
