
pooled_analytic <- function(mi_df){
  w_covariates = c("w_htn","w_dm","w_bmi","w_age","w_education_2", "w_education_3", "w_education_4", "w_tobacco_any", "w_alcohol")
  h_covariates = c("h_htn","h_dm","h_bmi", "h_age", "h_education_2", "h_education_3", "h_education_4", "h_tobacco_any", "h_alcohol")
  replace_covariates = c("spouse_htn","spouse_dm","spouse_bmi", "spouse_age", "spouse_education_2", "spouse_education_3", "spouse_education_4", "spouse_tobacco_any", "spouse_alcohol")
  spouse_covariates = c("spouse_htn","spouse_dm","spouse_bmi", "spouse_age", "spouse_education_2", "spouse_education_3", "spouse_education_4", "spouse_tobacco_any", "spouse_alcohol")
  hh_covariates = c("hh_low", "hh_medium", "hh_high", "hh_highest", "nmembers", "hh_children", "rural", "religion_muslim", "religion_other", "caste", "state")
  id_vars = c("psu","state","sampleweight","cluster","hhid","linenumber","spouse_id")
  
  wife_dm = mi_df %>% 
    dplyr::select(one_of(id_vars),one_of(w_covariates),one_of(h_covariates),one_of(hh_covariates)) %>% 
    rename_at(vars(w_covariates),~str_replace(.,"^w_","own_")) %>% 
    rename_at(vars(h_covariates),~str_replace(.,"^h_","spouse_")) %>% 
    mutate(husband = 0,
           household = paste0(sprintf("%05d",cluster),sprintf("%02d",hhid)))
  
  husband_dm = mi_df %>% 
    dplyr::select(one_of(id_vars),one_of(w_covariates),one_of(h_covariates),one_of(hh_covariates)) %>% 
    rename_at(vars(h_covariates),~str_replace(.,"^h_","own_")) %>% 
    rename_at(vars(w_covariates),~str_replace(.,"^w_","spouse_")) %>% 
    mutate(husband = 1,
           household = paste0(sprintf("%05d",cluster),sprintf("%02d",hhid)))
  
  pooled_df = bind_rows(wife_dm,
                        husband_dm)
  
  return(pooled_df)
  

}
