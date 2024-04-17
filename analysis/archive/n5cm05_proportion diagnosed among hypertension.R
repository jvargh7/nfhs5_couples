rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")
# ,"hh_lengthmar
# Husbands --------
husbands_htn_svy <- couples  %>% 
  dplyr::filter(h_htn == 1) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


husbands_svysummary_total <- svysummary(husbands_htn_svy,
                                  # c_vars = continuous_vars,
                                  p_vars = c("h_diagnosed_bp","h_treated_bp"),
                                  # id_vars = c("w_htn","w_diagnosed_bp")
                                  # g_vars = grouped_vars
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

husbands_svysummary <- svysummary(husbands_htn_svy,
                                 # c_vars = continuous_vars,
                                 p_vars = c("h_diagnosed_bp","h_treated_bp"),
                                 id_vars = c("w_htn","w_diagnosed_bp")
                                 # g_vars = grouped_vars
                                 ) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

# Wives --------
wives_htn_svy <- couples  %>% 
  dplyr::filter(w_htn == 1) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

wives_svysummary_total <- svysummary(wives_htn_svy,
                                  # c_vars = continuous_vars,
                                  p_vars = c("w_diagnosed_bp","w_treated_bp"),
                                  # id_vars = c("h_htn","h_diagnosed_bp")
                                  # g_vars = grouped_vars
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


wives_svysummary <- svysummary(wives_htn_svy,
                               # c_vars = continuous_vars,
                               p_vars = c("w_diagnosed_bp","w_treated_bp"),
                               id_vars = c("h_htn","h_diagnosed_bp")
                               # g_vars = grouped_vars
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

bind_rows(wives_svysummary %>% rename(spouse_htn = h_htn,
                                      spouse_diagnosed_bp = h_diagnosed_bp) %>% mutate(sex = "Wife"),
          wives_svysummary_total %>% mutate(sex = "Wife"),
          husbands_svysummary %>% rename(spouse_htn = w_htn,
                                         spouse_diagnosed_bp = w_diagnosed_bp) %>% mutate(sex = "Husband"),
          husbands_svysummary_total %>% mutate(sex = "Husband")) %>% 
  write_csv(.,"analysis/summary table of proportion diagnosed and treated among hypertension.csv")

