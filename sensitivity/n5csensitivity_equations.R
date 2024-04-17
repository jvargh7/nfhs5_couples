w_covariates = "+ w_bmi + w_age + w_education_2 + w_education_3 + w_education_4 + w_tobacco_any + w_alcohol"
hs_covariates = "+ h_bmi + h_education_2 + h_education_3 + h_education_4 + h_tobacco_any + h_alcohol"

h_covariates = "+ h_bmi + h_age + h_education_2 + h_education_3 + h_education_4 + h_tobacco_any + h_alcohol"
ws_covariates = "+ w_bmi + w_education_2 + w_education_3 + w_education_4 + w_tobacco_any + w_alcohol"

hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + nmembers + hh_children + rural + factor(state) + w_consang_marriage"
#   + hh_children 

sw1dm <- paste0("w_dm ~ h_dm",w_covariates,hs_covariates,hh_covariates) %>% as.formula()
sh1dm <- paste0("h_dm ~ w_dm",h_covariates,ws_covariates,hh_covariates) %>% as.formula()

sw1htn <- paste0("w_htn ~ h_htn",w_covariates,hs_covariates,hh_covariates) %>% as.formula()
sh1htn <- paste0("h_htn ~ w_htn",h_covariates,ws_covariates,hh_covariates) %>% as.formula()


sw2dm <- paste0("w_glucose ~ h_glucose",w_covariates,hh_covariates) %>% as.formula()
sh2dm <- paste0("h_glucose ~ w_glucose",h_covariates,hh_covariates) %>% as.formula()

sw2htn <- paste0("w_sbp ~ h_sbp",w_covariates,hh_covariates) %>% as.formula()
sh2htn <- paste0("h_sbp ~ w_sbp",h_covariates,hh_covariates) %>% as.formula()

sw2bmi <- paste0("w_bmi ~ h_bmi ",w_covariates,hh_covariates) %>% str_replace_all(.,"\\+\\sw_bmi","") %>%  as.formula()
sh2bmi <- paste0("h_bmi ~ w_bmi ",h_covariates,hh_covariates) %>% str_replace_all(.,"\\+\\sh_bmi","") %>% as.formula()


# Lists for models --------

overall_sw1dm = list()
overall_sh1dm = list()

overall_sw1htn = list()
overall_sh1htn = list()

overall_sw2dm = list()
overall_sh2dm = list()

overall_sw2htn = list()
overall_sh2htn = list()

overall_sw2bmi = list()
overall_sh2bmi = list()
