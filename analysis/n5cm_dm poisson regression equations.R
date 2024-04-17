w_covariates = "+ w_bmi + w_age + w_education_2 + w_education_3 + w_education_4 + w_tobacco_any + w_alcohol"
h_covariates = "+ h_bmi + h_age + h_education_2 + h_education_3 + h_education_4 + h_tobacco_any + h_alcohol"
hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + nmembers + hh_children + rural + factor(state) + w_consang_marriage"
#   + hh_children 

w0 <- paste0("w_dm ~ h_dm") %>% as.formula()
h0 <- paste0("h_dm ~ w_dm") %>% as.formula()

w1 <- paste0("w_dm ~ h_dm",w_covariates,hh_covariates) %>% as.formula()
h1 <- paste0("h_dm ~ w_dm",h_covariates,hh_covariates) %>% as.formula()

w2 <- paste0("w_dm ~ h_dm*w_ge40",w_covariates,hh_covariates) %>% str_replace(.,"\\+ w_age ","") %>% as.formula()
h2 <- paste0("h_dm ~ w_dm*h_ge40",h_covariates,hh_covariates) %>% str_replace(.,"\\+ h_age ","") %>% as.formula()

w3 <- paste0("w_dm ~ h_dm*w_education_2 + h_dm*w_education_3 + h_dm*w_education_4",w_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ w_education_2 \\+ w_education_3 \\+ w_education_4","") %>% as.formula()
h3 <- paste0("h_dm ~ w_dm*h_education_2 + w_dm*h_education_3 + w_dm*h_education_4",h_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ h_education_2 \\+ h_education_3 \\+ h_education_4","") %>% as.formula()

w4 <- paste0("w_dm ~ h_dm*rural",w_covariates,hh_covariates) %>% str_replace(.,"\\+ residence ","") %>% as.formula()
h4 <- paste0("h_dm ~ w_dm*rural",h_covariates,hh_covariates) %>% str_replace(.,"\\+ residence ","") %>% as.formula()


w5 <- paste0("w_dm ~ h_dm*hh_low + h_dm*hh_medium + h_dm*hh_high + h_dm*hh_highest",w_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ hh_low \\+ hh_medium \\+ hh_high \\+ hh_highest","") %>% as.formula()
h5 <- paste0("h_dm ~ w_dm*hh_low + w_dm*hh_medium + w_dm*hh_high + w_dm*hh_highest",h_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ hh_low \\+ hh_medium \\+ hh_high \\+ hh_highest","") %>% as.formula()

w6 <- paste0("w_dm ~ h_dm*both_general + h_dm*both_obc + h_dm*both_scst",w_covariates,hh_covariates) %>% as.formula()
h6 <- paste0("h_dm ~ w_dm*both_general + w_dm*both_obc + w_dm*both_scst",h_covariates,hh_covariates) %>% as.formula()



# Lists for models --------

overall_w0 = list()
overall_h0 = list()

overall_w1 = list()
overall_h1 = list()

overall_w2 = list()
overall_h2 = list()

overall_w3 = list()
overall_h3 = list()

overall_w4 = list()
overall_h4 = list()

overall_w5 = list()
overall_h5 = list()

overall_w6 = list()
overall_h6 = list()

