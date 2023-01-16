own_covariates = "+ own_bmi + own_age + own_education_2 + own_education_3 + own_education_4 + own_tobacco_any + own_alcohol"
hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + nmembers + hh_children + rural + factor(state)"
#   + hh_children 

p1dm <- paste0("own_dm ~ spouse_dm + husband",own_covariates,hh_covariates) %>% as.formula()
p2dm <- paste0("own_dm ~ spouse_dm*husband",own_covariates,hh_covariates) %>% as.formula()

p1htn <- paste0("own_htn ~ spouse_htn + husband",own_covariates,hh_covariates) %>% as.formula()
p2htn <- paste0("own_htn ~ spouse_htn*husband",own_covariates,hh_covariates) %>% as.formula()



# Lists for models --------

overall_p1dm = list()
overall_p2dm = list()


overall_p1htn = list()
overall_p2htn = list()
