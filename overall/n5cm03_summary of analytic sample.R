require(srvyr)
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")


continuous_vars <- c(paste0(rep(c("w_","h_"),each=10),
                            c("sbp","dbp","weight","height","glucose",
                              "bmi","waistcircumference","hipcircumference",
                              "age","eduyr")),
                     "nmembers","hh_children")
# ,"hh_lengthmar"
proportion_vars <- c(paste0(rep(c("w_","h_"),each=9),
                            c("diagnosed_bp","treated_bp",
                              "diagnosed_dm","treated_dm",
                              
                              "tobacco_any","alcohol",
                              "dm","htn")),"rural","dm_joint","htn_joint")

grouped_vars <- c("w_education","w_bmi_category","h_bmi_category","h_education","caste","religion","wealthq","swealthq_ur")


# Analytic sample --------
couples_svy <- couples  %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

couples_svysummary <- svysummary(couples_svy,
                                 c_vars = continuous_vars,
                                 p_vars = proportion_vars,
                                 g_vars = grouped_vars) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

couples_count <- couples %>% 
  summarize_at(vars(one_of(c(continuous_vars,
                             proportion_vars,
                             grouped_vars))),
               list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


left_join(couples_svysummary,
          couples_count,
          by="variable") %>% 
  write_csv(.,"overall/summary table of analytic sample.csv")

# Analytic sample --------
excluded_svy <- excluded  %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

excluded_svysummary <- svysummary(excluded_svy,
                                 c_vars = continuous_vars,
                                 p_vars = proportion_vars,
                                 g_vars = grouped_vars) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

excluded_count <- excluded %>% 
  summarize_at(vars(one_of(c(continuous_vars,
                             proportion_vars,
                             grouped_vars))),
               list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


left_join(excluded_svysummary,
          excluded_count,
          by="variable") %>% 
  write_csv(.,"overall/summary table of excluded sample.csv")







