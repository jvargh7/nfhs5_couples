require(srvyr)
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")

couples_svy <- couples  %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

husbands_svysummary <- svysummary(couples_svy,
                                  # c_vars = continuous_vars,
                                  p_vars = c("h_diagnosed_bp"),
                                  id_vars = c("h_htn")
                                  # g_vars = grouped_vars
) %>% 
  dplyr::filter(h_htn == 1) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.*100,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

wives_svysummary <- svysummary(couples_svy,
                                  # c_vars = continuous_vars,
                                  p_vars = c("w_diagnosed_bp"),
                                  id_vars = c("w_htn")
                                  # g_vars = grouped_vars
) %>% 
  dplyr::filter(w_htn == 1) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.*100,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))
