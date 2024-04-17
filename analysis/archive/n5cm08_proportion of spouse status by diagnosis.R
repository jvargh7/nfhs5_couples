rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")
# ,"hh_lengthmar
# Husbands --------
couples_svy <- couples  %>% 
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

# Among husbands who were ______, what is the proportion of wives who were ______

wives_svysummary <- svysummary(couples_svy,
                                  # c_vars = continuous_vars,
                                  id_vars = c("w_diagstatus_htn"),
                                  g_vars = c("h_diagstatus_htn")
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

husbands_svysummary <- svysummary(couples_svy,
                                  # c_vars = continuous_vars,
                                  id_vars = c("h_diagstatus_htn"),
                                  g_vars = c("w_diagstatus_htn")
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))
bind_rows(wives_svysummary %>% mutate(sex = "Wife") %>% rename(individual_diagstatus = w_diagstatus_htn),
          husbands_svysummary %>% mutate(sex = "Husband")%>% rename(individual_diagstatus = h_diagstatus_htn)) %>% 
  write_csv(.,"analysis/summary table of proportion spouse status by diagnosis.csv")

df = read_csv("analysis/summary table of proportion spouse status by diagnosis.csv")
