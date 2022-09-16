library(srvyr)

source("C:/code/external/nfhs5_couples/preprocessing/cp04_concordance datasets.R")

library(survey)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

strata_vars <- names(partner_df)[str_detect(names(partner_df),"^s_")]
var_to_replace <- c("\\s\\+\\sc_age",
                    "\\s\\+\\sc_age",
                    "null", # wealth 1 to 4
                    "\\s\\+\\sf_wealth",
                    "\\s\\+\\sf_rural",
                    "\\s\\+\\sf_rural",
                    "\\s\\+\\sf_religion",
                    "\\s\\+\\sf_religion",
                    "\\s\\+\\sf_religion")
names(var_to_replace) <- strata_vars


strata_associations <- map_dfr(strata_vars,
                               function(s){
                                 print(s);
                                 x_formula = " + c_age + f_eduyr + f_alcohol + f_smoke + f_bmi +
                                                     f_caste + f_wealth + f_rural + f_religion + 
                                                     m_eduyr + m_alcohol + m_smoke + m_bmi + factor(state)"
                                 
                                 x_formula_new = str_replace(x_formula,
                                                             var_to_replace[[s]],
                                                             "")
                                 
                                 dm_strata_design <- dm_df %>% 
                                   dplyr::rename(strata = s) %>% 
                                   dplyr::filter(strata == 1) %>% 
                                   as_survey_design(.data = .,
                                                    ids = cluster,strata = state,
                                                    weight = weight,
                                                    nest = TRUE,
                                                    variance = "YG",pps = "brewer")
                                 
                                 htn_strata_design <- htn_df %>% 
                                   dplyr::rename(strata = s) %>% 
                                   dplyr::filter(strata == 1) %>% 
                                   as_survey_design(.data = .,
                                                    ids = cluster,strata = state,
                                                    weight = weight,
                                                    nest = TRUE,
                                                    variance = "YG",pps = "brewer")
                                 
                                 wife_dm <- svyglm(paste0("f_dm ~ m_dm",x_formula_new,"+ f_children") %>% as.formula(.),
                                                   design=dm_strata_design,
                                                   family = poisson())
                                 wife_htn <- svyglm(paste0("f_htn ~ m_htn",x_formula_new,"+ f_children") %>% as.formula(.),
                                                    design=htn_strata_design,
                                                    family = poisson())
                                 husband_dm <- svyglm(paste0("m_dm ~ f_dm",x_formula_new,"+ m_children") %>% as.formula(.),
                                                      design=dm_strata_design,
                                                      family = poisson())
                                 husband_htn <- svyglm(paste0("m_htn ~ f_htn",x_formula_new,"+ m_children") %>% as.formula(.),
                                                       design=htn_strata_design,
                                                       family = poisson())
                                 
                                 
                                 bind_rows(
                                   broom::tidy(wife_dm,exponentiate = TRUE) %>% 
                                     mutate(partner = "Wife",
                                            disease = "Diabetes"),
                                   broom::tidy(wife_htn,exponentiate = TRUE) %>% 
                                     mutate(partner = "Wife",
                                            disease = "Hypertension"),
                                   broom::tidy(husband_dm,exponentiate = TRUE) %>% 
                                     mutate(partner = "Husband",
                                            disease = "Diabetes"),
                                   broom::tidy(husband_htn,exponentiate = TRUE) %>% 
                                     mutate(partner = "Husband",
                                            disease = "Hypertension")
                                 ) %>% 
                                   mutate(strata = s,
                                          replacement = var_to_replace[[s]])  %>% 
                                   return(.)
                                 
                               })

write_csv(strata_associations,paste0(path_couples_folder,"/working/nfhs5 concordance/strata specific spousal status associations long.csv"))



  
