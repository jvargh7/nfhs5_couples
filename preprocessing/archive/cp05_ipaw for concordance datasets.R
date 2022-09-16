# IPAW datasets ----------

ipaw_dm_df <- partner_df %>% 
  dplyr::select(-f_htn,-m_htn)  %>% 
  dplyr::filter(!is.na(f_dm),!is.na(m_dm),!is.na(f_bmi),!is.na(m_bmi)) %>% 
  mutate(na_caste = case_when(is.na(f_caste) ~ 1,
                              TRUE ~ 0)) 
ipaw_htn_df <- partner_df %>% 
  dplyr::select(-f_dm,-m_dm)  %>% 
  dplyr::filter(!is.na(f_htn),!is.na(m_htn),!is.na(f_bmi),!is.na(m_bmi)) %>% 
  mutate(na_caste = case_when(is.na(f_caste) ~ 1,
                              TRUE ~ 0)) 
ipaw_dm_df$ipaw <- ipaw_dm_df %>%  
  as_survey_design(.data = .,
                   ids = cluster,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") %>% 
  svyglm(formula = na_caste ~ m_dm + f_dm + c_age + f_eduyr + f_alcohol + f_smoke + f_bmi + f_children +
           f_wealth + f_rural + f_religion + 
           # m_age + 
           m_eduyr + m_alcohol + m_smoke + m_bmi + factor(state),design = .,family = binomial()) %>% 
  predict(type="response") %>% as.numeric(.)

ipaw_htn_df$ipaw <- ipaw_htn_df %>%  
  as_survey_design(.data = .,
                   ids = cluster,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") %>% 
  svyglm(formula = na_caste ~ m_htn + f_htn + c_age + f_eduyr + f_alcohol + f_smoke + f_bmi + f_children +
           f_wealth + f_rural + f_religion + 
           # m_age + 
           m_eduyr + m_alcohol + m_smoke + m_bmi + factor(state),design = .,family = binomial()) %>% 
  predict(type="response") %>% as.numeric(.)

ipaw_dm_svydesign <- ipaw_dm_df %>% 
  mutate(weight2 = weight/(1-ipaw)) %>% 
  dplyr::filter(na_caste == 0) %>%  
  as_survey_design(.data = .,
                   ids = cluster,strata = state,
                   weight = weight2,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") 

ipaw_htn_svydesign <- ipaw_htn_df %>% 
  mutate(weight2 = weight/(1-ipaw)) %>% 
  dplyr::filter(na_caste == 0) %>%  
  as_survey_design(.data = .,
                   ids = cluster,strata = state,
                   weight = weight2,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")
