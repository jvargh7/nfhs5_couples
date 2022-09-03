# Flowchart Step 3
partner_df %>% distinct(v001,v002,.keep_all = TRUE) %>% View()

partner_df <- readRDS(paste0(path_couples_folder,"/working/nfhs5 couples.RDS")) %>% 
  dplyr::filter(f_age>=18,m_age>=21) %>% 
  mutate(weight = mv005/10^6) %>% 
  dplyr::rename(state = mv024,
                cluster = mv001,
                f_rural = v025) %>% 
  dplyr::select(-mv005) %>% 
  dplyr::select(state,cluster,weight,
                f_wealth,
                f_rural,
                # m_caste,m_religion,
                m_age,m_eduyr,
                m_alcohol,m_smoke,
                m_bmi,
                m_children,
                # m_glucose,
                # m_sbp,
                # m_dbp,
                m_dm,
                m_htn,
                m_self_diabetes,
                m_self_hypertension,
                
                f_caste,f_religion,
                
                f_age,f_eduyr,
                f_alcohol,f_smoke,
                f_bmi,
                f_children,
                # f_glucose,
                # f_sbp,
                # f_dbp,
                f_dm,
                f_htn,
                f_self_diabetes,
                f_self_hypertension,v001,v002
  ) %>% 
  mutate(f_wealth = as.character(f_wealth),
         f_rural = f_rural - 1) %>%
  mutate_at(vars(f_bmi,m_bmi),function(x) x/100) %>% 
  mutate_at(vars(m_self_diabetes,m_self_hypertension,
                 f_self_diabetes,f_self_hypertension
  ),function(x) case_when(is.na(x) ~ 0,
                          TRUE ~ x)) %>% 
  
  mutate_at(vars(f_age,m_age,f_eduyr,m_eduyr),~as.numeric(.)) %>% 
  mutate(c_age = (f_age + m_age)/2) %>% 
  
  mutate(s_age_lt40 = case_when(c_age < 40 ~ 1,
                                TRUE ~ 0),
         s_age_ge40 = case_when(c_age >= 40 ~ 1,
                                TRUE ~ 0),
         s_wealth_ltq5 = case_when(f_wealth %in% c("1","2","3","4") ~ 1,
                                   TRUE ~ 0),
         s_wealth_q5 = case_when(f_wealth %in% c("1","2","3","4") ~ 0,
                                 TRUE ~ 1),
         
         s_region_rural = case_when(f_rural == 1 ~ 1,
                                    TRUE ~ 0),
         s_region_urban = case_when(f_rural == 1 ~ 0,
                                    TRUE ~ 1),
         
         s_religion_hindu = case_when(f_religion == "Hindu" ~ 1,
                                      TRUE ~ 0),
         s_religion_muslim = case_when(f_religion == "Muslim" ~ 1,
                                      TRUE ~ 0),
         s_religion_other = case_when(f_religion == "Other" ~ 1,
                                      TRUE ~ 0))

dm_df <- partner_df %>% 
  dplyr::select(-f_htn,-m_htn)  %>% 
  dplyr::filter(complete.cases(.)) 

# Flowchart Step 4.1
# dm_df %>% distinct(v001,v002,.keep_all = TRUE) %>% View()

dm_svydesign <- dm_df %>%  
  as_survey_design(.data = .,
                   ids = cluster,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

htn_df <- partner_df %>% 
  dplyr::select(-f_dm,-m_dm)  %>% 
  dplyr::filter(complete.cases(.))

htn_df %>% distinct(v001,v002,.keep_all = TRUE) %>% View()


htn_svydesign <- htn_df %>%  
  as_survey_design(.data = .,
                   ids = cluster,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

