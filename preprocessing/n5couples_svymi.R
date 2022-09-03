source("preprocessing/n5couples_analytic sample.R")


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
                              "dm","htn")),"rural")

grouped_vars <- c("w_education","h_education","caste","religion","wealthq","swealthq_ur")

require(survey)
require(mice)

before_imputation <- couples %>% 
  dplyr::select(cluster,hhid,linenumber,spouse_id,
                strata,state,psu,sampleweight,
                interview,phase,
                
                one_of(continuous_vars),one_of(proportion_vars),one_of(grouped_vars)) %>% 
  # mutate_at(vars(state,w_moderate_pa,w_vigorous_pa,
  #                h_moderate_pa,h_vigorous_pa,
  #                h_heavydrinker,w_heavydrinker,
  #                residence,hh_lengthmar),~as.numeric(.)) %>% 
  # Step 1: One hot encoding
  mutate(
    w_education_2 = case_when(w_education == "Primary" ~ 1,
                              !is.na(w_education) ~ 0,
                              TRUE ~ NA_real_),
    w_education_3 = case_when(w_education == "Secondary" ~ 1,
                              !is.na(w_education) ~ 0,
                              TRUE ~ NA_real_),
    w_education_4 = case_when(w_education == "Higher" ~ 1,
                              !is.na(w_education) ~ 0,
                              TRUE ~ NA_real_),
    
    
    h_education_2 = case_when(h_education == "Primary" ~ 1,
                              !is.na(h_education) ~ 0,
                              TRUE ~ NA_real_),
    h_education_3 = case_when(h_education == "Secondary" ~ 1,
                              !is.na(h_education) ~ 0,
                              TRUE ~ NA_real_),
    h_education_4 = case_when(h_education == "Higher" ~ 1,
                              !is.na(h_education) ~ 0,
                              TRUE ~ NA_real_),
    
    w_ge40 = case_when(w_age >= 40 ~ 1,
                       TRUE ~ 0),
    h_ge40 = case_when(h_age >= 40 ~ 1,
                       TRUE ~ 0),
    
    hh_low = case_when(wealthq == "Low" ~ 1,
                       TRUE ~ 0),
    hh_medium = case_when(wealthq == "Medium" ~ 1,
                          TRUE ~ 0),
    hh_high = case_when(wealthq == "High" ~ 1,
                        TRUE ~ 0),
    hh_highest = case_when(wealthq == "Highest" ~ 1,
                           TRUE ~ 0),
    
    religion_muslim = case_when(religion == "Muslim" ~ 1,
                                TRUE ~ 0),
    religion_other = case_when(religion == "Other" ~ 1,
                               TRUE ~ 0)
    
    
  ) %>% 
  # Step 2: Modeling interactions
  mutate(w_htn_rural = rural*w_htn,
         h_htn_rural = rural*h_htn,
         
         # Since exposure is w_htn and effect modifier is husband's education
         w_htn_h_education_2 = h_education_2*w_htn,
         w_htn_h_education_3 = h_education_3*w_htn,
         w_htn_h_education_4 = h_education_4*w_htn,
         
         h_htn_w_education_2 = w_education_2*h_htn,
         h_htn_w_education_3 = w_education_3*h_htn,
         h_htn_w_education_4 = w_education_4*h_htn,
         
         # Since exposure is w_htn and effect modifier is husband's age category (<65 vs >=65)
         w_htn_h_ge40 = h_ge40*w_htn,
         h_htn_w_ge40 = w_ge40*h_htn,
         
         # # Since exposure is w_htn and effect modifier is length of marriage (<10 vs >=10)
         # w_htn_hh_lengthmar_ge10 = hh_lengthmar_ge10*w_htn,
         # h_htn_hh_lengthmar_ge10 = hh_lengthmar_ge10*h_htn,
         
         # Since exposure is w_htn and effect modifier is household wealth quintile
         w_htn_hh_low = w_htn*hh_low,
         w_htn_hh_medium = w_htn*hh_medium,
         w_htn_hh_high = w_htn*hh_high,
         w_htn_hh_highest = w_htn*hh_highest,
         
         h_htn_hh_low = h_htn*hh_low,
         h_htn_hh_medium = h_htn*hh_medium,
         h_htn_hh_high = h_htn*hh_high,
         h_htn_hh_highest = h_htn*hh_highest
  ) %>% 
  mutate(w_dm_rural = rural*w_dm,
         h_dm_rural = rural*h_dm,
         
         # Since exposure is w_dm and effect modifier is husband's education
         w_dm_h_education_2 = h_education_2*w_dm,
         w_dm_h_education_3 = h_education_3*w_dm,
         w_dm_h_education_4 = h_education_4*w_dm,
         
         h_dm_w_education_2 = w_education_2*h_dm,
         h_dm_w_education_3 = w_education_3*h_dm,
         h_dm_w_education_4 = w_education_4*h_dm,
         
         # Since exposure is w_dm and effect modifier is husband's age category (<65 vs >=65)
         w_dm_h_ge40 = h_ge40*w_dm,
         h_dm_w_ge40 = w_ge40*h_dm,
         
         # # Since exposure is w_dm and effect modifier is length of marriage (<10 vs >=10)
         # w_dm_hh_lengthmar_ge10 = hh_lengthmar_ge10*w_dm,
         # h_dm_hh_lengthmar_ge10 = hh_lengthmar_ge10*h_dm,
         
         # Since exposure is w_dm and effect modifier is household wealth quintile
         w_dm_hh_low = w_dm*hh_low,
         w_dm_hh_medium = w_dm*hh_medium,
         w_dm_hh_high = w_dm*hh_high,
         w_dm_hh_highest = w_dm*hh_highest,
         
         h_dm_hh_low = h_dm*hh_low,
         h_dm_hh_medium = h_dm*hh_medium,
         h_dm_hh_high = h_dm*hh_high,
         h_dm_hh_highest = h_dm*hh_highest
  ) %>% 
  dplyr::select(-w_education,-h_education,-religion,
                -wealthq)


interaction_terms <- c("w_htn_rural","h_htn_rural",
                       "w_htn_h_education_2","w_htn_education_3","w_htn_education_4",
                       "h_htn_w_education_2","h_htn_w_education_3","h_htn_w_education_4",
                       "w_htn_h_ge40","h_htn_w_ge40",
                       "w_htn_hh_low","w_htn_hh_medium","w_htn_hh_high","w_htn_hh_highest",
                       "h_htn_hh_low","h_htn_hh_medium","h_htn_hh_high","h_htn_hh_highest",
                       
                       
)


