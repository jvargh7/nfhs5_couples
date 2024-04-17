rm(list=ls());gc();source(".Rprofile"); 
source("preprocessing/n5cpre02_analytic sample.R")


continuous_vars <- c(paste0(rep(c("w_","h_"),each=11),
                            c("sbp","dbp","weight",
                              "height","glucose",
                              "bmi","waistcircumference","hipcircumference",
                              "age","eduyr","lengthcohabitation")),
                     "nmembers","hh_children","h_npartners")
# ,"hh_lengthmar": Do not have data for most couples
proportion_vars <- c(paste0(rep(c("w_","h_"),each=9),
                            c("diagnosed_bp","treated_bp",
                              "diagnosed_dm","treated_dm",
                              
                              "tobacco_any","alcohol",
                              "dm","htn","lengthcohabitation_ge10")),"rural","w_consang_marriage")

grouped_vars <- c("w_education","h_education","w_caste","h_caste","caste_hh","religion","wealthq","swealthq_ur")

require(survey)
require(mice)

before_imputation <- couples %>% 
  dplyr::select(cluster,hhid,linenumber,spouse_id,
                strata,state,psu,sampleweight,
                interview,phase,district,
                
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
    
    hh_low = case_when(swealthq_ur == "Low" ~ 1,
                       TRUE ~ 0),
    hh_medium = case_when(swealthq_ur == "Medium" ~ 1,
                          TRUE ~ 0),
    hh_high = case_when(swealthq_ur == "High" ~ 1,
                        TRUE ~ 0),
    hh_highest = case_when(swealthq_ur == "Highest" ~ 1,
                           TRUE ~ 0),
    
    religion_muslim = case_when(religion == "Muslim" ~ 1,
                                TRUE ~ 0),
    religion_other = case_when(religion == "Other" ~ 1,
                               TRUE ~ 0),
    
    both_general = case_when(h_caste == "General" & w_caste == "General" ~ 1,
                             TRUE ~ 0),
    
    both_obc = case_when(h_caste == "OBC" & w_caste == "OBC" ~ 1,
                         TRUE ~ 0),
    both_scst = case_when(h_caste == "Schedule Caste" & w_caste == "Schedule Caste" ~ 1,
                          h_caste == "Schedule Tribe" & w_caste == "Schedule Tribe" ~ 1,
                         TRUE ~ 0),
    
    
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
         h_htn_hh_highest = h_htn*hh_highest,
         
         h_htn_religion_muslim = h_htn*religion_muslim,
         w_htn_religion_muslim = w_htn*religion_muslim,
         h_htn_religion_other = h_htn*religion_other,
         w_htn_religion_other = w_htn*religion_other,
         
         w_htn_both_general = w_htn*both_general,
         w_htn_both_obc = w_htn*both_obc,
         w_htn_both_scst = w_htn*both_scst,
         
         h_htn_both_general = h_htn*both_general,
         h_htn_both_obc = h_htn*both_obc,
         h_htn_both_scst = h_htn*both_scst
         
         
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
         h_dm_hh_highest = h_dm*hh_highest,
         
         h_dm_religion_muslim = h_dm*religion_muslim,
         w_dm_religion_muslim = w_dm*religion_muslim,
         h_dm_religion_other = h_dm*religion_other,
         w_dm_religion_other = w_dm*religion_other,
         
         
         w_dm_both_general = w_dm*both_general,
         w_dm_both_obc = w_dm*both_obc,
         w_dm_both_scst = w_dm*both_scst,
         
         h_dm_both_general = h_dm*both_general,
         h_dm_both_obc = h_dm*both_obc,
         h_dm_both_scst = h_dm*both_scst
         
  ) %>% 
  dplyr::select(-w_education,-h_education,-religion,
                -swealthq_ur)


interaction_terms <- c("w_htn_rural","h_htn_rural",
                       "w_htn_h_education_2","w_htn_h_education_3","w_htn_h_education_4",
                       "h_htn_w_education_2","h_htn_w_education_3","h_htn_w_education_4",
                       "w_htn_h_ge40","h_htn_w_ge40",
                       "w_htn_hh_low","w_htn_hh_medium","w_htn_hh_high","w_htn_hh_highest",
                       "h_htn_hh_low","h_htn_hh_medium","h_htn_hh_high","h_htn_hh_highest",
                       "w_htn_religion_muslim","w_htn_religion_other",
                       "h_htn_religion_muslim","h_htn_religion_other",
                       "w_htn_both_general","w_htn_both_obc","w_htn_both_scst",
                       "h_htn_both_general","h_htn_both_obc","h_htn_both_scst",
                       
                       
                       "w_dm_rural","h_dm_rural",
                       "w_dm_h_education_2","w_dm_h_education_3","w_dm_h_education_4",
                       "h_dm_w_education_2","h_dm_w_education_3","h_dm_w_education_4",
                       "w_dm_h_ge40","h_dm_w_ge40",
                       "w_dm_hh_low","w_dm_hh_medium","w_dm_hh_high","w_dm_hh_highest",
                       "h_dm_hh_low","h_dm_hh_medium","h_dm_hh_high","h_dm_hh_highest",
                       "w_dm_religion_muslim","w_dm_religion_other",
                       "h_dm_religion_muslim","h_dm_religion_other",
                       "w_dm_both_general","w_dm_both_obc","w_dm_both_scst",
                       "h_dm_both_general","h_dm_both_obc","h_dm_both_scst"
                       )

mi_null <- mice(before_imputation,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("cluster","hhid","linenumber","spouse_id","strata","state","psu","sampleweight", "interview","phase","district"),] <- 0
pred[,c("cluster","hhid","linenumber","spouse_id","strata","state","psu","sampleweight", "interview","phase","district")] <- 0



# Impute via equation and do not use for imputation , --------

method["w_ge40"] <- "~I((w_age>=40)*1)"
method["h_ge40"] <- "~I((h_age>=40)*1)"
pred[c("w_ge40","h_ge40"),] <- 0
pred[,c("w_ge40","h_ge40")] <- 0


for(i_t in interaction_terms){
  print(i_t)
  exposure_term = str_extract(i_t,"^(w|h)_(htn|dm)")
  em_term = str_replace(i_t,pattern=paste0(exposure_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",exposure_term,"*",em_term,")")
  
  # Do not use interaction terms for imputation of the source variables
  pred[c(exposure_term,em_term),i_t] <- 0
}

# Takes ~4h
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_couples_folder,"/working/nfhs5c couples_mi_dfs.RDS"))
