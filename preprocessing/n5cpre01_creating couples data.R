# Variables ------
iapr_id_vars <- c("cluster","hhid","linenumber")

iacr7c_female_variables <- readxl::read_excel("data/NFHS5 Couples Variable List.xlsx",
                                              sheet = "iacr7c variables") %>% 
  dplyr::filter(!is.na(female))

female_pr_variables <- readxl::read_excel("data/NFHS5 Couples Variable List.xlsx",
                                          sheet = "iacr7c variables") %>% 
  dplyr::filter((is.na(female) & !is.na(iapr7a_women))|new_var %in% c(iapr_id_vars,"age"))


iacr7c_male_variables <- readxl::read_excel("data/NFHS5 Couples Variable List.xlsx",
                                            sheet = "iacr7c variables") %>% 
  dplyr::filter(!is.na(male)) 

male_pr_variables <- readxl::read_excel("data/NFHS5 Couples Variable List.xlsx",
                                        sheet = "iacr7c variables") %>% 
  dplyr::filter((is.na(male) & !is.na(iapr7a_men))|new_var %in% c(iapr_id_vars,"age")) 


# Data ---------
female <- read_dta(paste0(path_india_raw_data,"/IACR7CDT/IACR7CFL.dta"),
                   col_select = iacr7c_female_variables$female)  %>% 
  rename_with(~ iacr7c_female_variables$new_var[which(iacr7c_female_variables$female == .x)], 
              .cols = iacr7c_female_variables$female)

female_pr <- read_dta(paste0(path_india_raw_data,"/IAPR7CDT/IAPR7CFL.dta"),
                      col_select = female_pr_variables$iapr7a_women)  %>% 
  rename_with(~ female_pr_variables$new_var[which(female_pr_variables$iapr7a_women == .x)], 
              .cols = female_pr_variables$iapr7a_women) %>% 
  dplyr::filter(!is.na(age))


male <- read_dta(paste0(path_india_raw_data,"/IACR7CDT/IACR7CFL.dta"),
                 col_select = iacr7c_male_variables$male)  %>% 
  rename_with(~ iacr7c_male_variables$new_var[which(iacr7c_male_variables$male == .x)], 
              .cols = iacr7c_male_variables$male)

male_pr <- read_dta(paste0(path_india_raw_data,"/IAPR7CDT/IAPR7CFL.dta"),
                    col_select = male_pr_variables$iapr7a_men)  %>% 
  rename_with(~ male_pr_variables$new_var[which(male_pr_variables$iapr7a_men == .x)], 
              .cols = male_pr_variables$iapr7a_men)  %>% 
  dplyr::filter(!is.na(age))


# Preprocessing --------

source("preprocessing/n5couples_preprocessing.R")
female_processed <- female %>% 
  mutate(sex = "Female") %>% 
  left_join(female_pr %>% 
              dplyr::select(-age),
            by=iapr_id_vars) %>% 
  n5couples_preprocessing(.)

male_processed <- male %>% 
  mutate(sex = "Male") %>% 
  left_join(male_pr %>% 
              dplyr::select(-age),
            by=iapr_id_vars)  %>% 
  n5couples_preprocessing(.)



saveRDS(female_processed,paste0(path_couples_folder,"/working/nfhs5c female.RDS"))
saveRDS(male_processed,paste0(path_couples_folder,"/working/nfhs5c male.RDS"))

# female_processed <- readRDS(paste0(path_couples_folder,"/working/nfhs5c female.RDS"))
# male_processed <- readRDS(paste0(path_couples_folder,"/working/nfhs5c male.RDS"))


# Flowchart: Heterosexual couples
female_processed %>% 
  distinct(cluster,hhid) %>% 
  nrow()


couples <- left_join(female_processed %>% 
                       dplyr::filter(!pregnant == 1) %>% 
                       dplyr::select(cluster,hhid,linenumber,spouse_id,
                                     strata,state,psu,sampleweight,
                                     interview,phase,district,
                                     
                                     caste,swealthq_ur,wealthq,
                                     religion,rural,nmembers,
                                     
                                     age,eduyr,education,nchildren,
                                     alcohol,tobacco_any,bmi,bmi_category,
                                     weight,height,waistcircumference,hipcircumference,
                                     waist_hip,highwc,highwhr,lengthmar,
                                     age1stmarriage,lengthmar,lengthmar_ge10,age_ge40,
                                     
                                     
                                     dm,glucose,screened_dm,diagnosed_dm,treated_dm,
                                     htn,sbp,dbp,screened_bp,diagnosed_bp,treated_bp
                       ) %>% 
                       rename_at(vars(age:treated_bp),~paste0("w_",.)),
                     
                     
                     
                     male_processed %>% 
                       dplyr::select(cluster,hhid,spouse_id,
                                     # caste,swealthq_ur,wealthq,
                                     # religion,residence,
                                     
                                     age,eduyr,education,nchildren,
                                     alcohol,tobacco_any,bmi,bmi_category,
                                     weight,height,waistcircumference,hipcircumference,
                                     waist_hip,highwc,highwhr,lengthmar,
                                     age1stmarriage,lengthmar,lengthmar_ge10,age_ge40,
                                     
                                     dm,glucose,screened_dm,diagnosed_dm,treated_dm,
                                     htn,sbp,dbp,screened_bp,diagnosed_bp,treated_bp
                       ) %>% 
                       rename_at(vars(age:treated_bp),~paste0("h_",.)),
                     by=c("cluster","hhid","linenumber"="spouse_id")
)

# Flowchart: Non-pregnant couples
couples %>% 
  distinct(cluster,hhid) %>% 
  nrow()


saveRDS(couples,paste0(path_couples_folder,"/working/nfhs5c couples.RDS"))


rm(female_pr,male_pr,female,male,
   female_processed,male_processed)








