
require(tidyverse)
require(haven)
require(lubridate)


if(Sys.info()["user"]=="JVARGH7"){
  path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"
  path_men_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA/IAMR7AFL"
  path_person_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA/IAPR7AFL"
  path_household_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA/IAHR7AFL"
  path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS Couples" 
  path_dbcouples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples Dual Burden" 
}

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

fasting_time <- 7.9

bmi_max = 6000

bmi_cutoff <- c(1850, 2500, 3000)
bmiasian_cutoff <- c(1850,2300,2750)

# Non-Asians
female_wc_cutoff = 80 
female_whr_cutoff = 0.80
male_wc_cutoff = 94 
male_whr_cutoff = 0.95


# Asians
# female_wc_cutoff = 80 
# female_whr_cutoff = 0.85
# male_wc_cutoff = 90 
# male_whr_cutoff = 0.9

rpg_cutoff <- 220
fpg_cutoff <- rpg_cutoff 
sbp_cutoff <- 140
dbp_cutoff <- 90




fpg_target <- 126
rpg_target <- 180 #Indian DM guidelines
# Indian HTN guidelines (Shah 2020: 130/80 for <= 60y, 140/90 otherwise)
# Indian HTN guidelines (ICMR 2018: 140/90 for <= 80y, 150/90 otherwise)
sbp_target <- c(140,150) 
agebp_cutoff <- 80
dbp_target <- c(90,90)
fpgpre_cutoff <- 100
rpgpre_cutoff <- 140

sbppre_cutoff <- 130
dbppre_cutoff <- 85
