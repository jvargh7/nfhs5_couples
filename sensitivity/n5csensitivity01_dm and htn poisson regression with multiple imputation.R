


mi_dfs <- readRDS(paste0(path_couples_folder,"/working/nfhs5c couples_mi_dfs.RDS"))

library(mice)
library(srvyr)
library(survey)

source("sensitivity/n5csensitivity_equations.R")

# Run Poisson Regression ------------

for(i in 1:mi_dfs$m){
  
  df = complete(mi_dfs,action = i);
  
  svy_des = df  %>% 
    # Need to impute and correct
    as_survey_design(.data = .,
                     ids = psu,strata = state,
                     weight = sampleweight,
                     nest = TRUE,
                     variance = "YG",pps = "brewer");
  
  overall_sw1dm[[i]] = svyglm(sw1dm,design=svy_des,family=quasipoisson());
  overall_sh1dm[[i]] = svyglm(sh1dm,design=svy_des,family=quasipoisson());
  overall_sw1htn[[i]] = svyglm(sw1htn,design=svy_des,family=quasipoisson());
  overall_sh1htn[[i]] = svyglm(sh1htn,design=svy_des,family=quasipoisson());
  
  overall_sw2dm[[i]] = svyglm(sw2dm,design=svy_des,family=gaussian());
  overall_sh2dm[[i]] = svyglm(sh2dm,design=svy_des,family=gaussian());
  overall_sw2htn[[i]] = svyglm(sw2htn,design=svy_des,family=gaussian());
  overall_sh2htn[[i]] = svyglm(sh2htn,design=svy_des,family=gaussian());
 
  gc();rm(df);rm(svy_des)
  
  
}

source("C:/code/external/functions/survey/mice_coef_svyglm.R")
# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing
overall_sw1dm_out = mice_coef_svyglm(overall_sw1dm,link="svyglm quasipoisson")
overall_sh1dm_out = mice_coef_svyglm(overall_sh1dm,link="svyglm quasipoisson")
overall_sw1htn_out = mice_coef_svyglm(overall_sw1htn,link="svyglm quasipoisson")
overall_sh1htn_out = mice_coef_svyglm(overall_sh1htn,link="svyglm quasipoisson")
overall_sw2dm_out = mice_coef_svyglm(overall_sw2dm,link = "svyglm gaussian")
overall_sh2dm_out = mice_coef_svyglm(overall_sh2dm,link = "svyglm gaussian")
overall_sw2htn_out = mice_coef_svyglm(overall_sw2htn,link = "svyglm gaussian")
overall_sh2htn_out = mice_coef_svyglm(overall_sh2htn,link = "svyglm gaussian")

bind_rows(
  overall_sw1dm_out %>% mutate(model = "SW1",outcome = "Diabetes"),
  overall_sh1dm_out %>% mutate(model = "SH1",outcome = "Diabetes"),
  overall_sw1htn_out %>% mutate(model = "SW1",outcome = "Hypertension"),
  overall_sh1htn_out %>% mutate(model = "SH1",outcome = "Hypertension"),
  overall_sw2dm_out %>% mutate(model = "SW2",outcome = "Glucose"),
  overall_sh2dm_out %>% mutate(model = "SH2",outcome = "Glucose"),
  overall_sw2htn_out %>% mutate(model = "SW2",outcome = "SBP"),
  overall_sh2htn_out %>% mutate(model = "SH2",outcome = "SBP")
  
) %>% 
  write_csv(.,"sensitivity/n5csensitivity01_poisson regression with multiple imputation.csv")


