mi_dfs <- readRDS(paste0(path_couples_folder,"/working/nfhs5c couples pre exclusion_mi_dfs.RDS"))

require(mice)
require(srvyr)
require(survey)

source("overall/n5cm_htn poisson regression equations.R")
htn_w1 = w1
htn_h1 = h1
source("overall/n5cm_dm poisson regression equations.R")
dm_w1 = w1
dm_h1 = h1
# Run Poisson Regression ------------
overall_htn_w1 <- overall_htn_h1 <- overall_dm_w1 <- overall_dm_h1 <- NULL

for(i in 1:mi_dfs$m){
  df = complete(mi_dfs,action = i);
  
  svy_des = df  %>% 
    # Need to impute and correct
    as_survey_design(.data = .,
                     ids = psu,strata = state,
                     weight = sampleweight,
                     nest = TRUE,
                     variance = "YG",pps = "brewer");
  
  overall_htn_w1[[i]] = svyglm(htn_w1,design=svy_des,family=quasipoisson());
  overall_htn_h1[[i]] = svyglm(htn_h1,design=svy_des,family=quasipoisson());
  overall_dm_w1[[i]] = svyglm(dm_w1,design=svy_des,family=quasipoisson());
  overall_dm_h1[[i]] = svyglm(dm_h1,design=svy_des,family=quasipoisson());
  
  
  gc();rm(df);rm(svy_des)
}

# Pooling coefficients ------------
source("C:/code/external/functions/survey/mice_coef_svyglm.R")
# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing
overall_htn_w1_out = mice_coef_svyglm(overall_htn_w1)
overall_htn_h1_out = mice_coef_svyglm(overall_htn_h1)
overall_dm_w1_out = mice_coef_svyglm(overall_dm_w1)
overall_dm_h1_out = mice_coef_svyglm(overall_dm_h1)


bind_rows(
  overall_htn_w1_out %>% mutate(model = "W1",outcome = "Hypertension"),
  overall_htn_h1_out %>% mutate(model = "H1",outcome = "Hypertension"),
  overall_dm_w1_out %>% mutate(model = "W1",outcome = "Diabetes"),
  overall_dm_h1_out %>% mutate(model = "H1",outcome = "Diabetes"),

) %>% 
  write_csv(.,"sensitivity/n5csensitivity02_dm and htn poisson regression with imputed outcomes.csv")

