


mi_dfs <- readRDS(paste0(path_couples_folder,"/working/nfhs5c couples_mi_dfs.RDS"))

library(mice)
library(geepack)

source("pooled/n5cpooled_equations.R")
source("functions/pooled_analytic.R")

# Run Poisson Regression ------------

for(i in 1:mi_dfs$m){
  
  df = complete(mi_dfs,action = i);
  
  pooled_data = pooled_analytic(df)
  
  overall_p1dm[[i]] = geeglm(formula = p1dm,data=pooled_data,family=poisson(),id=household,corstr="exchangeable",weights = sampleweight);
  overall_p2dm[[i]] = geeglm(formula = p2dm,data=pooled_data,family=poisson(),id=household,corstr="exchangeable",weights = sampleweight);
  
  overall_p1htn[[i]] = geeglm(formula = p1htn,data=pooled_data,family=poisson(),id=household,corstr="exchangeable",weights = sampleweight);
  overall_p2htn[[i]] = geeglm(formula = p2htn,data=pooled_data,family=poisson(),id=household,corstr="exchangeable",weights = sampleweight);

}

source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

overall_p1dm_out = clean_mi_conditionalregression(overall_p1dm,link="geeglm log")
overall_p2dm_out = clean_mi_conditionalregression(overall_p2dm,link="geeglm log")
overall_p1htn_out = clean_mi_conditionalregression(overall_p1htn,link="geeglm log")
overall_p2htn_out = clean_mi_conditionalregression(overall_p2htn,link="geeglm log")

bind_rows(
  overall_p1dm_out %>% mutate(model = "P1",outcome = "Diabetes"),
  overall_p2dm_out %>% mutate(model = "P2",outcome = "Diabetes"),
  overall_p1htn_out %>% mutate(model = "P1",outcome = "Hypertension"),
  overall_p2htn_out %>% mutate(model = "P2",outcome = "Hypertension")
  
  
) %>% 
  write_csv(.,"pooled/n5cpooled01_poisson regression with multiple imputation.csv")


# Contrasts ----------

source("C:/code/external/functions/imputation/clean_mi_contrasts.R")

# The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions

contrasts_p2dm_out = clean_mi_contrasts(model_list = overall_p2dm,link="geeglm log",modifier = "husband",exposure = "spouse_dm")
contrasts_p2htn_out = clean_mi_contrasts(model_list = overall_p2htn,link="geeglm log",modifier="husband",exposure="spouse_htn")


bind_rows(
  contrasts_p2dm_out %>% mutate(model = "P2",outcome = "Diabetes"),
  contrasts_p2htn_out %>% mutate(model = "P2",outcome = "Hypertension")
 
) %>% 
  write_csv(.,"pooled/n5cpooled01_contrasts for poisson regression with multiple imputation.csv")
