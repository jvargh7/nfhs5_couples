rm(list=ls());gc();source(".Rprofile"); 

mi_dfs <- readRDS(paste0(path_couples_folder,"/working/nfhs5c couples_mi_dfs.RDS"))

require(mice)
require(srvyr)
require(survey)

source("analysis/n5cm_htn poisson regression equations.R")

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
  
  overall_w0[[i]] = svyglm(w0,design=svy_des,family=quasipoisson());
  overall_h0[[i]] = svyglm(h0,design=svy_des,family=quasipoisson());
  overall_w1[[i]] = svyglm(w1,design=svy_des,family=quasipoisson());
  overall_h1[[i]] = svyglm(h1,design=svy_des,family=quasipoisson());
  overall_w2[[i]] = svyglm(w2,design=svy_des,family=quasipoisson());
  overall_h2[[i]] = svyglm(h2,design=svy_des,family=quasipoisson());
  overall_w3[[i]] = svyglm(w3,design=svy_des,family=quasipoisson());
  overall_h3[[i]] = svyglm(h3,design=svy_des,family=quasipoisson());
  overall_w4[[i]] = svyglm(w4,design=svy_des,family=quasipoisson());
  overall_h4[[i]] = svyglm(h4,design=svy_des,family=quasipoisson());
  overall_w5[[i]] = svyglm(w5,design=svy_des,family=quasipoisson());
  overall_h5[[i]] = svyglm(h5,design=svy_des,family=quasipoisson());
  overall_w6[[i]] = svyglm(w6,design=svy_des,family=quasipoisson());
  overall_h6[[i]] = svyglm(h6,design=svy_des,family=quasipoisson());
  
  gc();rm(df);rm(svy_des)
}

# Pooling coefficients ------------
source("C:/code/external/functions/survey/mice_coef_svyglm.R")
# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing
overall_w0_out = mice_coef_svyglm(overall_w0)
overall_h0_out = mice_coef_svyglm(overall_h0)
overall_w1_out = mice_coef_svyglm(overall_w1)
overall_h1_out = mice_coef_svyglm(overall_h1)
overall_w2_out = mice_coef_svyglm(overall_w2)
overall_h2_out = mice_coef_svyglm(overall_h2)
overall_w3_out = mice_coef_svyglm(overall_w3)
overall_h3_out = mice_coef_svyglm(overall_h3)
overall_w4_out = mice_coef_svyglm(overall_w4)
overall_h4_out = mice_coef_svyglm(overall_h4)
overall_w5_out = mice_coef_svyglm(overall_w5)
overall_h5_out = mice_coef_svyglm(overall_h5)
overall_w6_out = mice_coef_svyglm(overall_w6)
overall_h6_out = mice_coef_svyglm(overall_h6)


bind_rows(
  overall_w0_out %>% mutate(model = "W0"),
  overall_h0_out %>% mutate(model = "H0"),
  overall_w1_out %>% mutate(model = "W1"),
  overall_h1_out %>% mutate(model = "H1"),
  overall_w2_out %>% mutate(model = "W2"),
  overall_h2_out %>% mutate(model = "H2"),
  overall_w3_out %>% mutate(model = "W3"),
  overall_h3_out %>% mutate(model = "H3"),
  overall_w4_out %>% mutate(model = "W4"),
  overall_h4_out %>% mutate(model = "H4"),
  overall_w5_out %>% mutate(model = "W5"),
  overall_h5_out %>% mutate(model = "H5"),
  overall_w6_out %>% mutate(model = "W6"),
  overall_h6_out %>% mutate(model = "H6")
  
) %>% 
  write_csv(.,"analysis/n5cm01_htn poisson regression with multiple imputation.csv")

source("C:/code/external/functions/survey/mice_contrasts_svyglm.R")
# Check: https://github.com/jvargh7/functions/blob/main/survey/mice_contrasts_svyglm.R
# You would also have to download the following:
# https://github.com/jvargh7/functions/blob/main/preprocessing/prepare_contrasts.R
# https://github.com/jvargh7/functions/blob/main/survey/contrasts_svyglm.R
# The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions

contrasts_w2_out = mice_contrasts_svyglm(svymodel_list = overall_w2,modifier = "w_ge40",exposure = "h_htn")
contrasts_h2_out = mice_contrasts_svyglm(svymodel_list = overall_h2,modifier="h_ge40",exposure="w_htn")

contrasts_w3_out_ed2 = mice_contrasts_svyglm(svymodel_list = overall_w3,modifier = "w_education_2",exposure = "h_htn")
contrasts_h3_out_ed2 = mice_contrasts_svyglm(svymodel_list = overall_h3,modifier="h_education_2",exposure="w_htn")

contrasts_w3_out_ed3 = mice_contrasts_svyglm(svymodel_list = overall_w3,modifier = "w_education_3",exposure = "h_htn")
contrasts_h3_out_ed3 = mice_contrasts_svyglm(svymodel_list = overall_h3,modifier="h_education_3",exposure="w_htn")


contrasts_w3_out_ed4 = mice_contrasts_svyglm(svymodel_list = overall_w3,modifier = "w_education_4",exposure = "h_htn")
contrasts_h3_out_ed4 = mice_contrasts_svyglm(svymodel_list = overall_h3,modifier="h_education_4",exposure="w_htn")

contrasts_w4_out = mice_contrasts_svyglm(svymodel_list = overall_w4,modifier = "rural",exposure = "h_htn")
contrasts_h4_out = mice_contrasts_svyglm(svymodel_list = overall_h4,modifier="rural",exposure="w_htn")

contrasts_w5_out_wlt2 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_low",exposure = "h_htn")
contrasts_h5_out_wlt2 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier="hh_low",exposure="w_htn")

contrasts_w5_out_wlt3 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_medium",exposure = "h_htn")
contrasts_h5_out_wlt3 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier="hh_medium",exposure="w_htn")

contrasts_w5_out_wlt4 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_high",exposure = "h_htn")
contrasts_h5_out_wlt4 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier="hh_high",exposure="w_htn")

contrasts_w5_out_wlt5 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_highest",exposure = "h_htn")
contrasts_h5_out_wlt5 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier="hh_highest",exposure="w_htn")

contrasts_w6_out_gen = mice_contrasts_svyglm(svymodel_list = overall_w6,modifier = "both_general",exposure = "h_htn")
contrasts_h6_out_gen = mice_contrasts_svyglm(svymodel_list = overall_h6,modifier="both_general",exposure="w_htn")

contrasts_w6_out_obc = mice_contrasts_svyglm(svymodel_list = overall_w6,modifier = "both_obc",exposure = "h_htn")
contrasts_h6_out_obc = mice_contrasts_svyglm(svymodel_list = overall_h6,modifier="both_obc",exposure="w_htn")

contrasts_w6_out_scst = mice_contrasts_svyglm(svymodel_list = overall_w6,modifier = "both_scst",exposure = "h_htn")
contrasts_h6_out_scst = mice_contrasts_svyglm(svymodel_list = overall_h6,modifier="both_scst",exposure="w_htn")


bind_rows(
  contrasts_w2_out %>% mutate(model = "W2"),
  contrasts_h2_out %>% mutate(model = "H2"),
  contrasts_w3_out_ed2 %>% mutate(model = "W3 E2"),
  contrasts_h3_out_ed2 %>% mutate(model = "H3 E2"),
  contrasts_w3_out_ed3 %>% mutate(model = "W3 E3"),
  contrasts_h3_out_ed3 %>% mutate(model = "H3 E3"),
  contrasts_w3_out_ed4 %>% mutate(model = "W3 E4"),
  contrasts_h3_out_ed4 %>% mutate(model = "H3 E4"),
  
  contrasts_w4_out %>% mutate(model = "W4"),
  contrasts_h4_out %>% mutate(model = "H4"),
  
  contrasts_w5_out_wlt2 %>% mutate(model = "W5 Low"),
  contrasts_h5_out_wlt2 %>% mutate(model = "H5 Low"),
  
  contrasts_w5_out_wlt3 %>% mutate(model = "W5 Medium"),
  contrasts_h5_out_wlt3 %>% mutate(model = "H5 Medium"),
  
  contrasts_w5_out_wlt4 %>% mutate(model = "W5 High"),
  contrasts_h5_out_wlt4 %>% mutate(model = "H5 High"),
  
  contrasts_w5_out_wlt5 %>% mutate(model = "W5 Highest"),
  contrasts_h5_out_wlt5 %>% mutate(model = "H5 Highest"),
  
  contrasts_w6_out_gen %>% mutate(model = "W6 General"),
  contrasts_h6_out_gen %>% mutate(model = "H6 General"),
  contrasts_w6_out_obc %>% mutate(model = "W6 OBC"),
  contrasts_h6_out_obc %>% mutate(model = "H6 OBC"),
  contrasts_w6_out_scst %>% mutate(model = "W6 SCST"),
  contrasts_h6_out_scst %>% mutate(model = "H6 SCST")
  
) %>% 
  write_csv(.,"analysis/n5cm01_htn contrasts for poisson regression with multiple imputation.csv")
