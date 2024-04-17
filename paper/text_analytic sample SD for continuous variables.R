rm(list=ls());gc();source(".Rprofile")

source("preprocessing/n5cpre02_analytic sample.R")
source("C:/code/external/functions/survey/svysd.R")

continuous_vars <- c("bmi","age")
couples_svy <- couples  %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

svysd(couples_svy,
      c_vars = c("w_age","h_age","w_eduyr","h_eduyr"),
      # p_vars = proportion_vars,
      # g_vars = grouped_vars,
      # id_vars = i_v
)
