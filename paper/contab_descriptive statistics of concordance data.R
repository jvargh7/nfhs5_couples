library(srvyr)

source("preprocessing/cp04_concordance datasets.R")

descriptive_df <- partner_df %>% 
  dplyr::filter((!is.na(f_dm)&!is.na(m_dm))|(!is.na(f_htn)&!is.na(m_htn))) %>% 
  mutate(c_dm = case_when(f_dm == 1 & m_dm == 1 ~ 1,
                          f_dm == 0 | m_dm == 0 ~ 0,
                          TRUE ~ NA_real_),
         c_htn = case_when(f_htn == 1 & m_htn == 1 ~ 1,
                          f_htn == 0 | m_htn == 0 ~ 0,
                          TRUE ~ NA_real_)
         )


# descriptive_df - dm_df
# descriptive_df %>% 
#   dplyr::filter(!is.na(f_bmi),!is.na(m_bmi)) %>%
#   dplyr::filter(!is.na(f_caste)) %>%
#   nrow()

# descriptive_df %>% distinct(v001,v002) %>% View()

descriptive_svydesign <- descriptive_df %>% 
  as_survey_design(.data = .,
                   ids = cluster,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

all_variables = names(descriptive_df)
id_variables = c("state","cluster","weight","v001","v002")
group_variables = c("f_caste","f_religion","f_wealth")
other_variables = all_variables[!all_variables %in% c(id_variables,group_variables)]


counts_variables <- descriptive_df %>% 
  summarize_at(vars(other_variables,group_variables),list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


summary_survey_groups <- map_dfr(group_variables,
                                 function(g){
                                   
                                   descriptive_svydesign %>% 
                                     group_by_at(g) %>% 
                                     summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci")) %>% 
                                     rename(group = g) %>% 
                                     mutate(variable = g)
                                   
                                 }) %>% 
  dplyr::filter(!is.na(proportion))


summary_survey_means <- descriptive_svydesign %>% 
  summarize_at(vars(one_of(other_variables)),.funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=everything()) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

summary_survey_variances <- descriptive_svydesign %>% 
  summarize_at(vars(one_of(other_variables)),.funs = list(variance = ~survey_var(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_variance",cols=everything()) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate_at(vars(contains("proportion")),~sqrt(.)) %>% 
  mutate(variable = paste0(variable,"_sd"))


summary_table <- bind_rows(summary_survey_groups,
                           summary_survey_means,
                           summary_survey_variances) %>% 
  left_join(counts_variables,by="variable") %>% 
  dplyr::filter(!is.na(proportion)) %>% 
  mutate(coef_ci = case_when(str_detect(variable,"(_age|_eduyr|_bmi|_hb|_glucose|_sbp|_dbp|_children)") ~ 
                               paste0(round(proportion,1)," (",
                                      round(proportion_low,1),", ",
                                      round(proportion_upp,1),")"),
                             TRUE ~ paste0(round(proportion*100,1)," (",
                                           round(proportion_low*100,1),", ",
                                           round(proportion_upp*100,1),")")
  )) %>% 
  arrange(variable,group)

write_csv(summary_table,paste0(path_couples_folder,"/working/nfhs5 concordance/nfhs5 descriptives.csv"))

# # Associations ---------
# 
# survey::svytable(~m_religion + f_religion,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# survey::svytable(~m_caste + f_caste,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# 
# survey::svyvar(~f_age + m_age,design=descriptive_svydesign) %>% 
#   as.matrix(v) %>% 
#   cov2cor()
# with(descriptive_df,cor.test(f_age,m_age,use="complete.obs"))
# 
# survey::svyvar(~f_eduyr + m_eduyr,design=descriptive_svydesign,na.rm = TRUE) %>% 
#   as.matrix(v) %>% 
#   cov2cor()
# with(descriptive_df,cor.test(f_eduyr,m_eduyr,use="complete.obs"))
# 
# survey::svytable(~f_alcohol + m_alcohol,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# 
# survey::svytable(~f_smoke + m_smoke,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# 
# survey::svytable(~f_insurance + m_insurance,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# 
# survey::svytable(~f_self_diabetes + m_self_diabetes,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# survey::svytable(~f_self_diabmed + m_self_diabmed,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# 
# survey::svytable(~f_self_hypertension + m_self_hypertension,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# survey::svytable(~f_self_htnmed + m_self_htnmed,design = descriptive_svydesign) %>% 
#   summary(.,statistic = "Chisq")
# 
# 
# survey::svyvar(~f_bmi + m_bmi,design=descriptive_svydesign,na.rm = TRUE) %>% 
#   as.matrix(v) %>% 
#   cov2cor()
# 
# survey::svyvar(~f_glucose + m_glucose,design=descriptive_svydesign,na.rm = TRUE) %>% 
#   as.matrix(v) %>% 
#   cov2cor()
# with(descriptive_df,cor.test(f_glucose,m_glucose,use="complete.obs"))
# 
# survey::svyvar(~f_sbp + m_sbp,design=descriptive_svydesign,na.rm = TRUE) %>% 
#   as.matrix(v) %>% 
#   cov2cor()
# with(descriptive_df,cor.test(f_sbp,m_sbp,use="complete.obs"))
# 
# survey::svyvar(~f_dbp + m_dbp,design=descriptive_svydesign,na.rm = TRUE) %>% 
#   as.matrix(v) %>% 
#   cov2cor()
# with(descriptive_df,cor.test(f_dbp,m_dbp,use="complete.obs"))
# 
# # ODDS RATIO ------------
# 
# survey::svyglm(f_alcohol ~ m_alcohol,design=descriptive_svydesign,family = binomial()) %>% 
#   broom::tidy(exponentiate = TRUE) %>% 
#   mutate(coef_ci = paste0(round(estimate,1)," (",
#                           round(estimate - 1.96*std.error,1) , ",",
#                           round(estimate + 1.96*std.error,1), ")"
#   ))
# 
# survey::svyglm(f_smoke ~ m_smoke,design=descriptive_svydesign,family = binomial()) %>% 
#   broom::tidy(exponentiate = TRUE) %>% 
#   mutate(coef_ci = paste0(round(estimate,1)," (",
#                           round(estimate - 1.96*std.error,1) , ",",
#                           round(estimate + 1.96*std.error,1), ")"
#   ))
# 
# survey::svyglm(f_insurance ~ m_insurance,design=descriptive_svydesign,family = binomial()) %>% 
#   broom::tidy(exponentiate = TRUE) %>% 
#   mutate(coef_ci = paste0(round(estimate,1)," (",
#                           round(estimate - 1.96*std.error,1) , ",",
#                           round(estimate + 1.96*std.error,1), ")"
#   ))
# 
# survey::svyglm(f_self_diabetes ~ m_self_diabetes,design=descriptive_svydesign,family = binomial()) %>% 
#   broom::tidy(exponentiate = TRUE) %>% 
#   mutate(coef_ci = paste0(round(estimate,1)," (",
#                           round(estimate - 1.96*std.error,1) , ",",
#                           round(estimate + 1.96*std.error,1), ")"
#   ))
# 
# survey::svyglm(f_self_diabmed ~ m_self_diabmed,design=descriptive_svydesign,family = binomial()) %>% 
#   broom::tidy(exponentiate = TRUE) %>% 
#   mutate(coef_ci = paste0(round(estimate,1)," (",
#                           round(estimate - 1.96*std.error,1) , ",",
#                           round(estimate + 1.96*std.error,1), ")"
#   ))
# 
# survey::svyglm(f_self_hypertension ~ m_self_hypertension,design=descriptive_svydesign,family = binomial()) %>% 
#   broom::tidy(exponentiate = TRUE) %>% 
#   mutate(coef_ci = paste0(round(estimate,1)," (",
#                           round(estimate - 1.96*std.error,1) , ",",
#                           round(estimate + 1.96*std.error,1), ")"
#   ))
# 
# survey::svyglm(f_self_htnmed ~ m_self_htnmed,design=descriptive_svydesign,family = binomial()) %>% 
#   broom::tidy(exponentiate = TRUE) %>% 
#   mutate(coef_ci = paste0(round(estimate,1)," (",
#                           round(estimate - 1.96*std.error,1) , ",",
#                           round(estimate + 1.96*std.error,1), ")"
#   ))
