
group_vars = ""

require(srvyr)
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")

couples_svydesign = couples  %>% 
  # Need to impute and correct
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

proportion_vars <- c("w_dm","w_htn","h_dm","h_htn","dm_joint","htn_joint")

state_svysummary <- map_dfr(group_vars,
                                   function(g_v){
                                     id_vars = c("state");
                                     print(g_v);
                                     n5_sy <- svysummary(couples_svydesign,
                                                         # c_vars = continuous_vars,
                                                         p_vars = proportion_vars,
                                                         # g_vars = grouped_vars,
                                                         id_vars = id_vars
                                     ) %>% 
                                       mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                       mutate(est_ci = paste0(estimate," (",
                                                              lci,", ",uci,")"));
                                     
                                     # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                     n5_ct <- couples %>% 
                                       group_by_at(vars(one_of(id_vars))) %>% 
                                       summarize_at(vars(one_of(c(
                                         # continuous_vars,
                                         proportion_vars
                                         # grouped_vars
                                       ))),
                                       list(n = ~sum(!is.na(.)))) %>% 
                                       pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                       mutate(variable = str_replace(variable,"_n$",""));
                                     
                                     n5_out <- left_join(n5_sy,
                                                         n5_ct,
                                                         by=c(id_vars[id_vars!=""],"variable")) %>% 
                                       
                                       # Restrict to those cells with more than 100 observations
                                       # dplyr::filter(n > 100) %>% 
                                       mutate(stratification = g_v) %>% 
                                       rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                       mutate_at(vars(one_of("strata")),~as.character(.));
                                     gc();
                                     return(n5_out)
                                     
                                   })

state_svysummary %>% 
  left_join(readxl::read_excel("C:/code/external/nfhs_Cascade/data/NFHS Cascade Variable List.xlsx",sheet="map2020_v024") %>% 
              dplyr::select(v024,n5_state,zone) %>% 
              distinct(v024,n5_state,.keep_all=TRUE),
            by=c("state"="v024")) %>% 
  write_csv(.,file = "overall/n5cm04_state summary of joint prevalence.csv")
