rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/n5cpre02_analytic sample.R")

couples_bpcat_svy <- couples %>% 
  dplyr::filter(!is.na(h_sbp),!is.na(h_dbp),!is.na(w_sbp),!is.na(w_dbp)) %>% 
  mutate(h_bp_category = case_when(h_sbp %in% c(130:139) ~ 2,
                                     h_dbp %in% c(80:89) ~ 2,
                                     
                                     h_sbp %in% c(140:159) ~ 3,
                                     h_dbp %in% c(90:99) ~ 3,
                                     
                                     h_sbp %in% c(160:179) ~ 4,
                                     h_dbp %in% c(100:109) ~ 4,
                                     
                                     h_sbp >= 180 ~ 5,
                                     h_dbp >= 110 ~ 5,
                                     
                                     TRUE ~ 1
                                     ),
         w_bp_category = case_when(w_sbp %in% c(130:139) ~ 2,
                                     w_dbp %in% c(80:89) ~ 2,
                                     
                                     w_sbp %in% c(140:159) ~ 3,
                                     w_dbp %in% c(90:99) ~ 3,
                                     
                                     w_sbp %in% c(160:179) ~ 4,
                                     w_dbp %in% c(100:109) ~ 4,
                                     
                                     w_sbp >= 180 ~ 5,
                                     w_dbp >= 110 ~ 5,
                                     
                                     TRUE ~ 1
         )
         ) %>% 
  mutate(across(h_bp_category:w_bp_category,.fns=function(x) factor(x,levels=c(1:5),
                                                                        labels=c("Normal","High Normal BP",
                                                                                 "Grade 1","Grade 2","Grade 3"),
                                                                        ordered = TRUE))) %>% 
  # Need to impute and correct
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

bpcat_summary <- couples_bpcat_svy %>% 
  group_by(h_bp_category,w_bp_category)  %>% 
  survey_tally()

husband_bpcat_summary_proportion <- couples_bpcat_svy %>% 
  group_by(h_bp_category,w_bp_category)  %>% 
  summarize(p = survey_prop(vartype="ci",prop_method = "likelihood")) 
wife_bpcat_summary_proportion <- couples_bpcat_svy %>% 
  group_by(w_bp_category,h_bp_category)  %>% 
  summarize(p = survey_prop(vartype="ci",prop_method = "likelihood")) 


figA <- husband_bpcat_summary_proportion %>% 
  mutate(p_low = case_when(p_low < 0 ~ 0,
                           TRUE ~ p_low)) %>% 
  mutate(est_ci = paste0(round(p*100,1),"\n(",
                         round(p_low*100,1),", ",
                         round(p_upp*100,1),")")) %>% 
  ggplot(data=.,aes(x=h_bp_category,y=w_bp_category,fill=p*100,label=est_ci)) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient2(name = "As Proportion of \nHusbands' Category",low="white",mid="lightblue",high="darkblue") +
  xlab("Husbands' BP Category") +
  ylab("Wives' BP Category") +
  theme_bw() +
  theme(legend.position = "bottom")

figB <- wife_bpcat_summary_proportion %>% 
  mutate(p_low = case_when(p_low < 0 ~ 0,
                           TRUE ~ p_low)) %>% 
  mutate(est_ci = paste0(round(p*100,1),"\n(",
                         round(p_low*100,1),", ",
                         round(p_upp*100,1),")")) %>% 
  ggplot(data=.,aes(x=w_bp_category,y=h_bp_category,fill=p*100,label=est_ci)) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient2(name = "As Proportion of \nWives' Category",low="white",mid="lightblue",high="darkblue") +
  ylab("Husbands' BP Category") +
  xlab("Wives' BP Category") +
  theme_bw() +
  theme(legend.position = "bottom")

library(ggpubr)

ggarrange(figA,
          figB,
          labels=c("A","B"),
          nrow = 1,ncol=2) %>% 
  ggsave(.,filename=paste0(path_couples_folder,"/figures/summary of hypertension care cascade.png"),width=12,height = 4)
