rm(list=ls());gc();source(".Rprofile")


attributable_undiagnosed <- read_csv("analysis/n5cm05_summary undiagnosed hypertension by spousal status.csv")
proportion_in_total <- read_csv("analysis/n5cm05_summary proportion of total diagnosed undiagnosed by spouse diagnosis.csv") %>% 
  dplyr::filter(diagnosis_status %in% c("Total","Undiagnosed"))  %>% 
  left_join(attributable_undiagnosed %>% 
              dplyr::select(diagnosis_status,spouse_diagnosis_status,sex,est_ci) %>% 
              rename(text_est_ci = est_ci) %>% 
              mutate(text_est_ci = str_replace(text_est_ci,"\\(","\n\\(")),
            by = c("diagnosis_status","spouse_diagnosis_status","sex")) %>% 
  mutate(spouse_diagnosis_status = factor(spouse_diagnosis_status,
                                          levels=c("No Hypertension","Diagnosed","Undiagnosed"),
                                          labels=c("Spouse has no hypertension",
                                                   "Spouse is Diagnosed",
                                                   "Spouse is Undiagnosed")))

fig_A = proportion_in_total %>% 
  dplyr::filter(sex == "Women") %>% 
  ggplot(data=.,aes(x=spouse_diagnosis_status,y=estimate,ymin=lci,ymax=uci,group=diagnosis_status,fill=diagnosis_status)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width = 0.2) + 
  geom_text(aes(x=spouse_diagnosis_status,y=estimate*0.5,label=text_est_ci),position=position_dodge(width=0.9),size = 3) +
  scale_fill_manual(name="",values=c("lightblue","orange")) +
  scale_x_discrete(labels=function(x) str_wrap(x,width = 20)) + 
  theme_bw() +
  xlab("") +
  ylab("Prevalence (%)")
fig_A


fig_B = proportion_in_total %>% 
  dplyr::filter(sex == "Men") %>% 
  ggplot(data=.,aes(x=spouse_diagnosis_status,y=estimate,ymin=lci,ymax=uci,group=diagnosis_status,fill=diagnosis_status)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width = 0.2) + 
  geom_text(aes(x=spouse_diagnosis_status,y=estimate*0.5,label=text_est_ci),position=position_dodge(width=0.9),size = 3) +
  scale_fill_manual(name="",values=c("lightblue","orange")) +
  scale_x_discrete(labels=function(x) str_wrap(x,width = 20)) + 
  theme_bw() +
  xlab("") +
  ylab("Prevalence (%)")
fig_B

library(ggpubr)
ggarrange(fig_A,fig_B,ncol=2,labels = c("A","B"),
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_couples_folder,"/figures/column diagnosis by spousal status.jpg"),width = 9, height = 5)
