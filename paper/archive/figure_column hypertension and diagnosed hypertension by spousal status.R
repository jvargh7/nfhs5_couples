rm(list=ls());gc();source(".Rprofile")

diagnosed_by_spouse_among_htn <- read_csv("analysis/summary table of proportion diagnosed and treated among hypertension.csv") %>% 
  mutate(spouse_status = case_when(spouse_htn == 0 & spouse_diagnosed_bp == 0 ~ 1,
                                   spouse_htn == 1 & spouse_diagnosed_bp == 0 ~ 2,
                                   spouse_htn == 1 & spouse_diagnosed_bp == 1 ~ 3,
                                   TRUE ~ NA_real_)) %>% 
  mutate(spouse_status = factor(spouse_status,levels=c(1:3),
                                labels=c("No \nHypertension","Undiagnosed \nHypertension","Diagnosed \nHypertension"))) %>% 
  dplyr::filter(str_detect(variable,"(htn|diagnosed_bp)"))  %>% 
  mutate(variable = case_when(str_detect(variable,"htn") ~ "Hypertension",
                              TRUE ~ "Diagnosed"))


diagnosed_by_spouse_among_total <- read_csv("analysis/summary table of proportion diagnosed and treated among total.csv")  %>% 
  mutate(spouse_status = case_when(spouse_htn == 0 & spouse_diagnosed_bp == 0 ~ 1,
                                   spouse_htn == 1 & spouse_diagnosed_bp == 0 ~ 2,
                                   spouse_htn == 1 & spouse_diagnosed_bp == 1 ~ 3,
                                   TRUE ~ NA_real_)) %>% 
  mutate(spouse_status = factor(spouse_status,levels=c(1:3),
                                labels=c("No \nHypertension","Undiagnosed \nHypertension","Diagnosed \nHypertension"))) %>% 
  dplyr::filter(str_detect(variable,"(htn|diagnosed_bp)")) %>% 
  mutate(variable = case_when(str_detect(variable,"htn") ~ "Hypertension",
                              TRUE ~ "Diagnosed"))  %>% 
  left_join(diagnosed_by_spouse_among_htn %>% 
              dplyr::select(sex,spouse_status,variable,est_ci) %>% 
              rename(text_est_ci = est_ci) %>% 
              mutate(text_est_ci = str_replace(text_est_ci,"\\(","\n\\(")),
            by = c("sex","spouse_status","variable")) %>% 
  mutate(variable = factor(variable,levels=c("Hypertension","Diagnosed")))

fig_A = diagnosed_by_spouse_among_total %>% 
  dplyr::filter(sex == "Wife") %>% 
  ggplot(data=.,aes(x=spouse_status,y=estimate,ymin=lci,ymax=uci,group=variable,fill=variable)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width = 0.2) + 
  geom_text(aes(x=spouse_status,y=estimate*0.5,label=text_est_ci),position=position_dodge(width=0.9),size = 3) +
  scale_fill_manual(name="",values=c("lightblue","orange")) +
  theme_bw() +
  xlab("") +
  ylab("Prevalence (%)")
fig_B = diagnosed_by_spouse_among_total %>% 
  dplyr::filter(sex == "Husband") %>% 
  ggplot(data=.,aes(x=spouse_status,y=estimate,ymin=lci,ymax=uci,group=variable,fill=variable)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width = 0.2) + 
  geom_text(aes(x=spouse_status,y=estimate*0.5,label=text_est_ci),position=position_dodge(width=0.9),size = 3) +
  scale_fill_manual(name="",values=c("lightblue","orange")) +
  theme_bw() +
  xlab("") +
  ylab("Prevalence (%)")

library(ggpubr)
ggarrange(fig_A,fig_B,ncol=2,labels = c("A","B"),
          common.legend = TRUE,legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_couples_folder,"/figures/column diagnosis by spousal status.jpg"),width = 9, height = 5)

