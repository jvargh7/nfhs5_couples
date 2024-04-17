rm(list=ls());gc();source(".Rprofile")

spouse_status <- read_csv("analysis/summary proportion of diagnosed undiagnosed by spouse diagnosis.csv")
diagnosed_among_hypertension <-   read_csv("analysis/summary table of analytic sample.csv") %>% 
  dplyr::filter(str_detect(variable,"_diagnosed_bp"))

undiagnosed_among_hypertension <-   read_csv("analysis/summary table of analytic sample.csv") %>% 
  dplyr::filter(str_detect(variable,"_undiagnosed_bp"))

total_hypertension <-   read_csv("analysis/summary table of analytic sample.csv") %>% 
  dplyr::filter(str_detect(variable,"htn"),!str_detect(variable,"joint"))

# Wives ----------
wives_df <- bind_rows(
  total_hypertension %>% 
    dplyr::filter(variable == "w_htn") %>%
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Total Hypertension",
           spouse_status = "Total"),
  
  spouse_status %>% 
    dplyr::filter(sex == "Wife") %>% 
    dplyr::select(estimate,diagnosis_status,spouse_status,lci,uci,est_ci) %>% 
    rename(x_var = diagnosis_status)
  
) %>% 
  mutate(x_var = factor(x_var,levels=c("Total Hypertension","Diagnosed","Undiagnosed")),
         spouse_status = factor(spouse_status,levels=c("Total","Diagnosed","Undiagnosed","No Hypertension")))


wives_errorbar_df = bind_rows(
  total_hypertension %>% 
    dplyr::filter(variable == "w_htn") %>%
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Total Hypertension",
           spouse_status = "Total"),
  
  diagnosed_among_hypertension %>% 
    dplyr::filter(variable == "w_diagnosed_bp") %>% 
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Diagnosed",
           spouse_status = "Total"),
  
  undiagnosed_among_hypertension %>% 
    dplyr::filter(variable == "w_undiagnosed_bp") %>% 
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Undiagnosed",
           spouse_status = "Total")
  
)

# Husbands ---------

husbands_df <- bind_rows(
  total_hypertension %>% 
    dplyr::filter(variable == "h_htn") %>%
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Total Hypertension",
           spouse_status = "Total"),
  
  spouse_status %>% 
    dplyr::filter(sex == "Husband") %>% 
    dplyr::select(estimate,diagnosis_status,spouse_status,lci,uci,est_ci) %>% 
    rename(x_var = diagnosis_status)
  
) %>% 
  mutate(x_var = factor(x_var,levels=c("Total Hypertension","Diagnosed","Undiagnosed")),
         spouse_status = factor(spouse_status,levels=c("Total","Diagnosed","Undiagnosed","No Hypertension")))


husbands_errorbar_df = bind_rows(
  total_hypertension %>% 
    dplyr::filter(variable == "h_htn") %>%
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Total Hypertension",
           spouse_status = "Total"),
  
  diagnosed_among_hypertension %>% 
    dplyr::filter(variable == "h_diagnosed_bp") %>% 
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Diagnosed",
           spouse_status = "Total"),
  
  undiagnosed_among_hypertension %>% 
    dplyr::filter(variable == "h_undiagnosed_bp") %>% 
    dplyr::select(estimate,lci,uci,est_ci) %>% 
    mutate(x_var = "Undiagnosed",
           spouse_status = "Total")
  
)

fig_A = wives_df %>% 
  ggplot(data=.,aes(x=x_var,y=estimate,fill=spouse_status,group=x_var)) +
  geom_col() +
  geom_errorbar(data=wives_errorbar_df,aes(x=x_var,ymin=lci,ymax = uci),width=0.2) +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_fill_manual(name="Spouse:",values=c("grey20","#375a66","#698994","#cad8de")) +
  scale_y_continuous(limits=c(0,30))


fig_B = husbands_df %>% 
  ggplot(data=.,aes(x=x_var,y=estimate,fill=spouse_status,group=x_var)) +
  geom_col() +
  geom_errorbar(data=husbands_errorbar_df,aes(x=x_var,ymin=lci,ymax = uci),width=0.2) +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_fill_manual(name="Spouse:",values=c("grey20","#375a66","#698994","#cad8de"))  +
  scale_y_continuous(limits=c(0,30))

library(ggpubr)
ggarrange(fig_A,
          fig_B,
          labels=c("A","B"),
          common.legend = TRUE,
          nrow = 1,
          ncol = 2,
          legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_couples_folder,"/figures/column total diagnosed undiagnosed by spouse.png"),width=10,height = 6)
