htn <- read_csv("models/n5cm01_htn contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension")
dm <- read_csv("models/n5cm02_dm contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Diabetes")

htn_main <- read_csv("models/n5cm01_htn poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall")
dm_main <- read_csv("models/n5cm02_dm poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_dm","h_dm")) %>% 
  mutate(outcome = "Diabetes",
         label = "Overall")

contrast_map <- readxl::read_excel("models/Contrast Map.xlsx") %>% 
  dplyr::filter(!is.na(label))


tab_stratum <- bind_rows(htn_main,
                         dm_main,
                         bind_rows(htn,
                                   dm) %>% 
                           left_join(contrast_map,
                                     by=c("term","model")) %>% 
                           dplyr::filter(!is.na(label))) %>% 
  dplyr::select(label,outcome,model,RR,theta_D,lci,uci) %>% 
  mutate(est = exp(theta_D),
         sex_self = case_when(str_detect(model,"W") ~ "Wives",
                              TRUE ~ "Husbands"))   %>% 
  mutate(label = factor(label,levels=c("Overall",
                                       "Age < 40",
                                       "Age >= 40",
                                       "Education: None",
                                       "Education: Primary",
                                       "Education: Secondary",
                                       "Education: Higher",
                                       "Residence: Urban",
                                       "Residence: Rural",
                                       "Wealth: Lowest",
                                       "Wealth: Low",
                                       "Wealth: Medium",
                                       "Wealth: High",
                                       "Wealth: Highest",
                                       "Religion: Hindu",
                                       "Religion: Muslim",
                                       "Religion: Other"),ordered=TRUE))


figA <- tab_stratum %>% dplyr::filter(outcome == "Diabetes") %>% 
  ggplot(data=.,
               aes(x=est,y=label,
                   xmin=lci,xmax =uci,
                   color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,6),breaks=seq(0,6,by=1)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) 

figB <- tab_stratum %>% dplyr::filter(outcome == "Hypertension") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,6),breaks=seq(0,6,by=1)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) 

require(ggpubr)
ggarrange(figA,
          figB,
          labels=LETTERS[1:2],
          nrow = 1,ncol=2,
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,
         width = 10,height=6,
         filename=paste0(path_couples_folder,"/figures/nfhs5 strata specific associations.png"))
