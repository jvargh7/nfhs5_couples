strata_associations <- read_csv(paste0(path_couples_folder,"/working/nfhs5 concordance/strata specific spousal status associations long.csv"))
india_associations <- read_csv(paste0(path_couples_folder,"/working/nfhs5 concordance/india association of partner status long.csv"))
library(ggpubr)

fig_df <- bind_rows(strata_associations,
                    india_associations %>% mutate(strata = "All India",
                                                  replacement = NA)) %>% 
  dplyr::filter(str_detect(term,
                           "(_dm|_htn)")) %>% 
  mutate(strata = factor(strata,
                         levels=c("All India",
                                  "s_age_lt40",
                                  "s_age_ge40",
                                  "s_wealth_ltq5",
                                  "s_wealth_q5",
                                  "s_region_rural",
                                  "s_region_urban",
                                  "s_religion_hindu",
                                  "s_religion_muslim",
                                  "s_religion_other"),
                         labels=c("All India",
                                  "**Age < 40",
                                  "**Age >= 40",
                                  "Wealth: Bottom 80%",
                                  "Wealth: Top 20%",
                                  "Rurality: Rural",
                                  "Rurality: Urban",
                                  "Religion: Hindu",
                                  "Religion: Muslim",
                                  "Religion: Other"
                                  ))) %>% 
  mutate(lci = estimate/exp(1.96*std.error),
         uci = estimate*exp(1.96*std.error)) %>% 
  mutate(uci = case_when(uci > 3 ~ 3,
                         TRUE ~ uci),
         lci = case_when(lci < 0.5 ~ 0.5,
                         TRUE ~ lci))

figA <- ggplot(data=fig_df %>% dplyr::filter(disease == "Diabetes"),
               aes(x=estimate,y=strata,
                   xmin=lci,xmax =uci,
                   color=partner)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0.5,3),breaks=seq(0.5,3,by=0.5)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

figB <- ggplot(data=fig_df %>% dplyr::filter(disease == "Hypertension"),
               aes(x=estimate,y=strata,
                   xmin=lci,xmax =uci,
                   color=partner)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0.5,3),breaks=seq(0.5,3,by=0.5)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))



ggarrange(figA,
          figB,
          labels=LETTERS[1:2],
          nrow = 1,ncol=2,
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,
         width = 10,height=6,
         filename=paste0(path_couples_folder,"/figures/nfhs5 strata specific associations.png"))
