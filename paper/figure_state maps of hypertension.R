rm(list=ls());gc();source(".Rprofile")

state_joint <- read_csv(file = "analysis/n5cm04_state summary of joint prevalence.csv")



source("C:/code/external/nfhs_cascade/functions/state_map.R")

plot_type = "ggplot2"

figA <- state_joint %>% 
  state_map(.,plot_variable = "w_htn",plot_title = "A",breaks = seq(0,60,by=10),palette_chr = "-RdYlGn",type=plot_type)

figB <- state_joint %>% 
  state_map(.,plot_variable = "h_htn",plot_title = "B",breaks = seq(0,60,by=10),palette_chr = "-RdYlGn",type=plot_type)

figC <- state_joint %>% 
  state_map(.,plot_variable = "htn_joint",plot_title = "C",breaks = seq(0,60,by=10),palette_chr = "-RdYlGn",
            lgd_height = 0.4,lgd_width = 0.4,type=plot_type)


if(plot_type == "ggplot2"){
  library(ggpubr)
  ggarrange(
    figA,figB,
    # figC,
    ncol = 2,nrow=1,common.legend = TRUE,
    legend = "bottom") %>% 
    ggsave(.,filename=paste0(path_couples_folder,"/figures/figure_state maps of hypertension_gg.png"),width=16,height=8,dpi=600)
  
}

plot_type2 = "tmap"

figC <- state_joint %>% 
  state_map(.,plot_variable = "htn_joint",plot_title = "",breaks = seq(0,25,by=5),palette_chr = "-RdYlGn",
            lgd_height = 0.4,lgd_width = 0.4,type=plot_type2)
tmap_save(figC,filename=paste0(path_couples_folder,"/figures/figure_state map of joint prevalence of hypertension.png"),width=8,height=8,dpi=600)
