state_joint <- read_csv(file = "analysis/n5cm04_state summary of joint prevalence.csv")



source("C:/code/external/nfhs_cascade/functions/state_map.R")

# figA <- state_cascade %>% 
#   dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "dm_screened",plot_title = "A. Rural Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

# figB <- state_cascade %>% 
#   dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "dm_screened",plot_title = "B. Urban Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

plot_type = "ggplot2"


figA <- state_joint %>% 
  state_map(.,plot_variable = "w_dm",plot_title = "A. Wives Diabetes",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn",type=plot_type)

figB <- state_joint %>% 
  state_map(.,plot_variable = "h_dm",plot_title = "B. Husbands Diabetes",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn",type=plot_type)


figC <- state_joint %>% 
  state_map(.,plot_variable = "w_htn",plot_title = "C. Wives Hypertension",breaks = seq(0,60,by=10),palette_chr = "-RdYlGn",type=plot_type)

figD <- state_joint %>% 
  state_map(.,plot_variable = "h_htn",plot_title = "D. Husbands Hypertension",breaks = seq(0,60,by=10),palette_chr = "-RdYlGn",type=plot_type)

if(plot_type == "tmap"){
  tmap_arrange(
    figA,figB,
    figC,figD,
    ncol = 2,nrow=2) %>% 
    tmap_save(.,filename=paste0(path_couples_folder,"/figures/figure_state couples prevalence.png"),width=20,height=10,dpi=600)
  
}

if(plot_type == "ggplot2"){
  library(ggpubr)
  ggarrange(
    figA,figB,
    figC,figD,
    ncol = 2,nrow=2) %>% 
    ggsave(.,filename=paste0(path_couples_folder,"/figures/figure_state couples prevalence_gg.png"),width=16,height=16,dpi=600)
  
}

figA2 <- state_joint %>% 
  state_map(.,plot_variable = "dm_joint",plot_title = "A. Diabetes (%)",breaks = seq(0,10,by=2.5),palette_chr = "-RdYlGn",type=plot_type)


figB2 <- state_joint %>% 
  state_map(.,plot_variable = "htn_joint",plot_title = "B. Hypertension (%)",breaks = seq(0,25,by=5),palette_chr = "-RdYlGn",
            lgd_height = 0.4,lgd_width = 0.4,type=plot_type)


if(plot_type == "tmap"){
  tmap_arrange(
    figA2,figB2,
    ncol = 2,nrow=1) %>% 
    tmap_save(.,filename=paste0(path_couples_folder,"/figures/figure_state joint prevalence.png"),width=16,height=8,dpi=1000)
  
}

if(plot_type == "ggplot2"){
  library(ggpubr)
  ggarrange(
    figA2,figB2,
    ncol = 2,nrow=1) %>% 
    ggsave(.,filename=paste0(path_couples_folder,"/figures/figure_state joint prevalence_gg.png"),width=16,height=8,dpi=600)
  
}



