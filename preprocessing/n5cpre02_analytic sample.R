couples <- readRDS(paste0(path_couples_folder,"/working/nfhs5c couples.RDS")) %>% 
  dplyr::filter(w_age >= 18, h_age >= 21) %>% 
  mutate(w_htn_eligible = case_when(w_diagnosed_bp == 1 ~ 1,
                                !is.na(w_sbp) & !is.na(w_dbp) ~ 1,
                                TRUE ~ 0),
         w_dm_eligible = case_when(w_diagnosed_dm == 1 ~ 1,
                                    !is.na(w_glucose) ~ 1,
                                    TRUE ~ 0),
         
         h_htn_eligible = case_when(h_diagnosed_bp == 1 ~ 1,
                                !is.na(h_sbp) & !is.na(h_dbp) ~ 1,
                                TRUE ~ 0),
         h_dm_eligible = case_when(h_diagnosed_dm == 1 ~ 1,
                                   !is.na(h_glucose) ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(dm_eligible = case_when(w_dm_eligible == 1 & h_dm_eligible ~ 1,
                                 TRUE ~ 0),
         htn_eligible = case_when(w_htn_eligible == 1 & h_htn_eligible ~ 1,
                                 TRUE ~ 0))   %>% 
  mutate(
    hh_lengthmar = pmin(h_lengthmar,w_lengthmar,na.rm=TRUE),
    hh_children = pmin(h_nchildren,w_nchildren,na.rm=TRUE)) %>% 
  mutate(hh_lengthmar_ge10 = case_when(hh_lengthmar >= 10 ~ 1,
                                       hh_lengthmar < 10 ~ 0,
                                       TRUE ~ NA_real_)) %>% 
  
  mutate_at(vars(nmembers,w_nchildren,h_nchildren,w_weight,w_height,h_weight,h_height,
                 w_age,h_age),~as.numeric(.))  %>% 
  mutate(dm_joint = case_when(w_dm == 1 & h_dm == 1 ~ 1,
                              w_dm == 0 | h_dm == 0 ~ 0,
                              TRUE ~ NA_real_),
         htn_joint = case_when(w_htn == 1 & h_htn == 1 ~ 1,
                               w_htn == 0 | h_htn == 0 ~ 0,
                               TRUE ~ NA_real_)
  )


# couples %>% 
#   distinct(cluster,hhid) %>% 
#   nrow()

excluded <- couples %>% 
  dplyr::filter(!(dm_eligible == 1 & htn_eligible == 1)) 


couples <- couples %>% 
  dplyr::filter(dm_eligible == 1, htn_eligible == 1)

# couples %>% 
#   distinct(cluster,hhid) %>% 
#   nrow()

