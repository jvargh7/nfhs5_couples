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
                                 TRUE ~ 0)) %>% 
  dplyr::filter(dm_eligible == 1, htn_eligible == 1)  %>% 
  mutate(
         # hh_lengthmar = apply(.[,c("h_lengthmar","w_lengthmar")],1,min,na.rm=TRUE) %>% as.numeric(.),
         hh_children = apply(.[,c("h_nchildren","w_nchildren")],1,max,na.rm=TRUE) %>% as.numeric(.)
  )
