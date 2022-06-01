
couples_variables <- readxl::read_excel("C:/code/external/nfhs4_couples/preprocessing/NFHS4 Couples Variable List.xlsx",
                                        sheet = "iacr7a variables")

iacr_variables <- na.omit(c(couples_variables$female,
                            couples_variables$male))

iacr_variables <- iacr_variables[!iacr_variables %in% c("mcaseid")]


iacr_extract <- read_dta(paste0(path_india_raw_data,"/IACR7ADT/IACR7AFL.dta"),
                         col_select = iacr_variables)

iapr_extract <- read_dta(paste0(path_india_raw_data,"/IAPR7ADT/IAPR7AFL.dta"),
                         col_select = na.omit(couples_variables$iapr7a))


iapr_male <- iapr_extract %>% 
  dplyr::filter(!is.na(hb1))
rm(iapr_extract)
gc()

# couples %>% distinct(v001,v002,.keep_all = TRUE) %>% View()

couples <- iacr_extract %>% 
  left_join(iapr_male,
            by=c("mv003"="hvidx",
                 "mv002" = "hv002",
                 "mv001" = "hv001")) %>% 
  dplyr::filter(v454 == 0) %>% 
  # Blood pressure cleaning -------
mutate_at(vars(sb18s,sb18d,
               sb25s,sb25d,
               sb29s,sb29d,
               smb18s,smb18d,
               smb25s,smb25d,
               smb29s,smb29d),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                                    TRUE ~ as.numeric(x))) %>% 
  
  mutate(m_self_diabetes = case_when(sm627a == 1 | smb56 == 1 ~ 1,
                                     TRUE ~ 0),
         f_self_diabetes = case_when(s728a == 1 | sb56 == 1 ~ 1,
                                     TRUE ~ 0),
         m_self_everdiabmed = case_when(sm627ab == 1 ~ 1,
                                    TRUE ~ 0),
         f_self_everdiabmed = case_when(s728ab == 1 ~ 1,
                                        TRUE ~ 0),
         
         m_self_currdiabmed = case_when(smb57 == 1 ~ 1,
                                        TRUE ~ 0),
         f_self_currdiabmed = case_when(sb57 == 1 ~ 1,
                                        TRUE ~ 0),
         
         m_self_hypertension = case_when(sm627b == 1 | smb20 == 1 ~ 1,
                                         TRUE ~ 0),
         f_self_hypertension = case_when(s728b == 1 | sb20 == 1 ~ 1,
                                         TRUE ~ 0),
         m_self_everhtnmed = case_when(sm627bb == 1 ~ 1,
                                        TRUE ~ 0),
         f_self_everhtnmed = case_when(s728bb == 1 ~ 1,
                                        TRUE ~ 0),
         
         m_self_currhtnmed = case_when(smb21 == 1 ~ 1,
                                        TRUE ~ 0),
         f_self_currhtnmed = case_when(sb21 == 1 ~ 1,
                                        TRUE ~ 0)) %>% 
  
  mutate(f_hemo_anemia = case_when(v456 >= 250 | v456 < 3 ~ NA_real_,
                                   v456 < 120 ~ 1,
                                   v456 >= 120 ~ 0,
                                   TRUE ~ NA_real_),
         m_hemo_anemia = case_when(hb56 >= 250 | hb56 < 3 ~ NA_real_,
                                   hb56 < 130 ~ 1,
                                   hb56 >= 130 ~ 0,
                                   TRUE ~ NA_real_),
         
         f_bmi_underweight = case_when(v445 > 6000 ~ NA_real_,
                                       v445 < 1850 ~ 1,
                                       v445 >= 1850 ~ 0,
                                       TRUE ~ NA_real_),
         m_bmi_underweight = case_when(hb40 > 6000 ~ NA_real_,
                                       hb40 < 1850 ~ 1,
                                       hb40 >= 1850 ~ 0,
                                       TRUE ~ NA_real_),
         
         f_bmi_overweight = case_when(v445 > 6000 ~ NA_real_,
                                      v445 >= 2500 & v445 < 3000 ~ 1,
                                      v445 < 2500 | v445 >= 3000 ~ 0,
                                      TRUE ~ NA_real_),
         m_bmi_overweight = case_when(hb40 > 6000 ~ NA_real_,
                                      hb40 >= 2500 & hb40 < 3000 ~ 1,
                                      hb40 < 2500 | hb40 >= 3000 ~ 0,
                                      TRUE ~ NA_real_),
         
         f_bmi_obese = case_when(v445 > 6000 ~ NA_real_,
                                 v445 >= 3000 ~ 1,
                                 v445 < 3000 ~ 0,
                                 TRUE ~ NA_real_),
         m_bmi_obese = case_when(hb40 > 6000 ~ NA_real_,
                                 hb40 >= 3000 ~ 1,
                                 hb40 < 3000 ~ 0,
                                 TRUE ~ NA_real_),
         
         f_fasting = case_when(sb53 > 94 | sb54 > 94 ~ NA_real_,
                               sb53 > 8 & sb54 > 8 ~ 1,
                               sb53 <=8 | sb54 <= 8 ~ 0,
                               TRUE ~ NA_real_),
         m_fasting = case_when(smb53 > 94 | smb54 > 94 ~ NA_real_,
                               smb53 > 8 & smb54 > 8 ~ 1,
                               smb53 <=8 | smb54 <= 8 ~ 0,
                               TRUE ~ NA_real_),
         
         f_ifg = case_when(is.na(sb74) | sb74 > 498 ~ NA_real_,
                           f_self_diabetes == 1 ~ NA_real_, # Exclude those who have diabetes
                           f_fasting == 1 & sb74 >= 100 & sb74 < 126 ~ 1,
                           f_fasting == 1 & (sb74 < 100 | sb74 >= 126) ~ 0,
                           f_fasting == 0 ~ NA_real_,
                           TRUE ~ NA_real_),
         m_ifg = case_when(is.na(smb74) | smb74 > 498 ~ NA_real_,
                           m_self_diabetes == 1 ~ NA_real_, # Exclude those who have diabetes
                           m_fasting == 1 & smb74 >= 100 & smb74 < 126 ~ 1,
                           m_fasting == 1 & (smb74 < 100 | smb74 >= 126) ~ 0,
                           m_fasting == 0 ~ NA_real_,
                           TRUE ~ NA_real_),
         
         f_igt = case_when(is.na(sb74) | sb74 > 498 ~ NA_real_,
                           f_self_diabetes == 1 ~ NA_real_, # Exclude those who have diabetes
                           f_fasting == 0 & sb74 >= 140 & sb74 < 200 ~ 1,
                           f_fasting == 0 & (sb74 < 140 | sb74 >= 200) ~ 0,
                           f_fasting == 1 ~ NA_real_,
                           TRUE ~ NA_real_),
         m_igt = case_when(is.na(smb74) | smb74 > 498 ~ NA_real_,
                           m_self_diabetes == 1 ~ NA_real_, # Exclude those who have diabetes
                           m_fasting == 0 & smb74 >= 140 & smb74 < 200 ~ 1,
                           m_fasting == 0 & (smb74 < 140 | smb74 >= 200) ~ 0,
                           m_fasting == 1 ~ NA_real_,
                           TRUE ~ NA_real_),
         
         f_diagifg = case_when(is.na(sb74) | sb74 > 498 ~ NA_real_,
                               f_self_diabetes == 1 & f_fasting == 1 & sb74 >= 100 & sb74 < 126 ~ 1,
                               f_self_diabetes == 1 & f_fasting == 1 & (sb74 < 100 | sb74 >= 126) ~ 0,
                               f_self_diabetes == 1 & f_fasting == 0 ~ NA_real_,
                               TRUE ~ NA_real_),
         m_diagifg = case_when(is.na(smb74) | smb74 > 498 ~ NA_real_,
                               m_self_diabetes == 1 & m_fasting == 1 & smb74 >= 100 & smb74 < 126 ~ 1,
                               m_self_diabetes == 1 & m_fasting == 1 & (smb74 < 100 | smb74 >= 126) ~ 0,
                               m_self_diabetes == 1 & m_fasting == 0 ~ NA_real_,
                               TRUE ~ NA_real_),
         
         f_diagigt = case_when(is.na(sb74) | sb74 > 498 ~ NA_real_,
                               f_self_diabetes == 1 & f_fasting == 0 & sb74 >= 140 & sb74 < 200 ~ 1,
                               f_self_diabetes == 1 & f_fasting == 0 & (sb74 < 140 | sb74 >= 200) ~ 0,
                               f_self_diabetes == 1 & f_fasting == 1 ~ NA_real_,
                               TRUE ~ NA_real_),
         m_diagigt = case_when(is.na(smb74) | smb74 > 498 ~ NA_real_,
                               m_self_diabetes == 1 & m_fasting == 0 & smb74 >= 140 & smb74 < 200 ~ 1,
                               m_self_diabetes == 1 & m_fasting == 0 & (smb74 < 140 | smb74 >= 200) ~ 0,
                               m_self_diabetes == 1 & m_fasting == 1 ~ NA_real_,
                               TRUE ~ NA_real_),
         
         f_dm = case_when(f_self_diabetes == 1 ~ 1,
                          is.na(sb74) | sb74 > 498 ~ NA_real_,
                          f_fasting == 1 & sb74 >= 126 ~ 1,
                          f_fasting == 0 & sb74 >= 200 ~ 1,
                          is.na(f_fasting) & sb74 >= 200 ~ 1,
                          f_fasting == 1 & sb74 < 126 ~ 0,
                          f_fasting == 0 & sb74 < 200 ~ 0,
                          is.na(f_fasting) & sb74 < 200 ~ 0,
                          TRUE  ~ NA_real_
         ),
         m_dm = case_when(m_self_diabetes == 1 ~ 1,
                          is.na(smb74) | smb74 > 498 ~ NA_real_,
                          m_fasting == 1 & smb74 >= 126 ~ 1,
                          m_fasting == 0 & smb74 >= 200 ~ 1,
                          is.na(m_fasting) & smb74 >= 200 ~ 1,
                          m_fasting == 1 & smb74 < 126 ~ 0,
                          m_fasting == 0 & smb74 < 200 ~ 0,
                          is.na(m_fasting) & smb74 < 200 ~ 0,
                          TRUE  ~ NA_real_
         ),
         
         f_diagdm = case_when(
           is.na(sb74) | sb74 > 498 ~ NA_real_,
           f_self_diabetes == 1 & f_fasting == 1 & sb74 >= 126 ~ 1,
           f_self_diabetes == 1 & f_fasting == 0 & sb74 >= 200 ~ 1,
           f_self_diabetes == 1 & is.na(f_fasting) & sb74 >= 200 ~ 1,
           f_self_diabetes == 1 & f_fasting == 1 & sb74 < 126 ~ 0,
           f_self_diabetes == 1 & f_fasting == 0 & sb74 < 200 ~ 0,
           f_self_diabetes == 1 & is.na(f_fasting) & sb74 < 200 ~ 0,
           TRUE  ~ NA_real_
         ),
         m_diagdm = case_when(
           is.na(smb74) | smb74 > 498 ~ NA_real_,
           m_self_diabetes == 1 & m_fasting == 1 & smb74 >= 126 ~ 1,
           m_self_diabetes == 1 & m_fasting == 0 & smb74 >= 200 ~ 1,
           m_self_diabetes == 1 & is.na(m_fasting) & smb74 >= 200 ~ 1,
           m_self_diabetes == 1 & m_fasting == 1 & smb74 < 126 ~ 0,
           m_self_diabetes == 1 & m_fasting == 0 & smb74 < 200 ~ 0,
           m_self_diabetes == 1 & is.na(m_fasting) & smb74 < 200 ~ 0,
           TRUE  ~ NA_real_
         ),
         
         
         f_sbp = rowMeans(.[,c("sb18s","sb25s","sb29s")],na.rm=TRUE),
         m_sbp = rowMeans(.[,c("smb18s","smb25s","smb29s")],na.rm=TRUE),
         
         # "sb18d" has 108 everywhere
         f_dbp = rowMeans(.[,c("sb25d","sb29d")],na.rm=TRUE),
         m_dbp = rowMeans(.[,c("smb18d","smb25d","smb29d")],na.rm=TRUE),
         
         f_prehtn = case_when(f_self_hypertension == 1 ~ NA_real_,
                              is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                              f_sbp >= 120 & f_sbp < 140 ~ 1,
                              f_dbp >= 80 & f_dbp < 90 ~ 1,
                              f_sbp < 120 | f_sbp >= 140 ~ 0,
                              f_dbp < 80 | f_dbp >= 90 ~ 0,
                              TRUE ~ NA_real_),
         m_prehtn = case_when(m_self_hypertension == 1 ~ NA_real_,
                              is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                              m_sbp >= 120 & m_sbp < 140 ~ 1,
                              m_dbp >= 80 & m_dbp < 90 ~ 1,
                              m_dbp < 120 | m_dbp >= 140 ~ 0,
                              m_dbp < 80 | m_dbp >= 90 ~ 0,
                              TRUE ~ NA_real_),
         
         f_prehtn2 = case_when(f_self_hypertension == 1 ~ NA_real_,
                               is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                               f_sbp >= 130 & f_sbp < 140 ~ 1,
                               f_dbp >= 85 & f_dbp < 90 ~ 1,
                               f_sbp < 130 | f_sbp >= 140 ~ 0,
                               f_dbp < 85 | f_dbp >= 90 ~ 0,
                               TRUE ~ NA_real_),
         m_prehtn2 = case_when(m_self_hypertension == 1 ~ NA_real_,
                               is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                               m_sbp >= 130 & m_sbp < 140 ~ 1,
                               m_dbp >= 85 & m_dbp < 90 ~ 1,
                               m_dbp < 130 | m_dbp >= 140 ~ 0,
                               m_dbp < 85 | m_dbp >= 90 ~ 0,
                               TRUE ~ NA_real_),
         
         f_htn = case_when(f_self_hypertension == 1 ~ 1,
                           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                           f_sbp >= 140 ~ 1,
                           f_dbp >= 90 ~ 1,
                           f_sbp < 140 ~ 0,
                           f_dbp < 90 ~ 0,
                           TRUE ~ NA_real_),
         
         m_htn = case_when(m_self_hypertension == 1 ~ 1,
                           is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                           m_sbp >= 140 ~ 1,
                           m_dbp >= 90 ~ 1,
                           m_sbp < 140 ~ 0,
                           m_dbp < 90 ~ 0,
                           TRUE ~ NA_real_),
         
         f_diagprehtn = case_when(
           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
           f_self_hypertension == 1 & f_sbp >= 120 & f_sbp < 140 ~ 1,
           f_self_hypertension == 1 & (f_dbp >= 80 & f_dbp < 90) ~ 1,
           f_self_hypertension == 1 & (f_sbp < 120 | f_sbp >= 140) ~ 0,
           f_self_hypertension == 1 & (f_dbp < 80 | f_dbp >= 90) ~ 0,
           TRUE ~ NA_real_),
         m_diagprehtn = case_when(
           is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
           m_self_hypertension == 1 & (m_sbp >= 120 & m_sbp < 140) ~ 1,
           m_self_hypertension == 1 & (m_dbp >= 80 & m_dbp < 90) ~ 1,
           m_self_hypertension == 1 & (m_dbp < 120 | m_dbp >= 140) ~ 0,
           m_self_hypertension == 1 & (m_dbp < 80 | m_dbp >= 90) ~ 0,
           TRUE ~ NA_real_),
         
         f_diagprehtn2 = case_when(
           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
           f_self_hypertension == 1 & f_sbp >= 130 & f_sbp < 140 ~ 1,
           f_self_hypertension == 1 & (f_dbp >= 85 & f_dbp < 90) ~ 1,
           f_self_hypertension == 1 & (f_sbp < 130 | f_sbp >= 140) ~ 0,
           f_self_hypertension == 1 & (f_dbp < 85 | f_dbp >= 90) ~ 0,
           TRUE ~ NA_real_),
         m_diagprehtn2 = case_when(
           is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
           m_self_hypertension == 1 & (m_sbp >= 130 & m_sbp < 140) ~ 1,
           m_self_hypertension == 1 & (m_dbp >= 85 & m_dbp < 90) ~ 1,
           m_self_hypertension == 1 & (m_dbp < 130 | m_dbp >= 140) ~ 0,
           m_self_hypertension == 1 & (m_dbp < 85 | m_dbp >= 90) ~ 0,
           TRUE ~ NA_real_),
         
         
         
         f_diaghtn = case_when(
           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
           f_self_hypertension == 1 & f_sbp >= 140 ~ 1,
           f_self_hypertension == 1 & f_dbp >= 90 ~ 1,
           f_self_hypertension == 1 & f_sbp < 140 ~ 0,
           f_self_hypertension == 1 & f_dbp < 90 ~ 0,
           TRUE ~ NA_real_),
         
         m_diaghtn = case_when(
           is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
           m_self_hypertension == 1 & m_sbp >= 140 ~ 1,
           m_self_hypertension == 1 & m_dbp >= 90 ~ 1,
           m_self_hypertension == 1 & m_sbp < 140 ~ 0,
           m_self_hypertension == 1 & m_dbp < 90 ~ 0,
           TRUE ~ NA_real_)
         
  ) %>% 
  
  # Diabetes cascade -----
mutate(f_dm_sample = case_when(is.na(sb74) | sb74 > 498 | is.na(s728a) ~ 0,
                               TRUE ~ 1),
       m_dm_sample = case_when(is.na(smb74) | smb74 > 498 | is.na(sm627a) ~ 0,
                               TRUE ~ 1),
       f_dmpre_undiag = case_when(
         f_self_diabetes == 1 ~ NA_real_,
         is.na(f_ifg) & is.na(f_igt) ~ NA_real_,
         rowSums(.[,c("f_ifg","f_igt")],na.rm=TRUE) > 0 ~ 1,
         rowSums(.[,c("f_ifg","f_igt")],na.rm=TRUE) == 0 ~ 0,
         TRUE ~ NA_real_
       ),
       
       m_dmpre_undiag = case_when(
         m_self_diabetes == 1 ~ NA_real_,
         is.na(m_ifg) & is.na(m_igt) ~ NA_real_,
         rowSums(.[,c("m_ifg","m_igt")],na.rm=TRUE) > 0 ~ 1,
         rowSums(.[,c("m_ifg","m_igt")],na.rm=TRUE) == 0 ~ 0,
         TRUE ~ NA_real_
       ),
       # Diagnosis: No/DK, Blood sugar: in range
       f_dm_free = case_when(f_self_diabetes == 1 ~ 0,
                             is.na(f_ifg) & is.na(f_igt) & is.na(f_dm) ~ NA_real_,
                             rowSums(.[,c("f_ifg","f_igt","f_dm")],na.rm=TRUE) > 0 ~ 0,
                             rowSums(.[,c("f_ifg","f_igt","f_dm")],na.rm=TRUE) == 0 ~ 1,
                             TRUE ~ NA_real_),
       m_dm_free = case_when(m_self_diabetes == 1 ~ 0,
                             is.na(m_ifg) & is.na(m_igt) & is.na(m_dm) ~ NA_real_,
                             rowSums(.[,c("m_ifg","m_igt","m_dm")],na.rm=TRUE) > 0 ~ 0,
                             rowSums(.[,c("m_ifg","m_igt","m_dm")],na.rm=TRUE) == 0 ~ 1,
                             TRUE ~ NA_real_),
       
       # Diagnosis: No/DK + Blood sugar: prediabetes
       f_dm_undiagpre_uncontr = case_when(f_self_diabetes == 1 ~ NA_real_,
                                          is.na(f_ifg) & is.na(f_igt) & is.na(f_dm) ~ NA_real_,
                                          rowSums(.[,c("f_ifg","f_igt")],na.rm=TRUE) > 0 ~ 1,
                                          rowSums(.[,c("f_ifg","f_igt")],na.rm=TRUE) == 0 ~ 0,
                                          TRUE ~ NA_real_),
       m_dm_undiagpre_uncontr = case_when(m_self_diabetes == 1 ~ NA_real_,
                                          is.na(m_ifg) & is.na(m_igt) & is.na(m_dm) ~ NA_real_,
                                          rowSums(.[,c("m_ifg","m_igt")],na.rm=TRUE) > 0 ~ 1,
                                          rowSums(.[,c("m_ifg","m_igt")],na.rm=TRUE) == 0 ~ 0,
                                          TRUE ~ NA_real_),
       
       # Diagnosis: No/DK + Blood sugar: diabetes
       f_dm_undiag_uncontr = case_when(f_self_diabetes == 1 ~ NA_real_,
                                       is.na(f_ifg) & is.na(f_igt) & is.na(f_dm) ~ NA_real_,
                                       f_dm == 1 ~ 1,
                                       f_dm == 0 ~ 0,
                                       TRUE ~ NA_real_),
       m_dm_undiag_uncontr = case_when(m_self_diabetes == 1 ~ NA_real_,
                                       is.na(m_ifg) & is.na(m_igt) & is.na(m_dm) ~ NA_real_,
                                       m_dm == 1 ~ 1,
                                       m_dm == 0 ~ 0,
                                       TRUE ~ NA_real_),
       
       
       # Diagnosis: Yes + Treated: No, Blood sugar: <NA>
       f_dm_diag_untreat = case_when(f_self_diabetes == 1 & s728ab == 1 ~ 0,
                                     f_self_diabetes == 1 & s728ab == 0 ~ 1,
                                     TRUE ~ NA_real_),
       m_dm_diag_untreat = case_when(m_self_diabetes == 1 & sm627ab == 1 ~ 0,
                                     m_self_diabetes == 1 & sm627ab == 0 ~ 1,
                                     TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: Yes, Blood sugar: out of range
       f_dm_treat_uncontr = case_when(f_self_diabetes == 0 | (f_self_everdiabmed == 0 & f_self_currdiabmed == 0) ~ NA_real_,
                                      f_self_diabetes == 1 & (f_self_everdiabmed == 1 | f_self_currdiabmed == 1) & 
                                        rowSums(.[,c("f_diagifg","f_diagigt","f_diagdm")],na.rm=TRUE) > 0 ~ 1,
                                      f_self_diabetes == 1 & (f_self_everdiabmed == 1 | f_self_currdiabmed == 1) & 
                                        rowSums(.[,c("f_diagifg","f_diagigt","f_diagdm")],na.rm=TRUE) == 0 ~ 0,
                                      TRUE ~ NA_real_),
       m_dm_treat_uncontr = case_when(m_self_diabetes == 0 | (m_self_everdiabmed == 0 & m_self_currdiabmed == 0) ~ NA_real_,
                                      m_self_diabetes == 1 & (m_self_everdiabmed == 1 | m_self_currdiabmed == 1) & 
                                        rowSums(.[,c("m_diagifg","m_diagigt","m_diagdm")],na.rm=TRUE) > 0 ~ 1,
                                      m_self_diabetes == 1 & (m_self_everdiabmed == 1 | m_self_currdiabmed == 1) &
                                        rowSums(.[,c("m_diagifg","m_diagigt","m_diagdm")],na.rm=TRUE) == 0 ~ 0,
                                      TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: Yes, Blood sugar: out of range
       f_dm_treat_contr = case_when(f_self_diabetes == 0 | (f_self_everdiabmed == 0 & f_self_currdiabmed == 0) ~ NA_real_,
                                    f_self_diabetes == 1 & (f_self_everdiabmed == 1 | f_self_currdiabmed == 1) & 
                                      rowSums(.[,c("f_diagifg","f_diagigt","f_diagdm")],na.rm=TRUE) > 0 ~ 0,
                                    f_self_diabetes == 1 & (f_self_everdiabmed == 1 | f_self_currdiabmed == 1) & 
                                      rowSums(.[,c("f_diagifg","f_diagigt","f_diagdm")],na.rm=TRUE) == 0 ~ 1,
                                    TRUE ~ NA_real_),
       m_dm_treat_contr = case_when(m_self_diabetes == 0 | (m_self_everdiabmed == 0 & m_self_currdiabmed == 0) ~ NA_real_,
                                    m_self_diabetes == 1 & (m_self_everdiabmed == 1 | m_self_currdiabmed == 1) & 
                                      rowSums(.[,c("m_diagifg","m_diagigt","m_diagdm")],na.rm=TRUE) > 0 ~ 0,
                                    m_self_diabetes == 1 & (m_self_everdiabmed == 1 | m_self_currdiabmed == 1) &
                                      rowSums(.[,c("m_diagifg","m_diagigt","m_diagdm")],na.rm=TRUE) == 0 ~ 1,
                                    TRUE ~ NA_real_)
) %>% 
  # HTN cascade -----
mutate(
  f_htn_sample = case_when(!is.na(f_sbp)|!is.na(f_dbp) ~ 1,
                           is.na(f_sbp) & is.na(f_dbp) ~ 0,
                           TRUE ~ 1),
  m_htn_sample = case_when(!is.na(m_sbp)|!is.na(m_dbp) ~ 1,
                           is.na(m_sbp) & is.na(m_dbp) ~ 0,
                           TRUE ~ 1),
  # Diagnosis: No/DK, Blood pressure: in range
  f_htn_free = case_when(f_self_hypertension == 1 ~ 0,
                         f_prehtn == 1 | f_htn == 1 ~ 0,
                         f_prehtn == 0 & f_htn == 0 ~ 1,
                         TRUE ~ NA_real_),
  m_htn_free = case_when(m_self_hypertension == 1 ~ 0,
                         m_prehtn == 1 | m_htn == 1 ~ 0,
                         m_prehtn == 0 & m_htn == 0 ~ 1,
                         TRUE ~ NA_real_),
  
  # Diagnosis: No/DK, Blood pressure: in range
  f_htn_free2 = case_when(f_self_hypertension == 1 ~ 0,
                          f_prehtn2 == 1 | f_htn == 1 ~ 0,
                          f_prehtn2 == 0 & f_htn == 0 ~ 1,
                          TRUE ~ NA_real_),
  m_htn_free2 = case_when(m_self_hypertension == 1 ~ 0,
                          m_prehtn2 == 1 | m_htn == 1 ~ 0,
                          m_prehtn2 == 0 & m_htn == 0 ~ 1,
                          TRUE ~ NA_real_),
  
  # Diagnosis: No/DK + Blood pressure: out of range
  f_htn_undiagpre_uncontr = case_when(f_self_hypertension == 1 ~ NA_real_,
                                      f_prehtn == 1  ~ 1,
                                      f_prehtn == 0  ~ 0,
                                      TRUE ~ NA_real_),
  m_htn_undiagpre_uncontr = case_when(m_self_hypertension == 1 ~ NA_real_,
                                      m_prehtn == 1  ~ 1,
                                      m_prehtn == 0 ~ 0,
                                      TRUE ~ NA_real_),
  
  # Diagnosis: No/DK + Blood pressure: out of range
  f_htn_undiagpre2_uncontr = case_when(f_self_hypertension == 1 ~ NA_real_,
                                       f_prehtn2 == 1  ~ 1,
                                       f_prehtn2 == 0  ~ 0,
                                       TRUE ~ NA_real_),
  m_htn_undiagpre2_uncontr = case_when(m_self_hypertension == 1 ~ NA_real_,
                                       m_prehtn2 == 1  ~ 1,
                                       m_prehtn2 == 0 ~ 0,
                                       TRUE ~ NA_real_),
  
  # Diagnosis: No/DK + Blood pressure: out of range
  f_htn_undiag_uncontr = case_when(f_self_hypertension == 1 ~ NA_real_,
                                   f_htn == 1 ~ 1,
                                   f_htn == 0 ~ 0,
                                   TRUE ~ NA_real_),
  m_htn_undiag_uncontr = case_when(m_self_hypertension == 1 ~ NA_real_,
                                   m_htn == 1 ~ 1,
                                   m_htn == 0 ~ 0,
                                   TRUE ~ NA_real_),
  
  # Diagnosis: Yes + Treated: No, Blood pressure: <NA>
  f_htn_diag_untreat = case_when(f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) ~ 0,
                                 f_self_hypertension == 1 & (f_self_everhtnmed == 0 & f_self_currhtnmed == 0) ~ 1,
                                 TRUE ~ NA_real_),
  m_htn_diag_untreat = case_when(m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) ~ 0,
                                 m_self_hypertension == 1 & (m_self_everhtnmed == 0 & m_self_currhtnmed == 0) ~ 1,
                                 TRUE ~ NA_real_),
  # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
  f_htn_treat_uncontr = case_when((f_self_everhtnmed == 0 & f_self_currhtnmed == 0) ~ NA_real_,
                                  f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                    (f_diagprehtn == 1 | f_diaghtn == 1) ~ 1,
                                  f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                    (f_diagprehtn == 0 & f_diaghtn == 0) ~ 0,
                                  TRUE ~ NA_real_),
  m_htn_treat_uncontr = case_when((m_self_everhtnmed == 0 & m_self_currhtnmed == 0) ~ NA_real_,
                                  m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                    (m_diagprehtn == 1 | m_diaghtn == 1) ~ 1,
                                  m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                    (m_diagprehtn == 0 & m_diaghtn == 0) ~ 0,
                                  TRUE ~ NA_real_),
  # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
  f_htn_treat2_uncontr = case_when((f_self_everhtnmed == 0 & f_self_currhtnmed == 0) ~ NA_real_,
                                   f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                     (f_diagprehtn2 == 1 | f_diaghtn == 1) ~ 1,
                                   f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                     (f_diagprehtn2 == 0 & f_diaghtn == 0) ~ 0,
                                   TRUE ~ NA_real_),
  m_htn_treat2_uncontr = case_when((m_self_everhtnmed == 0 & m_self_currhtnmed == 0) ~ NA_real_,
                                   m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                     (m_diagprehtn2 == 1 | m_diaghtn == 1) ~ 1,
                                   m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                     (m_diagprehtn2 == 0 & m_diaghtn == 0) ~ 0,
                                   TRUE ~ NA_real_),
  
  # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
  f_htn_treat_contr = case_when((f_self_everhtnmed == 0 & f_self_currhtnmed == 0) ~ NA_real_,
                                f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                  (f_diagprehtn == 1 | f_diaghtn == 1) ~ 0,
                                f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                  (f_diagprehtn == 0 & f_diaghtn == 0) ~ 1,
                                TRUE ~ NA_real_),
  m_htn_treat_contr = case_when((m_self_everhtnmed == 0 & m_self_currhtnmed == 0) ~ NA_real_,
                                m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                  (m_diagprehtn == 1 | m_diaghtn == 1) ~ 0,
                                m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                  (m_diagprehtn == 0 & m_diaghtn == 0) ~ 1,
                                TRUE ~ NA_real_),
  
  # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
  f_htn_treat2_contr = case_when((f_self_everhtnmed == 0 & f_self_currhtnmed == 0) ~ NA_real_,
                                 f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                   (f_diagprehtn2 == 1 | f_diaghtn == 1) ~ 0,
                                 f_self_hypertension == 1 & (f_self_everhtnmed == 1 | f_self_currhtnmed == 1) & 
                                   (f_diagprehtn2 == 0 & f_diaghtn == 0) ~ 1,
                                 TRUE ~ NA_real_),
  m_htn_treat2_contr = case_when((m_self_everhtnmed == 0 & m_self_currhtnmed == 0) ~ NA_real_,
                                 m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                   (m_diagprehtn2 == 1 | m_diaghtn == 1) ~ 0,
                                 m_self_hypertension == 1 & (m_self_everhtnmed == 1 | m_self_currhtnmed == 1) & 
                                   (m_diagprehtn2 == 0 & m_diaghtn == 0) ~ 1,
                                 TRUE ~ NA_real_)
) %>%
  # HB
  mutate_at(vars(v456,hb56), function(x) case_when(x >= 250 | x < 3 ~ NA_real_,
                                                   TRUE ~ as.numeric(x))) %>% 
  # BMI
  mutate_at(vars(v445,hb40),function(x) case_when(x > 6000 ~ NA_real_,
                                                  TRUE ~ as.numeric(x))) %>% 
  # Glucose
  mutate_at(vars(smb74,sb74), function(x) case_when(is.na(x) | x > 498 ~ NA_real_,
                                                    TRUE ~ as.numeric(x))) %>% 
  # Caste
  mutate_at(vars(s116,sm118),function(x) case_when(x == 1 ~ "Schedule Caste",
                                                   x == 2 ~ "Schedule Tribe",
                                                   x == 3 ~ "OBC",
                                                   x == 4 ~ "General",
                                                   x == 8 ~ "General",
                                                   TRUE ~ NA_character_)) %>% 
  # Education
  mutate_at(vars(v106,mv106),function(x) case_when(x == 0 ~ "No education",
                                                   x == 1 ~ "Primary",
                                                   x == 2 ~ "Secondary",
                                                   x == 3 ~ "Higher",
                                                   x == 9 ~ NA_character_,
                                                   TRUE ~ NA_character_)) %>% 
  # Religion
  mutate_at(vars(v130,mv130),function(x) case_when(x == 1 ~ "Hindu",
                                                   x == 2 ~ "Muslim",
                                                   TRUE ~ "Other")) %>% 
  # insurance, alcohol
  mutate_at(vars(
                 v481,mv481,s720,sm619), function(x) case_when(x == 0 ~ 0,
                                                               x == 1 ~ 1,
                                                               TRUE ~ NA_real_)) %>% 
  # Smoking
  mutate_at(vars(v463z,mv463z), function(x) case_when(x == 1 ~ 0,
                                                      x == 0 ~ 1,
                                                      TRUE ~ NA_real_)) %>% 
  
  mutate_at(vars(v501,mv501,
                 s301,sm213), function(x) case_when(x %in% c(2) ~ 0,
                                                    x %in% c(1) ~ 1,
                                                    TRUE ~ NA_real_)) %>% 
  mutate(v107 = case_when(v106 == "No education" ~ 0,
                          TRUE ~ as.numeric(v107)),
         mv107 = case_when(mv106 == "No education" ~ 0,
                           TRUE ~ as.numeric(mv107)),
         v133 = case_when(v106 == "No education" ~ 0,
                          TRUE ~ as.numeric(v133)),
         mv133 = case_when(mv106 == "No education" ~ 0,
                           TRUE ~ as.numeric(mv133)),
         ) %>% 
  rename(
    f_wealth = v190,
    f_maritalstatus = v501,
    f_caste = s116,
    m_caste = sm118,
    m_religion = mv130,
    f_religion = v130,
    
    m_educationlevel = mv106,
    f_educationlevel = v106,
    m_age = mv012,
    f_age = v012,
    m_edulvl = mv107,
    f_edulvl = v107,
    m_eduyr = mv133,
    f_eduyr = v133,
    
    
    
    m_insurance = mv481,
    f_insurance = v481,
    
    m_children = mv218,
    f_children = v218,
    
    m_alcohol = sm619,
    f_alcohol = s720,
    m_smoke = mv463z,
    f_smoke = v463z,
    
    m_bmi = hb40,
    f_bmi = v445,
    m_hb = hb56,
    f_hb = v456,
    m_glucose = smb74,
    f_glucose = sb74
    
  )

saveRDS(couples,paste0(path_couples_folder,"/working/nfhs5 couples.RDS"))
