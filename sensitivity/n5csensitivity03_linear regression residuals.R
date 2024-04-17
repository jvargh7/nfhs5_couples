library(mice)
df <- complete(readRDS(paste0(path_couples_folder,"/working/nfhs5c couples_mi_dfs.RDS")),1)


male_res = residuals(lm(h_bmi ~ h_age, data=df))
female_res = residuals(lm(w_bmi ~ w_age, data=df))


options(scipen=999)
lm(male_res ~ female_res) %>% 
  broom::tidy() %>% 
  View()

lm(h_bmi ~ w_bmi + h_age + w_age, data=df) %>% 
  broom::tidy() %>% 
  View()

lm(h_bmi ~ female_res + h_age + w_age, data=df %>% mutate(female_res = female_res)) %>% 
  broom::tidy() %>% 
  View()
