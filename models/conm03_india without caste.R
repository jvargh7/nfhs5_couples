library(srvyr)

source("preprocessing/cp04_concordance datasets.R")

library(geepack)
# WIFE-DIABETES ----------

wife_dm <- geeglm(f_dm ~ m_dm + c_age + f_eduyr + f_alcohol + f_smoke + f_bmi + f_children +
                    f_wealth + f_rural + f_religion + 
                    # m_age + 
                    m_eduyr + m_alcohol + m_smoke + m_bmi + factor(state),
                  data=dm_df,family = poisson(),id = cluster)

# WIFE-HYPERTENSION ----------

wife_htn <- geeglm(f_htn ~ m_htn + c_age + f_eduyr + f_alcohol + f_smoke + f_bmi + f_children +
                     f_wealth + f_rural + f_religion + 
                     # m_age + 
                     m_eduyr + m_alcohol + m_smoke + m_bmi + factor(state),
                   data=htn_df,family = poisson(),id = cluster)
# HUSBAND-DIABETES ----------

husband_dm <- geeglm(m_dm ~ f_dm + c_age + m_eduyr + m_alcohol + m_smoke + m_bmi + m_children +
                       f_wealth + f_rural + f_religion + 
                       # f_age + 
                       f_eduyr + f_alcohol + f_smoke + f_bmi + factor(state),
                     data=dm_df,family = poisson(),id = cluster)
# WIFE-HYPERTENSION ----------

husband_htn <- geeglm(m_htn ~ f_htn + c_age + m_eduyr + m_alcohol + m_smoke + m_bmi + m_children +
                        f_wealth + f_rural + f_religion + 
                        # f_age + 
                        f_eduyr + f_alcohol + f_smoke + f_bmi + factor(state),
                      data=htn_df,family = poisson(),id = cluster)


india_associations <- bind_rows(
  broom::tidy(wife_dm,exponentiate = TRUE) %>% 
    mutate(partner = "Wife",
           disease = "Diabetes"),
  broom::tidy(wife_htn,exponentiate = TRUE) %>% 
    mutate(partner = "Wife",
           disease = "Hypertension"),
  broom::tidy(husband_dm,exponentiate = TRUE) %>% 
    mutate(partner = "Husband",
           disease = "Diabetes"),
  broom::tidy(husband_htn,exponentiate = TRUE) %>% 
    mutate(partner = "Husband",
           disease = "Hypertension")
)
write_csv(india_associations,paste0(path_couples_folder,"/working/concordance/india association without caste of partner status long.csv"))




