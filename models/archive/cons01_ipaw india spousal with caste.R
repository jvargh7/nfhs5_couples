require(srvyr)
require(survey)

source("preprocessing/cp04_concordance datasets.R")
source("preprocessing/cp05_ipaw for concordance datasets.R")

# WIFE-DIABETES ----------

wife_dm <- svyglm(f_dm ~ m_dm + c_age + f_eduyr + f_alcohol + f_smoke + f_bmi + f_children +
                    f_caste + f_wealth + f_rural + f_religion + 
                    # m_age + 
                    m_eduyr + m_alcohol + m_smoke + m_bmi + factor(state),
                  design=ipaw_dm_svydesign,family = poisson())

# WIFE-HYPERTENSION ----------

wife_htn <- svyglm(f_htn ~ m_htn + c_age + f_eduyr + f_alcohol + f_smoke + f_bmi + f_children +
                     f_caste + f_wealth + f_rural + f_religion + 
                     # m_age + 
                     m_eduyr + m_alcohol + m_smoke + m_bmi + factor(state),
                   design=ipaw_htn_svydesign,family = poisson())
# HUSBAND-DIABETES ----------

husband_dm <- svyglm(m_dm ~ f_dm + c_age + m_eduyr + m_alcohol + m_smoke + m_bmi + m_children +
                       f_caste + f_wealth + f_rural + f_religion + 
                       # f_age + 
                       f_eduyr + f_alcohol + f_smoke + f_bmi + factor(state),
                     design=ipaw_dm_svydesign,family = poisson())
# WIFE-HYPERTENSION ----------

husband_htn <- svyglm(m_htn ~ f_htn + c_age + m_eduyr + m_alcohol + m_smoke + m_bmi + m_children +
                        f_caste + f_wealth + f_rural + f_religion + 
                        # f_age + 
                        f_eduyr + f_alcohol + f_smoke + f_bmi + factor(state),
                      design=ipaw_htn_svydesign,family = poisson())


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
write_csv(india_associations,paste0(path_couples_folder,"/working/concordance/ipaw india association of partner status long.csv"))




