rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/n5couples_svymi.R")
rm(list=ls()); gc(); source(".Rprofile")

source("models/n5cm03_summary of analytic sample.R")

rm(list=ls()); gc(); source(".Rprofile")
source("models/n5cm01_htn poisson regression with multiple imputation.R")


rm(list=ls()); gc(); source(".Rprofile")
source("models/n5cm02_dm poisson regression with multiple imputation.R")

rm(list=ls()); gc(); source(".Rprofile")
source("sensitivity/n5cs01_htn poisson regression with multiple imputation.R")


rm(list=ls()); gc(); source(".Rprofile")
source("sensitivity/n5cs02_dm poisson regression with multiple imputation.R")

# Paper ------

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_descriptive characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_main analysis results.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_spouse covariates results.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_stratum specific prevalence ratios.R")
