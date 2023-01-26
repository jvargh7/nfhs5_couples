rm(list=ls()); gc(); source(".Rprofile")
source("overall/n5cm03_summary of analytic sample.R")

# Eq 2
rm(list=ls()); gc(); source(".Rprofile")
source("pooled/n5cpooled01_dm and htn poisson regression with multiple imputation.R")

# Eq 1a-b, 3a-b
rm(list=ls()); gc(); source(".Rprofile")
source("overall/n5cm01_htn poisson regression with multiple imputation.R")

rm(list=ls()); gc(); source(".Rprofile")
source("overall/n5cm02_dm poisson regression with multiple imputation.R")


rm(list=ls()); gc(); source(".Rprofile")
source("sensitivity/n5csensitivity01_dm and htn poisson regression with multiple imputation.R")

# Paper ------

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_descriptive characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_main analysis results.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_sensitivity results.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_stratum specific prevalence ratios.R")
