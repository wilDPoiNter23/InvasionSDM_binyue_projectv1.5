remotes::install_github("rvalavi/blockCV", dependencies = TRUE)
library(remotes)
install_github("rvalavi/blockCV")
install.packages("blockCV", dependencies = TRUE)
install.packages("remotes")

library(biomod2)
individual_models <- readRDS("E:/RcodeforTest/database/Zinnia peruviana_ensemble.rds")
str(individual_models)
required_vars <- individual_models@expl.var.names
required_vars
