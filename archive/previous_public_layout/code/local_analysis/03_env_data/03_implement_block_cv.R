# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 52eaac53290e7e3ab9253e24dd09cd149c4f96a7758e45b921314806138641bb
remotes::install_github("rvalavi/blockCV", dependencies = TRUE)
library(remotes)
install_github("rvalavi/blockCV")
install.packages("blockCV", dependencies = TRUE)
install.packages("remotes")

library(biomod2)
individual_models <- readRDS("${LOCAL_DATABASE_ROOT}/Zinnia peruviana_ensemble.rds")
str(individual_models)
required_vars <- individual_models@expl.var.names
required_vars
