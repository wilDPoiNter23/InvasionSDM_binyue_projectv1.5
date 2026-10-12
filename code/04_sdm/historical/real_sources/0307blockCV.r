# Original source: 0307blockCV.r
# Original source location: <LOCAL_CODE_ROOT>/0307blockCV.r
# Original source SHA-256: 52eaac53290e7e3ab9253e24dd09cd149c4f96a7758e45b921314806138641bb
# Public-safe original snapshot: archive/original_sources/local/0307blockCV.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
remotes::install_github("rvalavi/blockCV", dependencies = TRUE)
library(remotes)
install_github("rvalavi/blockCV")
install.packages("blockCV", dependencies = TRUE)
install.packages("remotes")

library(biomod2)
individual_models <- readRDS(publication_path("LOCAL_DATABASE_ROOT", "Zinnia peruviana_ensemble.rds"))
str(individual_models)
required_vars <- individual_models@expl.var.names
required_vars
