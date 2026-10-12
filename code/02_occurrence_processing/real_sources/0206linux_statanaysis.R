# Original source: 0206linux_statanaysis.R
# Original source location: <LOCAL_CODE_ROOT>/0206linux_statanaysis.R
# Original source SHA-256: bfcfee0639f6d3b7cecbe6d1d1aece258b7f529648925ea74341908dfe88c64e
# Public-safe original snapshot: archive/original_sources/local/0206linux_statanaysis.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")

library(dplyr)


setwd(publication_path("HPC_INPUT_ROOT", "script"))
family_geodatapoint_cleaned <- read.csv(publication_path("HPC_INPUT_ROOT", "script/output/family_geodatapoint_cleaned.csv"))


species_counts <- family_geodatapoint_cleaned %>%
  group_by(species) %>%
  summarise(num_occurrences = n()) %>%
  arrange(desc(num_occurrences))

family_counts <- family_geodatapoint_cleaned %>%
  group_by(family) %>%
  summarise(num_occurrences = n()) %>%
  arrange(desc(num_occurrences))

write.csv(species_counts,'/output/species_occnums.csv')
write.csv(family_counts,'/output/family_occnums.csv')