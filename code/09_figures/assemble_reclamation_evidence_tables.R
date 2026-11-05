# Assemble complete supplementary tables from saved comparison results.
suppressPackageStartupMessages(library(jsonlite))
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- file.path(cfg$output_root, 'tables')
stopifnot(dir.exists(out))
models <- read.csv(cfg$absolute_model_summary)
contrasts <- read.csv(cfg$absolute_comparison)
primary <- contrasts[contrasts$mode == 'COMMON_ABSOLUTE_IQR', ]
table <- merge(models, primary, by = c('response', 'model'), sort = FALSE)
stopifnot(nrow(table) == 15L, all(table$n == 83L))
write.csv(table, file.path(out, 'reclamation_absolute_model_sensitivity.csv'), row.names = FALSE)
full <- read.csv(cfg$full_land_use_contrasts)
paired <- read.csv(cfg$paired_land_use_contrasts)
full <- full[full$type == 'USE_VS_REFERENCE' & full$case == 'primary', ]
paired <- paired[paired$type == 'USE_VS_REFERENCE' & paired$case == 'primary', ]
fields <- c('type', 'from', 'to', 'age_index_years', 'difference', 'lower', 'upper',
  'probability_positive', 'group', 'case', 'chain_status')
use <- rbind(full[, fields], paired[, fields])
stopifnot(nrow(use) == 27L, all(use$chain_status == 'PASS_CHAIN_DIAGNOSTICS'))
write.csv(use, file.path(out, 'reclamation_land_use_complete_contrasts.csv'), row.names = FALSE)
cohorts <- read.csv(cfg$cohort_summary)
stopifnot(nrow(cohorts) == 6L)
write.csv(cohorts, file.path(out, 'reclamation_formation_cohort_comparison.csv'), row.names = FALSE)
write_json(list(status = 'ASSEMBLED_FROM_SAVED_RESULTS', model_rows = nrow(table),
  land_use_rows = nrow(use), cohort_rows = nrow(cohorts), no_model_refits = TRUE),
  file.path(out, 'reclamation_evidence_table_receipt.json'), auto_unbox = TRUE, pretty = TRUE)
cat('Saved full model, land-use and formation-cohort tables; no fitting.\n')
