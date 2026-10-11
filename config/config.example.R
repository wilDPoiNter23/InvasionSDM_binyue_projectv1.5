# Copy to config/config.local.R (ignored). PATHS ONLY; historical settings stay in scripts.
PROJECT_ROOT <- normalizePath(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), mustWork = TRUE)
DATA_ROOT <- Sys.getenv("INVASIONSDM_DATA_ROOT", "SET_EXTERNAL_DATA_ROOT")
MODEL_ROOT <- file.path(DATA_ROOT, "work", "models")
RESULT_ROOT <- file.path(DATA_ROOT, "work", "results")
TEMP_ROOT <- file.path(DATA_ROOT, "temp")
PATHS <- list(PROJECT_ROOT = PROJECT_ROOT, DATA_ROOT = DATA_ROOT,
              MODEL_ROOT = MODEL_ROOT, RESULT_ROOT = RESULT_ROOT, TEMP_ROOT = TEMP_ROOT,
              WORK_ROOT = file.path(DATA_ROOT, "work"),
              HPC_INPUT_ROOT = file.path(DATA_ROOT, "shared_input"),
              LOCAL_ANALYSIS_ROOT = file.path(DATA_ROOT, "author_analysis"),
              LOCAL_DATABASE_ROOT = file.path(DATA_ROOT, "local_database"),
              GLONAF_ROOT = file.path(DATA_ROOT, "GloNAF"),
              OCCURRENCE_ROOT = file.path(DATA_ROOT, "occurrence_preparation"),
              LOCAL_AUX_ROOT = file.path(DATA_ROOT, "auxiliary"),
              NON_NATIVE_ROOT = file.path(DATA_ROOT, "later_non_native"))
# Add INPUT_<key> entries listed in metadata/path_aliases.csv for unmapped source paths.
# Optional: WORKSPACE_RDS <- "path/to/author_verified_workspace.rds"
# It must be a NAMED LIST; it cannot substitute for a documented preprocessing step.
# WORK_ROOT retains historical models/results subtrees; MODEL_ROOT/RESULT_ROOT must
# correspond to those locations when running the preserved batch implementation.
