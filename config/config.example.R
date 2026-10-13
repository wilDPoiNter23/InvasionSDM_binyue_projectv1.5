# Copy to config/config.local.R and set the data locations.
# Keep credentials outside this file and outside version control.
PROJECT_ROOT <- normalizePath(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), mustWork=TRUE)
DATA_ROOT <- Sys.getenv("INVASIONSDM_DATA_ROOT", "SET_PATH_TO_DATA")
OUTPUT_ROOT <- Sys.getenv("INVASIONSDM_OUTPUT_ROOT", file.path(DATA_ROOT, "outputs"))
PATHS <- list(
  PROJECT_ROOT=PROJECT_ROOT,
  DATA_ROOT=DATA_ROOT,
  OUTPUT_ROOT=OUTPUT_ROOT,
  WORK_ROOT=file.path(DATA_ROOT,"sdm"),
  SDM_ROOT=file.path(DATA_ROOT,"sdm"),
  HPC_INPUT_ROOT=file.path(DATA_ROOT,"predictor_inputs"),
  LOCAL_ANALYSIS_ROOT=file.path(DATA_ROOT,"city_analysis"),
  LOCAL_DATABASE_ROOT=file.path(DATA_ROOT,"model_summaries"),
  GLONAF_ROOT=file.path(DATA_ROOT,"GloNAF"),
  GBIF_DOWNLOADS=file.path(DATA_ROOT,"occurrences","downloads"),
  OCCURRENCE_OUTPUT=file.path(OUTPUT_ROOT,"occurrences"),
  OCCURRENCE_ROOT=file.path(DATA_ROOT,"occurrences"),
  LOCAL_AUX_ROOT=file.path(DATA_ROOT,"auxiliary"),
  SCRIPT_DATA_ROOT=file.path(DATA_ROOT,"supporting_tables"),
  SCRIPT_ROOT=file.path(PROJECT_ROOT,"code"),
  SPECIES_LIST=file.path(DATA_ROOT,"species","main_species_list.csv"),
  PREDICTOR_STACK=file.path(DATA_ROOT,"predictors","full_stack.tif"),
  MAIN_RESPONSE_RASTER=file.path(DATA_ROOT,"suitability","manuscript_suitability.tif"),
  CITY_BOUNDARIES=file.path(DATA_ROOT,"geography","city_boundaries.shp"),
  CITY_BOUNDARIES_ORIGINAL=file.path(DATA_ROOT,"geography","city_boundaries_original.shp"),
  COASTLINE_FILE=file.path(DATA_ROOT,"geography","coastline.shp"),
  WATER_LEVEL_RASTER=file.path(DATA_ROOT,"geography","water_level.tif"),
  PCA_REFERENCE_INPUT=file.path(DATA_ROOT,"city_tables","city_pca_reference.csv"),
  PCA_LOADING_INPUT=file.path(DATA_ROOT,"city_tables","city_pca_reference.csv"),
  GAM_INPUT=file.path(DATA_ROOT,"city_tables","city_gam_input.csv"),
  GAM_WORKDIR=file.path(DATA_ROOT,"city_tables"),
  GAM_OUTPUT=file.path(OUTPUT_ROOT,"gam"),
  SEM_INPUT=file.path(DATA_ROOT,"city_tables","city_sem_composites.csv"),
  SEM_BASE_INPUT=file.path(DATA_ROOT,"city_tables","city_sem_base.csv"),
  SEM_OUTPUT=file.path(OUTPUT_ROOT,"sem"),
  PREDICTOR_CORRELATIONS=file.path(DATA_ROOT,"model_summaries","predictor_correlations.csv"),
  MODEL_EVALUATIONS=file.path(DATA_ROOT,"model_summaries","all_model_evaluations.csv"),
  SELECTED_MODEL_EVALUATIONS=file.path(DATA_ROOT,"model_summaries","selected_model_evaluations.csv"),
  PREDICTOR_IMPORTANCE=file.path(DATA_ROOT,"model_summaries","predictor_importance.csv"),
  CITY_EFFECT_TABLE=file.path(DATA_ROOT,"city_tables","city_effects.csv"),
  ENV_ONLY_ROOT=file.path(DATA_ROOT,"environment_only"),
  CLIMATE_STACK=file.path(DATA_ROOT,"predictors","climate_stack.tif"),
  ENV_PRIMARY_STACK=file.path(DATA_ROOT,"predictors","environment_stack.tif"),
  ENV_VIIRS_STACK=file.path(DATA_ROOT,"predictors","environment_stack_alternative.tif")
)
# The directory-valued entries retain input subtrees documented in docs/DATA.md.
# File-valued entries can point directly to the downloaded data-package files.
# Species fitting uses the settings in the chosen batch script.
