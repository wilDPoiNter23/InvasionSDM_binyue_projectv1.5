# Original source: setup_gap75_symlinks.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/setup_gap75_symlinks.r
# Original source SHA-256: 3fb0ff05463cbaa4d36a3acf2bb446ae3d1f2465e39e7bf29ae8a9099bedfd7f
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# setup_gap75_symlinks.r — Create symlink structure for BIOMOD_Projection
# For each species, BIOMOD expects: {sp.name}/.BIOMOD_DATA and {sp.name}/models/
# Actual locations are ../.BIOMOD_DATA and ../models/
# This script creates: {sp.name}/.BIOMOD_DATA -> ../.BIOMOD_DATA
#                       {sp.name}/models -> ../models

suppressMessages(library(biomod2))

BASE <- "<WORK_ROOT>"
AUDIT <- file.path(BASE, "publication_release_v1/audits")

# Read gap75 results
results <- read.csv(file.path(AUDIT, "gap75_full_classification_and_selection.csv"), stringsAsFactors=FALSE)

# All 75 gap species need symlinks (even repair species may need them later)
gap_sp <- results$species

setup_count <- 0
skip_count <- 0
fail_count <- 0

for (sp in gap_sp) {
  spd <- gsub(" ", ".", sp)
  sp_dir <- file.path(BASE, "models", spd)
  
  if (!dir.exists(sp_dir)) {
    cat(sprintf("SKIP: %s - model dir not found: %s\n", sp, sp_dir))
    skip_count <- skip_count + 1
    next
  }
  
  # Load model to get sp.name
  mf <- list.files(sp_dir, pattern="\\.models\\.out$")
  mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
  if (length(mf) == 0) {
    cat(sprintf("SKIP: %s - no .models.out found\n", sp))
    skip_count <- skip_count + 1
    next
  }
  
  m <- tryCatch(get(load(file.path(sp_dir, mf[1]))), error=function(e) NULL)
  if (is.null(m)) {
    cat(sprintf("FAIL: %s - cannot load model\n", sp))
    fail_count <- fail_count + 1
    next
  }
  
  sp_name <- m@sp.name  # e.g., "Arceuthobium.abietinum"
  
  # Create symlink structure
  link_dir <- file.path(sp_dir, sp_name)
  
  # Remove existing if it's a file or broken link
  existing_link <- tryCatch(Sys.readlink(link_dir), error=function(e) NA)
  if (!is.na(existing_link) && existing_link != "") {
    unlink(link_dir, recursive=TRUE, force=TRUE)
  } else if (file.exists(link_dir) && is.na(existing_link)) {
    # It's a regular directory, not a symlink — remove it
    unlink(link_dir, recursive=TRUE, force=TRUE)
  }
  
  dir.create(link_dir, showWarnings=FALSE)
  
  # Create symlinks
  biomod_link <- file.path(link_dir, ".BIOMOD_DATA")
  models_link <- file.path(link_dir, "models")
  
  # Remove existing links
  unlink(biomod_link, force=TRUE)
  unlink(models_link, force=TRUE)
  
  # Create new links (relative paths)
  file.symlink("../.BIOMOD_DATA", biomod_link)
  file.symlink("../models", models_link)
  
  # Verify using modeling.id from the loaded model
  modeling_id <- m@modeling.id  # e.g., "Arceuthobium abietinumMultiModel_Ensemble"
  biomod_ok <- dir.exists(file.path(biomod_link, modeling_id))
  models_ok <- dir.exists(file.path(models_link, modeling_id))
  
  if (biomod_ok && models_ok) {
    setup_count <- setup_count + 1
    if (setup_count %% 10 == 0) cat(sprintf("  %d/%d setup OK\n", setup_count, length(gap_sp)))
  } else {
    cat(sprintf("WARN: %s - BIOMOD_OK=%s models_OK=%s\n", sp, biomod_ok, models_ok))
    fail_count <- fail_count + 1
  }
}

cat(sprintf("\nSetup complete: %d OK, %d skipped, %d failed (total %d)\n",
  setup_count, skip_count, fail_count, length(gap_sp)))
