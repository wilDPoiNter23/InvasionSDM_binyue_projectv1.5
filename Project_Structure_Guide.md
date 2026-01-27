# InvasionSDM Project Structure & Workflow Guide

This guide describes a clear, standardized, and logical project structure based on Scientific Reproducibility Standards. It separates code, data (raw/intermediate/final), and results to ensure the project is easy to manage, review, and share.

## 1. 📂 Recommended Project Directory Structure

Use the following folder structure in the project root:

```text
InvasionSDM/
├── code/
│   ├── hpc_jobs/                # [HPC] High Performance Computing Scripts (Linux/SLURM)
│   │   ├── 00_setup/            # HPC Environment Setup (Library paths, modules)
│   │   ├── 01_data_prep/        # Large-scale Data Prep
│   │   ├── 03_env_data/         # Environmental Data Processing (HPC)
│   │   ├── 04_sdm/              # Core SDM Training (Batch processing)
│   │   └── 05_results/          # Result Aggregation (HPC)
│   │
│   └── local_analysis/          # [Local] Workstation Analysis Scripts (Windows/RStudio)
│       ├── 00_setup/            # Local Environment Setup
│       ├── 01_data_prep/        # Light Data Cleaning
│       ├── 02_species/          # Species Data visualization/checks
│       ├── 03_env_data/         # Environmental Data visualization
│       ├── 04_sdm_viz/          # SDM Visualization & Response Curves
│       ├── 05_results/          # Evaluation Plots & Continental Risk
│       └── 06_stats/            # GAM/SEM Statistical Analysis
│
├── data/                        # All data storage, strictly separating raw and processed data
│   ├── raw/                     # [READ-ONLY] Raw data, NEVER modify manually
│   │   ├── gbif_downloads/      # GBIF raw downloads
│   │   ├── worldclim/           # Raw climate layers (.tif)
│   │   ├── soil_marine/         # Soil and marine data
│   │   └── shapefiles/          # Base logical maps (World, Coastal lines)
│   │
│   ├── processed/               # [INTERMEDIATE] Cleaned and pre-processed data
│   │   ├── species_occ/         # Cleaned species occurrence points (.csv)
│   │   ├── environmental/       # Cropped/stacked environmental layers (.tif, .rds)
│   │   ├── city_masks/          # City reclamation and buffer zone mask files
│   │   └── taxonomy/            # Taxonomic correction tables
│   │
│   └── final/                   # [FINAL] Aggregated data tables for final analysis
│       ├── model_input/         # Final formatted data entering SDM models
│       └── analysis_tables/     # Organised tables for GAM/SEM analysis (e.g., 108_cities_data.csv)
│
├── results/                     # Model outputs and analysis results
│   ├── models/                  # Saved model objects (.rds, .out) - Large size
│   │   ├── individual/          # Individual algorithm models (GLM, RF, MaxEnt)
│   │   └── ensemble/            # Ensemble models
│   │
│   ├── projections/             # Spatial projection results
│   │   ├── binary/              # Binary projection maps (0/1)
│   │   └── probability/         # Probability projection maps (0-1000)
│   │
│   ├── evaluations/             # Model evaluation metrics
│   │   ├── validation_scores/   # TSS/ROC score tables (.csv)
│   │   └── var_importance/      # Variable importance tables
│   │
│   ├── tables/                  # Manuscript tables (Supplementary Tables)
│   │   ├── pca_loadings/        # PCA loading tables
│   │   └── taxonomy_summary/    # Species taxonomy summary tables
│   │
│   └── figures/                 # Manuscript figures (Figures)
│       ├── curves/              # Response Curves
│       ├── maps/                # Global/Continental risk maps
│       ├── correlations/        # Correlation heatmaps
│       └── mechanism/           # GAM and SEM analysis plots (Mechanism analysis)
│
├── logs/                        # Execution logs (SLURM logs, Rscript logs)
├── temp/                        # Temporary files (Safe to delete)
├── config.R                     # Global path configuration file (Corresponds to 00_config.R)
└── README.md                    # Project documentation
```

---

## 2. 🔄 Code to Data Mapping

The table below shows how your codebase maps to the standard structure above, describing the input/output flow for each module.

### 1. HPC Cluster Workflow (Linux)
*   **Key Task**: Heavy computational tasks (SDM training, global projections).
*   **Key Scripts**:
    *   `code/hpc_jobs/04_sdm/04_batch_sdm_modeling.R`
    *   `code/hpc_jobs/03_env_data/03_aggregate_2.5arcmin_grid.R`
*   **Input**: `data/processed/environmental/` and `data/processed/species_occ/` (uploaded to HPC).
*   **Output**: `results/models/` (.rds) and `results/projections/` (.tif).

### 2. Local Workstation Workflow (Windows)
*   **Key Task**: Data exploration, statistics (GAM/SEM), plotting.
*   **Key Scripts**:
    *   `code/local_analysis/06_stats/06_gam_reclamation_risk_analysis.R`
    *   `code/local_analysis/06_stats/06_visualize_city_details.R`
*   **Data Sync**: Download `results/evaluations/` and `data/processed/final/` from HPC.

### 00_setup
*   **Scripts**: `local_analysis/00_setup/` (Local), `hpc_jobs/00_setup/` (HPC).
*   **Purpose**: Define global path variables (e.g., `config$data_raw`) to ensure subsequent scripts do not hardcode absolute paths. Ensure `00_config.R` detects OS or has separate versions.

*   **Input**: `data/raw/` (GBIF CSVs, Shapefiles)
*   **Scripts**:
    *   `01_clean_variable_names.R`
    *   `01_process_reclamation_data.R`
    *   `02_generate_sql_queries.R` -> `02_download_gbif_occurrences.R`
*   **Output**: `data/processed/species_occ/` (Cleaned species CSV)

### 03_environmental_data (Environmental Data)
*   **Environment**: **HPC (Recommended)** or Local (if memory sufficient)
*   **Input**: `data/raw/worldclim/`, `data/raw/soil_marine/`
*   **Scripts**:
    *   `03_aggregate_2.5arcmin_grid.R`
    *   `03_select_variables_vif.R` (Variable selection)
*   **Output**: Generate `full_stack.tif`, which must exist on HPC for modeling.

### 04_sdm_modeling (Core Modeling)
*   **Environment**: **HPC Only** (Computationally intensive)
*   **Input**:
    *   `data/processed/species_occ/`
    *   `data/processed/environmental/full_stack.tif`
*   **Scripts**:
    *   `04_batch_sdm_modeling.R` (Core batch processing script)
*   **Output**: Generated model files are usually very large, keep on HPC, only export evaluation CSVs to local.

### 05_results_analysis (Result Evaluation)
*   **Environment**: **HPC (Initial aggregation)** -> **Local (Plotting)**
*   **Scripts**:
    *   `05_evaluate_model_performance.R` (Run on HPC to aggregate TSS/ROC for all models)
    *   `05_analyze_continental_risk.R` (Plot locally)
*   **Data Flow**: HPC generates `all_models_scores.csv` -> Download to local -> R plotting.

### 06_statistical_analysis (Attribution & Mechanism)
*   **Environment**: **Local Only**
*   **Input**: `108_cities_data.csv` downloaded from HPC or extracted risk value tables.
*   **Scripts**:
    *   `local_analysis/06_stats/06_gam_reclamation_risk_analysis.R` (GAM nonlinear analysis)
    *   `local_analysis/06_stats/06_pdp_sem_analysis.R` (SEM path analysis)
    *   `local_analysis/06_stats/06_visualize_city_details.R` (City details)
*   **Output**: Final Publication Figures (PDF/PNG).

---

## 3. 📝 Implementation Checklist

To fully migrate from the current state to this standard structure, the following steps are recommended:

1.  **Path Standardization**:
    *   Ensure all scripts start by sourcing `source("00_setup/00_config.R")`.
    *   Replace all hardcoded paths (e.g., `E:/k_PhD_NKU/...`) in the code with variables like `config$data_raw`, `config$results_figures`.

2.  **Data Freezing**:
    *   Move the initially extracted GIS data and CSV organised data to `data/processed/final/` as **Read-Only Input** for Statistical Analysis (06).

3.  **Intermediate File Cleanup**:
    *   SDM modeling generates huge temporary files. It is recommended to periodically clean the `temp/` folder after `04_batch_sdm_modeling.R` finishes.

4.  **Figure Output Specification**:
    *   Output paths for all plotting scripts (`ggsave`) should point to specific subfolders under `results/figures/` using meaningful filenames (e.g., `Fig1_Global_Risk_Map.pdf` instead of `plot1.pdf`).

This structure complies with requirements from top journals like Nature/Science for "Data Availability" and "Code Reproducibility", and also facilitates subsequent uploading of the codebase to GitHub/Zenodo.
