# Coastal Reclamation and Invasive Species Distribution

**Species Distribution Modeling (SDM) analysis of 800+ invasive plant species in coastal cities with reclamation activities**

## 📋 Project Overview

This repository contains the complete workflow for analyzing the impact of coastal reclamation on invasive species distribution using Species Distribution Modeling (SDM). The project processes data for 800+ invasive plant species across global coastal cities and builds ensemble SDM models to assess invasion risk.

## 🎯 Research Questions

1. How does coastal reclamation affect invasive species distribution?
2. What environmental factors drive invasion risk in coastal cities?
3. Which species pose the highest invasion threats?

## 📊 Project Statistics

- **Species Modeled:** 800+ invasive plant species
- **Coastal Cities:** 108 cities globally
- **Ensemble Methods:** EMmean, EMmedian

## 📁 Repository Structure

```
InvasionSDM/
├── code/
│   ├── local_analysis/          # Analysis workflow for local workstations
│   │   ├── 00_setup/            # Package installation and setup
│   │   ├── 01_data_prep/        # Data cleaning and integration
│   │   ├── 02_species/          # Species occurrence data acquisition
│   │   ├── 03_env_data/         # Environmental variable processing
│   │   ├── 04_sdm_viz/          # Visualization of SDM results
│   │   ├── 05_results/          # Model performance evaluation
│   │   └── 06_stats/            # Statistical analysis (GAM, SEM)
│   │
│   └── hpc_jobs/                # Batch scripts for HPC cluster computing
│       └── [files for batch modeling]
│
├── docs/                        # Project documentation
├── data/                        # Input data (not included in repo)
├── results/                     # Analysis outputs
└── README.md                    # Project overview
```

## 🚀 Quick Start

### Prerequisites

- R >= 4.5.2
- Linux HPC with SLURM (for Phase 4 batch modeling)

### Installation

```r
# Install all required packages
source("code/local_analysis/00_setup/00_install_packages.R")
```

### Workflow Execution

The workflow consists of sequential phases.

#### Phase 1: Data Preparation
```r
# Data integration and cleaning
source("code/local_analysis/01_data_prep/01_prepare_glonaf_invasion_data.R")
source("code/local_analysis/01_data_prep/01_define_coastal_cities.R")
source("code/local_analysis/01_data_prep/01_process_reclamation_data.R")
source("code/local_analysis/01_data_prep/01_clean_variable_names.R")
source("code/local_analysis/01_data_prep/01_correlation_analysis.R")
```

#### Phase 2: Species Data Acquisition
```r
# GBIF occurrence data
source("code/local_analysis/02_species/02_download_gbif_occurrences.R")
source("code/local_analysis/02_species/02_generate_sql_queries.R")
source("code/local_analysis/02_species/02_create_richness_heatmap.R")
```

#### Phase 3: Environmental Data Processing
```r
source("code/local_analysis/03_env_data/03_download_environmental_layers.R")
source("code/local_analysis/03_env_data/03_resample_environmental_layers.R")
source("code/local_analysis/03_env_data/03_combine_environmental_stack.R")
source("code/local_analysis/03_env_data/03_select_variables_vif.R")
source("code/local_analysis/03_env_data/03_retain_anthropogenic_variables.R")
```

#### Phase 4: SDM Modeling (High-Performance Computing)
_These scripts are designed for batch processing on an HPC cluster._
```r
# Main batch modeling script
# code/hpc_jobs/04_batch_sdm_modeling.R
```

#### Phase 5: Results Analysis
```r
source("code/local_analysis/05_results/05_evaluate_model_performance.R")
source("code/local_analysis/05_results/05_analyze_continental_risk.R")
```

#### Phase 6: Statistical Analysis
```r
source("code/local_analysis/06_stats/06_gam_reclamation_risk_analysis.R")
source("code/local_analysis/06_stats/06_pdp_sem_analysis.R")
source("code/local_analysis/06_stats/06_visualize_city_details.R")
```

## � Methodology

### Species Distribution Modeling
- **Algorithms:** GLM, Random Forest (default settings), MaxEnt
- **Cross-Validation:** Block CV (k=3, 80% training, 1 repetition)
- **Ensemble:** EMmean and EMmedian with TSS-based weighting
- **Evaluation Metrics:** TSS, ROC, KAPPA

### Model Selection Criteria
- **Primary Threshold:** TSS > 0.8 AND ROC > 0.8
- **Fallback Threshold:** TSS > 0.7 AND ROC > 0.7
- **Minimum Valid Points:** 100 presence records
- **NA Threshold:** 10% maximum missing data

### Statistical Analysis
- **GAM:** Generalized Additive Models with factor-smooth interactions
- **SEM:** Structural Equation Modeling for causal pathways
- **PCA:** Principal Component Analysis for variable reduction

## 📊 Key Outputs

### Main Figures
1. Global invasion heatmap
2. Algorithm performance comparison
3. Continental invasion risk distribution
4. Reclamation-invasion relationship (GAM)
5. Variable importance rankings
6. Response curves for environmental predictors

### Supplementary Materials
1. Model evaluation metrics (TSS, ROC, KAPPA)
2. PCA loadings for environmental variables
3. SEM path diagrams
4. City-level detailed analysis

## 💻 System Requirements

### Computational Resources
- **Phase 1-3:** Standard desktop/laptop (8GB RAM minimum)
- **Phase 4 (SDM):** HPC cluster with SLURM (recommended: 64GB RAM per node)
- **Phase 5-6:** Standard desktop/laptop (16GB RAM recommended)

### Storage
- Minimum 500GB for all species projections and intermediate files
- Additional space for raw data and results

### Estimated Runtime
- **Phase 1-3:** ~2-4 hours
- **Phase 4 (HPC):** ~2-3 weeks (depends on cluster availability)
- **Phase 5-6:** ~4-8 hours

## 📝 Citation

If you use this code, please cite:

```
[Your paper citation here]
```

## 📧 Contact

**Author:** Binyue Kang  
**Institution:** Nankai University and University of Glasgow

## 📄 License

[Your license here - e.g., MIT, GPL-3.0, etc.]

## 🙏 Acknowledgments

- GloNAF database for invasion species data
- GBIF for species occurrence records
- WorldClim, SoilGrids, Bio-ORACLE for environmental data
- biomod2 package developers

## 🔄 Version History

- **v1.0.0** (2026-01-23) - Initial clean release with scientific naming conventions

---

**Note:** This is the clean, production-ready version of the code. The original code with development history is archived in the `code/` directory.
