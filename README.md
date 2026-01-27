# Coastal Reclamation and Invasive Species Distribution

**Species Distribution Modeling (SDM) analysis of 800+ invasive plant species in coastal cities with reclamation activities**

## 📋 Project Overview

This repository contains the complete workflow for analyzing the impact of coastal reclamation on invasive species distribution using Species Distribution Modeling (SDM). The project processes data for 800+ invasive plant species across global coastal cities and builds ensemble SDM models to assess invasion risk.

## 🎯 Research Questions

1. How does coastal reclamation affect invasive species distribution?
2. What environmental factors drive invasion risk in coastal cities?
3. Which species pose the highest invasion threats?

## 📊 Project Statistics

- **Total Code:** 15,379 lines of R code
- **Species Modeled:** 800+ invasive plant species
- **Coastal Cities:** 108 cities globally
- **Environmental Variables:** Climate, soil, marine, anthropogenic, topographic factors
- **Modeling Algorithms:** GLM, Random Forest, MaxEnt
- **Ensemble Methods:** EMmean, EMmedian

## 📁 Repository Structure

```
InvasionSDM/
├── code_clean/                          # Clean, scientifically-named code
│   ├── 00_setup/                        # Environment setup (1 file)
│   ├── 01_data_preparation/             # Data integration (5 files)
│   ├── 02_species_data/                 # GBIF data acquisition (4 files)
│   ├── 03_environmental_data/           # Environmental processing (12 files)
│   ├── 04_sdm_modeling/                 # SDM batch modeling (9 files)
│   ├── 05_results_analysis/             # Model evaluation (3 files)
│   └── 06_statistical_analysis/         # GAM/SEM analysis (8 files)
│
├── scripts_clean/                       # Shell scripts for HPC
│   ├── submit_slurm_job.sh
│   ├── submit_batch_modeling.sh
│   └── batch_rename_files.sh
│
├── docs/                                # Documentation
│   ├── InvasionSDM_Workflow_Documentation.md
│   ├── Project_Cleanup_Guide.md
│   ├── Code_Statistics_Report.md
│   └── project_structure.ini
│
├── code/                                # Original code (archived)
├── data/                                # Data directory
├── results/                             # Results directory
└── README.md                            # This file
```

## 🚀 Quick Start

### Prerequisites

- R >= 4.5.2
- Linux HPC with SLURM (for Phase 4 batch modeling)
- Required R packages (see `code_clean/00_setup/00_install_packages.R`)

### Installation

```r
# Install all required packages
source("code_clean/00_setup/00_install_packages.R")
```

### Workflow Execution

The workflow consists of 6 phases that should be executed sequentially:

#### Phase 1: Data Preparation
```r
# Integrate GloNAF invasion data with reclamation cities
source("code_clean/01_data_preparation/01_prepare_glonaf_invasion_data.R")
source("code_clean/01_data_preparation/01_define_coastal_cities.R")
source("code_clean/01_data_preparation/01_process_reclamation_data.R")
source("code_clean/01_data_preparation/01_clean_variable_names.R")
source("code_clean/01_data_preparation/01_correlation_analysis.R")
```

#### Phase 2: Species Data Acquisition
```r
# Download species occurrence data from GBIF
source("code_clean/02_species_data/02_download_gbif_occurrences.R")
source("code_clean/02_species_data/02_generate_sql_queries.R")
source("code_clean/02_species_data/02_create_richness_heatmap.R")
```

#### Phase 3: Environmental Data Processing
```r
# Process environmental variables
source("code_clean/03_environmental_data/03_download_environmental_layers.R")
source("code_clean/03_environmental_data/03_resample_environmental_layers.R")
source("code_clean/03_environmental_data/03_combine_environmental_stack.R")
source("code_clean/03_environmental_data/03_select_variables_vif.R")
source("code_clean/03_environmental_data/03_retain_anthropogenic_variables.R")
```

#### Phase 4: SDM Modeling (HPC Required)
```bash
# Submit batch modeling job to SLURM
cd scripts_clean
sbatch submit_batch_modeling.sh
```

#### Phase 5: Results Analysis
```r
# Evaluate model performance
source("code_clean/05_results_analysis/05_evaluate_model_performance.R")
source("code_clean/05_results_analysis/05_analyze_continental_risk.R")
```

#### Phase 6: Statistical Analysis
```r
# GAM and SEM analysis
source("code_clean/06_statistical_analysis/06_gam_reclamation_risk_analysis.R")
source("code_clean/06_statistical_analysis/06_pdp_sem_analysis.R")
source("code_clean/06_statistical_analysis/06_visualize_city_details.R")
```

## 📖 Documentation

- **[Workflow Documentation](docs/InvasionSDM_Workflow_Documentation.md)** - Complete workflow description with file purposes and dependencies
- **[Cleanup Guide](docs/Project_Cleanup_Guide.md)** - Guide for maintaining clean code
- **[Code Statistics](docs/Code_Statistics_Report.md)** - Detailed code statistics and analysis

## 🔬 Methodology

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
**Institution:** [Your institution]  
**Email:** [Your email]

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
