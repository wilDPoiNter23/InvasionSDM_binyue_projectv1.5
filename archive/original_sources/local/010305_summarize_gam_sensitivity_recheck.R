# Original source: 010305_summarize_gam_sensitivity_recheck.R
# Original source location: <LOCAL_CODE_ROOT>/010305_summarize_gam_sensitivity_recheck.R
# Original source SHA-256: 3c84c6fb80584893ee2b0e52b6fc9b25624a36ae41887c905ca3e1eb3982dca1
# Public-safe original snapshot: archive/original_sources/local/010305_summarize_gam_sensitivity_recheck.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(lme4)

final_root <- "<LOCAL_ANALYSIS_ROOT>/FinalResults"
text_dir <- file.path(final_root, "text")
data_dir <- file.path(final_root, "data")
dir.create(text_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, formatC(x, digits = digits, format = "f"))
}

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f")))
}

candidate_tbl <- read.csv(
  file.path(final_root, "060506_gam_candidate_model_test", "candidate_model_summary.csv"),
  stringsAsFactors = FALSE
)
robust_tbl <- read.csv(
  file.path(final_root, "060505_robustness_size_reclamation_choice", "robustness_size_model_summary.csv"),
  stringsAsFactors = FALSE
)
raw_proxy_tbl <- read.csv(
  file.path(final_root, "060504_Mediation_Mechanisms_and_Sensitivity", "stats", "m_scale_B_raw_proxy_sensitivity_summary.csv"),
  stringsAsFactors = FALSE
)
spatial_tbl <- read.csv(
  file.path(final_root, "060507_spatial_correlogram_sensitivity", "m_scale_B_spatial_comparison.csv"),
  stringsAsFactors = FALSE
)
bin_tbl <- read.csv(
  file.path(final_root, "060507_spatial_correlogram_sensitivity", "m_scale_B_bin_sensitivity.csv"),
  stringsAsFactors = FALSE
)
gam_diag_tbl <- read.csv(
  file.path(final_root, "060503scale_dependentEffect", "ModelDiagnostics_060503.csv"),
  stringsAsFactors = FALSE
)

get_row <- function(tbl, key_col, key_val) {
  out <- tbl[tbl[[key_col]] == key_val, , drop = FALSE]
  if (!nrow(out)) stop(sprintf("Could not find %s = %s", key_col, key_val))
  out[1, , drop = FALSE]
}

dual_no_marine <- get_row(candidate_tbl, "model", "dual__no_marine")
single_no_marine <- get_row(candidate_tbl, "model", "single_logrecl__no_marine")
combo_no_marine <- get_row(candidate_tbl, "model", "combo__no_marine")
ti_no_marine <- get_row(candidate_tbl, "model", "ti_interaction__no_marine")
te_no_marine <- get_row(candidate_tbl, "model", "te_interaction__no_marine")

size_logarea <- get_row(robust_tbl, "model", "size_logarea")
size_area <- get_row(robust_tbl, "model", "size_area")
recl_dual <- get_row(robust_tbl, "model", "recl_dual")
recl_dual_plus_logarea <- get_row(robust_tbl, "model", "recl_dual_plus_logarea")
recl_combo_plus_logarea <- get_row(robust_tbl, "model", "recl_combo_plus_logarea")
te_area <- get_row(robust_tbl, "model", "te_area")
te_logarea <- get_row(robust_tbl, "model", "te_logarea")

raw_main <- raw_proxy_tbl[1, , drop = FALSE]
raw_sens <- raw_proxy_tbl[2, , drop = FALSE]

sp_baseline <- get_row(spatial_tbl, "model", "m_scale_B")
sp_spatial <- get_row(spatial_tbl, "model", "m_scale_B_spatial")
sp_region <- get_row(spatial_tbl, "model", "m_scale_B_region")
sp_geom <- get_row(spatial_tbl, "model", "m_scale_B_geom")
sp_geom_region <- get_row(spatial_tbl, "model", "m_scale_B_geom_region")

city_df <- read.csv(file.path(data_dir, "gamtest_withpca_recheck.csv"), stringsAsFactors = FALSE)
city_df$log_recl <- log1p(city_df$reclamation_area)
city_df$log_recl_frac <- log1p(city_df$reclamation_area / pmax(city_df$area_km2, .Machine$double.eps))
city_df$Regions <- as.factor(city_df$Regions)

lmm_check <- lmer(
  risk ~ log_recl + log_recl_frac + anthro_pc1 + climate_pc1 + soil_pc1 + geom_pc1 + (1 | Regions),
  data = city_df,
  REML = TRUE
)

lmm_fixef <- as.data.frame(summary(lmm_check)$coefficients)
lmm_fixef$term <- rownames(lmm_fixef)
rownames(lmm_fixef) <- NULL
names(lmm_fixef) <- c("estimate", "std_error", "t_value", "term")
lmm_fixef <- lmm_fixef[, c("term", "estimate", "std_error", "t_value")]

lmm_ci <- as.data.frame(confint(lmm_check, method = "Wald"))
lmm_ci$term <- rownames(lmm_ci)
rownames(lmm_ci) <- NULL
names(lmm_ci)[1:2] <- c("conf_low", "conf_high")
lmm_fixef <- merge(lmm_fixef, lmm_ci, by = "term", all.x = TRUE, sort = FALSE)

lmm_summary <- data.frame(
  model = "lmm_check",
  sample_size = nobs(lmm_check),
  AIC = AIC(lmm_check),
  is_singular = isSingular(lmm_check, tol = 1e-5),
  region_intercept_var = as.data.frame(VarCorr(lmm_check))$vcov[1],
  residual_var = as.data.frame(VarCorr(lmm_check))$vcov[2],
  stringsAsFactors = FALSE
)

write.csv(lmm_fixef, file.path(data_dir, "lmm_check_recheck_fixed_effects.csv"), row.names = FALSE)
write.csv(lmm_summary, file.path(data_dir, "lmm_check_recheck_fit_summary.csv"), row.names = FALSE)

overview_tbl <- data.frame(
  sensitivity_module = c(
    "Candidate GAM specification screening",
    "City-size and reclamation-representation robustness",
    "Mediator PCA replacement sensitivity",
    "Spatial autocorrelation sensitivity",
    "Linear mixed-model contrast"
  ),
  latest_output = c(
    "060506_gam_candidate_model_test/candidate_model_summary.csv",
    "060505_robustness_size_reclamation_choice/robustness_size_model_summary.csv",
    "060504_Mediation_Mechanisms_and_Sensitivity/stats/m_scale_B_raw_proxy_sensitivity_summary.csv",
    "060507_spatial_correlogram_sensitivity/m_scale_B_spatial_comparison.csv",
    "data/lmm_check_recheck_fit_summary.csv"
  ),
  key_take_home = c(
    "The additive dual-smooth formulation remained the preferred interpretable GAM for absolute versus relative reclamation effects, while interaction, single-index, and single-predictor alternatives were compared explicitly.",
    "Results were checked against raw versus log-transformed city area, dual versus combined reclamation representations, and additive versus tensor-product interaction formulations.",
    "The main m_scale_B model was re-fit after replacing anthro_pc1 and soil_pc1 with raw proxies (log_gdp and log_soc_den).",
    "Residual great-circle correlograms, alternative added terms, and lag-bin definitions were compared to test whether Model B was driven by residual spatial structure.",
    "A Gaussian mixed-effects model with linear reclamation terms was used as a conservative linear benchmark against the non-linear GAM."
  ),
  stringsAsFactors = FALSE
)
write.csv(overview_tbl, file.path(data_dir, "GAM_sensitivity_overview_recheck.csv"), row.names = FALSE)

methods_lines <- c(
  "GAM sensitivity analyses",
  "",
  "All GAM-based sensitivity analyses were rerun after boundary harmonization using the updated city dataset in FinalResults/data/gamtest_withpca_recheck.csv.",
  "",
  "Candidate model screening",
  "We first screened alternative GAM formulations for the city-level invasion-pressure response using the updated dataset. Five reclamation structures were compared: (i) additive dual smooths for absolute reclamation extent and relative reclamation intensity, (ii) a single smooth for log-transformed reclamation extent plus a city-size smooth, (iii) a one-index bridge model using the first principal component of log reclamation extent and log reclamation fraction, (iv) a tensor-product interaction, and (v) a main-effect plus tensor interaction formulation. Each specification was evaluated under four control sets that retained or removed the marine and regional terms. Model ranking was based on AIC, adjusted R2, deviance explained, smooth-term significance, and concurvity.",
  "",
  "City-size and reclamation-representation robustness",
  "We then assessed whether the inferred reclamation effect depended on how city size and reclamation were encoded. Alternative models replaced raw city area with log-transformed city area, compared the additive dual-smooth reclamation structure against the combined reclamation index, and contrasted tensor-product interactions defined on raw versus log-transformed city area. These comparisons were used to evaluate whether the main conclusions depended on scale transformation, over-control by city size, or compression of the reclamation signal into a single index.",
  "",
  "Mediator representation sensitivity",
  "To test whether the scale-dependent reclamation signal depended on PCA-based mediator compression, the main tensor-product GAM (m_scale_B) was re-fit after replacing anthro_pc1 and soil_pc1 with log-transformed GDP density (log_gdp) and log-transformed soil organic carbon density (log_soc_den). We compared AIC, adjusted R2, deviance explained, the significance and effective degrees of freedom of te(log_recl, area_km2), and the conditional city-area range over which the fitted reclamation slope changed sign.",
  "",
  "Spatial sensitivity",
  "Residual spatial dependence was evaluated for Model B using great-circle Moran correlograms constructed from pairwise haversine distances among city centroids. We compared the baseline scale-dependent GAM against versions that added a regional random-effect smooth, a geomorphic smooth, both terms jointly, or a bivariate geographic-process smooth s(Longitude, Latitude). Residual correlogram sensitivity was further checked by varying the number of lag bins (6, 8, and 10) and the maximum-distance quantile used to define lag classes (0.5, 0.6, and 0.7).",
  "",
  "Linear mixed-model benchmark",
  "As a conservative linear benchmark, we fit a Gaussian linear mixed-effects model with log-transformed reclamation extent, log-transformed reclamation fraction, and the same pathway covariates, with region included as a random intercept. This benchmark was used only to assess whether a simpler linear mixed-effects formulation could recover the same signal as the non-linear GAM."
)

si_lines <- c(
  "Supplementary note: GAM sensitivity analyses on the boundary-harmonized dataset",
  "",
  sprintf(
    "Candidate model screening showed that the additive dual-smooth model retained under the no-marine control set (dual__no_marine) remained the preferred interpretable additive GAM (AIC = %s, adjusted R2 = %s, deviance explained = %s%%), with a significant relative-intensity smooth (P = %s). Under the same control set, the single log-reclamation model (AIC = %s), the combined-index bridge model (AIC = %s), the ti interaction model (AIC = %s), and the te interaction model (AIC = %s) all fit less well than dual__no_marine.",
    fmt_num(dual_no_marine$AIC, 2),
    fmt_num(dual_no_marine$adj_r2, 3),
    fmt_num(dual_no_marine$dev_exp * 100, 1),
    fmt_p(dual_no_marine$p_log_recl_frac),
    fmt_num(single_no_marine$AIC, 2),
    fmt_num(combo_no_marine$AIC, 2),
    fmt_num(ti_no_marine$AIC, 2),
    fmt_num(te_no_marine$AIC, 2)
  ),
  "",
  sprintf(
    "Robustness tests for city-size and reclamation representation indicated that log-transformed city area outperformed raw area when city size was included as an additive covariate (size_logarea AIC = %s versus size_area AIC = %s). The additive dual-smooth specification (recl_dual; AIC = %s) remained competitive relative to the bridge specification using a combined reclamation index plus log city area (recl_combo_plus_logarea; AIC = %s). For the tensor-product interaction, raw-area and log-area formulations were very similar (te_area AIC = %s; te_logarea AIC = %s), indicating that the scale-dependent surface was not an artifact of the area transformation.",
    fmt_num(size_logarea$AIC, 2),
    fmt_num(size_area$AIC, 2),
    fmt_num(recl_dual$AIC, 2),
    fmt_num(recl_combo_plus_logarea$AIC, 2),
    fmt_num(te_area$AIC, 2),
    fmt_num(te_logarea$AIC, 2)
  ),
  "",
  sprintf(
    "Replacing the PCA mediators in the main tensor-product model with raw proxies reduced model support but did not remove the scale-dependent reclamation signal. The PCA-based main model had AIC = %s, adjusted R2 = %s, deviance explained = %s%%, and a significant te(log_recl, area_km2) term (P = %s). The raw-proxy model had AIC = %s (Delta AIC = %s), adjusted R2 = %s, deviance explained = %s%%, and still retained a significant tensor-product term (P = %s). The conditional sign-flip interval shifted upward from %s-%s km2 in the PCA model to %s-%s km2 in the raw-proxy model.",
    fmt_num(raw_main$AIC, 2),
    fmt_num(raw_main$Adjusted_R2, 3),
    fmt_num(raw_main$Deviance_explained_pct, 1),
    fmt_p(raw_main$Reclamation_smooth_P),
    fmt_num(raw_sens$AIC, 2),
    fmt_num(raw_sens$Delta_AIC_vs_main, 2),
    fmt_num(raw_sens$Adjusted_R2, 3),
    fmt_num(raw_sens$Deviance_explained_pct, 1),
    fmt_p(raw_sens$Reclamation_smooth_P),
    fmt_num(raw_main$Conditional_slope_flip_lower_km2, 1),
    fmt_num(raw_main$Conditional_slope_flip_upper_km2, 1),
    fmt_num(raw_sens$Conditional_slope_flip_lower_km2, 1),
    fmt_num(raw_sens$Conditional_slope_flip_upper_km2, 1)
  ),
  "",
  sprintf(
    "Spatial sensitivity analysis supported the robustness of Model B. The baseline scale-dependent GAM achieved AIC = %s, adjusted R2 = %s, deviance explained = %s%%, and te(log_recl, area_km2) P = %s. Adding a bivariate spatial smooth slightly increased adjusted R2 to %s but worsened AIC to %s; the tensor-product reclamation term remained significant (P = %s), whereas the spatial term itself was not significant (P = %s). Regional and geomorphic add-ons also did not materially alter the fitted tensor-product result. Residual spatial structure was weak, with only one significant lag class in the baseline and spatial models, centered at approximately %s km and showing negative rather than clustered Moran's I values.",
    fmt_num(sp_baseline$AIC, 2),
    fmt_num(sp_baseline$adj_r2, 3),
    fmt_num(sp_baseline$deviance_explained * 100, 1),
    fmt_p(sp_baseline$te_p),
    fmt_num(sp_spatial$adj_r2, 3),
    fmt_num(sp_spatial$AIC, 2),
    fmt_p(sp_spatial$te_p),
    fmt_p(sp_spatial$spatial_term_p),
    fmt_num(sp_baseline$first_residual_sig_mid_km, 1)
  ),
  "",
  "Across alternative lag-bin definitions, the residual correlogram signal was either absent or limited to a single short-distance bin. Significant residual bins disappeared entirely when the maximum-distance quantile was increased to 0.7, confirming that the weak residual signal was not stable across reasonable lag definitions.",
  "",
  sprintf(
    "The linear mixed-effects benchmark also supported the use of GAMs. In the updated dataset, lmm_check had AIC = %s, far worse than the additive GAM m_scale_A (AIC = %s). In the linear mixed model, log_recl showed a weak negative association (estimate = %s, 95%% CI %s to %s), whereas log_recl_frac was near zero (estimate = %s, 95%% CI %s to %s). This contrast indicates that linear mixed-effects models underfit the non-linear reclamation signal captured by the GAMs.",
    fmt_num(lmm_summary$AIC, 2),
    fmt_num(get_row(gam_diag_tbl, "model", "m_scale_A")$AIC, 2),
    fmt_num(lmm_fixef$estimate[lmm_fixef$term == "log_recl"], 4),
    fmt_num(lmm_fixef$conf_low[lmm_fixef$term == "log_recl"], 4),
    fmt_num(lmm_fixef$conf_high[lmm_fixef$term == "log_recl"], 4),
    fmt_num(lmm_fixef$estimate[lmm_fixef$term == "log_recl_frac"], 4),
    fmt_num(lmm_fixef$conf_low[lmm_fixef$term == "log_recl_frac"], 4),
    fmt_num(lmm_fixef$conf_high[lmm_fixef$term == "log_recl_frac"], 4)
  )
)

caption_lines <- c(
  "Supplementary Table | Sensitivity analyses for the GAM framework on the boundary-harmonized city dataset.",
  "The table set summarizes five complementary checks: candidate GAM specification screening, robustness to city-size and reclamation representation, replacement of PCA mediators with raw proxies, spatial autocorrelation sensitivity for Model B, and a linear mixed-effects benchmark.",
  "All analyses were rerun after boundary harmonization using the updated 108-city dataset.",
  "For each module, the relevant outputs are reported in the corresponding csv files listed in GAM_sensitivity_overview_recheck.csv."
)

writeLines(methods_lines, con = file.path(text_dir, "GAM_sensitivity_methods_nature.txt"), useBytes = TRUE)
writeLines(si_lines, con = file.path(text_dir, "GAM_sensitivity_SI_results_nature.txt"), useBytes = TRUE)
writeLines(caption_lines, con = file.path(text_dir, "GAM_sensitivity_SI_table_caption.txt"), useBytes = TRUE)

cat("Wrote:\n")
cat(file.path(text_dir, "GAM_sensitivity_methods_nature.txt"), "\n", sep = "")
cat(file.path(text_dir, "GAM_sensitivity_SI_results_nature.txt"), "\n", sep = "")
cat(file.path(text_dir, "GAM_sensitivity_SI_table_caption.txt"), "\n", sep = "")
cat(file.path(data_dir, "GAM_sensitivity_overview_recheck.csv"), "\n", sep = "")
cat(file.path(data_dir, "lmm_check_recheck_fit_summary.csv"), "\n", sep = "")
cat(file.path(data_dir, "lmm_check_recheck_fixed_effects.csv"), "\n", sep = "")
