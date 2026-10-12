# Original source: 060504_Mediation_Mechanisms_and_Sensitivity.r
# Original source location: <LOCAL_CODE_ROOT>/060504_Mediation_Mechanisms_and_Sensitivity.r
# Original source SHA-256: 0dc3942b43bffb4450382e87293a34609c1963741d15533665ccc2cb171a926c
# Public-safe original snapshot: archive/original_sources/local/060504_Mediation_Mechanisms_and_Sensitivity.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# =============================================================================
# 060504_Mediation_Mechanisms_and_Sensitivity.r
#
# FULL PIPELINE: Quantitative Mediation, Scale Thresholds, and Robustness
#
# This script consolidates the supplementary quantitative analyses required
# to robustly prove the "Competitive Mediation" hypothesis:
#   1. Algebraic plotting of the strictly positive indirect effect.
#   2. Extraction of exact scale thresholds (zero-crossing points).
#   3. Directionality checks for mediator pathways (Recl -> Mediators -> Risk).
#   4. Sensitivity analysis replacing PCA indices with raw elemental proxies.
# =============================================================================

library(mgcv)
library(dplyr)
library(gratia)
library(ggplot2)
library(ggeffects)

workdir <- Sys.getenv("GAM_WORKDIR", unset = "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft")
input_csv <- Sys.getenv("GAM_INPUT_CSV", unset = file.path(workdir, "gamtest_withpca.csv"))
output_dir <- Sys.getenv("GAM_OUTPUT_DIR", unset = file.path(workdir, "gamFinalResult"))
stats_dir <- file.path(output_dir, "stats")
text_dir <- file.path(output_dir, "text")
setwd(workdir)
dir.create(stats_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(text_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 0. Data Preparation
# -----------------------------------------------------------------------------
cat("=== Loading Data ===\n")
city_df <- read.csv(input_csv)
city_df$reclamation_area <- city_df$reclamation_area + 1e-6
city_df$area_km2 <- city_df$area_km2 + 1e-6
city_df$log_recl <- log1p(city_df$reclamation_area)
city_df$recl_frac <- city_df$reclamation_area / city_df$area_km2
city_df$log_recl_frac <- log1p(city_df$recl_frac)
city_df$log_city_area <- log1p(city_df$area_km2)
city_df$Regions <- as.factor(city_df$Regions)

# -----------------------------------------------------------------------------
# 1. Algebraic Indirect Effect Calculation (Path A * Path B)
# -----------------------------------------------------------------------------
cat("\n=== 1. ALGEBRAIC INDIRECT EFFECT ===\n")
m2a_SE <- gam(anthro_pc1 ~ s(log_recl, k = 5) + s(log_recl_frac, k = 5) + s(climate_pc1, k = 5) + s(marine_pc1, k = 5) + s(geom_pc1, k = 5) + s(log_city_area, k = 5) + s(Regions, bs = "re"), data = city_df, method = "REML")
m2b_soil <- gam(soil_pc1 ~ s(log_recl, k = 5) + s(log_recl_frac, k = 5) + s(climate_pc1, k = 5) + s(marine_pc1, k = 5) + s(geom_pc1, k = 5) + s(log_city_area, k = 5) + s(Regions, bs = "re"), data = city_df, method = "REML")
m3_direct <- gam(risk ~ s(log_recl, k = 5) + s(log_recl_frac, k = 5) + s(anthro_pc1, k = 5) + s(soil_pc1, k = 5) + s(climate_pc1, k = 5) + s(marine_pc1, k = 5) + s(geom_pc1, k = 5) + s(log_city_area, k = 5) + s(Regions, bs = "re"), data = city_df, method = "REML")

sm_SE_path <- smooth_estimates(m2a_SE, select = "s(log_recl_frac)") |> arrange(log_recl_frac)
sm_soil_path <- smooth_estimates(m2b_soil, select = "s(log_recl_frac)") |> arrange(log_recl_frac)

deriv_SE <- derivatives(m3_direct, select = "s(anthro_pc1)")
mean_SE_slope <- mean(deriv_SE$.derivative)

deriv_soil <- derivatives(m3_direct, select = "s(soil_pc1)")
mean_soil_slope <- mean(deriv_soil$.derivative)

ind_eff <- sm_SE_path
ind_eff$.estimate <- (sm_SE_path$.estimate * mean_SE_slope) + (sm_soil_path$.estimate * mean_soil_slope)

p_ind <- ggplot(ind_eff, aes(x = log_recl_frac, y = .estimate)) +
  geom_line(linewidth = 1.8, colour = "#e6ab02") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  theme_classic(base_size = 18) +
  labs(
    # title    = "Calculated Indirect Facilitation (Algebraic Product)",
    # subtitle = "Derived via gradient multiplication of SE and Soil pathways",
    x        = "Reclamation fraction (log1p-transformed)",
    y        = "Computed indirect contribution to pressure"
  )
# ) +
# annotate("text", x = 0.05, y = max(ind_eff$.estimate)*0.8, hjust = 0, size = 5,
#          colour = "#e6ab02", fontface = "bold",
#          label = "Algebraically confirmed:\nThe socioeconomic + soil\npathway is strictly POSITIVE.")
p_ind
tryCatch(
  {
    ggsave(file.path(output_dir, "step2_indirect_effect_algebraic.jpg"), p_ind, width = 6, height = 6, dpi = 900)
    cat("Exported: step2_indirect_effect_algebraic.jpg\n")
  },
  error = function(e) {
    cat("Warning: failed to export step2_indirect_effect_algebraic.jpg -", conditionMessage(e), "\n")
  }
)

# -----------------------------------------------------------------------------
# 2. Extract Exact Thresholds & M1 vs M3 Magnitudes
# -----------------------------------------------------------------------------
cat("\n=== 2. SCALE THRESHOLDS & M1 VS M3 MAGNITUDES ===\n")
m_scale_A <- gam(risk ~ s(log_recl, k = 5) + s(log_recl_frac, k = 5) + s(anthro_pc1, k = 5) + s(climate_pc1, k = 5) + s(soil_pc1, k = 5) + s(marine_pc1, k = 5) + s(geom_pc1, k = 5) + s(Regions, bs = "re"), data = city_df, method = "REML")
m1_total <- gam(risk ~ s(log_recl, k = 5) + s(log_recl_frac, k = 5) + s(climate_pc1, k = 5) + s(marine_pc1, k = 5) + s(geom_pc1, k = 5) + s(log_city_area, k = 5) + s(Regions, bs = "re"), data = city_df, method = "REML")

write.csv(summary(m_scale_A)$s.table, file.path(stats_dir, "m_scale_A_smooths.csv"))
write.csv(summary(m3_direct)$s.table, file.path(stats_dir, "m3_direct_smooths.csv"))
cat("Exported: m_scale_A and m3_direct smooth tables to stats/\n")

sm_A_frac <- smooth_estimates(m_scale_A, select = "s(log_recl_frac)")
cat(sprintf("m_scale_A Zero-Crossing Threshold: log_recl_frac = %.3f\n", min(sm_A_frac$log_recl_frac[sm_A_frac$.estimate < 0])))
cat(sprintf("m_scale_A Total Scale Drop: %.3f\n", max(sm_A_frac$.estimate) - min(sm_A_frac$.estimate)))

sm1_frac <- smooth_estimates(m1_total, select = "s(log_recl_frac)")
cat(sprintf("\nM1 (Total) Zero-Crossing Threshold: %.3f\n", min(sm1_frac$log_recl_frac[sm1_frac$.estimate < 0])))
cat(sprintf("M1 (Total) Scale Drop: %.3f\n", max(sm1_frac$.estimate) - min(sm1_frac$.estimate)))

sm3_frac <- smooth_estimates(m3_direct, select = "s(log_recl_frac)")
cat(sprintf("\nM3 (Direct) Zero-Crossing Threshold: %.3f\n", min(sm3_frac$log_recl_frac[sm3_frac$.estimate < 0])))
cat(sprintf("M3 (Direct) Scale Drop: %.3f\n", max(sm3_frac$.estimate) - min(sm3_frac$.estimate)))

# -----------------------------------------------------------------------------
# 3. Mediator Directionality & Tensor Product (Method B) Interaction Sign Reversal
# -----------------------------------------------------------------------------
cat("\n=== 3. MEDIATION DIRECTIONALITY AND TE() SIGN REVERSAL ===\n")
cat(sprintf("Path A1 (Recl->SE Average Slope): %+.3f\n", mean(derivatives(m2a_SE, select = "s(log_recl_frac)")$.derivative)))
cat(sprintf("Path A2 (Recl->Soil Average Slope): %+.3f\n", mean(derivatives(m2b_soil, select = "s(log_recl_frac)")$.derivative)))
cat(sprintf("Path B1 (SE->Risk Average Slope): %+.3f\n", mean(derivatives(m3_direct, select = "s(anthro_pc1)")$.derivative)))
cat(sprintf("Path B2 (Soil->Risk Average Slope): %+.3f\n", mean(derivatives(m3_direct, select = "s(soil_pc1)")$.derivative)))

scan_conditional_slopes <- function(model, data, probs_test = seq(0.05, 0.95, by = 0.05)) {
  slope_scan <- data.frame(
    City_Size_Quantile = probs_test,
    City_Area_km2 = NA_real_,
    LogRecl_Effect_Slope = NA_real_,
    Direction = NA_character_
  )

  for (i in seq_along(probs_test)) {
    q_val <- quantile(data$area_km2, probs_test[i], na.rm = TRUE)
    pred <- ggpredict(model, terms = c("log_recl [all]", paste0("area_km2 [", round(q_val, 1), "]")))
    pred_df <- as.data.frame(pred)
    slope_scan$City_Area_km2[i] <- q_val
    slope_scan$LogRecl_Effect_Slope[i] <- pred_df$predicted[which.max(pred_df$x)] - pred_df$predicted[which.min(pred_df$x)]
    slope_scan$Direction[i] <- ifelse(slope_scan$LogRecl_Effect_Slope[i] > 0, "+ (Positive)", "- (Negative)")
  }

  slope_scan
}

extract_flip_range <- function(slope_scan) {
  neg_areas <- slope_scan$City_Area_km2[slope_scan$LogRecl_Effect_Slope < 0]
  pos_areas <- slope_scan$City_Area_km2[slope_scan$LogRecl_Effect_Slope > 0]

  c(
    lower_km2 = if (length(neg_areas) && length(pos_areas)) max(neg_areas, na.rm = TRUE) else NA_real_,
    upper_km2 = if (length(neg_areas) && length(pos_areas)) min(pos_areas, na.rm = TRUE) else NA_real_
  )
}

extract_smooth_metrics <- function(model, pattern) {
  st <- summary(model)$s.table
  idx <- grep(pattern, rownames(st))
  if (!length(idx)) {
    return(data.frame(
      term = NA_character_,
      edf = NA_real_,
      ref_df = NA_real_,
      F_value = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    term = rownames(st)[idx[1]],
    edf = unname(st[idx[1], "edf"]),
    ref_df = unname(st[idx[1], "Ref.df"]),
    F_value = unname(st[idx[1], "F"]),
    p_value = unname(st[idx[1], "p-value"]),
    stringsAsFactors = FALSE
  )
}

format_p <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  if (x < 0.001) {
    return("< 0.001")
  }
  sprintf("%.3f", x)
}

format_num <- function(x, digits = 3) {
  if (is.na(x)) {
    return("NA")
  }
  formatC(x, digits = digits, format = "f")
}

describe_flip_range <- function(lower, upper) {
  if (is.na(lower) || is.na(upper)) {
    return("no conditional sign flip was detected across the observed city-area range")
  }
  paste0("the fitted reclamation effect changes sign between ", format_num(lower, 1), " and ", format_num(upper, 1), " km2")
}

collapse_top_loadings <- function(df, n_top = 3) {
  if (!nrow(df)) {
    return("NA")
  }
  df <- df[seq_len(min(nrow(df), n_top)), , drop = FALSE]
  paste(sprintf("%s (%+.3f)", df$Variable, df$PC1_Loading), collapse = ", ")
}

m_scale_B <- gam(risk ~ te(log_recl, area_km2, k = c(5, 5)) + s(anthro_pc1, k = 5) + s(climate_pc1, k = 5) + s(soil_pc1, k = 5) + s(marine_pc1, k = 5) + s(geom_pc1, k = 5) + s(Regions, bs = "re"), data = city_df, method = "REML")
probs_test <- seq(0.05, 0.95, by = 0.05)
slope_scan <- scan_conditional_slopes(m_scale_B, city_df, probs_test = probs_test)
write.csv(slope_scan, file.path(stats_dir, "m_scale_B_conditional_slopes.csv"), row.names = FALSE)
cat("Exported: m_scale_B conditional slopes to stats/\n")

# -----------------------------------------------------------------------------
# 4. Sensitivity Analysis: Main-model mediator replacement for m_scale_B
# -----------------------------------------------------------------------------
cat("\n=== 4. SENSITIVITY ANALYSIS (m_scale_B RAW PROXIES VS PCA) ===\n")
# Replace mediator PCA terms with the dominant raw proxies for each group.
city_df$log_gdp <- log1p(city_df$GDP2020_2.5min)
city_df$log_soc_den <- log1p(city_df$ocd_mean_2024_2_5min_aligned)

m_scale_B_sens <- gam(
  risk ~ te(log_recl, area_km2, k = c(5, 5)) +
    s(log_gdp, k = 5) +
    s(log_soc_den, k = 5) +
    s(climate_pc1, k = 5) +
    s(marine_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

slope_scan_sens <- scan_conditional_slopes(m_scale_B_sens, city_df, probs_test = probs_test)
write.csv(slope_scan_sens, file.path(stats_dir, "m_scale_B_sensitivity_conditional_slopes.csv"), row.names = FALSE)
write.csv(summary(m_scale_B_sens)$s.table, file.path(stats_dir, "m_scale_B_sensitivity_raw_proxy_smooths.csv"))

main_recl <- extract_smooth_metrics(m_scale_B, "^te\\(")
sens_recl <- extract_smooth_metrics(m_scale_B_sens, "^te\\(")
main_anthro <- extract_smooth_metrics(m_scale_B, "anthro_pc1")
main_soil <- extract_smooth_metrics(m_scale_B, "soil_pc1")
sens_gdp <- extract_smooth_metrics(m_scale_B_sens, "log_gdp")
sens_soc <- extract_smooth_metrics(m_scale_B_sens, "log_soc_den")
main_flip <- extract_flip_range(slope_scan)
sens_flip <- extract_flip_range(slope_scan_sens)

summary_table <- bind_rows(
  data.frame(
    Model = "Main PCA mediator model (m_scale_B)",
    Mediator_representation = "anthro_pc1 + soil_pc1",
    Reclamation_term = main_recl$term,
    Sample_size = nrow(model.frame(m_scale_B)),
    AIC = AIC(m_scale_B),
    Delta_AIC_vs_main = 0,
    Adjusted_R2 = summary(m_scale_B)$r.sq,
    Deviance_explained_pct = summary(m_scale_B)$dev.expl * 100,
    Reclamation_smooth_edf = main_recl$edf,
    Reclamation_smooth_F = main_recl$F_value,
    Reclamation_smooth_P = main_recl$p_value,
    Mediator_1 = "anthro_pc1",
    Mediator_1_edf = main_anthro$edf,
    Mediator_1_P = main_anthro$p_value,
    Mediator_2 = "soil_pc1",
    Mediator_2_edf = main_soil$edf,
    Mediator_2_P = main_soil$p_value,
    Conditional_slope_flip_lower_km2 = unname(main_flip["lower_km2"]),
    Conditional_slope_flip_upper_km2 = unname(main_flip["upper_km2"]),
    stringsAsFactors = FALSE
  ),
  data.frame(
    Model = "Raw-proxy sensitivity model (m_scale_B_sens)",
    Mediator_representation = "log_gdp + log_soc_den",
    Reclamation_term = sens_recl$term,
    Sample_size = nrow(model.frame(m_scale_B_sens)),
    AIC = AIC(m_scale_B_sens),
    Delta_AIC_vs_main = AIC(m_scale_B_sens) - AIC(m_scale_B),
    Adjusted_R2 = summary(m_scale_B_sens)$r.sq,
    Deviance_explained_pct = summary(m_scale_B_sens)$dev.expl * 100,
    Reclamation_smooth_edf = sens_recl$edf,
    Reclamation_smooth_F = sens_recl$F_value,
    Reclamation_smooth_P = sens_recl$p_value,
    Mediator_1 = "log_gdp",
    Mediator_1_edf = sens_gdp$edf,
    Mediator_1_P = sens_gdp$p_value,
    Mediator_2 = "log_soc_den",
    Mediator_2_edf = sens_soc$edf,
    Mediator_2_P = sens_soc$p_value,
    Conditional_slope_flip_lower_km2 = unname(sens_flip["lower_km2"]),
    Conditional_slope_flip_upper_km2 = unname(sens_flip["upper_km2"]),
    stringsAsFactors = FALSE
  )
)

publication_table <- summary_table |>
  transmute(
    `Model` = Model,
    `Mediator representation` = Mediator_representation,
    `Reclamation term` = Reclamation_term,
    `Sample size` = Sample_size,
    `AIC` = round(AIC, 2),
    `Delta AIC vs main` = round(Delta_AIC_vs_main, 2),
    `Adjusted R2` = round(Adjusted_R2, 3),
    `Deviance explained (%)` = round(Deviance_explained_pct, 1),
    `Reclamation smooth EDF` = round(Reclamation_smooth_edf, 3),
    `Reclamation smooth F` = round(Reclamation_smooth_F, 3),
    `Reclamation smooth P` = vapply(Reclamation_smooth_P, format_p, character(1)),
    `Mediator 1` = Mediator_1,
    `Mediator 1 EDF` = round(Mediator_1_edf, 3),
    `Mediator 1 P` = vapply(Mediator_1_P, format_p, character(1)),
    `Mediator 2` = Mediator_2,
    `Mediator 2 EDF` = round(Mediator_2_edf, 3),
    `Mediator 2 P` = vapply(Mediator_2_P, format_p, character(1)),
    `Conditional slope-flip lower bound (km2)` = round(Conditional_slope_flip_lower_km2, 1),
    `Conditional slope-flip upper bound (km2)` = round(Conditional_slope_flip_upper_km2, 1)
  )

write.csv(summary_table, file.path(stats_dir, "m_scale_B_raw_proxy_sensitivity_summary.csv"), row.names = FALSE)
write.csv(publication_table, file.path(output_dir, "TableS_m_scale_B_raw_proxy_sensitivity.csv"), row.names = FALSE)

pca_path <- "<RECOVERED_CODE_ROOT>/SI_Table_PCA_Loadings_Fixed.csv"
if (file.exists(pca_path)) {
  pca_loadings <- read.csv(pca_path)
  mediator_pca_summary <- pca_loadings |>
    filter(Category %in% c("Anthropogenic", "Soil")) |>
    mutate(abs_loading = abs(PC1_Loading)) |>
    arrange(Category, desc(abs_loading)) |>
    group_by(Category) |>
    slice_head(n = 5) |>
    ungroup()
  write.csv(mediator_pca_summary, file.path(stats_dir, "m_scale_B_mediator_pca_loadings_summary.csv"), row.names = FALSE)

  anthro_text <- collapse_top_loadings(filter(mediator_pca_summary, Category == "Anthropogenic"))
  soil_text <- collapse_top_loadings(filter(mediator_pca_summary, Category == "Soil"))
} else {
  anthro_text <- "PCA loading file not found"
  soil_text <- "PCA loading file not found"
}

caption_lines <- c(
  "Supplementary Table | Raw-proxy sensitivity analysis for the main scale-dependent GAM.",
  paste(
    "Comparison of the main model m_scale_B, which includes the mediator composites anthro_pc1 and soil_pc1,",
    "with a raw-proxy sensitivity model replacing those PCA mediators with log-transformed GDP density (log_gdp)",
    "and log-transformed soil organic carbon density (log_soc_den). Both models retain the same scale-dependent",
    "reclamation term te(log_recl, area_km2) and the same climate, marine, geomorphic, and regional controls.",
    "Because the main model is a bivariate tensor-product GAM, threshold behaviour is reported as the city-area",
    "interval over which the conditional reclamation slope changes sign, rather than as a single zero-crossing point."
  )
)

intext_lines <- c(
  paste0(
    "Replacing the mediator PCA terms in the main scale-dependent model with raw proxies yielded a comparable fit",
    " (main model AIC = ", format_num(summary_table$AIC[1], 2),
    "; raw-proxy model AIC = ", format_num(summary_table$AIC[2], 2),
    "; Delta AIC = ", format_num(summary_table$Delta_AIC_vs_main[2], 2), ")."
  ),
  paste0(
    "The scale-dependent reclamation term remained ",
    ifelse(summary_table$Reclamation_smooth_P[2] < 0.05, "significant", "non-significant"),
    " after mediator replacement (te P = ", format_p(summary_table$Reclamation_smooth_P[2]),
    "; main-model te P = ", format_p(summary_table$Reclamation_smooth_P[1]), ")."
  ),
  paste0(
    "In the PCA representation, Anthropogenic PC1 was dominated by ", anthro_text,
    ", whereas Soil PC1 was dominated by ", soil_text, "."
  ),
  paste0(
    "For m_scale_B, threshold interpretation should be framed as a conditional sign-flip range, not as a single PCA threshold:",
    " ", describe_flip_range(
      summary_table$Conditional_slope_flip_lower_km2[1],
      summary_table$Conditional_slope_flip_upper_km2[1]
    ), " in the main model, whereas ",
    describe_flip_range(
      summary_table$Conditional_slope_flip_lower_km2[2],
      summary_table$Conditional_slope_flip_upper_km2[2]
    ), " in the raw-proxy sensitivity model."
  )
)

header_lines <- c(
  "Recommended table title: Supplementary Table | Raw-proxy sensitivity analysis for the main scale-dependent GAM.",
  paste("Column headers:", paste(colnames(publication_table), collapse = " | "))
)

writeLines(header_lines, file.path(text_dir, "TableS_m_scale_B_raw_proxy_sensitivity_header.txt"), useBytes = TRUE)
writeLines(caption_lines, file.path(text_dir, "TableS_m_scale_B_raw_proxy_sensitivity_caption.txt"), useBytes = TRUE)
writeLines(intext_lines, file.path(text_dir, "TableS_m_scale_B_raw_proxy_sensitivity_intext.txt"), useBytes = TRUE)

cat(sprintf("m_scale_B AIC: %.1f\n", AIC(m_scale_B)))
cat(sprintf("m_scale_B_sens AIC: %.1f\n", AIC(m_scale_B_sens)))
cat(sprintf("m_scale_B te() P: %s\n", format_p(main_recl$p_value)))
cat(sprintf("m_scale_B_sens te() P: %s\n", format_p(sens_recl$p_value)))
cat("Exported: m_scale_B sensitivity tables and publication text files.\n")

cat("\n=== PIPELINE COMPLETE ===\n")
