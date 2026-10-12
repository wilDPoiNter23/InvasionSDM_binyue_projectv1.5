# Original source: suppressPackageStartupMessages({.r
# Original source location: <LOCAL_CODE_ROOT>/suppressPackageStartupMessages({.r
# Original source SHA-256: 1bd1902fcf489eb184705664efed8bb14759fc4953129f8431d8dfcffb206d25
# Public-safe original snapshot: archive/original_sources/local/gam_log_area_alternative.R
# Classification: HISTORICAL
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
suppressPackageStartupMessages({
  library(mgcv)
  library(gratia)
  library(ggeffects)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(geosphere)
  library(lme4)
})

# =============================================================================
# 060503scale_dependentEffect.r
#
# Final specification after 060506_gam_candidate_model_test.r:
#   - Main Step 1 model: dual__no_marine
#   - Marine PC1 is removed from inferential GAMs
#   - Regions is retained as a random-effect smooth
#   - Interaction supplement uses log_city_area rather than raw area_km2
# =============================================================================

setwd("<LOCAL_ANALYSIS_ROOT>/figure/gam_draft")
output_dir <- "gamFinalResult"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

city_df <- read.csv("gamtest_withpca.csv")
city_df$log_city_area <- log1p(city_df$area_km2)
city_df$log_recl <- log1p(city_df$reclamation_area)
city_df$recl_frac <- city_df$reclamation_area / pmax(city_df$area_km2, .Machine$double.eps)
city_df$log_recl_frac <- log1p(city_df$recl_frac)
city_df$risk_class <- as.factor(city_df$risk_class)
city_df$Regions <- as.factor(city_df$Regions)

stopifnot(all(c(
  "risk", "reclamation_area", "area_km2", "log_recl", "log_recl_frac",
  "log_city_area", "anthro_pc1", "climate_pc1", "soil_pc1", "geom_pc1",
  "Longitude", "Latitude", "Regions"
) %in% names(city_df)))

save_plot <- function(filename, plot_obj, width, height, dpi = 400) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    device = grDevices::png
  )
}

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0.001")
  formatC(p, digits = 3, format = "f")
}

extract_pvals <- function(model, label) {
  sm <- summary(model)$s.table
  cat("\n=====", label, "=====\n")
  cat(sprintf(
    "  %-30s edf=%5.2f  F=%7.3f  p=%s\n",
    rownames(sm),
    sm[, "edf"],
    sm[, "F"],
    vapply(sm[, "p-value"], fmt_p, character(1))
  ))
  cat(sprintf(
    "  Adj-R2=%.3f  Dev.Exp=%.1f%%  AIC=%.2f\n",
    summary(model)$r.sq,
    summary(model)$dev.expl * 100,
    AIC(model)
  ))
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

extract_concurvity_metrics <- function(model) {
  cc <- suppressWarnings(concurvity(model, full = TRUE))
  observed_row <- if ("observed" %in% rownames(cc)) cc["observed", ] else setNames(rep(NA_real_, ncol(cc)), colnames(cc))
  estimate_row <- if ("estimate" %in% rownames(cc)) cc["estimate", ] else setNames(rep(NA_real_, ncol(cc)), colnames(cc))
  get_term <- function(term_name, row_obj) {
    if (term_name %in% names(row_obj)) unname(row_obj[[term_name]]) else NA_real_
  }
  data.frame(
    cc_obs_max = safe_max(observed_row),
    cc_est_max = safe_max(estimate_row),
    cc_obs_log_recl = get_term("s(log_recl)", observed_row),
    cc_obs_log_recl_frac = get_term("s(log_recl_frac)", observed_row),
    cc_obs_log_city_area = get_term("s(log_city_area)", observed_row),
    cc_obs_te = get_term("te(log_recl,log_city_area)", observed_row),
    stringsAsFactors = FALSE
  )
}

find_smooth <- function(model, pattern, which_match = 1) {
  sm_names <- gratia::smooths(model)
  matches <- grep(pattern, sm_names, value = TRUE)
  if (length(matches) < which_match) {
    stop(sprintf("Could not find smooth matching pattern: %s", pattern))
  }
  matches[[which_match]]
}

get_sm <- function(model, smooth_name, x_name, model_label, var_label) {
  df <- smooth_estimates(model, select = smooth_name) |>
    add_confint()
  df$x_val <- df[[x_name]]
  df$model_label <- model_label
  df$var_label <- var_label
  df
}

recl_sig_text <- function(mod) {
  st <- summary(mod)$s.table
  idx <- grep("log_recl|te\\(log_recl", rownames(st))
  if (!length(idx)) return("n.a.")
  msgs <- vapply(idx, function(i) {
    sprintf("%s: p=%s", rownames(st)[i], fmt_p(st[i, "p-value"]))
  }, character(1))
  paste(msgs, collapse = "; ")
}

make_row <- function(section, model_name, recl_struct, outcome, mod, note = "", selected = FALSE) {
  sm <- summary(mod)
  st <- sm$s.table
  get_p <- function(pattern) {
    idx <- grep(pattern, rownames(st))
    if (!length(idx)) return(NA_character_)
    p <- st[idx[1], "p-value"]
    paste0(
      fmt_p(p),
      ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
    )
  }
  data.frame(
    Section = section,
    Model = ifelse(selected, paste0(model_name, " [MAIN]"), model_name),
    Reclamation_structure = recl_struct,
    Outcome = outcome,
    Adj_R2 = round(sm$r.sq, 3),
    Dev_pct = round(sm$dev.expl * 100, 1),
    AIC = round(AIC(mod), 2),
    n_obs = sm$n,
    p_log_recl = get_p("^s\\(log_recl\\)|^te\\(log_recl"),
    p_log_recl_frac = get_p("^s\\(log_recl_frac\\)"),
    p_anthro_pc1 = get_p("anthro_pc1"),
    p_soil_pc1 = get_p("soil_pc1"),
    Note = note,
    stringsAsFactors = FALSE
  )
}

summarize_gam_diag <- function(model_name, outcome, model_obj) {
  cc <- extract_concurvity_metrics(model_obj)
  data.frame(
    model = model_name,
    outcome = outcome,
    AIC = AIC(model_obj),
    adj_r2 = summary(model_obj)$r.sq,
    dev_exp = summary(model_obj)$dev.expl,
    p_log_recl = suppressWarnings({
      st <- summary(model_obj)$s.table
      idx <- grep("^s\\(log_recl\\)|^te\\(log_recl", rownames(st))
      if (!length(idx)) NA_real_ else unname(st[idx[1], "p-value"])
    }),
    p_log_recl_frac = suppressWarnings({
      st <- summary(model_obj)$s.table
      idx <- grep("^s\\(log_recl_frac\\)", rownames(st))
      if (!length(idx)) NA_real_ else unname(st[idx[1], "p-value"])
    }),
    p_anthro_pc1 = suppressWarnings({
      st <- summary(model_obj)$s.table
      idx <- grep("anthro_pc1", rownames(st))
      if (!length(idx)) NA_real_ else unname(st[idx[1], "p-value"])
    }),
    p_soil_pc1 = suppressWarnings({
      st <- summary(model_obj)$s.table
      idx <- grep("soil_pc1", rownames(st))
      if (!length(idx)) NA_real_ else unname(st[idx[1], "p-value"])
    }),
    cc_obs_max = cc$cc_obs_max,
    cc_est_max = cc$cc_est_max,
    cc_obs_log_recl = cc$cc_obs_log_recl,
    cc_obs_log_recl_frac = cc$cc_obs_log_recl_frac,
    cc_obs_log_city_area = cc$cc_obs_log_city_area,
    cc_obs_te = cc$cc_obs_te,
    stringsAsFactors = FALSE
  )
}

make_gc_correlogram <- function(values, coords,
                                n_bins = 8,
                                max_dist_quantile = 0.60,
                                min_pairs = 30,
                                n_perm = 199,
                                seed = 123) {
  ok <- is.finite(values) &
    is.finite(coords[, 1]) &
    is.finite(coords[, 2])
  values <- values[ok]
  coords <- coords[ok, , drop = FALSE]

  n <- length(values)
  if (n < 10) stop("Too few observations for a correlogram.")

  z <- as.numeric(scale(values, center = TRUE, scale = FALSE))
  denom <- sum(z^2)
  if (!is.finite(denom) || denom == 0) stop("Variance is zero; correlogram cannot be computed.")

  dist_km <- geosphere::distm(coords, fun = geosphere::distHaversine) / 1000
  upper_id <- which(upper.tri(dist_km), arr.ind = TRUE)
  d_vec <- dist_km[upper.tri(dist_km)]
  max_dist_km <- as.numeric(quantile(d_vec, probs = max_dist_quantile, na.rm = TRUE))
  lag_breaks <- seq(0, max_dist_km, length.out = n_bins + 1)

  row_id <- upper_id[, 1]
  col_id <- upper_id[, 2]
  pair_prod <- z[row_id] * z[col_id]

  out <- data.frame(
    lag_lower_km = lag_breaks[-length(lag_breaks)],
    lag_upper_km = lag_breaks[-1],
    lag_mid_km = (lag_breaks[-length(lag_breaks)] + lag_breaks[-1]) / 2,
    MoranI = NA_real_,
    p_value = NA_real_,
    n_pairs = NA_integer_
  )

  set.seed(seed)
  for (i in seq_len(nrow(out))) {
    band <- d_vec > out$lag_lower_km[i] & d_vec <= out$lag_upper_km[i]
    pair_count <- sum(band, na.rm = TRUE)
    out$n_pairs[i] <- pair_count
    if (pair_count < min_pairs) next

    obs_i <- (n / pair_count) * sum(pair_prod[band], na.rm = TRUE) / denom
    out$MoranI[i] <- obs_i

    perm_i <- replicate(n_perm, {
      z_perm <- sample(z, replace = FALSE)
      (n / pair_count) *
        sum((z_perm[row_id] * z_perm[col_id])[band], na.rm = TRUE) /
        sum(z_perm^2)
    })
    out$p_value[i] <- (sum(abs(perm_i) >= abs(obs_i)) + 1) / (n_perm + 1)
  }

  out
}

# =============================================================================
# STEP 1. SCALE-DEPENDENT RECLAMATION EFFECTS
# =============================================================================

m_scale_A <- gam(
  risk ~
    s(log_recl, k = 5) +
    s(log_recl_frac, k = 5) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

summary(m_scale_A)
AIC(m_scale_A)
k.check(m_scale_A)
cc_scale_A <- concurvity(m_scale_A, full = TRUE)
print(cc_scale_A)
extract_pvals(m_scale_A, "m_scale_A (dual__no_marine main model)")

p_A_all <- draw(m_scale_A, residuals = TRUE) &
  theme_classic(base_size = 11)
save_plot("step1A_all_smooths.png", p_A_all, width = 14, height = 10, dpi = 300)

sm_abs <- smooth_estimates(m_scale_A, select = "s(log_recl)") |>
  add_confint()
sm_rel <- smooth_estimates(m_scale_A, select = "s(log_recl_frac)") |>
  add_confint()

p_A_abs <- ggplot(sm_abs, aes(x = log_recl, y = .estimate)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = "#F48892", alpha = 0.25) +
  geom_line(colour = "#b84550", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    x = "Reclamation area (log-transformed)",
    y = "Partial effect on invasion risk",
    title = "s(log_recl)",
    subtitle = "Absolute reclamation extent"
  ) +
  theme_classic(base_size = 16)

p_A_rel <- ggplot(sm_rel, aes(x = log_recl_frac, y = .estimate)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = "#91CAE8", alpha = 0.25) +
  geom_line(colour = "#347aa0", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    x = "Reclamation fraction (log-transformed)",
    y = "Partial effect on invasion risk",
    title = "s(log_recl_frac)",
    subtitle = "Relative reclamation intensity"
  ) +
  theme_classic(base_size = 16)

p_A_core <- p_A_abs | p_A_rel
p_A_core
save_plot("step1A_scale_dependency_core.png", p_A_core, width = 10, height = 5, dpi = 400)

npg_main_cols <- c(	'#94B5D8FC', '#ACD2C7FC', '#D57B70FC' )

summary(m_scale_B)
AIC(m_scale_A, m_scale_B)
k.check(m_scale_B)
cc_scale_B <- concurvity(m_scale_B, full = TRUE)
print(cc_scale_B)
extract_pvals(m_scale_B, "m_scale_B (supplementary te interaction)")

q_small <- unname(quantile(city_df$area_km2, 0.10, na.rm = TRUE))
q_large <- unname(quantile(city_df$area_km2, 0.95, na.rm = TRUE))

pred_B <- ggpredict(
  m_scale_B,
  terms = c(
    "log_recl [all]",
    paste0("area_km2 [", paste(round(c(q_small, q_large), 2), collapse = ", "), "]")
  )
)

probs_test <- seq(0.05, 0.95, by = 0.05)
slope_scan <- data.frame(
  quantile_prob = probs_test,
  q_val = NA_real_,
  slope = NA_real_
)
for (i in seq_along(probs_test)) {
  q_val <- unname(quantile(city_df$area_km2, probs_test[i], na.rm = TRUE))
  pred_i <- ggpredict(
    m_scale_B,
    terms = c("log_recl [all]", paste0("area_km2[", round(q_val, 2), "]"))
  )
  pred_i <- as.data.frame(pred_i)
  slope_scan$q_val[i] <- q_val
  slope_scan$slope[i] <- pred_i$predicted[which.max(pred_i$x)] - pred_i$predicted[which.min(pred_i$x)]
}
slope_scan$sign <- ifelse(slope_scan$slope > 0, "positive", "negative")
slope_scan
write.csv(slope_scan, file.path(output_dir, "step1B_slope_scan.csv"), row.names = FALSE)

pred_B_df <- as.data.frame(pred_B)
pred_B_df$city_size <- factor(
  pred_B_df$group,
  levels = unique(pred_B_df$group),
  labels = c("Small city (P10 log area)", "Large city (P95 log area)")
)

p_B_cond <- ggplot(pred_B_df, aes(x = x, y = predicted, colour = city_size, fill = city_size)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.16, colour = NA) +
  geom_line(linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("#050507", "#1399B2")) +
  scale_fill_manual(values = c("#050507", "#1399B2")) +
  labs(
    x = "Reclamation area (log-transformed)",
    y = "Predicted invasion risk",
    title = "Step 1B: te(log_recl, log_city_area)",
    subtitle = paste(
      "Conditional predictions at low and high log city area.",
      "This is the supplementary interaction check retained after model screening.",
      sep = "\n"
    ),
    colour = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "top")
p_B_cond
save_plot("step1B_conditional_te_effect.png", p_B_cond, width = 8, height = 5, dpi = 400)

te_name <- find_smooth(m_scale_B, "^te\\(")
p_B_draw <- draw(m_scale_B, select = te_name, n = 80, contour = TRUE) +
  labs(
    title = "Tensor product smooth: te(log_recl, log_city_area)",
    subtitle = "Fill colour shows the partial effect on invasion risk",
    x = "log(Reclamation area + 1)",
    y = "log(City area + 1)"
  ) +
  theme_classic(base_size = 12)
p_B_draw 
save_plot("step1B_te_heatmap.png", p_B_draw, width = 6, height = 5, dpi = 300)

city_df$city_class <- factor(
  ifelse(city_df$area_km2 <= median(city_df$area_km2, na.rm = TRUE), "Small", "Large"),
  levels = c("Small", "Large")
)

m_scale_C <- gam(
  risk ~
    city_class +
    s(log_recl, by = city_class, k = 4) +
    s(log_recl_frac, by = city_class, k = 4) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

summary(m_scale_C)
AIC(m_scale_A, m_scale_B, m_scale_C)
cc_scale_C <- concurvity(m_scale_C, full = TRUE)
print(cc_scale_C)
extract_pvals(m_scale_C, "m_scale_C (median split by-smooths)")

sm_names_C <- gratia::smooths(m_scale_C)
sm_C <- bind_rows(
  get_sm(m_scale_C, grep("^s\\(log_recl\\).*Small", sm_names_C, value = TRUE)[1], "log_recl", "Small", "log(Absolute area)"),
  get_sm(m_scale_C, grep("^s\\(log_recl\\).*Large", sm_names_C, value = TRUE)[1], "log_recl", "Large", "log(Absolute area)"),
  get_sm(m_scale_C, grep("^s\\(log_recl_frac\\).*Small", sm_names_C, value = TRUE)[1], "log_recl_frac", "Small", "Log relative ratio"),
  get_sm(m_scale_C, grep("^s\\(log_recl_frac\\).*Large", sm_names_C, value = TRUE)[1], "log_recl_frac", "Large", "Log relative ratio")
)

p_C <- ggplot(sm_C, aes(x = x_val, y = .estimate, colour = model_label, fill = model_label)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_grid(var_label ~ model_label, scales = "free") +
  scale_colour_manual(values = c("Small" = "#1b7837", "Large" = "#4393c3")) +
  scale_fill_manual(values = c("Small" = "#1b7837", "Large" = "#4393c3")) +
  labs(
    x = "Predictor value",
    y = "Partial effect on invasion risk",
    title = "Step 1C: Median-split group-specific smooths",
    subtitle = "Separate smooths for smaller and larger cities",
    colour = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")
save_plot("step1C_median_split_bygroup.png", p_C, width = 10, height = 6, dpi = 300)

# =============================================================================
# STEP 2. COMPETITIVE MEDIATION / SUPPRESSION
# =============================================================================

m1_total <- gam(
  risk ~
    s(log_recl, k = 5) +
    s(log_recl_frac, k = 5) +
    s(climate_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m2a_SE <- gam(
  anthro_pc1 ~
    s(log_recl, k = 5) +
    s(log_recl_frac, k = 5) +
    s(climate_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m2b_soil <- gam(
  soil_pc1 ~
    s(log_recl, k = 5) +
    s(log_recl_frac, k = 5) +
    s(climate_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m3_direct <- gam(
  risk ~
    s(log_recl, k = 5) +
    s(log_recl_frac, k = 5) +
    s(anthro_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

summary(m1_total)
summary(m2a_SE)
summary(m2b_soil)
summary(m3_direct)
AIC(m1_total, m2a_SE, m2b_soil, m3_direct)
print(concurvity(m1_total, full = TRUE))
print(concurvity(m2a_SE, full = TRUE))
print(concurvity(m2b_soil, full = TRUE))
print(concurvity(m3_direct, full = TRUE))
extract_pvals(m1_total, "m1_total")
extract_pvals(m2a_SE, "m2a_SE")
extract_pvals(m2b_soil, "m2b_soil")
extract_pvals(m3_direct, "m3_direct")

sm_abs_m1 <- get_sm(m1_total, "s(log_recl)", "log_recl", "Model 1: Total effect", "log(Absolute area)")
sm_abs_m3 <- get_sm(m3_direct, "s(log_recl)", "log_recl", "Model 3: Direct effect", "log(Absolute area)")
sm_frc_m1 <- get_sm(m1_total, "s(log_recl_frac)", "log_recl_frac", "Model 1: Total effect", "Log relative ratio")
sm_frc_m3 <- get_sm(m3_direct, "s(log_recl_frac)", "log_recl_frac", "Model 3: Direct effect", "Log relative ratio")

sm_grid <- bind_rows(sm_abs_m1, sm_abs_m3, sm_frc_m1, sm_frc_m3)
sm_grid$model_label <- factor(sm_grid$model_label, levels = c("Model 1: Total effect", "Model 3: Direct effect"))
sm_grid$var_label <- factor(sm_grid$var_label, levels = c("log(Absolute area)", "Log relative ratio"))

p_4panel <- ggplot(sm_grid, aes(x = x_val, y = .estimate, colour = model_label, fill = model_label)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_grid(var_label ~ model_label, scales = "free") +
  scale_colour_manual(values = c("Model 1: Total effect" = "#636363", "Model 3: Direct effect" = "#d73027")) +
  scale_fill_manual(values = c("Model 1: Total effect" = "#636363", "Model 3: Direct effect" = "#d73027")) +
  labs(
    x = "Predictor value",
    y = "Partial effect on invasion risk",
    title = "Step 2: Total versus direct reclamation effects",
    subtitle = "Comparing m1_total and m3_direct for both reclamation smooths",
    colour = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(legend.position = "top")
save_plot("step2_mediation_4panel.png", p_4panel, width = 11, height = 8, dpi = 300)

make_med_panel <- function(model, med_name, y_label, col_abs, col_frc, path_label) {
  s_abs <- smooth_estimates(model, select = "s(log_recl)") |>
    add_confint()
  s_frc <- smooth_estimates(model, select = "s(log_recl_frac)") |>
    add_confint()

  p_abs <- ggplot(s_abs, aes(x = log_recl, y = .estimate)) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = col_abs, alpha = 0.15) +
    geom_line(colour = col_abs, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    labs(
      x = "log(Reclamation area + 1)",
      y = y_label,
      title = paste0("s(log_recl) -> ", med_name),
      subtitle = "Absolute reclamation extent"
    ) +
    theme_classic(base_size = 11)

  p_frc <- ggplot(s_frc, aes(x = log_recl_frac, y = .estimate)) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = col_frc, alpha = 0.15) +
    geom_line(colour = col_frc, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    labs(
      x = "log(Relative reclamation ratio + 1)",
      y = y_label,
      title = paste0("s(log_recl_frac) -> ", med_name),
      subtitle = "Relative reclamation intensity"
    ) +
    theme_classic(base_size = 11)

  (p_abs | p_frc) +
    plot_annotation(
      title = paste("Step 2 mediator pathway", path_label, ":", med_name),
      subtitle = "Each panel shows how the mediator changes across reclamation gradients"
    )
}

p_med_SE <- make_med_panel(
  m2a_SE,
  med_name = "Socioeconomic expansion (anthro_pc1)",
  y_label = "Partial effect on anthro_pc1",
  col_abs = "#4d9221",
  col_frc = "#1a9641",
  path_label = "A"
)
save_plot("step2_mediatorA_SE_pathways.png", p_med_SE, width = 11, height = 5, dpi = 300)

p_med_soil <- make_med_panel(
  m2b_soil,
  med_name = "Soil modification (soil_pc1)",
  y_label = "Partial effect on soil_pc1",
  col_abs = "#8c510a",
  col_frc = "#bf812d",
  path_label = "B"
)
save_plot("step2_mediatorB_soil_pathways.png", p_med_soil, width = 11, height = 5, dpi = 300)

sm6 <- bind_rows(
  get_sm(m1_total, "s(log_recl)", "log_recl", "1. Total (m1)", "log(Absolute area)"),
  get_sm(m2a_SE, "s(log_recl)", "log_recl", "2a. SE mediator (m2a)", "log(Absolute area)"),
  get_sm(m2b_soil, "s(log_recl)", "log_recl", "2b. Soil mediator (m2b)", "log(Absolute area)"),
  get_sm(m3_direct, "s(log_recl)", "log_recl", "3. Direct (m3)", "log(Absolute area)"),
  get_sm(m1_total, "s(log_recl_frac)", "log_recl_frac", "1. Total (m1)", "Log relative ratio"),
  get_sm(m2a_SE, "s(log_recl_frac)", "log_recl_frac", "2a. SE mediator (m2a)", "Log relative ratio"),
  get_sm(m2b_soil, "s(log_recl_frac)", "log_recl_frac", "2b. Soil mediator (m2b)", "Log relative ratio"),
  get_sm(m3_direct, "s(log_recl_frac)", "log_recl_frac", "3. Direct (m3)", "Log relative ratio")
)

sm6$model_label <- factor(
  sm6$model_label,
  levels = c("1. Total (m1)", "2a. SE mediator (m2a)", "2b. Soil mediator (m2b)", "3. Direct (m3)")
)
sm6$var_label <- factor(sm6$var_label, levels = c("log(Absolute area)", "Log relative ratio"))

p_six <- ggplot(sm6, aes(x = x_val, y = .estimate, colour = model_label, fill = model_label)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_grid(var_label ~ model_label, scales = "free") +
  scale_colour_manual(values = c(
    "1. Total (m1)" = "#636363",
    "2a. SE mediator (m2a)" = "#4d9221",
    "2b. Soil mediator (m2b)" = "#8c510a",
    "3. Direct (m3)" = "#d73027"
  )) +
  scale_fill_manual(values = c(
    "1. Total (m1)" = "#636363",
    "2a. SE mediator (m2a)" = "#4d9221",
    "2b. Soil mediator (m2b)" = "#8c510a",
    "3. Direct (m3)" = "#d73027"
  )) +
  labs(
    x = "Predictor value",
    y = "Partial effect",
    title = "Step 2: Full six-panel mediation decomposition",
    subtitle = "Rows separate absolute extent and relative intensity; columns separate the pathway models",
    colour = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(legend.position = "top")
save_plot("step2_six_panel_decomposition.png", p_six, width = 14, height = 8, dpi = 300)

# =============================================================================
# SUPPLEMENTARY ROBUSTNESS AND MODEL TABLES
# =============================================================================

m_null_area <- gam(
  risk ~
    s(log_city_area, k = 5) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m_control_area <- gam(
  risk ~
    s(log_recl, k = 5) +
    s(log_recl_frac, k = 5) +
    s(area_km2, k = 5) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m_control_logarea <- gam(
  risk ~
    s(log_recl, k = 5) +
    s(log_recl_frac, k = 5) +
    s(log_city_area, k = 5) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m_frac_only <- gam(
  risk ~
    s(log_recl_frac, k = 5) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m_frac_raw <- gam(
  risk ~
    s(recl_frac, k = 5) +
    s(log_city_area, k = 5) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

lmm_check <- lmer(
  risk ~
    log_recl + log_recl_frac +
    anthro_pc1 + climate_pc1 + soil_pc1 + geom_pc1 +
    (1 | Regions),
  data = city_df,
  REML = TRUE
)

cat("\n===== Robustness AIC checks =====\n")
print(AIC(m_control_logarea, m_control_area, m_scale_A))
print(AIC(m_frac_only, m_scale_A, m_scale_B))
print(AIC(m_frac_raw, m_frac_only, m_scale_A, m_scale_B))
print(AIC(lmm_check, m_scale_A))
summary(m_frac_raw)
summary(lmm_check)

supp_table <- do.call(rbind, list(
  make_row(
    "A: Step1 scale-dependence", "m_scale_A",
    "s(log_recl) + s(log_recl_frac)", "risk", m_scale_A,
    note = "Chosen main model after 060506 screening; dual__no_marine specification",
    selected = TRUE
  ),
  make_row(
    "A: Step1 scale-dependence", "m_scale_B",
    "te(log_recl, log_city_area)", "risk", m_scale_B,
    note = "Supplementary interaction check using the log city-area axis"
  ),
  make_row(
    "A: Step1 scale-dependence", "m_scale_C",
    "group-specific smooths by city_class", "risk", m_scale_C,
    note = "Median split display of small versus large city smooths"
  ),
  make_row(
    "A: Step1 scale-dependence", "m_frac_only",
    "s(log_recl_frac) only", "risk", m_frac_only,
    note = "Single-predictor relative intensity benchmark"
  ),
  make_row(
    "A: Step1 scale-dependence", "m_null_area",
    "s(log_city_area) only", "risk", m_null_area,
    note = "No reclamation term; city-size baseline"
  ),
  make_row(
    "A: Step1 scale-dependence", "m_control_area",
    "dual reclamation smooths + s(area_km2)", "risk", m_control_area,
    note = "Raw-area robustness check"
  ),
  make_row(
    "A: Step1 scale-dependence", "m_control_logarea",
    "dual reclamation smooths + s(log_city_area)", "risk", m_control_logarea,
    note = "Log-area over-control check"
  ),
  make_row(
    "B: Step2 mediation", "m1_total",
    "dual reclamation smooths", "risk", m1_total,
    note = "Total effect without mediators"
  ),
  make_row(
    "B: Step2 mediation", "m2a_SE",
    "dual reclamation smooths", "anthro_pc1", m2a_SE,
    note = "Pathway A: reclamation to socioeconomic expansion"
  ),
  make_row(
    "B: Step2 mediation", "m2b_soil",
    "dual reclamation smooths", "soil_pc1", m2b_soil,
    note = "Pathway B: reclamation to soil modification"
  ),
  make_row(
    "B: Step2 mediation", "m3_direct",
    "dual reclamation smooths + mediator smooths", "risk", m3_direct,
    note = "Direct effect after anthro_pc1 and soil_pc1 adjustment"
  )
))
supp_table$dAIC_vs_null <- round(supp_table$AIC - AIC(m_null_area), 2)
write.csv(supp_table, file.path(output_dir, "SuppTable_GAMM_model_comparison.csv"), row.names = FALSE)

diag_table <- bind_rows(
  summarize_gam_diag("m_scale_A", "risk", m_scale_A),
  summarize_gam_diag("m_scale_B", "risk", m_scale_B),
  summarize_gam_diag("m_scale_C", "risk", m_scale_C),
  summarize_gam_diag("m_null_area", "risk", m_null_area),
  summarize_gam_diag("m_control_area", "risk", m_control_area),
  summarize_gam_diag("m_control_logarea", "risk", m_control_logarea),
  summarize_gam_diag("m_frac_only", "risk", m_frac_only),
  summarize_gam_diag("m_frac_raw", "risk", m_frac_raw),
  summarize_gam_diag("m1_total", "risk", m1_total),
  summarize_gam_diag("m2a_SE", "anthro_pc1", m2a_SE),
  summarize_gam_diag("m2b_soil", "soil_pc1", m2b_soil),
  summarize_gam_diag("m3_direct", "risk", m3_direct)
)
write.csv(diag_table, file.path(output_dir, "ModelDiagnostics_060503.csv"), row.names = FALSE)

cat("\n===== Supplementary model table =====\n")
print(supp_table[, c("Section", "Model", "Adj_R2", "Dev_pct", "AIC", "dAIC_vs_null", "p_log_recl", "p_log_recl_frac", "Note")], row.names = FALSE)

cat("\n===== GAM diagnostic summary =====\n")
print(diag_table[, c("model", "AIC", "adj_r2", "dev_exp", "p_log_recl", "p_log_recl_frac", "cc_obs_max", "cc_obs_log_recl", "cc_obs_log_recl_frac", "cc_obs_log_city_area", "cc_obs_te")], row.names = FALSE)

# =============================================================================
# STEP 3. GREAT-CIRCLE MORAN CORRELOGRAM
# =============================================================================

coords_all <- as.matrix(city_df[, c("Longitude", "Latitude")])
obs_idx_A <- as.integer(rownames(m_scale_A$model))
obs_idx_m3 <- as.integer(rownames(m3_direct$model))

coords_A <- coords_all[obs_idx_A, , drop = FALSE]
coords_m3 <- coords_all[obs_idx_m3, , drop = FALSE]
risk_A <- city_df$risk[obs_idx_A]
resid_A <- residuals(m_scale_A, type = "deviance")
resid_m3 <- residuals(m3_direct, type = "deviance")

stopifnot(
  nrow(coords_A) == length(risk_A),
  nrow(coords_A) == length(resid_A),
  nrow(coords_m3) == length(resid_m3)
)

cor_risk <- make_gc_correlogram(
  values = risk_A,
  coords = coords_A,
  n_bins = 8,
  max_dist_quantile = 0.60,
  min_pairs = 30,
  n_perm = 199,
  seed = 123
)
cor_risk$series <- "Observed invasion risk"

cor_A_gc <- make_gc_correlogram(
  values = resid_A,
  coords = coords_A,
  n_bins = 8,
  max_dist_quantile = 0.60,
  min_pairs = 30,
  n_perm = 199,
  seed = 123
)
cor_A_gc$series <- "Residuals of m_scale_A"

cor_m3_gc <- make_gc_correlogram(
  values = resid_m3,
  coords = coords_m3,
  n_bins = 8,
  max_dist_quantile = 0.60,
  min_pairs = 30,
  n_perm = 199,
  seed = 123
)
cor_m3_gc$series <- "Residuals of m3_direct"

cor_all_gc <- bind_rows(cor_risk, cor_A_gc, cor_m3_gc) |>
  mutate(sig_flag = ifelse(!is.na(p_value) & p_value < 0.05, "p < 0.05", "n.s."))

write.csv(cor_all_gc, file.path(output_dir, "step3_gc_moran_correlogram.csv"), row.names = FALSE)

p_correlogram <- ggplot(cor_all_gc, aes(x = lag_mid_km, y = MoranI)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(colour = "#2f4858", linewidth = 0.9, na.rm = TRUE) +
  geom_point(
    aes(fill = sig_flag),
    shape = 21,
    size = 3.2,
    colour = "grey25",
    stroke = 0.7,
    na.rm = TRUE
  ) +
  facet_wrap(~ series, ncol = 3) +
  scale_fill_manual(values = c("p < 0.05" = "#d73027", "n.s." = "white"), name = "Permutation test") +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = "Great-circle distance lag (km)",
    y = "Moran's I",
    title = "Step 3: Great-circle correlogram for global city clustering",
    subtitle = paste(
      "Left panel tests clustering in invasion risk itself.",
      "Middle and right panels test whether the fitted GAMs leave residual clustering.",
      "Red points indicate permutation p < 0.05.",
      sep = "\n"
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
save_plot("step3_gc_moran_correlogram.png", p_correlogram, width = 12, height = 4.6, dpi = 400)

cat("\n===== Great-circle Moran correlogram =====\n")
print(cor_all_gc[, c("series", "lag_mid_km", "MoranI", "p_value", "n_pairs")], row.names = FALSE)
