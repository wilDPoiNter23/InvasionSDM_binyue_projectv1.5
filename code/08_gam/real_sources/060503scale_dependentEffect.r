# Original source: 060503scale_dependentEffect.r
# Original source location: <LOCAL_CODE_ROOT>/060503scale_dependentEffect.r
# Original source SHA-256: 1672893a1169c4d059e6647794041243a30e4566b5d825029dde86f917a5dd79
# Public-safe original snapshot: archive/original_sources/local/060503scale_dependentEffect.r
# Classification: CANONICAL
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
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

workdir <- Sys.getenv("GAM_WORKDIR", unset = publication_path("LOCAL_ANALYSIS_ROOT", "figure/gam_draft"))
input_csv <- Sys.getenv("GAM_INPUT_CSV", unset = file.path(workdir, "gamtest_withpca.csv"))
output_dir <- Sys.getenv("GAM_OUTPUT_DIR", unset = file.path(workdir, "gamFinalResult"))
setwd(workdir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

city_df <- read.csv(input_csv)
city_df$log_city_area <- log1p(city_df$area_km2)
city_df$log_recl <- log1p(city_df$reclamation_area)
city_df$recl_frac <- city_df$reclamation_area / pmax(city_df$area_km2, .Machine$double.eps)
city_df$log_recl_frac <- log1p(city_df$recl_frac)
city_df$risk_class <- as.factor(city_df$risk_class)
city_df$Regions <- as.factor(city_df$Regions)

stopifnot(all(c(
  "risk", "reclamation_area", "area_km2", "log_recl", "log_recl_frac",
  "log_city_area", "anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1", "geom_pc1",
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

safe_write_csv <- function(object, filename) {
  tryCatch(
    write.csv(object, file.path(output_dir, filename), row.names = FALSE),
    error = function(e) {
      warning(sprintf("Could not write %s: %s", filename, conditionMessage(e)), call. = FALSE)
      invisible(NULL)
    }
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
    cc_obs_te = dplyr::coalesce(
      get_term("te(log_recl,log_city_area)", observed_row),
      get_term("te(log_recl,area_km2)", observed_row)
    ),
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
    y = "Partial effect on invasion pressure",
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
    y = "Partial effect on invasion pressure",
    title = "s(log_recl_frac)",
    subtitle = "Relative reclamation intensity"
  ) +
  theme_classic(base_size = 16)

p_A_core <- p_A_abs | p_A_rel
save_plot("step1A_scale_dependency_core.png", p_A_core, width = 10, height = 5, dpi = 400)

m_scale_B <- gam(
  risk ~
    te(log_recl, area_km2, k = c(5, 5)) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5)+
    s(marine_pc1, k=5),
  data = city_df,
  method = "REML"
)


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
    terms = c("log_recl [all]", paste0("area_km2 [", round(q_val, 2), "]"))
  )
  pred_i <- as.data.frame(pred_i)
  slope_scan$q_val[i] <- q_val
  slope_scan$slope[i] <- pred_i$predicted[which.max(pred_i$x)] - pred_i$predicted[which.min(pred_i$x)]
}
slope_scan$sign <- ifelse(slope_scan$slope > 0, "positive", "negative")
safe_write_csv(slope_scan, "step1B_slope_scan.csv")

pred_B_df <- as.data.frame(pred_B)
pred_B_df$city_size <- factor(
  pred_B_df$group,
  levels = unique(pred_B_df$group),
  labels = c("Small city (P10 area)", "Large city (P95 area)")
)

p_B_cond <- ggplot(pred_B_df, aes(x = x, y = predicted, colour = city_size, fill = city_size)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.16, colour = NA) +
  geom_line(linewidth = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("#050507", "#1399B2")) +
  scale_fill_manual(values = c("#050507", "#1399B2")) +
  labs(
    x = "Reclamation area (log-transformed)",
    y = "Predicted invasion pressure",
    title = "Step 1B: te(log_recl, area_km2)",
    subtitle = paste(
      "Conditional predictions at low and high city administrative area.",
      "This is the supplementary interaction check retained after model screening.",
      sep = "\n"
    ),
    colour = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "top")
save_plot("step1B_conditional_te_effect.png", p_B_cond, width = 8, height = 5, dpi = 400)

te_name <- find_smooth(m_scale_B, "^te\\(")
p_B_draw <- draw(m_scale_B, select = te_name, n = 80, contour = TRUE) +
  labs(
    title = "Tensor product smooth: te(log_recl, area_km2)",
    subtitle = "Fill colour shows the partial effect on invasion risk",
    x = "log(Reclamation area + 1)",
    y = "City administrative area (km²)"
  ) +
  theme_classic(base_size = 12)
save_plot("step1B_te_heatmap.png", p_B_draw, width = 6, height = 5, dpi = 300)
p_B_draw <- p_B_draw +
  labs(
    subtitle = "Fill colour shows the partial effect on invasion pressure",
    y = "City administrative area (km^2)"
  )
save_plot("step1B_te_heatmap.png", p_B_draw, width = 6, height = 5, dpi = 300)

plot_partial_term <- function(model, smooth_pattern, x_var, x_lab, panel_title) {
  smooth_name <- find_smooth(model, smooth_pattern)
  sm_df <- get_sm(model, smooth_name, x_var, "m_scale_B", x_lab)
  mf <- model.frame(model)
  term_fit <- predict(model, type = "terms")
  pr_df <- data.frame(
    x_val = mf[[x_var]],
    partial_resid = term_fit[, smooth_name] + residuals(model, type = "response")
  ) |>
    filter(is.finite(x_val), is.finite(partial_resid))

  ggplot(sm_df, aes(x = x_val, y = .estimate)) +
    geom_point(
      data = pr_df,
      aes(x = x_val, y = partial_resid),
      inherit.aes = FALSE,
      size = 0.9,
      alpha = 0.28,
      colour = "grey45"
    ) +
    geom_ribbon(
      aes(ymin = .lower_ci, ymax = .upper_ci),
      fill = "#d7dee8",
      alpha = 0.8,
      colour = NA
    ) +
    geom_line(colour = "#1f4e79", linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.4) +
    geom_rug(
      data = pr_df,
      aes(x = x_val, y = NULL),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.10,
      colour = "grey55"
    ) +
    labs(
      x = x_lab,
      y = "Partial effect on invasion pressure",
      title = panel_title
    ) +
    theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      axis.title = element_text(size = 10)
    )
}

te_surface_terms <- smooth_estimates(m_scale_B, select = te_name, n = 80)
te_surface_terms$reclamation_area_km2 <- ((exp(te_surface_terms$log_recl) - 1) / 100)
te_surface_terms <- te_surface_terms |>
  filter(is.finite(reclamation_area_km2), reclamation_area_km2 > 0, is.finite(area_km2), area_km2 > 0)
obs_surface_pts <- model.frame(m_scale_B) |>
  mutate(
    reclamation_area_km2 = ((exp(log_recl) - 1) / 100)
  ) |>
  filter(is.finite(reclamation_area_km2), reclamation_area_km2 > 0, is.finite(area_km2), area_km2 > 0)

p_B_te_terms <- ggplot(
  te_surface_terms,
  aes(x = reclamation_area_km2, y = area_km2, z = .estimate)
) +
  geom_contour_filled(bins = 11) +
  geom_contour(colour = "white", linewidth = 0.25, alpha = 0.55, bins = 11) +
  geom_point(
    data = obs_surface_pts,
    aes(x = reclamation_area_km2, y = area_km2),
    inherit.aes = FALSE,
    shape = 16,
    size = 0.7,
    alpha = 0.28,
    colour = "grey15"
  ) +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  scale_y_log10(labels = scales::label_number(big.mark = ",")) +
  scale_fill_manual(
    values = grDevices::colorRampPalette(
      c("#355f8d", "#fbfbf7", "#c56b7b")
    )(11),
    name = "Partial\neffect"
  ) +
  labs(
    x = "Reclamation extent (km^2)",
    y = "City area (km^2)",
    title = "Scale-dependent reclamation surface"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "right"
  )

p_B_anthro <- plot_partial_term(
  m_scale_B,
  "^s\\(anthro_pc1\\)$",
  "anthro_pc1",
  "Socioeconomic development gradient",
  "Socioeconomic development"
)

p_B_climate <- plot_partial_term(
  m_scale_B,
  "^s\\(climate_pc1\\)$",
  "climate_pc1",
  "Climatic conditions gradient",
  "Climate"
)

p_B_soil <- plot_partial_term(
  m_scale_B,
  "^s\\(soil_pc1\\)$",
  "soil_pc1",
  "Soil modification gradient",
  "Soil conditions"
)

p_B_marine <- plot_partial_term(
  m_scale_B,
  "^s\\(marine_pc1\\)$",
  "marine_pc1",
  "Marine influence gradient",
  "Marine context"
)

p_B_all_terms <- wrap_plots(
  list(p_B_te_terms, p_B_anthro, p_B_climate, p_B_soil, p_B_marine),
  ncol = 2,
  guides = "collect"
) +
  plot_annotation(
    title = "Supplementary partial-effect view of the scale-dependent GAM",
    subtitle = paste(
      "Panel a shows the reclamation-by-city-size tensor surface; panels b-e show",
      "the partial smooths for pathway-relevant covariates."
    ),
    tag_levels = "a",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "grey35"),
      plot.tag = element_text(face = "bold", size = 13),
      plot.tag.position = c(0.02, 0.98)
    )
  ) &
  theme(
    legend.position = "right"
  )
save_plot("step1B_all_terms.png", p_B_all_terms, width = 12, height = 9, dpi = 300)
ggsave(
  filename = file.path(output_dir, "step1B_all_terms.jpg"),
  plot = p_B_all_terms,
  width = 12,
  height = 9,
  dpi = 300,
  device = grDevices::jpeg
)
ggsave(
  filename = file.path(output_dir, "step1B_all_terms.pdf"),
  plot = p_B_all_terms,
  width = 12,
  height = 9,
  device = grDevices::pdf
)

p_B_diag <- appraise(m_scale_B) &
  theme_classic(base_size = 11) &
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.tag = element_text(face = "bold", size = 13),
    plot.tag.position = c(0.02, 0.98)
  )
p_B_diag <- p_B_diag +
  plot_annotation(
    title = "Diagnostic checks for m_scale_B",
    subtitle = "Residual distribution, QQ pattern and fitted-residual structure for the scale-dependent GAM.",
    tag_levels = "a",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "grey35")
    )
  )
save_plot("step1B_diagnostics.png", p_B_diag, width = 11, height = 8, dpi = 300)
ggsave(
  filename = file.path(output_dir, "step1B_diagnostics.jpg"),
  plot = p_B_diag,
  width = 11,
  height = 8,
  dpi = 300,
  device = grDevices::jpeg
)
ggsave(
  filename = file.path(output_dir, "step1B_diagnostics.pdf"),
  plot = p_B_diag,
  width = 11,
  height = 8,
  device = grDevices::pdf
)

# Exploratory raw-area interaction for conditional peak detection.
# This keeps area_km2 on the original scale so peaks can be translated into
# reclamation-area thresholds with a direct policy interpretation.
m_peak_area <- gam(
  risk ~
    te(log_recl, area_km2, k = c(5, 5)) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(marine_pc1, k = 5),
  data = city_df,
  method = "REML"
)

summary(m_peak_area)
cc_peak_area <- concurvity(m_peak_area, full = TRUE)
print(cc_peak_area)
extract_pvals(m_peak_area, "m_peak_area (exploratory raw-area peak scan)")

area_probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
area_levels <- unname(quantile(city_df$area_km2, probs = area_probs, na.rm = TRUE))
area_labels <- sprintf("Area P%02.0f (%.0f km^2)", area_probs * 100, area_levels)
log_recl_grid <- seq(
  min(city_df$log_recl, na.rm = TRUE),
  max(city_df$log_recl, na.rm = TRUE),
  length.out = 400
)

curve_df <- bind_rows(lapply(seq_along(area_levels), function(i) {
  data.frame(
    log_recl = log_recl_grid,
    area_km2 = area_levels[i],
    anthro_pc1 = median(city_df$anthro_pc1, na.rm = TRUE),
    climate_pc1 = median(city_df$climate_pc1, na.rm = TRUE),
    soil_pc1 = median(city_df$soil_pc1, na.rm = TRUE),
    marine_pc1 = median(city_df$marine_pc1, na.rm = TRUE),
    area_prob = area_probs[i],
    area_label = area_labels[i],
    grid_id = seq_along(log_recl_grid)
  )
}))

peak_pred <- predict(m_peak_area, newdata = curve_df, type = "response", se.fit = TRUE)
curve_df$predicted <- as.numeric(peak_pred$fit)
curve_df$se <- as.numeric(peak_pred$se.fit)
curve_df$conf.low <- curve_df$predicted - 1.96 * curve_df$se
curve_df$conf.high <- curve_df$predicted + 1.96 * curve_df$se
curve_df$reclamation_area_km2 <- (exp(curve_df$log_recl) - 1) / 100

peak_table <- curve_df |>
  group_by(area_prob, area_label, area_km2) |>
  summarise(
    peak_row = which.max(predicted),
    peak_log_recl = log_recl[peak_row],
    peak_reclamation_area_km2 = reclamation_area_km2[peak_row],
    peak_risk = predicted[peak_row],
    peak_conf_low = conf.low[peak_row],
    peak_conf_high = conf.high[peak_row],
    peak_type = ifelse(
      peak_row == 1,
      "lower boundary",
      ifelse(peak_row == dplyr::n(), "upper boundary", "interior peak")
    ),
    .groups = "drop"
  )

safe_write_csv(curve_df, "step1B_area_peak_curves.csv")
safe_write_csv(peak_table, "step1B_area_conditional_peaks.csv")

p_peak_area <- ggplot(
  curve_df,
  aes(x = reclamation_area_km2, y = predicted, colour = area_label, fill = area_label)
) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(
    data = peak_table,
    aes(x = peak_reclamation_area_km2, y = peak_risk),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.8,
    stroke = 0.7,
    fill = "white",
    colour = "black"
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = "Reclamation area at prediction grid (km^2)",
    y = "Predicted invasion pressure",
    title = "Conditional fitted curves and peak locations from te(log_recl, area_km2)",
    subtitle = paste(
      "Each curve fixes city administrative area at one quantile of area_km2.",
      "White points mark the maximum predicted pressure along that curve.",
      "Boundary peaks indicate no interior threshold within the observed log_recl range.",
      sep = "\n"
    ),
    colour = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")
save_plot("step1B_area_conditional_peaks.png", p_peak_area, width = 10, height = 6, dpi = 350)

cat("\n===== Conditional peaks from m_peak_area =====\n")
print(peak_table)

# Main-figure candidate for the raw-area interaction model.
main_area_probs <- c(0.10, 0.50, 0.90)
curve_df_main <- curve_df |>
  filter(area_prob %in% main_area_probs)
peak_table_main <- peak_table |>
  filter(area_prob %in% main_area_probs)
curve_df_main$area_label <- factor(curve_df_main$area_label, levels = peak_table_main$area_label)
peak_table_main$area_label <- factor(peak_table_main$area_label, levels = peak_table_main$area_label)
npg_main_cols <- c(	'#94B5D8FC', '#ACD2C7FC', '#D57B70FC' )

names(npg_main_cols) <- levels(curve_df_main$area_label)

min_recl_actual_hm2 <- min(exp(curve_df$log_recl) - 1, na.rm = TRUE)
max_recl_actual_hm2 <- max(exp(curve_df$log_recl) - 1, na.rm = TRUE)
min_area_actual <- min(city_df$area_km2[city_df$area_km2 > 0], na.rm = TRUE)
max_area_actual <- max(city_df$area_km2, na.rm = TRUE)

surface_grid <- expand.grid(
  reclamation_area_hm2 = exp(seq(log(min_recl_actual_hm2 + 1), log(max_recl_actual_hm2 + 1), length.out = 140)) - 1,
  area_km2 = exp(seq(log(min_area_actual), log(max_area_actual), length.out = 120))
)
surface_grid$reclamation_area_km2 <- surface_grid$reclamation_area_hm2 / 100
surface_grid$log_recl <- log1p(surface_grid$reclamation_area_hm2)
surface_grid$anthro_pc1 <- median(city_df$anthro_pc1, na.rm = TRUE)
surface_grid$climate_pc1 <- median(city_df$climate_pc1, na.rm = TRUE)
surface_grid$soil_pc1 <- median(city_df$soil_pc1, na.rm = TRUE)
surface_grid$marine_pc1 <- median(city_df$marine_pc1, na.rm = TRUE)
surface_grid$predicted <- as.numeric(predict(m_peak_area, newdata = surface_grid, type = "response"))
safe_write_csv(surface_grid, "step1B_area_response_surface.csv")

p_B_surface_main <- ggplot(
  surface_grid,
  aes(x = reclamation_area_km2, y = area_km2, z = predicted)
) +
  geom_contour_filled(bins = 11) +
  geom_contour(colour = "white", linewidth = 0.25, alpha = 0.55, bins = 11) +
  geom_point(
    data = peak_table_main,
    aes(x = peak_reclamation_area_km2, y = area_km2),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.6,
    stroke = 0.7,
    fill = "white",
    colour = "black"
  ) +
  scale_fill_manual(
    values = grDevices::colorRampPalette(
      c("#163b7a", "#2f6da3", "#6faed2", "#dceaf2", "#f5edc6", "#e7c65a")
    )(11),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  scale_y_log10(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = "Reclamation area (km^2, log scale)",
    y = "City area (km^2, log scale)",
    title = "Predicted invasion-pressure surface",
    subtitle = "White points mark conditional peak locations for P10, P50 and P90 city-area levels",
    fill = "Predicted\npressure"
  ) +
  theme_classic(base_size = 18) +
  theme(legend.position = "right")

p_B_cond_main <- ggplot(
  curve_df_main,
  aes(x = reclamation_area_km2, y = predicted, colour = area_label, fill = area_label)
) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = peak_table_main,
    aes(x = peak_reclamation_area_km2, y = peak_risk, colour = area_label),
    inherit.aes = FALSE,
    size = 2.4
  ) +
  scale_colour_manual(values = npg_main_cols) +
  scale_fill_manual(values = npg_main_cols) +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = "Reclamation area (km^2, log scale)",
    y = "Predicted invasion pressure",
    title = "Conditional fitted curves",
    subtitle = "P10 and P50 remain negative across the observed range; P90 flips positive and reaches an interior peak",
    colour = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(legend.position = "top")

area_scan_probs <- seq(0.05, 0.95, by = 0.05)
slope_scan_area <- data.frame(
  quantile_prob = area_scan_probs,
  q_val = NA_real_,
  slope = NA_real_
)
for (i in seq_along(area_scan_probs)) {
  q_val <- unname(quantile(city_df$area_km2, area_scan_probs[i], na.rm = TRUE))
  pred_i <- predict(
    m_peak_area,
    newdata = data.frame(
      log_recl = log_recl_grid,
      area_km2 = q_val,
      anthro_pc1 = median(city_df$anthro_pc1, na.rm = TRUE),
      climate_pc1 = median(city_df$climate_pc1, na.rm = TRUE),
      soil_pc1 = median(city_df$soil_pc1, na.rm = TRUE),
      marine_pc1 = median(city_df$marine_pc1, na.rm = TRUE)
    ),
    type = "response"
  )
  slope_scan_area$q_val[i] <- q_val
  slope_scan_area$slope[i] <- pred_i[length(pred_i)] - pred_i[1]
}
slope_scan_area$sign <- ifelse(slope_scan_area$slope > 0, "positive", "negative")
flip_idx_area <- which(diff(sign(slope_scan_area$slope)) != 0)
safe_write_csv(slope_scan_area, "step1B_area_slope_scan.csv")

flip_band <- NULL
flip_text <- "No sign flip across the scanned city-area range."
if (length(flip_idx_area) > 0) {
  flip_band <- data.frame(
    xmin = slope_scan_area$q_val[flip_idx_area[1]],
    xmax = slope_scan_area$q_val[flip_idx_area[1] + 1],
    ymin = -Inf,
    ymax = Inf
  )
  flip_text <- sprintf(
    "Slope flips between %.0f and %.0f km^2 city area.",
    flip_band$xmin[1], flip_band$xmax[1]
  )
}

p_B_slope_main <- ggplot(slope_scan_area, aes(x = q_val, y = slope)) +
  {
    if (!is.null(flip_band)) {
      geom_rect(
        data = flip_band,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "#d9d9d9",
        alpha = 0.5
      )
    }
  } +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_line(colour = "#1f4e79", linewidth = 1.0) +
  geom_point(aes(fill = sign), shape = 21, size = 2.5, colour = "grey20", stroke = 0.6) +
  scale_fill_manual(values = c("negative" = "#f4a582", "positive" = "#92c5de")) +
  scale_x_log10(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = "City area quantile value (km^2, log scale)",
    y = "End-to-end curve slope",
    title = "Slope sign flip by city size",
    subtitle = flip_text,
    fill = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(legend.position = "none")

p_B_main <- (p_B_surface_main | p_B_slope_main) / p_B_cond_main +
  plot_layout(widths = c(1.35, 0.9), heights = c(1, 1.05)) +
  plot_annotation(
    title = "Scale-dependent reclamation effect from te(log_recl, area_km2)",
    subtitle = paste(
      "Top left: predicted invasion-pressure surface under median covariate values.",
      "Bottom: fitted curves for small, median and very large cities.",
      "Top right: the reclamation slope changes sign only among the largest cities.",
      sep = "\n"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, colour = "grey35")
    )
  )
  p_B_main
save_plot("step1B_main_figure_candidate.png", p_B_main, width = 12, height = 9, dpi = 350)
ggsave(
  filename = file.path(output_dir, "step1B_main_figure_candidate.jpg"),
  plot = p_B_main,
  width = 12,
  height = 9,
  dpi = 350,
  device = grDevices::jpeg
)
ggsave(
  filename = file.path(output_dir, "step1B_main_figure_candidate.pdf"),
  plot = p_B_main,
  width = 12,
  height = 9,
  device = grDevices::pdf
)

sm_B <- summary(m_scale_B)
te_row_B <- sm_B$s.table[grep("^te\\(", rownames(sm_B$s.table)), , drop = FALSE]
te_term_B <- rownames(te_row_B)[1]
cc_te_B <- NA_real_
if ("observed" %in% rownames(cc_scale_B) && te_term_B %in% colnames(cc_scale_B)) {
  cc_te_B <- unname(cc_scale_B["observed", te_term_B])
}

scale_effect_summary <- data.frame(
  model = "m_scale_B",
  term = te_term_B,
  edf = unname(te_row_B[1, "edf"]),
  ref_df = unname(te_row_B[1, "Ref.df"]),
  F_value = unname(te_row_B[1, "F"]),
  p_value = unname(te_row_B[1, "p-value"]),
  adj_r2 = sm_B$r.sq,
  deviance_explained = sm_B$dev.expl,
  AIC = AIC(m_scale_B),
  n = nrow(model.frame(m_scale_B)),
  cc_obs_te = cc_te_B,
  slope_flip_lower_km2 = if (!is.null(flip_band)) flip_band$xmin[1] else NA_real_,
  slope_flip_upper_km2 = if (!is.null(flip_band)) flip_band$xmax[1] else NA_real_,
  stringsAsFactors = FALSE
)
safe_write_csv(scale_effect_summary, "TableS1_scale_effect_model_summary.csv")

term_label_lookup_B <- c(
  "te(log_recl,area_km2)" = "Scale-dependent reclamation surface",
  "s(anthro_pc1)" = "Socioeconomic development",
  "s(climate_pc1)" = "Climate",
  "s(soil_pc1)" = "Soil conditions",
  "s(marine_pc1)" = "Marine context"
)

m_scale_B_term_table <- data.frame(
  term = rownames(sm_B$s.table),
  term_label = unname(term_label_lookup_B[rownames(sm_B$s.table)]),
  edf = round(sm_B$s.table[, "edf"], 3),
  ref_df = round(sm_B$s.table[, "Ref.df"], 3),
  F_value = round(sm_B$s.table[, "F"], 3),
  p_value = signif(sm_B$s.table[, "p-value"], 3),
  stringsAsFactors = FALSE
)
safe_write_csv(m_scale_B_term_table, "TableS1b_m_scale_B_term_summary.csv")

scale_peak_table <- peak_table |>
  mutate(
    peak_reclamation_area_km2 = round(peak_reclamation_area_km2, 2),
    peak_risk = round(peak_risk, 3),
    peak_conf_low = round(peak_conf_low, 3),
    peak_conf_high = round(peak_conf_high, 3)
  )
safe_write_csv(scale_peak_table, "TableS2_scale_effect_conditional_peaks.csv")

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
    y = "Partial effect on invasion pressure",
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
    y = "Partial effect on invasion pressure",
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

supp_table_auto <- do.call(rbind, list(
  make_row(
    "A: Step1 scale-dependence", "m_scale_A",
    "s(log_recl) + s(log_recl_frac)", "risk", m_scale_A,
    note = "Additive dual-smooth benchmark highlighting the relative-intensity component"
  ),
  make_row(
    "A: Step1 scale-dependence", "m_scale_B",
    "te(log_recl, area_km2)", "risk", m_scale_B,
    note = "Raw-area interaction model used as the primary scale-dependent result",
    selected = TRUE
  ),
  make_row(
    "A: Step1 scale-dependence", "m_peak_area",
    "te(log_recl, area_km2) + marine_pc1", "risk", m_peak_area,
    note = "Exploratory raw-area interaction used for conditional peak detection"
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
supp_table_auto$dAIC_vs_null <- round(supp_table_auto$AIC - AIC(m_null_area), 2)
safe_write_csv(supp_table_auto, "SuppTable_GAMM_model_comparison_auto.csv")

supp_table <- data.frame(
  check.names = FALSE,
  `Analysis purpose` = c(
    "Scale dependency",
    "Scale dependency",
    "Scale dependency",
    "Scale dependency",
    "Scale dependency",
    "Scale dependency",
    "Mediation decomposition",
    "Mediation decomposition",
    "Mediation decomposition",
    "Mediation decomposition",
    "Robustness check",
    "Robustness check",
    "Robustness check",
    "Robustness check"
  ),
  Model = c(
    "m_scale_A",
    "m_scale_B",
    "m_scale_C",
    "m_frac_only",
    "m_null_area",
    "m_control_area",
    "m1_total",
    "m2a_SE",
    "m2b_soil",
    "m3_direct",
    "m_scale_A",
    "m3_direct",
    "lmm_check",
    "m_scale_A"
  ),
  `Formulation / role` = c(
    "Separate smooths for log reclamation extent and relative reclamation intensity",
    "Tensor-product smooth for reclamation extent x city size",
    "Separate smooths by small vs large city group",
    "Relative reclamation intensity only",
    "City area only; no reclamation predictor",
    "Reclamation extent + relative intensity + city area",
    "Total effect model (no mediators)",
    "Reclamation -> socioeconomic pathway",
    "Reclamation -> soil pathway",
    "Direct effect model controlling for socioeconomic and soil mediators",
    "Residual Moran's I",
    "Residual Moran's I",
    "Linear mixed-effects model with linear reclamation terms",
    "GAM with non-linear smooths"
  ),
  `Adj R2` = c(
    "0.719",
    "0.739",
    "0.717",
    "0.726",
    "0.686",
    "0.725",
    "0.393",
    "0.447",
    "0.645",
    "0.720",
    "-",
    "-",
    "-",
    "-"
  ),
  `Deviance explained (%)` = c(
    "76.8",
    "79.8",
    "77.6",
    "77.2",
    "74.7",
    "77.8",
    "47.8",
    "51.2",
    "72.9",
    "77.2",
    "-",
    "-",
    "-",
    "-"
  ),
  AIC = c(
    "-198.6",
    "-200.9",
    "-195.8",
    "-200.4",
    "-184.4",
    "-198.4",
    "-135.3",
    "265.9*",
    "298.4*",
    "-197.5",
    "-",
    "-",
    "-75.2",
    "-198.6"
  ),
  `Key reclamation result` = c(
    "log_recl: ns; log_recl_frac: significant (P = 0.005)",
    "Joint 2D surface significant (P = 0.008)",
    "Relative intensity significant only in large cities",
    "Relative intensity significant (P = 0.004)",
    "No reclamation term included",
    "log_recl_frac remains significant (P = 0.003)",
    "Baseline total reclamation effect",
    "Reclamation associated with socioeconomic expansion",
    "Reclamation associated with soil modification",
    "Strong improvement over total-effect model (dAIC = -62.2 vs m1_total)",
    "Moran's I = -0.097, P = 0.917",
    "Moran's I = -0.097, P = 0.778",
    "No significant reclamation effect detected",
    "Significant non-linear effect of relative intensity"
  ),
  `Retained as main evidence` = c(
    "Yes (main statistical evidence)",
    "Yes",
    "No",
    "No",
    "No",
    "No",
    "Reference",
    "Pathway model",
    "Pathway model",
    "Yes (main direct-effect model)",
    "Spatially robust",
    "Spatially robust",
    "No",
    "Yes"
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(supp_table, "SuppTable_GAMM_model_comparison.csv")

diag_table <- bind_rows(
  summarize_gam_diag("m_scale_A", "risk", m_scale_A),
  summarize_gam_diag("m_scale_B", "risk", m_scale_B),
  summarize_gam_diag("m_peak_area", "risk", m_peak_area),
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
safe_write_csv(diag_table, "ModelDiagnostics_060503.csv")

cat("\n===== Supplementary model table =====\n")
print(supp_table, row.names = FALSE)

cat("\n===== Supplementary model table (auto diagnostic version) =====\n")
print(supp_table_auto[, c("Section", "Model", "Adj_R2", "Dev_pct", "AIC", "dAIC_vs_null", "p_log_recl", "p_log_recl_frac", "Note")], row.names = FALSE)

cat("\n===== GAM diagnostic summary =====\n")
print(diag_table[, c("model", "AIC", "adj_r2", "dev_exp", "p_log_recl", "p_log_recl_frac", "cc_obs_max", "cc_obs_log_recl", "cc_obs_log_recl_frac", "cc_obs_log_city_area", "cc_obs_te")], row.names = FALSE)

# =============================================================================
# STEP 3. GREAT-CIRCLE MORAN CORRELOGRAM
# =============================================================================

coords_all <- as.matrix(city_df[, c("Longitude", "Latitude")])
obs_idx_B <- as.integer(rownames(m_scale_B$model))
obs_idx_A <- as.integer(rownames(m_scale_A$model))
obs_idx_m3 <- as.integer(rownames(m3_direct$model))

coords_B <- coords_all[obs_idx_B, , drop = FALSE]
coords_A <- coords_all[obs_idx_A, , drop = FALSE]
coords_m3 <- coords_all[obs_idx_m3, , drop = FALSE]
risk_B <- city_df$risk[obs_idx_B]
risk_A <- city_df$risk[obs_idx_A]
resid_B <- residuals(m_scale_B, type = "deviance")
resid_A <- residuals(m_scale_A, type = "deviance")
resid_m3 <- residuals(m3_direct, type = "deviance")

stopifnot(
  nrow(coords_B) == length(risk_B),
  nrow(coords_B) == length(resid_B),
  nrow(coords_A) == length(risk_A),
  nrow(coords_A) == length(resid_A),
  nrow(coords_m3) == length(resid_m3)
)

cor_risk_B <- make_gc_correlogram(
  values = risk_B,
  coords = coords_B,
  n_bins = 8,
  max_dist_quantile = 0.60,
  min_pairs = 30,
  n_perm = 199,
  seed = 123
)
cor_risk_B$series <- "Observed invasion pressure"

cor_B_gc <- make_gc_correlogram(
  values = resid_B,
  coords = coords_B,
  n_bins = 8,
  max_dist_quantile = 0.60,
  min_pairs = 30,
  n_perm = 199,
  seed = 123
)
cor_B_gc$series <- "Residuals of m_scale_B"

cor_B_all_gc <- bind_rows(cor_risk_B, cor_B_gc) |>
  mutate(sig_flag = ifelse(!is.na(p_value) & p_value < 0.05, "p < 0.05", "n.s."))
safe_write_csv(cor_B_all_gc, "step1B_gc_moran_correlogram.csv")

p_B_correlogram <- ggplot(cor_B_all_gc, aes(x = lag_mid_km, y = MoranI)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(colour = "#2f4858", linewidth = 0.9, na.rm = TRUE) +
  geom_point(
    aes(fill = sig_flag),
    shape = 21,
    size = 3.0,
    colour = "grey25",
    stroke = 0.7,
    na.rm = TRUE
  ) +
  facet_wrap(~series, ncol = 2) +
  scale_fill_manual(values = c("p < 0.05" = "#d73027", "n.s." = "white"), name = "Permutation test") +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = "Great-circle distance lag (km)",
    y = "Moran's I",
    title = "Spatial correlogram for the scale-dependent GAM",
    subtitle = paste(
      "Left panel shows clustering in observed invasion pressure for the m_scale_B sample.",
      "Right panel shows the correlogram of m_scale_B residuals.",
      sep = "\n"
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
save_plot("step1B_gc_moran_correlogram.png", p_B_correlogram, width = 10, height = 4.6, dpi = 350)
ggsave(
  filename = file.path(output_dir, "step1B_gc_moran_correlogram.jpg"),
  plot = p_B_correlogram,
  width = 10,
  height = 4.6,
  dpi = 350,
  device = grDevices::jpeg
)
ggsave(
  filename = file.path(output_dir, "step1B_gc_moran_correlogram.pdf"),
  plot = p_B_correlogram,
  width = 10,
  height = 4.6,
  device = grDevices::pdf
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
cor_risk$series <- "Observed invasion pressure"

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

safe_write_csv(cor_all_gc, "step3_gc_moran_correlogram.csv")

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
      "Left panel tests clustering in invasion pressure itself.",
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
