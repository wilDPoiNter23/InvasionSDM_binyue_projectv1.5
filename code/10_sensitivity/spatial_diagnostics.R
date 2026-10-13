# Spatial residual diagnostics
# Input: City-level PCA/GAM input table
# Output: Moran correlograms and spatial-sensitivity tables

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
suppressPackageStartupMessages({
  library(mgcv)
  library(ggplot2)
  library(dplyr)
  library(geosphere)
})

workdir <- Sys.getenv("GAM_WORKDIR", unset = publication_path("GAM_WORKDIR"))
input_csv <- Sys.getenv("GAM_INPUT_CSV", unset = publication_path("GAM_INPUT"))
output_dir <- Sys.getenv("GAM_OUTPUT_DIR", unset = file.path(workdir, "gamFinalResult/spatial_sensitivity"))
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
  "risk", "reclamation_area", "area_km2", "log_recl",
  "anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1", "geom_pc1",
  "Longitude", "Latitude", "Regions"
) %in% names(city_df)))

save_plot <- function(filename, plot_obj, width, height, dpi = 350) {
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
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p)
  out[ok] <- formatC(p[ok], digits = 3, format = "f")
  out[ok & p < 0.001] <- "<0.001"
  out
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, formatC(x, digits = digits, format = "f"))
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
      (n / pair_count) * sum((z_perm[row_id] * z_perm[col_id])[band], na.rm = TRUE) / sum(z_perm^2)
    })
    out$p_value[i] <- (sum(abs(perm_i) >= abs(obs_i)) + 1) / (n_perm + 1)
  }

  out
}

run_gc_check <- function(model, model_name, data, output_stub) {
  model_rows <- as.integer(rownames(model$model))
  coords <- as.matrix(data[model_rows, c("Longitude", "Latitude")])
  observed <- data$risk[model_rows]
  resid_vals <- residuals(model, type = "deviance")

  stopifnot(
    nrow(coords) == length(observed),
    nrow(coords) == length(resid_vals)
  )

  cor_obs <- make_gc_correlogram(observed, coords)
  cor_obs$series <- "Observed invasion pressure"

  cor_resid <- make_gc_correlogram(resid_vals, coords)
  cor_resid$series <- paste("Residuals of", model_name)

  cor_all <- bind_rows(cor_obs, cor_resid) |>
    mutate(sig_flag = ifelse(!is.na(p_value) & p_value < 0.05, "p < 0.05", "n.s."))

  p <- ggplot(cor_all, aes(x = lag_mid_km, y = MoranI)) +
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
      title = paste("Spatial correlogram for", model_name),
      subtitle = "Observed pressure and model residuals are evaluated on the exact analysis sample of the fitted model."
    ) +
    theme_classic(base_size = 11) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )

  save_plot(paste0(output_stub, ".png"), p, width = 10, height = 4.6, dpi = 350)
  ggsave(
    filename = file.path(output_dir, paste0(output_stub, ".jpg")),
    plot = p,
    width = 10,
    height = 4.6,
    dpi = 350,
    device = grDevices::jpeg
  )
  ggsave(
    filename = file.path(output_dir, paste0(output_stub, ".pdf")),
    plot = p,
    width = 10,
    height = 4.6,
    device = grDevices::pdf
  )
  safe_write_csv(cor_all, paste0(output_stub, ".csv"))

  st <- summary(model)$s.table
  te_idx <- grep("^te\\(", rownames(st))
  region_idx <- grep("^s\\(Regions\\)$", rownames(st))
  geom_idx <- grep("^s\\(geom_pc1\\)$", rownames(st))
  spatial_idx <- grep("Longitude,Latitude", rownames(st), fixed = TRUE)
  te_p <- if (length(te_idx)) unname(st[te_idx[1], "p-value"]) else NA_real_
  te_edf <- if (length(te_idx)) unname(st[te_idx[1], "edf"]) else NA_real_
  region_p <- if (length(region_idx)) unname(st[region_idx[1], "p-value"]) else NA_real_
  geom_p <- if (length(geom_idx)) unname(st[geom_idx[1], "p-value"]) else NA_real_
  spatial_p <- if (length(spatial_idx)) unname(st[spatial_idx[1], "p-value"]) else NA_real_

  resid_only <- cor_all |>
    filter(series == paste("Residuals of", model_name))
  resid_sig <- resid_only |>
    filter(!is.na(p_value), p_value < 0.05)

  summary_row <- data.frame(
    model = model_name,
    n = nrow(model.frame(model)),
    AIC = AIC(model),
    adj_r2 = summary(model)$r.sq,
    deviance_explained = summary(model)$dev.expl,
    te_edf = te_edf,
    te_p = te_p,
    region_p = region_p,
    geom_p = geom_p,
    spatial_term_p = spatial_p,
    residual_sig_bins = nrow(resid_sig),
    first_residual_sig_mid_km = if (nrow(resid_sig)) resid_sig$lag_mid_km[1] else NA_real_,
    first_residual_sig_I = if (nrow(resid_sig)) resid_sig$MoranI[1] else NA_real_,
    first_residual_sig_p = if (nrow(resid_sig)) resid_sig$p_value[1] else NA_real_,
    stringsAsFactors = FALSE
  )

  list(correlogram = cor_all, summary = summary_row)
}

run_gc_summary_only <- function(model, model_name, data,
                                n_bins = 8,
                                max_dist_quantile = 0.60,
                                min_pairs = 30,
                                n_perm = 199) {
  model_rows <- as.integer(rownames(model$model))
  coords <- as.matrix(data[model_rows, c("Longitude", "Latitude")])
  resid_vals <- residuals(model, type = "deviance")
  cor_resid <- make_gc_correlogram(
    values = resid_vals,
    coords = coords,
    n_bins = n_bins,
    max_dist_quantile = max_dist_quantile,
    min_pairs = min_pairs,
    n_perm = n_perm
  )
  cor_resid$series <- paste("Residuals of", model_name)
  resid_sig <- cor_resid |>
    filter(!is.na(p_value), p_value < 0.05)

  data.frame(
    model = model_name,
    n_bins = n_bins,
    max_dist_quantile = max_dist_quantile,
    residual_sig_bins = nrow(resid_sig),
    first_sig_mid_km = if (nrow(resid_sig)) resid_sig$lag_mid_km[1] else NA_real_,
    first_sig_I = if (nrow(resid_sig)) resid_sig$MoranI[1] else NA_real_,
    first_sig_p = if (nrow(resid_sig)) resid_sig$p_value[1] else NA_real_,
    stringsAsFactors = FALSE
  )
}

m_scale_B <- gam(
  risk ~
    te(log_recl, area_km2, k = c(5, 5)) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(marine_pc1, k = 5),
  data = city_df,
  method = "REML"
)

m_scale_B_spatial <- gam(
  risk ~
    te(log_recl, area_km2, k = c(5, 5)) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(marine_pc1, k = 5) +
    s(Longitude, Latitude, bs = "gp", k = 30),
  data = city_df,
  method = "REML"
)

m_scale_B_region <- gam(
  risk ~
    te(log_recl, area_km2, k = c(5, 5)) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(marine_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

m_scale_B_geom <- gam(
  risk ~
    te(log_recl, area_km2, k = c(5, 5)) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(marine_pc1, k = 5) +
    s(geom_pc1, k = 5),
  data = city_df,
  method = "REML"
)

m_scale_B_geom_region <- gam(
  risk ~
    te(log_recl, area_km2, k = c(5, 5)) +
    s(anthro_pc1, k = 5) +
    s(climate_pc1, k = 5) +
    s(soil_pc1, k = 5) +
    s(marine_pc1, k = 5) +
    s(geom_pc1, k = 5) +
    s(Regions, bs = "re"),
  data = city_df,
  method = "REML"
)

res_B <- run_gc_check(
  model = m_scale_B,
  model_name = "m_scale_B",
  data = city_df,
  output_stub = "m_scale_B_gc_correlogram"
)

res_B_region <- run_gc_check(
  model = m_scale_B_region,
  model_name = "m_scale_B_region",
  data = city_df,
  output_stub = "m_scale_B_region_gc_correlogram"
)

res_B_geom <- run_gc_check(
  model = m_scale_B_geom,
  model_name = "m_scale_B_geom",
  data = city_df,
  output_stub = "m_scale_B_geom_gc_correlogram"
)

res_B_geom_region <- run_gc_check(
  model = m_scale_B_geom_region,
  model_name = "m_scale_B_geom_region",
  data = city_df,
  output_stub = "m_scale_B_geom_region_gc_correlogram"
)

res_B_spatial <- run_gc_check(
  model = m_scale_B_spatial,
  model_name = "m_scale_B_spatial",
  data = city_df,
  output_stub = "m_scale_B_spatial_gc_correlogram"
)

comparison_table <- bind_rows(
  res_B$summary,
  res_B_region$summary,
  res_B_geom$summary,
  res_B_geom_region$summary,
  res_B_spatial$summary
)
safe_write_csv(comparison_table, "m_scale_B_spatial_comparison.csv")

sensitivity_grid <- expand.grid(
  n_bins = c(6, 8, 10),
  max_dist_quantile = c(0.50, 0.60, 0.70)
)

bin_sensitivity_table <- bind_rows(lapply(seq_len(nrow(sensitivity_grid)), function(i) {
  run_gc_summary_only(
    model = m_scale_B,
    model_name = "m_scale_B",
    data = city_df,
    n_bins = sensitivity_grid$n_bins[i],
    max_dist_quantile = sensitivity_grid$max_dist_quantile[i]
  )
}))
safe_write_csv(bin_sensitivity_table, "m_scale_B_bin_sensitivity.csv")

model_label_lookup <- c(
  "m_scale_B" = "Baseline scale-dependent GAM",
  "m_scale_B_region" = "Baseline GAM + regional random effect",
  "m_scale_B_geom" = "Baseline GAM + geomorphic gradient",
  "m_scale_B_geom_region" = "Baseline GAM + geomorphic gradient + regional random effect",
  "m_scale_B_spatial" = "Baseline GAM + bivariate spatial smooth"
)

add_term_lookup <- c(
  "m_scale_B" = "None",
  "m_scale_B_region" = "s(Regions, bs = 're')",
  "m_scale_B_geom" = "s(geom_pc1)",
  "m_scale_B_geom_region" = "s(geom_pc1) + s(Regions, bs = 're')",
  "m_scale_B_spatial" = "s(Longitude, Latitude, bs = 'gp')"
)

publication_model_table <- comparison_table |>
  mutate(
    Section = "Model specification",
    Setting = unname(model_label_lookup[model]),
    `Additional term(s)` = unname(add_term_lookup[model]),
    `Sample size` = n,
    `AIC` = fmt_num(AIC, 2),
    `Adj. R2` = fmt_num(adj_r2, 3),
    `Deviance explained (%)` = fmt_num(deviance_explained * 100, 1),
    `te(log_recl, area_km2) P` = fmt_p(te_p),
    `Additional term P` = dplyr::coalesce(fmt_p(spatial_term_p), fmt_p(region_p), fmt_p(geom_p)),
    `Significant residual lag classes (n)` = residual_sig_bins,
    `First significant lag midpoint (km)` = ifelse(
      is.na(first_residual_sig_mid_km), "None", formatC(first_residual_sig_mid_km, digits = 1, format = "f")
    ),
    `Moran's I in first significant lag` = ifelse(
      is.na(first_residual_sig_I), "None", formatC(first_residual_sig_I, digits = 3, format = "f")
    ),
    `Permutation P` = ifelse(
      is.na(first_residual_sig_p), "None", fmt_p(first_residual_sig_p)
    )
  ) |>
  select(
    Section, Setting, `Additional term(s)`, `Sample size`, `AIC`, `Adj. R2`,
    `Deviance explained (%)`, `te(log_recl, area_km2) P`, `Additional term P`,
    `Significant residual lag classes (n)`, `First significant lag midpoint (km)`,
    `Moran's I in first significant lag`, `Permutation P`
  )

publication_bin_table <- bin_sensitivity_table |>
  mutate(
    Section = "Lag-bin definition",
    Setting = paste0(n_bins, " bins; max-distance quantile = ", formatC(max_dist_quantile, digits = 1, format = "f")),
    `Additional term(s)` = "Baseline scale-dependent GAM",
    `Sample size` = NA,
    `AIC` = NA,
    `Adj. R2` = NA,
    `Deviance explained (%)` = NA,
    `te(log_recl, area_km2) P` = NA,
    `Additional term P` = NA,
    `Significant residual lag classes (n)` = residual_sig_bins,
    `First significant lag midpoint (km)` = ifelse(
      is.na(first_sig_mid_km), "None", formatC(first_sig_mid_km, digits = 1, format = "f")
    ),
    `Moran's I in first significant lag` = ifelse(
      is.na(first_sig_I), "None", formatC(first_sig_I, digits = 3, format = "f")
    ),
    `Permutation P` = ifelse(
      is.na(first_sig_p), "None", fmt_p(first_sig_p)
    )
  ) |>
  select(
    Section, Setting, `Additional term(s)`, `Sample size`, `AIC`, `Adj. R2`,
    `Deviance explained (%)`, `te(log_recl, area_km2) P`, `Additional term P`,
    `Significant residual lag classes (n)`, `First significant lag midpoint (km)`,
    `Moran's I in first significant lag`, `Permutation P`
  )

publication_sensitivity_table <- bind_rows(publication_model_table, publication_bin_table)
safe_write_csv(publication_sensitivity_table, "TableS_spatial_residual_sensitivity.csv")

caption_lines <- c(
  "Table Sx | Sensitivity of residual spatial autocorrelation to model specification and lag-bin definition.",
  "The upper section compares the baseline scale-dependent GAM with alternative specifications that add regional, geomorphic, or spatial terms.",
  "The lower section evaluates sensitivity of the residual correlogram to the number of lag bins and the maximum distance quantile used to define great-circle distance classes.",
  "For each setting, the table reports the number of significant residual lag classes and, where applicable, the midpoint, Moran's I, and permutation P value of the first significant lag."
)
writeLines(caption_lines, con = file.path(output_dir, "TableS_spatial_residual_sensitivity_caption.txt"), useBytes = TRUE)

cat("\n===== Scale-dependent GAM vs spatial sensitivity GAM =====\n")
print(comparison_table, row.names = FALSE)

cat("\n===== Binning sensitivity for m_scale_B residual correlogram =====\n")
print(bin_sensitivity_table, row.names = FALSE)

cat("\n===== Publication-ready spatial sensitivity table =====\n")
print(publication_sensitivity_table, row.names = FALSE)
