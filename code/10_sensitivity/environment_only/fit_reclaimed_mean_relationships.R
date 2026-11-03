#!/usr/bin/env Rscript
# Add continuous reclaimed-land responses and reuse saved binary-response GAMs.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1])
out <- cfg$output_root
if (dir.exists(out)) stop("Existing analysis retained; use its saved outputs.")
dir.create(out, recursive = TRUE)
hash <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(path, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
key <- function(x) tolower(trimws(x))
groups <- c("FULL_814", "PAIRED_FULL", "PAIRED_ENV")
paths <- c(cfg$zone_means, cfg$city_predictors)
binary <- list()
for (g in groups) {
  root <- if (g == "FULL_814") cfg$full_binary_root else cfg$paired_binary_root
  label <- if (g == "PAIRED_ENV") "ENV" else "FULL"
  d <- read.csv(file.path(root, "matched_city_data.csv"))
  d <- d[d$region == "MAPPED_RECLAMATION" & d$group == label, ]
  binary[[g]] <- d
  paths <- c(paths, file.path(root, c("matched_city_data.csv", "reclamation_contrasts.csv")))
  for (th in c("T10", "MAX_TSS")) for (response in c("fraction", "area")) {
    file <- paste0("MAPPED_RECLAMATION_", label, "_", th,
                   "_mean_species_suitable_", response, ".rds")
    paths <- c(paths, file.path(root, file))
  }
}
paths <- unique(paths)
before <- vapply(paths, hash, "")
z <- read.csv(cfg$zone_means)
z <- z[z$zone == "RECLAIMED" & z$footprint == "HISTORICAL" & z$rule == "alien_mean", ]
p <- read.csv(cfg$city_predictors)
z$key <- key(z$city_id); p$key <- key(p$city)
stopifnot(!anyDuplicated(p$key), !anyDuplicated(paste(z$key, z$group)))
d <- merge(z, p, by = "key", all.x = TRUE)
d$log_recl <- log1p(d$reclamation_area)
d$log_area <- log1p(d$area_km2)
required <- c("mean", "log_recl", "area_km2", "anthro_pc1",
              "climate_pc1", "soil_pc1", "marine_pc1")
d$missing_fields <- apply(d[, required], 1, function(x) {
  paste(required[!is.finite(as.numeric(x))], collapse = ";")
})
d$included <- d$missing_fields == "" & !d$key %in% cfg$exclude_city_keys
write.csv(d[, c("key", "city_id", "group", "included", "missing_fields")],
          file.path(out, "city_support.csv"), row.names = FALSE)
d <- d[d$included, ]
write.csv(d, file.path(out, "continuous_city_inputs.csv"), row.names = FALSE)
formula <- y ~ te(log_recl, area_km2, k = c(5, 5)) +
  s(anthro_pc1, k = 5) + s(climate_pc1, k = 5) +
  s(soil_pc1, k = 5) + s(marine_pc1, k = 5)
write_json(list(status = "SPECIFIED_BEFORE_FITTING", groups = groups,
  response = "Area-weighted cellwise mean of finite non-native species predictions",
  formula = paste(deparse(formula), collapse = " "), method = "REML",
  new_fits = 3L, binary_refits = 0L, sdm_refits = 0L,
  comparison = "Mean prediction contrast at the 25th and 75th percentiles of log1p reclamation area",
  prediction = "Average predictions over the retained cities with their own other covariates",
  area_display = "expm1 of the average fitted log1p area; no residual smearing"),
  file.path(out, "analysis_specification.json"), auto_unbox = TRUE, pretty = TRUE)
curves <- effects <- summaries <- models <- list()
errors <- numeric()
process <- function(m, q, g, response, threshold, source, reused) {
  stopifnot(nobs(m) == nrow(q), all(is.finite(q$log_recl)))
  lo <- as.numeric(quantile(q$log_recl, .25))
  hi <- as.numeric(quantile(q$log_recl, .75))
  grid <- sort(unique(c(seq(min(q$log_recl), max(q$log_recl), length.out = 241), lo, hi)))
  X <- t(vapply(grid, function(x) {
    nd <- q; nd$log_recl <- x
    colMeans(predict(m, nd, type = "lpmatrix"))
  }, numeric(length(coef(m)))))
  eta <- drop(X %*% coef(m))
  se <- sqrt(pmax(0, rowSums((X %*% vcov(m)) * X)))
  inv <- if (response == "area") expm1 else identity
  cdata <- data.frame(group = g, response = response, threshold = threshold,
    reclamation_km2 = expm1(grid) / 100, estimate = inv(eta),
    lower = inv(eta - 1.96 * se), upper = inv(eta + 1.96 * se),
    model_scale_estimate = eta, model_scale_se = se, n_cities = nrow(q))
  diff <- X[match(hi, grid), ] - X[match(lo, grid), ]
  effect <- sum(diff * coef(m))
  ese <- sqrt(drop(diff %*% vcov(m) %*% diff))
  ndlo <- ndhi <- q; ndlo$log_recl <- lo; ndhi$log_recl <- hi
  direct <- mean(predict(m, ndhi) - predict(m, ndlo))
  stopifnot(abs(direct - effect) < 1e-10)
  ef <- data.frame(group = g, response = response, threshold = threshold,
    n_cities = nrow(q), low_reclamation_km2 = expm1(lo) / 100,
    high_reclamation_km2 = expm1(hi) / 100, difference = effect,
    se = ese, lower = effect - 1.96 * ese, upper = effect + 1.96 * ese,
    low_fitted = inv(eta[match(lo, grid)]), high_fitted = inv(eta[match(hi, grid)]),
    contrast_scale = if (response == "area") "log1p_km2" else "0_to_1",
    reused = reused, model_path = source)
  st <- summary(m)
  sm <- data.frame(group = g, response = response, threshold = threshold,
    n_cities = nrow(q), adj_r2 = st$r.sq, deviance_explained = st$dev.expl,
    focal_edf = st$s.table[1, 1], focal_p = st$s.table[1, 4],
    maximum_concurvity = max(concurvity(m, full = TRUE)["estimate", -1]),
    formula = paste(deparse(formula(m)), collapse = " "),
    converged = isTRUE(m$converged), reused = reused)
  list(curves = cdata, effect = ef, summary = sm)
}
for (g in groups) {
  q <- d[d$group == g, ]; q <- q[order(q$key), ]; q$y <- q$mean
  b <- binary[[g]]
  stopifnot(setequal(q$key, b$key), nrow(q) >= 50, !anyDuplicated(q$key))
  file <- file.path(out, paste0(g, "_mean_suitability.rds"))
  m <- gam(formula, data = q, method = "REML")
  stopifnot(isTRUE(m$converged), m$outer.info$conv == "full convergence")
  saveRDS(m, file)
  models[[paste(g, "mean", "NONE")]] <- process(m, q, g, "mean", "NONE", file, FALSE)
  for (th in c("T10", "MAX_TSS")) for (response in c("fraction", "area")) {
    root <- if (g == "FULL_814") cfg$full_binary_root else cfg$paired_binary_root
    label <- if (g == "PAIRED_ENV") "ENV" else "FULL"
    file <- file.path(root, paste0("MAPPED_RECLAMATION_", label, "_", th,
                                  "_mean_species_suitable_", response, ".rds"))
    old <- readRDS(file); qb <- b[b$threshold == th, ]; qb <- qb[order(qb$key), ]
    stopifnot(identical(q$key, qb$key))
    for (v in c("log_recl", "area_km2", "anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1")) {
      stopifnot(max(abs(q[[v]] - qb[[v]])) < 1e-10)
    }
    result <- process(old, qb, g, response, th, file, TRUE)
    saved <- read.csv(file.path(root, "reclamation_contrasts.csv"))
    saved <- saved[saved$region == "MAPPED_RECLAMATION" & saved$group == label &
      saved$threshold == th & saved$response == paste0("mean_species_suitable_", response), ]
    error <- max(abs(unlist(result$effect[c("difference", "se", "lower", "upper")]) -
      unlist(saved[c("mean_q75_minus_q25", "se", "lower", "upper")])))
    stopifnot(error < 1e-10)
    errors <- c(errors, error)
    models[[paste(g, response, th)]] <- result
  }
  message("Completed continuous fit and reused binary models: ", g, "; cities=", nrow(q))
}
for (name in c("curves", "effect", "summary")) {
  table <- do.call(rbind, lapply(models, `[[`, name))
  write.csv(table, file.path(out, paste0(name, ".csv")), row.names = FALSE)
}
after <- vapply(paths, hash, "")
stopifnot(identical(before, after))
write.csv(data.frame(path = paths, sha256 = before),
          file.path(out, "input_checksums.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
write_json(list(status = "COMPLETE", sdm_fits = 0L, new_continuous_gam_fits = 3L,
  reused_binary_gam_fits = 12L, existing_inputs_unchanged = TRUE,
  maximum_error_to_saved_binary_contrasts = max(errors),
  source_zone = "HISTORICAL / RECLAIMED", native_rule = "alien_mean",
  city_support = 70L, input_hashes = as.list(before)),
  file.path(out, "receipt.json"), auto_unbox = TRUE, pretty = TRUE)
