#!/usr/bin/env Rscript
# Verify saved model inputs and predictions without refitting.
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- cfg$output_root
hash <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(path, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
manifest <- read.csv(file.path(out, "input_checksums.csv"))
stopifnot(identical(unname(vapply(manifest$path, hash, "")), manifest$sha256))
d <- read.csv(file.path(out, "continuous_city_inputs.csv"))
z <- read.csv(cfg$zone_means)
z <- z[z$zone == "RECLAIMED" & z$footprint == "HISTORICAL" & z$rule == "alien_mean", ]
key <- function(x) tolower(trimws(x))
z$key <- key(z$city_id)
lookup <- match(paste(d$key, d$group), paste(z$key, z$group))
stopifnot(!anyNA(lookup), max(abs(d$mean - z$mean[lookup])) < 1e-12,
          all(d$mean >= 0 & d$mean <= 1))
effects <- read.csv(file.path(out, "effect.csv"))
curves <- read.csv(file.path(out, "curves.csv"))
stopifnot(nrow(effects) == 15L, sum(!effects$reused) == 3L, sum(effects$reused) == 12L)
errors <- numeric()
for (i in seq_len(nrow(effects))) {
  e <- effects[i, ]; m <- readRDS(e$model_path)
  q <- d[d$group == e$group, ]; q <- q[order(q$key), ]
  stopifnot(nobs(m) == nrow(q), nrow(q) == 70L)
  if (e$response == "mean") {
    stopifnot(max(abs(m$y - q$mean)) < 1e-12, isTRUE(m$converged),
              m$outer.info$conv == "full convergence")
  }
  a <- b <- q
  a$log_recl <- as.numeric(quantile(q$log_recl, .25))
  b$log_recl <- as.numeric(quantile(q$log_recl, .75))
  delta <- mean(predict(m, b) - predict(m, a))
  design <- colMeans(predict(m, b, type = "lpmatrix") - predict(m, a, type = "lpmatrix"))
  se <- sqrt(drop(design %*% vcov(m) %*% design))
  errors <- c(errors, abs(delta - e$difference), abs(se - e$se))
  cv <- curves[curves$group == e$group & curves$response == e$response &
                 curves$threshold == e$threshold, ]
  stopifnot(nrow(cv) == 243L, all(is.finite(as.matrix(cv[, c("estimate", "lower", "upper")]))))
  for (j in unique(round(seq(1, nrow(cv), length.out = 7)))) {
    nd <- q; nd$log_recl <- log1p(cv$reclamation_km2[j] * 100)
    direct <- mean(predict(m, nd))
    inverse <- if (e$response == "area") expm1 else identity
    errors <- c(errors, abs(direct - cv$model_scale_estimate[j]),
                abs(inverse(direct) - cv$estimate[j]))
  }
}
stopifnot(max(errors) < 1e-9)
effects$contrast_p <- 2 * pnorm(-abs(effects$difference / effects$se))
write.csv(effects, file.path(out, "contrasts_with_p.csv"), row.names = FALSE)
support <- read.csv(file.path(out, "city_support.csv"))
stopifnot(all(table(support$group, support$included)[, "TRUE"] == 70L),
          all(table(support$group, support$included)[, "FALSE"] == 14L))
write_json(list(status = "PASS", checks = c("Continuous responses match original zone table",
  "All saved inputs and binary models retain their hashes", "All model samples contain 70 cities",
  "Three new fits converged", "Twelve binary fits were reused",
  "Saved contrasts and curve values reproduce from stored models"),
  maximum_numeric_error = max(errors), refits_during_verification = 0L),
  file.path(out, "verification.json"), auto_unbox = TRUE, pretty = TRUE)
message("PASS; maximum numeric error=", format(max(errors), scientific = TRUE))
