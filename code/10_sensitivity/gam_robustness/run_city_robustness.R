# City GAM parameterisation and geographic robustness.
# Usage: Rscript run_city_robustness.R --input file --output-root directory
# Optional: --paired file --roster workbook --bootstrap 30
options(stringsAsFactors = FALSE)
Sys.setlocale("LC_COLLATE", "C")
suppressPackageStartupMessages(library(mgcv))
a <- commandArgs(TRUE)
if (length(a) %% 2L) stop("Arguments must be named pairs.")
opt <- setNames(as.list(a[seq(2, length(a), 2)]), sub("^--", "", a[seq(1, length(a), 2)]))
if (is.null(opt$input) || is.null(opt[["output-root"]])) stop("Input and output-root are required.")
if (is.null(opt[["city-metadata"]])) stop("Independent city metadata is required for verified geographic sensitivities.")
root <- opt[["output-root"]]
if(file.exists(file.path(root,"02_gam_robustness","qc","module_status.csv"))) stop("Completed output exists; use an explicit new approved output root.")
paths <- c(gam = "02_gam_robustness", sampling = "03_sampling_selection", spatial = "05_spatial_diagnostics")
for (p in paths) for (s in c("tables", "figures", "model_objects", "qc", "logs")) dir.create(file.path(root, p, s), recursive = TRUE, showWarnings = FALSE)
out <- function(module, kind, name) file.path(root, paths[[module]], kind, name)
write_table <- function(x, module, name, kind = "tables") write.csv(x, out(module, kind, name), row.names = FALSE)
seed <- 20261014L
B <- if (is.null(opt$bootstrap)) 30L else as.integer(opt$bootstrap)
if (!is.finite(B) || B < 0 || B > 200) stop("Bootstrap must be between 0 and 200.")
write_table(data.frame(seed = seed, bootstrap_replicates = B, permutations = 499L,
  fraction = "reclamation_area_ha / 100 / city_area_km2", scan = "endpoint difference, not derivative",
  reference_distribution = "108 input cities", bootstrap_distribution = "84 complete cities resampled",
  support_rule = "G3 requires >=25 unique fractions and area values; local scans use nearest 20 area neighbours",
  purpose = "sensitivity; no ecological breakpoint fitted"), "gam", "prespecified_settings.csv", "qc")
d <- read.csv(opt$input, check.names = FALSE)
input_columns <- ncol(d)
required <- c("city", "risk", "reclamation_area", "area_km2", "anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1", "Regions", "Longitude", "Latitude")
if (!all(required %in% names(d)) || nrow(d) != 108L || anyDuplicated(d$city)) stop("Expected unique 108-city canonical input contract.")
if (any(d$area_km2 <= 0) || any(d$reclamation_area < 0)) stop("Invalid area inputs.")
d$log_recl <- log1p(d$reclamation_area)
d$log_area <- log1p(d$area_km2)
d$true_fraction <- d$reclamation_area / 100 / d$area_km2
d$old_ha_per_km2 <- d$reclamation_area / d$area_km2
d$continent <- ifelse(grepl("Europe", d$Regions, fixed = TRUE), "Europe",
  ifelse(grepl("Africa", d$Regions, fixed = TRUE), "Africa",
  ifelse(grepl("Asia", d$Regions, fixed = TRUE), "Asia",
  ifelse(d$Regions == "Oceania", "Oceania", ifelse(d$Regions == "South America", "South America", "North America")))))
if(!is.null(opt[["city-metadata"]])) {
  script_arg<-grep("^--file=",commandArgs(FALSE),value=TRUE)
  stage_path<-dirname(dirname(normalizePath(sub("^--file=","",script_arg))))
  source(file.path(stage_path,"sampling_selection","verify_continent_crosswalk.R"))
  crosswalk<-verify_continent_crosswalk(d,opt[["city-metadata"]],opt$input)
  d$continent<-crosswalk$verified_continent
  write_table(crosswalk,"sampling","geographic_crosswalk_verified.csv","qc")
}
cc <- complete.cases(d[, required])
if (sum(cc) != 84L) stop("Canonical complete-case sample must be 84.")
write_table(data.frame(city = d$city, included = cc, continent = d$continent, missing_marine = is.na(d$marine_pc1), missing_soil = is.na(d$soil_pc1)), "gam", "city_sample_contract.csv", "qc")
write_table(data.frame(input = normalizePath(opt$input, winslash = "/"), md5 = unname(tools::md5sum(opt$input)), rows = nrow(d), columns = input_columns, n_complete = sum(cc)), "gam", "input_receipt.csv", "qc")
cov <- "s(anthro_pc1,k=5)+s(climate_pc1,k=5)+s(soil_pc1,k=5)+s(marine_pc1,k=5)"
forms <- lapply(c(G0 = "te(log_recl,area_km2,k=c(5,5))", G1 = "s(log_recl,k=5)+s(log_area,k=5)", G2 = "s(true_fraction,k=5)+s(log_area,k=5)", G3 = "te(true_fraction,log_area,k=c(5,5))"), function(x) as.formula(paste("risk~", x, "+", cov)))
if (length(unique(d$true_fraction[cc])) < 25 || length(unique(d$area_km2[cc])) < 25) forms$G3 <- NULL
write_table(data.frame(model = names(forms), formula = vapply(forms, function(x) paste(deparse(x), collapse = ""), "")), "gam", "model_definitions.csv", "qc")
summary_row <- function(m, response, model, subset = "complete") {
  s <- summary(m); t <- s$s.table; focal <- if (model %in% c("G0", "G3")) 1L else grep("log_recl|true_fraction", rownames(t))[1]
  cv <- concurvity(m, full = TRUE)
  data.frame(response = response, model = model, subset = subset, n = nobs(m), adj_r2 = s$r.sq,
    deviance_explained = s$dev.expl, AIC = AIC(m), total_edf = sum(m$edf), focal_edf = t[focal, 1], focal_F = t[focal, 3], focal_p = t[focal, 4],
    maximum_estimated_concurvity = max(cv["estimate", -1], na.rm = TRUE), residual_sd = sd(residuals(m)), residual_skew = mean(scale(residuals(m))^3))
}
scan_model <- function(m, model, reference, support, response, restricted = FALSE) {
  areas <- as.numeric(quantile(reference$area_km2, seq(.05, .95, .05)))
  ans <- lapply(seq_along(areas), function(i) {
    area <- areas[i]
    focal <- if (model %in% c("G0", "G1")) "log_recl" else "true_fraction"
    pool <- if (restricted) support[order(abs(log1p(support$area_km2) - log1p(area)))[seq_len(min(20L, nrow(support)))], ] else reference
    rg <- range(pool[[focal]], na.rm = TRUE)
    nd <- data.frame(log_recl = median(reference$log_recl), true_fraction = median(reference$true_fraction),
      area_km2 = area, log_area = log1p(area), anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE),
      climate_pc1 = median(reference$climate_pc1, na.rm = TRUE), soil_pc1 = median(reference$soil_pc1, na.rm = TRUE), marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
    nd <- nd[rep(1, 2), ]; nd[[focal]] <- rg
    pr <- predict(m, nd, se.fit = TRUE, type = "response")
    xp <- predict(m, nd, type = "lpmatrix"); delta <- xp[2, ] - xp[1, ]
    se <- sqrt(as.numeric(delta %*% vcov(m) %*% delta))
    effect <- diff(pr$fit)
    data.frame(response = response, model = model, quantile = seq(.05, .95, .05)[i], city_area_km2 = area,
      endpoint_difference = effect, SE = se, lower = effect - 1.96 * se, upper = effect + 1.96 * se,
      focal_low = rg[1], focal_high = rg[2], support = if (restricted) "nearest20_area_neighbours" else "whole108_range",
      extrapolation_warning = TRUE)
  })
  do.call(rbind, ans)
}
bracket <- function(scan) {
  i <- which(diff(sign(scan$endpoint_difference)) != 0)
  data.frame(response = scan$response[1], model = scan$model[1], support = scan$support[1], flips = length(i),
    lower_km2 = if (length(i)) scan$city_area_km2[i[1]] else NA_real_, upper_km2 = if (length(i)) scan$city_area_km2[i[1] + 1] else NA_real_)
}
frames <- list(HISTORICAL_MAIN = d[cc, ])
if (!is.null(opt$paired)) {
  p <- read.csv(opt$paired, check.names = FALSE)
  if(all(c("PAIRED_FULL_684","PAIRED_ENV_684")%in%names(p))) {
    p$paired_full <- p$PAIRED_FULL_684; p$paired_env <- p$PAIRED_ENV_684
  }
  if (!all(c("city", "paired_full", "paired_env") %in% names(p)) || anyDuplicated(p$city)) stop("Paired contract requires city, paired_full, paired_env.")
  paired_keys <- tolower(trimws(p$city))
  if(anyDuplicated(paired_keys)) stop("Paired normalised city names are not unique.")
  ix <- match(tolower(trimws(d$city[cc])), paired_keys)
  if (anyNA(ix) || any(!is.finite(p$paired_full[ix])) || any(!is.finite(p$paired_env[ix]))) stop("Paired responses must cover the same 84 cities.")
  frames$PAIRED_FULL_684 <- frames$HISTORICAL_MAIN; frames$PAIRED_FULL_684$risk <- p$paired_full[ix]
  frames$PAIRED_ENV_684 <- frames$HISTORICAL_MAIN; frames$PAIRED_ENV_684$risk <- p$paired_env[ix]
}
models <- list(); summaries <- scans <- terms <- checks <- residuals_table <- influences <- list()
for (response in names(frames)) for (model in names(forms)) {
  f <- frames[[response]]
  set.seed(seed)
  m <- gam(forms[[model]], data = f, method = "REML")
  key <- paste(response, model, sep = "_"); models[[key]] <- m
  saveRDS(m, out("gam", "model_objects", paste0(key, ".rds")))
  summaries[[key]] <- summary_row(m, response, model)
  st <- as.data.frame(summary(m)$s.table); st$term <- rownames(st); st$response <- response; st$model <- model; terms[[key]] <- st
  cv <- as.data.frame(concurvity(m,full=TRUE)); cv$measure <- rownames(cv)
  write_table(cv,"gam",paste0(key,"_concurvity.csv"))
  kt <- as.data.frame(k.check(m, n.rep = 200)); kt$term <- rownames(kt); kt$response <- response; kt$model <- model; checks[[key]] <- kt
  r <- residuals(m, type = "deviance"); h <- m$hat; infl <- r^2 / mean(r^2) * h / pmax((1-h)^2, 1e-8) / sum(m$edf)
  residuals_table[[key]] <- data.frame(response = response, model = model, city = f$city, Longitude = f$Longitude, Latitude = f$Latitude, fitted = fitted(m), residual = r, leverage = h, influence_score = infl)
  influences[[key]] <- residuals_table[[key]][order(infl, decreasing = TRUE)[1:10], ]
  scans[[paste0(key, "_global")]] <- scan_model(m, model, d, f, response)
  scans[[paste0(key, "_local")]] <- scan_model(m, model, d, f, response, TRUE)
}
summary_table <- do.call(rbind, summaries); scan_table <- do.call(rbind, scans); residual_table <- do.call(rbind, residuals_table)
write_table(summary_table, "gam", "model_comparison.csv")
write_table(do.call(rbind, terms), "gam", "smooth_terms.csv")
write_table(do.call(rbind, checks), "gam", "basis_dimension_checks.csv")
write_table(residual_table, "gam", "residuals_and_influence.csv")
write_table(do.call(rbind, influences), "gam", "highest_influence_cities.csv")
write_table(scan_table, "gam", "endpoint_scan.csv")
write_table(do.call(rbind, lapply(scans, bracket)), "gam", "sign_change_intervals.csv")
g0 <- summaries$HISTORICAL_MAIN_G0
verification <- data.frame(quantity = c("n", "adj_r2", "deviance_explained", "AIC", "focal_edf", "focal_F", "focal_p"),
  expected = c(84, .7878737, .83368766, -220.189470, 7.370560, 3.491571, .001747759), observed = as.numeric(unlist(g0[1, c("n", "adj_r2", "deviance_explained", "AIC", "focal_edf", "focal_F", "focal_p")])), tolerance = c(0, .00001, .00001, .001, .001, .001, .00001))
verification$pass <- abs(verification$expected - verification$observed) <= verification$tolerance
write_table(verification, "gam", "historical_reproduction.csv", "qc")
boot <- list()
if (B > 0) for (b in seq_len(B)) {
  set.seed(seed + b); f <- frames$HISTORICAL_MAIN[sample.int(84, 84, replace = TRUE), ]
  for (model in names(forms)) {
    z <- tryCatch({m <- gam(forms[[model]], data = f, method = "REML"); q <- bracket(scan_model(m, model, d, f, "HISTORICAL_MAIN")); q$success <- TRUE; q$error <- ""; q}, error = function(e) data.frame(response="HISTORICAL_MAIN",model=model,support="whole108_range",flips=NA,lower_km2=NA,upper_km2=NA,success=FALSE,error=conditionMessage(e)))
    z$replicate <- b; boot[[paste(b,model)]] <- z
  }
}
if (length(boot)) write_table(do.call(rbind, boot), "gam", "bootstrap_sign_change_intervals.csv")
suppressPackageStartupMessages(library(ggplot2))
ggsave(out("gam", "figures", "endpoint_scan.png"), ggplot(scan_table, aes(city_area_km2, endpoint_difference, colour = model)) + geom_hline(yintercept=0, colour="grey50") + geom_line() + facet_grid(response ~ support, scales="free_y") + scale_x_log10() + labs(x="City area (km²; logarithmic axis)", y="Predicted high-minus-low endpoint difference") + theme_bw(), width=11, height=7, dpi=200)
ggsave(out("gam","figures","residual_qq.png"),ggplot(residual_table,aes(sample=residual))+stat_qq()+stat_qq_line()+facet_grid(response~model,scales="free")+theme_bw()+labs(x="Normal theoretical quantile",y="Deviance residual quantile"),width=11,height=7,dpi=200)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stage <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg), winslash="/")))
source(file.path(stage, "sampling_selection", "geographic_influence.R"), local=TRUE)
source(file.path(stage, "spatial_diagnostics", "city_residual_moran.R"), local=TRUE)
run_geographic(frames, forms, d, opt$roster)
run_spatial(residual_table)
write_table(data.frame(module="gam_robustness",status=if(all(verification$pass)) "TECHNICALLY_PASSED" else "NUMERICAL_REPRODUCTION_FAILED",responses=length(frames),models=nrow(summary_table),scientific_acceptance="not established"),"gam","module_status.csv","qc")
capture.output(sessionInfo(), file=out("gam","qc","sessionInfo.txt"))
if (!all(verification$pass)) stop("Historical G0 did not reproduce stored numeric targets.")
