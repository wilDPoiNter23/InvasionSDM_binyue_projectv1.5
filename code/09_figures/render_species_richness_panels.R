# Recompose hierarchical-model and recorded-richness figures from saved estimates.
# Usage: Rscript render_species_richness_panels.R configuration.json [land_use]
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(jsonlite)
  library(svglite); library(ragg); library(digest)
})
args <- commandArgs(TRUE)
stopifnot(length(args) %in% 1:2, length(args) == 1 || args[2] == "land_use")
cfg <- fromJSON(args[1])
out <- cfg$output_root
stopifnot(dir.exists(out), capabilities("cairo"))
for (sub in c("preview", "source_data"))
  dir.create(file.path(out, sub), showWarnings = FALSE)
sources <- list(); exports <- list()
sha <- function(p) {
  con <- file(p, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(p, extra_cols = FALSE)$size),
    algo = "sha256", serialize = FALSE)
}
read_source <- function(key) {
  path <- cfg$inputs[[key]]
  stopifnot(file.exists(path))
  sources[[key]] <<- list(path = path, sha256 = sha(path))
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
cols <- c(FULL = "#7CA9A3", ENV = "#A49DBB")
adjust_cols <- c("City area" = "#365F7D", "City area + records" = "#A97550")
font <- "Arial"
pdfFonts(Arial = pdfFonts("Helvetica")[[1]])
theme_set(theme_classic(base_family = font, base_size = 7.5) + theme(
  text = element_text(colour = "#29343D"),
  axis.text = element_text(size = 7, colour = "#39434B"),
  axis.title = element_text(size = 7.5),
  axis.line = element_line(linewidth = .3, colour = "#899198"),
  axis.ticks = element_line(linewidth = .3, colour = "#899198"),
  plot.title = element_text(size = 8, face = "bold", margin = margin(b = 5)),
  plot.subtitle = element_text(size = 6.8, colour = "#68737C", margin = margin(b = 7)),
  plot.tag = element_text(size = 9, face = "bold"),
  legend.title = element_text(size = 7), legend.text = element_text(size = 7),
  legend.key.width = grid::unit(10, "mm"), legend.position = "bottom",
  plot.margin = margin(6, 8, 6, 6)))
annotation_theme <- theme(plot.title = element_text(family = font, size = 10,
  face = "bold", margin = margin(b = 5)), plot.subtitle = element_text(
    family = font, size = 7, colour = "#68737C", margin = margin(b = 8)),
  plot.caption = element_text(family = font, size = 6.5, colour = "#68737C", hjust = 0,
    margin = margin(t = 5)))
save_panel <- function(p, name, height, caption, width = 135) {
  base <- file.path(out, name)
  svglite(paste0(base, ".svg"), width = width / 25.4, height = height / 25.4,
    bg = "white"); print(p); dev.off()
  pdf(paste0(base, ".pdf"), width = width / 25.4, height = height / 25.4,
    family = font, bg = "white", useDingbats = FALSE)
  print(p)
  dev.off()
  agg_png(file.path(out, "preview", paste0(name, ".png")), width = width,
    height = height, units = "mm", res = 300, background = "white")
  print(p); dev.off()
  writeLines(caption, file.path(out, paste0(name, "_legend.txt")))
  exports[[name]] <<- list(width_mm = width, height_mm = height,
    pdf_sha256 = sha(paste0(base, ".pdf")), svg_sha256 = sha(paste0(base, ".svg")))
  message("EXPORTED ", name)
}
write_source <- function(d, name) write.csv(d,
  file.path(out, "source_data", paste0(name, ".csv")), row.names = FALSE)

# City distributions and the saved hierarchical model summaries.
full <- read_source("full_contrasts"); env <- read_source("env_contrasts")
land <- rbind(transform(subset(full, type == "USE_VS_REFERENCE" & case == "primary"), response = "FULL"),
 transform(subset(env, type == "USE_VS_REFERENCE" & case == "primary" & group == "ENV"), response = "ENV"))
stopifnot(nrow(land) == 18L, all(land$from == "transp"), all(land$age_index_years == 15),
 all(land$chain_status == "PASS_CHAIN_DIAGNOSTICS"))
land$supported <- land$lower > 0 | land$upper < 0
fp <- read_source("full_predictions"); ep <- read_source("env_predictions")
pred <- rbind(transform(subset(fp, age_index_years == 15), response = "FULL"),
 transform(subset(ep, age_index_years == 15), response = "ENV"))
stopifnot(nrow(pred) == 20L)
pred$supported <- FALSE
for (i in seq_len(nrow(pred))) {
 c <- land[land$response == pred$response[i] & land$to == pred$landuse[i], , drop = FALSE]
 if (nrow(c) == 1L) pred$supported[i] <- c$supported
}
read_rds_source <- function(key) {
 path <- cfg$inputs[[key]]; stopifnot(file.exists(path))
 sources[[key]] <<- list(path = path, sha256 = sha(path)); readRDS(path)
}
zone_city <- function(zone_key, input_key, group) {
 z <- read_source(zone_key); input <- read_rds_source(input_key)
 areas <- unique(input[, c("zone_id", "city_id", "landuse", "area_km2")])
 areas$landuse <- as.character(areas$landuse)
 stopifnot(!anyDuplicated(areas$zone_id), nrow(areas) == 1194L, all(areas$area_km2 > 0))
 z <- merge(z, areas, by = c("zone_id", "city_id", "landuse"), all.x = TRUE, sort = FALSE)
 stopifnot(nrow(z) == 1194L, !anyNA(z$area_km2), all(is.finite(z$mean_suitability)))
 weighted <- aggregate(cbind(weighted = z$mean_suitability * z$area_km2, area = z$area_km2),
   list(city_id = z$city_id, landuse = z$landuse), sum)
 weighted$city_mean <- weighted$weighted / weighted$area
 weighted$response <- group; weighted
}
city <- rbind(zone_city("full_zones", "full_model_input", "FULL"),
 zone_city("env_zones", "env_model_input", "ENV"))
codes <- c("transp", "comm", "public", "resid", "indus", "crop", "water", "bare", "grass", "forest")
labels <- c("Transport", "Commercial", "Public use", "Residential", "Industrial",
 "Cropland", "Water", "Bare land", "Grassland", "Forest")
nf <- table(factor(city$landuse[city$response == "FULL"], levels = codes))
ne <- table(factor(city$landuse[city$response == "ENV"], levels = codes))
stopifnot(identical(as.integer(nf), as.integer(ne)), sum(nf) == nrow(city) / 2)
row_labels <- labels
city$position <- 11 - match(city$landuse, codes)
pred$position <- 11 - match(pred$landuse, codes)
# Each point is a single city, shifted slightly to separate it from the model interval.
set.seed(41); city$draw_y <- city$position - .16 + runif(nrow(city), -.095, .095)
pred$draw_y <- pred$position + .17
write_source(city, "hierarchical_city_landuse_display")
write_source(pred, "hierarchical_estimates_display")
write_source(land, "hierarchical_land_use_display")
land_panel <- function(group, show_labels = TRUE) {
 d <- city[city$response == group, , drop = FALSE]
 q <- pred[pred$response == group, , drop = FALSE]
 ggplot() +
  geom_point(data = d, aes(x = city_mean * 100, y = draw_y), size = 1.05,
    shape = 16, colour = "#87939A", alpha = .35) +
  geom_segment(data = q, aes(x = lower * 100, xend = upper * 100, y = draw_y, yend = draw_y),
    colour = "#34434A", linewidth = .65) +
  geom_point(data = q, aes(x = mean * 100, y = draw_y), shape = 21,
    fill = if (group == "FULL") "#A2554B" else "#3C7983", colour = "white", stroke = .28, size = 2.3) +
  geom_text(data = subset(q, supported), aes(x = upper * 100 + 1.8, y = draw_y + .02),
    label = "*", family = font, fontface = "bold", size = 3.1, colour = "#34434A") +
  scale_y_continuous(breaks = 1:10, labels = if (show_labels) rev(row_labels) else NULL,
    limits = c(.45, 10.6), expand = expansion(mult = 0)) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
    expand = expansion(mult = 0)) +
  labs(title = group, x = "Predicted suitability (0-100)", y = NULL) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    plot.title = element_text(size = 8.5, face = "bold"), axis.text.y = element_text(size = 7),
    plot.margin = margin(6, 5, 5, 5), legend.position = "none")
}
p <- (land_panel("FULL") | land_panel("ENV", FALSE)) + plot_layout(widths = c(1.12, 1)) +
 plot_annotation(tag_levels = "a", caption = "Grey points: cities; coloured points and bars: model estimates and 95% credible intervals.\n* Difference from transport: 95% credible interval excludes zero.", theme = annotation_theme)
save_panel(p, "coastal_land_use", 112, paste(
 "Supplementary Fig. 9 | Predicted suitability across reclaimed land uses.",
 "Grey points show the area-weighted mean suitability for each land use in each city.",
 "Coloured points show predictions from Bayesian hierarchical models; bars indicate 95% credible intervals.",
 "Asterisks identify land uses whose difference from transport land has a 95% credible interval that",
 "excludes zero; no adjustment for multiple comparisons was applied. The models account for variation",
 "among cities, zones and species, and include land use, formation age and zone area.",
 "Model predictions are shown at a formation age index of 15 years and a reference zone area and city age.",
 "Grey points include land from different formation periods. FULL uses an 814-species roster with",
 "811 finite contributors; ENV uses 684 species. Both analyses cover 71 cities and 1,194 zones.",
 "City counts by land use are 64, 25, 12, 13, 16, 12, 28, 29, 4 and 4 in plotted order.",
 "Suitability is shown on a scale from 0 to 100. Complete contrasts are in Supplementary Table 9."), width = 183)

if (length(args) == 1) {
# Separate age predictions from the land-use comparison.
fp <- read_source("full_predictions"); ep <- read_source("env_predictions")
age <- rbind(transform(subset(fp, landuse == "transp"), response = "FULL"),
 transform(subset(ep, landuse == "transp"), response = "ENV"))
stopifnot(nrow(age) == 14L)
write_source(age, "hierarchical_age_display")
p <- ggplot(age, aes(age_index_years, mean * 100, colour = response, fill = response)) +
 geom_ribbon(aes(ymin = lower * 100, ymax = upper * 100), colour = NA, alpha = .12) +
 geom_line(linewidth = .7) + scale_colour_manual(values = cols, name = NULL) +
 scale_fill_manual(values = cols, name = NULL) +
 scale_x_continuous(breaks = c(0, 10, 20, 30)) +
 labs(title = "Formation-age gradient", subtitle = "Transport land | 71 cities",
   x = "Formation-age index (years)", y = "Predicted suitability (0-100)",
   caption = "Lines: posterior means; bands: 95% credible intervals.") +
 theme(plot.title = element_text(size = 10), plot.caption = element_text(size = 6.5, hjust = 0))
save_panel(p, "hierarchical_formation_age", 86, paste(
 "Formation-age gradient in predicted suitability for the reference transport land use.",
 "Curves and bands reproduce saved posterior means and 95% credible intervals from the primary",
 "species-level FULL and ENV models. Formation classes are compared at the prediction date.",
 "This is a separate display of the age predictions accompanying Supplementary Table 9."))

# Observed cities and the saved area-adjusted model, 2010-2024.
co <- read_source("richness_coefficients")
all_co <- subset(co, scope == "RAW_SELECTED_INVASIVE" & term == "reclamation" & model == "M0")
stopifnot(nrow(all_co) == 4L, all(is.finite(all_co$se)), all(all_co$se > 0))
all_co$wald_z <- all_co$estimate / all_co$se
all_co$p_two_sided_wald <- 2 * pnorm(-abs(all_co$wald_z))
all_co$q_bh_four_windows <- p.adjust(all_co$p_two_sided_wald, "BH")
write_source(all_co, "observed_richness_regression_display")
coef <- subset(co, scope == "RAW_SELECTED_INVASIVE" & window == "common2010_2024" & model == "M0")
models <- read_source("richness_models")
model <- subset(models, scope == "RAW_SELECTED_INVASIVE" & window == "common2010_2024" & model == "M0")
raw <- read_source("richness_city_data")
cities <- subset(raw, scope == "RAW_SELECTED_INVASIVE" & window == "common2010_2024" & target_record_n > 0)
fitted <- read_source("richness_fitted")
fitted <- subset(fitted, scope == "RAW_SELECTED_INVASIVE" & window == "common2010_2024" & model == "M0")
stopifnot(nrow(cities) == 107L, nrow(fitted) == 107L, nrow(model) == 1L,
 !anyDuplicated(cities$city_id), !anyDuplicated(fitted$city_id),
 setequal(cities$city_id, fitted$city_id), all(cities$reclamation_area > 0))
b <- setNames(coef$estimate, coef$term); theta <- model$theta
predict_count <- function(reclamation_ha, city_area_km2) {
 mu <- exp(b["(Intercept)"] + b["reclamation"] * log1p(reclamation_ha) + b["area"] * log1p(city_area_km2))
 mu / (1 - dnbinom(0, size = theta, mu = mu))
}
reproduced <- predict_count(cities$reclamation_area, cities$area_km2)
stopifnot(max(abs(reproduced - fitted$fitted[match(cities$city_id, fitted$city_id)])) < 1e-7)
city_area_reference <- median(cities$area_km2)
area_grid <- exp(seq(log(min(cities$reclamation_area)), log(max(cities$reclamation_area)), length.out = 250))
curve <- data.frame(reclaimed_km2 = area_grid / 100,
 expected_richness = predict_count(area_grid, city_area_reference),
 reference_city_area_km2 = city_area_reference)
# Reconstruct the observed-information matrix at the saved estimate, without optimization.
X <- cbind(`(Intercept)` = 1, reclamation = log1p(cities$reclamation_area), area = log1p(cities$area_km2))
y <- cities$richness
saved_par <- c(b[colnames(X)], log(theta))
nll <- function(par) {
 mu <- exp(drop(X %*% par[1:3])); th <- exp(par[4])
 p0 <- dnbinom(0, mu = mu, size = th)
 -sum(dnbinom(y, mu = mu, size = th, log = TRUE) - log1p(-p0))
}
score <- function(par) {
 mu <- exp(drop(X %*% par[1:3])); th <- exp(par[4]); p0 <- dnbinom(0, mu = mu, size = th)
 beta <- drop(crossprod(X, th * (mu-y)/(th+mu) + p0/(1-p0)*th*mu/(th+mu)))
 lp <- digamma(y+th)-digamma(th)+log(th)-log(th+mu)+1-(th+y)/(th+mu)
 dp0 <- th*(-log1p(mu/th)+mu/(th+mu))
 c(beta, sum(-th*lp-p0/(1-p0)*dp0))
}
info <- optimHess(saved_par, nll, gr = score)
stopifnot(max(abs(score(saved_par))) < .01, min(eigen(info, symmetric = TRUE, only.values = TRUE)$values) > 0)
covariance <- solve(info)
saved_se <- coef$se[match(colnames(X), coef$term)]
stopifnot(max(abs(sqrt(diag(covariance))[1:3] - saved_se)) < 1e-5)
# Pointwise delta-method Wald limits for the conditional mean, on the log scale.
log_mean <- function(par) {
 eta <- par[1] + par[2] * log1p(area_grid) + par[3] * log1p(city_area_reference)
 mu <- exp(eta); th <- exp(par[4]); eta - log1p(-dnbinom(0, mu = mu, size = th))
}
eps <- 1e-5
gradient <- sapply(seq_along(saved_par), function(j) {
 hi <- lo <- saved_par; hi[j] <- hi[j] + eps; lo[j] <- lo[j] - eps
 (log_mean(hi) - log_mean(lo)) / (2 * eps)
})
se_log <- sqrt(pmax(0, rowSums((gradient %*% covariance) * gradient)))
curve$lower <- exp(log_mean(saved_par) - qnorm(.975) * se_log)
curve$upper <- exp(log_mean(saved_par) + qnorm(.975) * se_log)
stopifnot(all(curve$lower <= curve$expected_richness), all(curve$upper >= curve$expected_richness))
cities$reclaimed_km2 <- cities$reclamation_area / 100
pvalue <- subset(all_co, window == "common2010_2024")$p_two_sided_wald
stopifnot(length(pvalue) == 1L, abs(pvalue - 0.0490208648295022) < 1e-10)
write_source(cities[, c("city_id", "richness", "reclamation_area", "reclaimed_km2", "area_km2")],
 "observed_richness_city_display")
write_source(curve, "observed_richness_area_adjusted_curve")
# Grey points are unadjusted observations; the coloured curve holds city area fixed.
p <- ggplot() +
 geom_ribbon(data = curve, aes(reclaimed_km2, ymin = lower, ymax = upper), fill = "#B25950", alpha = .16) +
 geom_point(data = cities, aes(reclaimed_km2, richness), shape = 21, colour = "#65747B",
   fill = "#A7B4BA", stroke = .25, alpha = .54, size = 1.45) +
 geom_line(data = curve, aes(reclaimed_km2, expected_richness), colour = "#B25950", linewidth = .85) +
 expand_limits(y = 0) +
 annotate("text", x = .005, y = 345, label = sprintf("n = 107   P = %.4f", pvalue),
   family = font, size = 2.65, hjust = 0, colour = "#303A3F") +
 scale_x_log10(breaks = c(.01, .1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"),
   limits = c(.003, 400), expand = expansion(mult = 0)) +
 scale_y_continuous(breaks = c(0, 100, 200, 300, 400), expand = expansion(mult = c(0, .02))) +
 labs(x = expression("Reclaimed area ("*km^2*")"), y = "Recorded invasive plant species",
   caption = "Points: city records; line and shading: adjusted mean and 95% confidence interval.") +
 theme(plot.caption = element_text(size = 6.5, hjust = 0, colour = "#59656C"),
   plot.margin = margin(7, 8, 6, 7), legend.position = "none")
save_panel(p, "S02_observed_richness_and_geography", 92, paste(
 "Recorded invasive-plant richness and reclamation extent across 107 cities, 2010-2024.",
 "Each point is one city's unadjusted observed target-species count. The red curve shows",
 "the conditional mean from the saved zero-truncated negative-binomial model when city area",
 "is held at the sample median of 1,324.15 square kilometres. Shading shows pointwise 95% Wald",
 "confidence intervals for the conditional mean, calculated on the log-mean scale using the delta",
 "method and the full covariance matrix, including dispersion. The covariance matrix was reconstructed",
 "from the observed likelihood Hessian at the saved parameter estimates; its standard errors reproduce",
 "the saved values. Reclamation and city area enter",
 "as log(1 + area) in hectares and square kilometres, respectively. The plotted line was",
 "computed directly from saved coefficients and dispersion, without refitting. Reconstructed",
 "city-specific fitted means agree with the saved fitted values to numerical precision.",
 "The displayed nominal two-sided Wald P value tests the saved reclamation coefficient.",
 "This is one prespecified recording window; the remaining windows and their P values remain",
 "in Supplementary Table 12 and the plotted-source table. Points retain their actual varying",
 "city areas; the single curve is the adjusted prediction at the stated reference area."), width = 115)

# Geographic context is a standalone figure with its saved global test.
geo <- read_source("continental_residuals"); test <- read_source("continental_test")
lev <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
geo$region <- factor(geo$verified_continent, levels = rev(lev)); counts <- table(geo$verified_continent)
test <- subset(test, model == "M0"); stopifnot(nrow(geo) == 107L, nrow(test) == 1L)
write_source(geo, "observed_richness_geography_display")
p <- ggplot(geo, aes(residual, region)) +
 geom_vline(xintercept = 0, linetype = 2, linewidth = .3, colour = "#929BA2") +
 geom_boxplot(width = .4, orientation = "y", outlier.shape = NA,
   fill = "#EBF0F2", colour = "#587080", linewidth = .35) +
 geom_point(position = position_jitter(width = 0, height = .1, seed = 41),
   size = 1.1, alpha = .65, colour = "#5F8398") +
 scale_y_discrete(labels = function(x) paste0(x, " (", counts[x], ")")) +
 labs(title = "Geographic variation in recorded richness",
   subtitle = sprintf("107 cities | Global continental test: P = %.3f", test$p),
   x = "Recorded species below or above the baseline", y = NULL) +
 theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(size = 9))
save_panel(p, "observed_richness_geography", 98, paste(
 "Continental distributions of recorded-richness residuals after area adjustment.",
 "Points are cities; boxes and whiskers show medians, interquartile ranges and 1.5 IQR limits.",
 "Numbers in parentheses are city counts. The global P value is the saved continental",
 "likelihood-ratio test with country random intercepts. Pairwise Holm-adjusted P values are in Table 12."))

# A single area-adjusted observed-richness pathway decomposition.
obs <- read_source("observed_sem")
z <- subset(obs, case == "OBSERVED_M0" & effect %in% c("direct", "indirect", "total"))
stopifnot(nrow(z) == 3L)
z$effect_label <- factor(z$effect, levels = c("direct", "indirect", "total"),
 labels = c("Direct", "Indirect", "Total"))
write_source(z, "observed_richness_pathway_display")
p <- ggplot(z, aes(effect_label, estimate)) +
 geom_hline(yintercept = 0, colour = "#9CA5AB", linewidth = .35) +
 geom_errorbar(aes(ymin = lower, ymax = upper), width = .12, linewidth = .55, colour = "#689A9D") +
 geom_point(size = 2.6, colour = "#377E83") +
 labs(title = "Observed-richness pathways", subtitle = "City-area adjusted | 84 cities",
   x = NULL, y = "Standardized association") +
 theme(plot.title = element_text(size = 9), legend.position = "none")
save_panel(p, "observed_richness_pathways", 83, paste(
 "Direct, summed indirect and total associations in the area-adjusted standardized",
 "observed-richness component model. Points show saved estimates and intervals show saved",
 "95% percentile intervals from 1,999 city-bootstrap resamples over 84 cities. Null-distribution",
 "P values were not saved for these decompositions, and are not reconstructed from interval",
 "widths. Other model variants remain in the complete Supplementary Table 12."), width = 115)

}
for (entry in sources) stopifnot(identical(sha(entry$path), entry$sha256))
writeLines(capture.output(sessionInfo()), file.path(out, "species_richness_session.txt"))
write_json(list(status = "RENDERED_SAVED_ESTIMATES", backend = "R", no_model_refitting = TRUE,
  richness_interval = "pointwise delta-method Wald from reconstructed observed information", sources = sources, exports = exports,
  completed = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  file.path(out, "species_richness_render.json"), auto_unbox = TRUE, pretty = TRUE)
