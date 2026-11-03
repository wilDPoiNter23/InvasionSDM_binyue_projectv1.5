# Render supplementary pathway and scale figures from existing fitted models.
# Usage: Rscript render_pathway_scale_figures.R configuration.json [S14|S17|S18]
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(mgcv)
  library(jsonlite)
  library(svglite)
  library(ragg)
  library(digest)
})
args <- commandArgs(TRUE)
stopifnot(length(args) %in% 1:2)
cfg <- fromJSON(args[1])
selection <- if (length(args) == 2) args[2] else "all"
stopifnot(selection %in% c("all", "S14", "S17", "S18"), capabilities("cairo"))
out <- cfg$output_root
if (selection == "S14" && is.null(cfg$current)) cfg$current <- dirname(out)
stopifnot(dir.exists(out))
for (d in c("source_data", "preview")) dir.create(file.path(out, d), showWarnings = FALSE)
font <- "Arial"
labels <- c(FULL_814 = "FULL | 814 species", FULL_684 = "Paired FULL | 684 species",
            ENV_684 = "ENV | 684 species")
response_keys <- c(FULL_814 = "ARCHIVED_GAM_RISK", FULL_684 = "PAIRED_FULL", ENV_684 = "PAIRED_ENV")
colours <- c(FULL_814 = "#40454B", FULL_684 = "#447C9D", ENV_684 = "#C5824B")
city_colours <- c("Small (P10)" = "#447C9D", "Medium (P50)" = "#88959B", "Large (P90)" = "#B57353")
inputs <- list()
exports <- list()
captions <- list()
numeric_checks <- list()
hash_file <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(path, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
remember <- function(path) {
  stopifnot(file.exists(path))
  inputs[[path]] <<- hash_file(path)
  path
}
read_table <- function(path) read.csv(remember(path), stringsAsFactors = FALSE, check.names = FALSE)
write_source <- function(d, name) write.csv(d, file.path(out, "source_data", paste0(name, ".csv")),
                                           row.names = FALSE, na = "")
theme_set(theme_classic(base_family = font, base_size = 7) + theme(
  axis.text = element_text(size = 6.7, colour = "#343A40"),
  axis.title = element_text(size = 7), axis.line = element_line(linewidth = 0.3, colour = "#72777D"),
  axis.ticks = element_line(linewidth = 0.3, colour = "#72777D"),
  strip.background = element_blank(), strip.text = element_text(size = 7, face = "bold"),
  plot.title = element_text(size = 8, face = "bold", margin = margin(b = 5)),
  plot.subtitle = element_text(size = 6.7, colour = "#5E656C", margin = margin(b = 7)),
  plot.tag = element_text(size = 8, face = "bold"),
  plot.caption = element_text(size = 6.3, colour = "#5E656C", hjust = 0, lineheight = 1.15,
                            margin = margin(t = 9)),
  legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size = 6.8),
  legend.key.width = grid::unit(10, "mm"), panel.spacing = grid::unit(4, "mm"),
  plot.margin = margin(7, 8, 7, 8)
))
save_figure <- function(p, name, width, height, title, description) {
  base <- file.path(out, name)
  svglite(paste0(base, ".svg"), width = width / 25.4, height = height / 25.4, bg = "white")
  print(p); dev.off()
  cairo_pdf(paste0(base, ".pdf"), width = width / 25.4, height = height / 25.4, family = font)
  print(p); dev.off()
  agg_png(file.path(out, "preview", paste0(name, ".png")), width = width, height = height,
          units = "mm", res = 300, background = "white")
  print(p); dev.off()
  exports[[name]] <<- list(width_mm = width, height_mm = height,
                          pdf_sha256 = hash_file(paste0(base, ".pdf")),
                          svg_sha256 = hash_file(paste0(base, ".svg")))
  captions[[name]] <<- list(title = title, description = description)
  message("EXPORTED ", name)
}

if (selection %in% c("all", "S14")) {
  sem_root <- file.path(cfg$current, "city_component_sem")
  effects <- read_table(file.path(sem_root, "effects.csv"))
  edges <- read_table(file.path(sem_root, "edges.csv"))
  support <- read_table(file.path(sem_root, "supports.csv"))
  stopifnot(nrow(effects) == 24, nrow(edges) == 57, all(support$cities == 83),
            all(effects$successful_bootstrap == 1999))
  for (g in names(labels)) {
    e <- effects[effects$group == g, ]
    b <- function(response, predictor) {
      z <- edges[edges$group == g & edges$response == response & edges$predictor == predictor, ]
      stopifnot(nrow(z) == 1)
      z$estimate
    }
    expected <- c(direct = b("y", "recl"), via_soc = b("soc", "recl") * b("y", "soc"),
                  via_soil = b("soil", "recl") * b("y", "soil"),
                  via_marine_soil = b("marine", "recl") * b("soil", "marine") * b("y", "soil"),
                  via_soc_soil = b("soc", "recl") * b("soil", "soc") * b("y", "soil"),
                  via_soc_marine_soil = b("soc", "recl") * b("marine", "soc") *
                    b("soil", "marine") * b("y", "soil"))
    expected <- c(expected, indirect = sum(expected[-1]), total = sum(expected))
    stopifnot(max(abs(e$estimate - expected[e$effect])) < 1e-12)
  }
  effects <- effects[effects$group %in% c("FULL_814", "ENV_684"), , drop = FALSE]
  edges <- edges[edges$group %in% c("FULL_814", "ENV_684"), , drop = FALSE]
  env_edges <- edges[edges$group == "ENV_684", , drop = FALSE]
  stopifnot(nrow(effects) == 16L, nrow(edges) == 38L, nrow(env_edges) == 19L, all(is.finite(edges$p)))
  nodes <- data.frame(id = c("recl", "soc", "marine", "soil", "y"),
    x = c(1, 3.7, 3.7, 6.7, 9.7), y = c(2.4, 4.2, .7, .7, 2.4),
    label = c("Reclamation", "Socioeconomic\nconditions", "Marine\nconditions",
      "Soil\nconditions", "Predicted\nsuitability"))
  paths <- data.frame(
    predictor = c("recl", "recl", "recl", "recl", "soc", "soc", "soc", "marine", "soil"),
    response = c("y", "soc", "marine", "soil", "y", "marine", "soil", "soil", "y"),
    x = c(1.8, 1.7, 1.7, 1.0, 4.5, 3.7, 4.35, 4.55, 7.35),
    y = c(2.4, 2.8, 2.0, 1.9, 4.0, 3.65, 3.7, .7, 1.15),
    xend = c(8.85, 2.85, 2.85, 6.7, 8.85, 3.7, 6.0, 5.85, 8.95),
    yend = c(2.4, 3.8, 1.1, .12, 2.85, 1.25, 1.25, .7, 1.98),
    lx = c(7.65, 2.05, 2.05, 5.3, 6.55, 3.65, 5.6, 5.2, 8.15),
    ly = c(2.65, 3.55, 1.32, -.06, 3.75, 2.98, 2.07, 1.02, 1.17))
  paths <- merge(paths, env_edges[, c("predictor", "response", "estimate", "p")],
    by = c("predictor", "response"), all.x = TRUE, sort = FALSE)
  stopifnot(nrow(paths) == 9L, !anyNA(paths$estimate), !anyNA(paths$p))
  paths$sign <- ifelse(paths$estimate >= 0, "Positive", "Negative")
  paths$sig <- ifelse(paths$p < .05, "P < 0.05", "P >= 0.05")
  paths$label <- sprintf("%+.2f%s", paths$estimate, ifelse(paths$p < .05, "*", ""))
  straight <- subset(paths, !(predictor == "recl" & response == "soil"))
  bend <- subset(paths, predictor == "recl" & response == "soil")
  network <- ggplot() +
    geom_segment(data = straight, aes(x = x, y = y, xend = xend, yend = yend,
      colour = sign, linetype = sig), linewidth = .65,
      arrow = grid::arrow(length = grid::unit(1.7, "mm"), type = "closed")) +
    geom_path(data = data.frame(x = c(1, 1, 6.7, 6.7), y = c(1.85, -.28, -.28, .12)),
      aes(x, y), colour = if (bend$sign == "Positive") "#497B8A" else "#AD665A",
      linetype = if (bend$sig == "P < 0.05") "solid" else "dashed", linewidth = .65,
      arrow = grid::arrow(length = grid::unit(1.7, "mm"), type = "closed")) +
    geom_rect(data = nodes, aes(xmin = x - .82, xmax = x + .82, ymin = y - .55, ymax = y + .55),
      fill = "white", colour = "#8F9EA5", linewidth = .45) +
    geom_text(data = nodes, aes(x, y, label = label), family = font, size = 2.4, lineheight = 1.05) +
    geom_label(data = paths, aes(lx, ly, label = label, colour = sign), family = font,
      size = 2.5, fill = "white", linewidth = 0, label.padding = grid::unit(.55, "mm")) +
    scale_colour_manual(values = c(Positive = "#497B8A", Negative = "#AD665A")) +
    scale_linetype_manual(values = c("P < 0.05" = "solid", "P >= 0.05" = "dashed")) +
    coord_cartesian(xlim = c(0, 10.8), ylim = c(-.65, 4.9), clip = "off") +
    labs(title = "ENV path coefficients", subtitle = "Solid: P < 0.05; dashed: P >= 0.05") +
    theme_void(base_family = font) + theme(legend.position = "none",
      plot.title = element_text(size = 8, face = "bold"),
      plot.subtitle = element_text(size = 6.7, colour = "#5E656C"), plot.margin = margin(6, 9, 5, 9))
  effect_order <- c("direct", "via_soc", "via_soil", "via_marine_soil", "via_soc_soil",
                    "via_soc_marine_soil", "indirect", "total")
  effect_names <- c("Direct", "Via socioeconomic conditions", "Via soil",
                    "Via marine conditions, then soil", "Via socioeconomic conditions, then soil",
                    "Via socioeconomic, marine, then soil", "All indirect routes", "Total")
  effects$position <- match(effects$effect, rev(effect_order))
  effects$group <- factor(effects$group, levels = c("FULL_814", "ENV_684"))
  forest <- ggplot(effects, aes(estimate, position, colour = group)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 2.5, fill = "#F1F3F4") +
    geom_vline(xintercept = 0, linetype = 2, colour = "#A2A8AD", linewidth = .3) +
    geom_segment(aes(x = lower, xend = upper, yend = position), linewidth = .65) +
    geom_point(aes(shape = effect %in% c("indirect", "total")), size = 1.8) +
    facet_grid(. ~ group, labeller = as_labeller(c(FULL_814 = "FULL | 814 species", ENV_684 = "ENV | 684 species"))) +
    scale_colour_manual(values = colours) + scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18)) +
    scale_y_continuous(breaks = 1:8, labels = rev(effect_names), limits = c(.5, 8.6),
                       expand = expansion(mult = 0)) +
    scale_x_continuous(breaks = c(-.4, -.2, 0, .2), limits = c(-.43, .28)) +
    labs(x = "Standardized pathway association", y = NULL,
         title = "Pathway estimates and 95% city-bootstrap intervals") +
    theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(), legend.position = "none",
          axis.text.y = element_text(size = 6.5), plot.title = element_text(size = 7.5))
  p <- network / forest + plot_layout(heights = c(1.15, 1)) +
    plot_annotation(title = "City-level pathways of continuous invasion suitability",
      subtitle = "83 cities | FULL and ENV response models",
      caption = "Panel a: standardized coefficients; * denotes P < 0.05. Panel b: estimates and 95% city-bootstrap intervals.",
      tag_levels = "a", theme = theme(plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 7), plot.caption = element_text(size = 6.2)))
  write_source(effects, "S14_continuous_sem_effects")
  write_source(edges, "S14_continuous_sem_edges")
  numeric_checks$sem_effects_reconstructed_from_existing_edges <- TRUE
  save_figure(p, "Supplementary_Fig_S14_continuous_SEM", 183, 185,
    "Supplementary Fig. 14 | City-level pathway decomposition of continuous invasion suitability.",
    paste("a, Standardized coefficients for the reclamation-linked paths in the ENV continuous",
      "suitability component model. Blue and red indicate positive and negative estimates; solid and",
      "dashed arrows distinguish unadjusted P < 0.05 and P >= 0.05 from two-sided coefficient t tests.",
      "Asterisks mark P < 0.05. Climate and city area were included in each component equation but are",
      "not shown as arrows. b, Direct, individual indirect, summed indirect and total associations.",
      "Points show estimates and bars indicate 95% percentile intervals from 1,999 city bootstrap",
      "resamples for FULL (814-species roster) and ENV (684-species roster). The tests in panel a",
      "and intervals in panel b use different methods. All variables were standardized, and both",
      "response models include the same 83 cities. These are continuous-response component models."))
}

if (selection %in% c("all", "S17", "S18")) {
  gam_root <- file.path(cfg$current, "city_gam")
  profiles <- read_table(file.path(gam_root, "endpoint_profiles.csv"))
  summaries <- read_table(file.path(gam_root, "model_summary.csv"))
  observed <- read_table(file.path(gam_root, "complete_city_input.csv"))
  model_config <- fromJSON(remember(file.path(dirname(args[1]), "city_gam.json")))
  reference <- read_table(model_config$city_input)
  verification <- fromJSON(remember(file.path(gam_root, "verification.json")))
  stopifnot(hash_file(model_config$city_input) == verification$source_sha256$city_input,
            nrow(observed) == 83, nrow(reference) == 108, nrow(summaries) == 12,
            all(summaries$n == 83))
  key <- function(x) tolower(trimws(x))
  reference <- reference[!key(reference$city) %in% key(model_config$exclude_city_keys), ]
  stopifnot(nrow(reference) == 107)
  reference$log_recl <- log1p(reference$reclamation_area)
  reference$log_area <- log1p(reference$area_km2)
  reference$true_fraction <- reference$reclamation_area / (100 * reference$area_km2)
  baseline <- data.frame(log_recl = median(reference$log_recl),
    true_fraction = median(reference$true_fraction), area_km2 = median(reference$area_km2),
    log_area = median(reference$log_area),
    anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE),
    climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
    soil_pc1 = median(reference$soil_pc1, na.rm = TRUE),
    marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
  common <- profiles[profiles$mode == "COMMON_ABSOLUTE_IQR", ]
  stopifnot(nrow(common) == 228, all(common$physically_within_city))
  fits <- list()
  prediction_error <- profile_error <- 0
  for (group in names(labels)) for (model in c("G0", "G1", "G2", "G3")) {
    response <- response_keys[[group]]
    fit <- readRDS(remember(file.path(gam_root, paste0(response, "_", model, ".rds"))))
    stopifnot(inherits(fit, "gam"), nrow(fit$model) == 83)
    fits[[paste(group, model)]] <- fit
    q <- common[common$response == response & common$model == model, ]
    for (i in seq_len(nrow(q))) {
      nd <- baseline[rep(1, 2), ]
      nd$area_km2 <- q$city_area_km2[i]; nd$log_area <- log1p(nd$area_km2)
      amounts <- c(q$reclamation_low_ha[i], q$reclamation_high_ha[i])
      if (model %in% c("G0", "G1")) nd$log_recl <- log1p(amounts) else
        nd$true_fraction <- amounts / (100 * q$city_area_km2[i])
      X <- predict(fit, nd, type = "lpmatrix")
      delta <- X[2, ] - X[1, ]
      estimate <- sum(delta * coef(fit))
      se <- sqrt(drop(delta %*% vcov(fit) %*% delta))
      profile_error <- max(profile_error, abs(c(estimate, estimate - 1.96 * se, estimate + 1.96 * se) -
                                                  c(q$difference[i], q$lower[i], q$upper[i])))
    }
  }
  stopifnot(profile_error < 1e-10)
  numeric_checks$maximum_saved_profile_error <- profile_error
  common$group <- names(response_keys)[match(common$response, response_keys)]
  common$group <- factor(common$group, levels = names(labels))
  common$estimate_points <- 100 * common$difference
  common$lower_points <- 100 * common$lower
  common$upper_points <- 100 * common$upper
  write_source(common, "S17_S18_saved_reclamation_contrasts")
}

if (selection %in% c("all", "S17")) {
  hull <- cbind(log1p(observed$reclamation_area), log1p(observed$area_km2))
  hull <- hull[chull(hull), , drop = FALSE]
  inside_hull <- function(x, y) {
    points <- cbind(x, y)
    vapply(seq_len(nrow(points)), function(i) {
      h <- rbind(hull, hull[1, ])
      cross <- (h[-1, 1] - h[-nrow(h), 1]) * (points[i, 2] - h[-nrow(h), 2]) -
        (h[-1, 2] - h[-nrow(h), 2]) * (points[i, 1] - h[-nrow(h), 1])
      all(cross >= -1e-10) || all(cross <= 1e-10)
    }, logical(1))
  }
  city_classes <- names(city_colours)
  curves <- list()
  for (group in names(labels)) for (j in seq_along(city_classes)) {
    profile <- common[common$group == group & common$model == "G0" &
                        abs(common$area_quantile - c(.1, .5, .9)[j]) < 1e-8, ]
    stopifnot(nrow(profile) == 1)
    area <- profile$city_area_km2
    amount <- exp(seq(log(min(reference$reclamation_area)),
                      log(min(max(reference$reclamation_area), 100 * area)), length.out = 181))
    nd <- baseline[rep(1, length(amount)), ]
    nd$log_recl <- log1p(amount); nd$true_fraction <- amount / (100 * area)
    nd$area_km2 <- area; nd$log_area <- log1p(area)
    m <- fits[[paste(group, "G0")]]
    pred <- predict(m, nd, type = "response", se.fit = TRUE)
    X <- predict(m, nd, type = "lpmatrix")
    prediction_error <- max(prediction_error, max(abs(pred$fit - drop(X %*% coef(m)))))
    supported <- inside_hull(log1p(amount), rep(log1p(area), length(amount)))
    run <- cumsum(c(TRUE, diff(supported) != 0))
    curves[[length(curves) + 1]] <- data.frame(group = group, city_class = city_classes[j],
      city_area_km2 = area, reclamation_km2 = amount / 100,
      fit = 100 * pred$fit, lower = 100 * (pred$fit - 1.96 * pred$se.fit),
      upper = 100 * (pred$fit + 1.96 * pred$se.fit), inside_hull = supported,
      run = paste(group, j, run, sep = "_"))
  }
  curves <- do.call(rbind, curves)
  stopifnot(prediction_error < 1e-10, all(is.finite(curves$fit)),
            all(curves$reclamation_km2 <= curves$city_area_km2 + 1e-8))
  curves$group <- factor(curves$group, levels = names(labels))
  curves$city_class <- factor(curves$city_class, levels = city_classes)
  within <- curves[curves$inside_hull, ]
  pa <- ggplot(curves, aes(reclamation_km2, fit, colour = city_class)) +
    geom_ribbon(data = within, aes(ymin = lower, ymax = upper, fill = city_class,
                                  group = interaction(city_class, run)), alpha = .11, colour = NA) +
    geom_line(aes(linetype = inside_hull, group = interaction(city_class, run)), linewidth = .65) +
    scale_colour_manual(values = city_colours) + scale_fill_manual(values = city_colours) +
    scale_linetype_manual(values = c(`FALSE` = "dashed", `TRUE` = "solid"), guide = "none") +
    scale_x_log10(breaks = c(.01, .1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) +
    labs(x = expression("Reclaimed area (km"^2*")"), y = "Predicted suitability (0-100 scale)",
         title = "Conditional curves for small, medium and large cities") +
    theme(legend.position = "bottom")
  q <- common[common$model == "G0", ]
  pb <- ggplot(q, aes(city_area_km2, estimate_points, colour = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = .3, colour = "#A2A8AD") +
    geom_ribbon(aes(ymin = lower_points, ymax = upper_points), alpha = .14, colour = NA) +
    geom_line(linewidth = .65) + geom_point(size = .65) +
    scale_colour_manual(values = colours) + scale_fill_manual(values = colours) +
    scale_x_log10(breaks = c(100, 1000, 10000), labels = c("100", "1,000", "10,000")) +
    labs(x = expression("City area (km"^2*")"),
         y = "Suitability difference (points)\nHigher minus lower reclamation",
         title = "Reclamation contrasts across the city-size gradient") + theme(legend.position = "none")
  p <- pa / pb + plot_layout(heights = c(1.25, 1)) +
    plot_annotation(title = "Reclamation-suitability curves across city sizes",
      subtitle = "83 cities | Area-by-city-size GAM",
      caption = paste("Top: solid curves and 95% bands within the observed area-space hull; dashed curves outside.\n",
                      "Bottom: higher-minus-lower reclamation contrasts and 95% intervals; zero means no difference.", sep = ""),
      tag_levels = "a", theme = theme(plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 7), plot.caption = element_text(size = 6.2)))
  write_source(curves, "S17_conditional_curves")
  write_source(data.frame(log1p_reclamation_ha = hull[, 1], log1p_city_km2 = hull[, 2]), "S17_observed_area_hull")
  numeric_checks$maximum_curve_prediction_error <- prediction_error
  save_figure(p, "Supplementary_Fig_S17_complete_GAM", 183, 164,
    "Supplementary Fig. 17 | City-size-dependent associations between reclamation and predicted suitability.",
    paste("a, Conditional predictions from the tensor-product GAM, G0, for small, medium and large",
          "cities at the 10th, 50th and 90th percentiles of the original 107-city reference distribution",
          "(106.0, 1,206.5 and 8,681.4 km2, respectively). Other predictor PC scores are held at the same",
          "reference medians used for the reclamation contrasts. Solid curves lie within the convex hull",
          "of log-transformed reclamation and city areas for the 83 fitted cities; shading shows pointwise",
          "95% model-based intervals in this region. Dashed curves extend outside this joint hull. Predictions",
          "with reclaimed area exceeding city area are omitted. b, Differences",
          "between higher and lower reclamation at 19 city sizes. At each size, the endpoints are the 25th and",
          "75th percentiles of reclamation amount among the 20 nearest cities in log city area, capped by city",
          "area. The endpoints therefore vary along the city-size gradient. Lines and ribbons show fitted",
          "contrasts and pointwise 95% coefficient-covariance intervals over these ranges.",
          "Columns distinguish the 814-species FULL roster from matched FULL and ENV rosters of 684 species.",
          "All fits use the same 83 complete-data cities. Suitability and differences are multiplied by 100",
          "for display. G0 includes a tensor smooth of log1p reclamation area in hectares and city area in km2,",
          "with smooth terms for socioeconomic, climatic, soil and marine PCs."))
}

if (selection %in% c("all", "S18")) {
  model_names <- c(G0 = "G0\nArea x city size", G1 = "G1\nArea + city size",
                   G2 = "G2\nShare + city size", G3 = "G3\nShare x city size")
  common$model_label <- factor(model_names[common$model], levels = unname(model_names))
  stopifnot(all(is.finite(common$lower_points)), all(is.finite(common$upper_points)))
  p <- ggplot(common, aes(city_area_km2, estimate_points, colour = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = 2, colour = "#A2A8AD", linewidth = .3) +
    geom_ribbon(aes(ymin = lower_points, ymax = upper_points), alpha = .15, colour = NA) +
    geom_line(linewidth = .65) + geom_point(size = .6) +
    facet_grid(model_label ~ group, labeller = labeller(group = as_labeller(labels)), switch = "y") +
    scale_colour_manual(values = colours) + scale_fill_manual(values = colours) +
    scale_x_log10(breaks = c(100, 1000, 10000), labels = c("100", "1,000", "10,000")) +
    scale_y_continuous(breaks = c(-6, -3, 0, 3)) +
    labs(x = expression("City area (km"^2*")"),
         y = "Suitability difference (points on a 0-100 scale)\nHigher minus lower reclamation",
         title = "Reclamation associations across area and proportion formulations",
         subtitle = "83 cities | Four formulas, evaluated at the same reclamation amounts within each city size",
         caption = paste("Share = reclaimed area / city area. 'x' denotes a tensor interaction; '+' denotes separate smooth terms.\n",
                         "Lines: fitted contrasts; bands: pointwise 95% intervals. All panels use the same axis scales.", sep = "")) +
    theme(legend.position = "none", strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, size = 6.8, hjust = 1),
          panel.spacing.y = grid::unit(4, "mm"), plot.title = element_text(size = 8.7))
  save_figure(p, "Supplementary_Fig_S18_area_proportion_formulations", 183, 184,
    "Supplementary Fig. 18 | Sensitivity of reclamation associations to area and proportion formulations.",
    paste("Rows show four GAM formulations: G0, a tensor-product smooth of log1p reclaimed",
          "area and city area; G1, separate smooths of log1p reclaimed area and log1p city area; G2, separate",
          "smooths of reclaimed-area proportion and log1p city area; and G3, a tensor-product smooth of",
          "reclaimed-area proportion and log1p city area. Reclaimed-area proportion is reclaimed area in km2",
          "divided by city area in km2. All formulations include the same socioeconomic, climatic, soil and",
          "marine PC smooths. Columns show FULL with its 814-species roster and matched FULL and ENV with",
          "684 species. All 12 fits use the same 83 complete-data cities. Lines and shading reproduce saved",
          "higher-minus-lower reclamation contrasts and their pointwise 95% coefficient-covariance intervals",
          "at 19 city sizes. At each size, identical absolute reclamation amounts are compared across formulas",
          "and response groups; these amounts are obtained from the neighbouring-city interquartile range",
          "used in Supplementary Fig. 17 and vary between city sizes. Zero denotes equal predicted suitability",
          "at the two reclamation amounts. Differences are multiplied by 100 for display and all panels have",
          "identical scales."))
}

for (path in names(inputs)) stopifnot(identical(hash_file(path), inputs[[path]]))
legend_text <- unlist(lapply(captions, function(x) c(x$title, x$description, "")), use.names = FALSE)
writeLines(legend_text, file.path(out, paste0("SUPPLEMENTARY_PATHWAY_SCALE_CAPTIONS_", selection, ".md")))
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
write_json(list(status = "RENDERED_FROM_EXISTING_MODELS", selection = selection,
                backend = "R", no_model_refits = TRUE, no_resampling = TRUE,
                script_sha256 = hash_file(script), input_sha256 = inputs,
                numerical_checks = numeric_checks, exports = exports, captions = captions),
           file.path(out, paste0("pathway_scale_render_", selection, ".json")),
           auto_unbox = TRUE, pretty = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, paste0("pathway_scale_session_", selection, ".txt")))
message("Completed all selected exports; existing model and table checksums unchanged.")
