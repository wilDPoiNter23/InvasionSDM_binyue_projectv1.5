#!/usr/bin/env Rscript
# Render paired city comparisons from saved city-level summaries.
suppressPackageStartupMessages({
  library(ggplot2); library(svglite); library(ragg); library(jsonlite); library(digest)
})
a <- commandArgs(TRUE)
stopifnot(length(a) == 1L, dir.exists(a[1]))
root <- a[1]
source_file <- file.path(root, "tables", "all_108_city_comparisons.csv")
stats_file <- file.path(root, "tables", "all_108_city_statistics.csv")
hash_file <- function(p) {
  con <- file(p, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(p, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
paths <- c(source_file, stats_file)
before <- vapply(paths, hash_file, "")
d <- read.csv(source_file, stringsAsFactors = FALSE)
st <- read.csv(stats_file, stringsAsFactors = FALSE)
stopifnot(nrow(d) == 108L, !anyDuplicated(tolower(d$city)))
paired <- d[complete.cases(d[, c("OUTSIDE", "CITY", "RECLAIMED", "OTHER_URBAN")]), ]
whole <- d[complete.cases(d[, c("OUTSIDE", "CITY")]), ]
stopifnot(nrow(whole) == 108L, nrow(paired) == 85L)
assemble <- function(q, field, zone) data.frame(city = q$city, zone = zone, value = q[[field]])
data <- rbind(assemble(whole, "OUTSIDE", "OUTSIDE"), assemble(whole, "CITY", "CITY"),
  assemble(paired, "OTHER_URBAN", "OTHER_URBAN"), assemble(paired, "RECLAIMED", "RECLAIMED"))
data$zone <- factor(data$zone, levels = c("OUTSIDE", "CITY", "OTHER_URBAN", "RECLAIMED"))
stopifnot(all(is.finite(data$value)), all(data$value >= 0), max(data$value) < .77)
zones <- c("OUTSIDE", "CITY", "OTHER_URBAN", "RECLAIMED")
positions <- data.frame(x = c(1, 3, 2, 1, 2, 1), xend = c(2, 4, 3, 3, 4, 4),
                        y = c(.74, .74, .81, .88, .95, 1.02))
pairwise <- do.call(rbind, lapply(seq_len(nrow(positions)), function(i) {
  left <- zones[positions$x[i]]; right <- zones[positions$xend[i]]
  q <- if (left == "OUTSIDE" && right == "CITY") whole else paired
  test <- t.test(q[[left]], q[[right]], paired = TRUE)
  data.frame(first = left, second = right, n = nrow(q),
    mean_first = mean(q[[left]]), mean_second = mean(q[[right]]),
    mean_difference_first_minus_second = unname(test$estimate),
    ci_lower = unname(test$conf.int[1]), ci_upper = unname(test$conf.int[2]),
    p_raw = test$p.value)
}))
pairwise$p_holm <- p.adjust(pairwise$p_raw, method = "holm")
stopifnot(nrow(pairwise) == 6L, pairwise$n[1] == 108L, all(pairwise$n[-1] == 85L))
fmt_p <- function(p) if (p < .001) "P < 0.001" else sprintf("P = %.3f", p)
brackets <- positions
brackets$label <- vapply(pairwise$p_holm, fmt_p, "")
write.csv(pairwise, file.path(root, "tables", "all_108_six_pairwise_tests.csv"), row.names = FALSE)
palette <- c(OUTSIDE = "#9DA9AA", CITY = "#73529A", OTHER_URBAN = "#AD93BE", RECLAIMED = "#198D79")
lab <- c(OUTSIDE = "Outside city\n(n = 108)", CITY = "Whole city\n(n = 108)",
  OTHER_URBAN = "Other city land\n(n = 85)", RECLAIMED = "Reclaimed land\n(n = 85)")
p <- ggplot(data, aes(zone, value)) +
  geom_boxplot(aes(fill = zone), width = .52, outlier.shape = NA,
    alpha = .38, linewidth = .42, colour = "#3D4142") +
  geom_point(aes(colour = zone), position = position_jitter(width = .14, height = 0, seed = 20261027),
    size = 1.05, alpha = .65) +
  geom_segment(data = brackets, aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE, linewidth = .4, colour = "#303638") +
  geom_segment(data = brackets, aes(x = x, xend = x, y = y, yend = y - .018),
    inherit.aes = FALSE, linewidth = .4, colour = "#303638") +
  geom_segment(data = brackets, aes(x = xend, xend = xend, y = y, yend = y - .018),
    inherit.aes = FALSE, linewidth = .4, colour = "#303638") +
  geom_text(data = brackets, aes(x = (x + xend) / 2, y = y + .022, label = label),
    inherit.aes = FALSE, family = "Arial", size = 2.6, colour = "#303638") +
  scale_fill_manual(values = palette, guide = "none") +
  scale_colour_manual(values = palette, guide = "none") +
  scale_x_discrete(labels = lab, expand = expansion(add = .5)) +
  scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, .8, .2), expand = expansion(mult = c(0, .02))) +
  labs(tag = "d", x = NULL, y = "Predicted invasion suitability", caption = "P values adjusted using the Holm method") +
  theme_classic(base_family = "Arial", base_size = 8) +
  theme(axis.line = element_line(linewidth = .35), axis.ticks = element_line(linewidth = .35),
    axis.title.y = element_text(size = 8), axis.text = element_text(size = 7, colour = "#343A3C"),
    axis.text.x = element_text(lineheight = 1.05, margin = margin(t = 4)),
    plot.caption = element_text(size = 6.5, hjust = 0, margin = margin(t = 7)),
    plot.tag = element_text(size = 10, face = "bold"),
    plot.margin = margin(5, 7, 6, 7))
stem <- file.path(root, "figures", "original_surface_common_baseline")
svglite(paste0(stem, ".svg"), width = 120 / 25.4, height = 125 / 25.4, bg = "white"); print(p); dev.off()
cairo_pdf(paste0(stem, ".pdf"), width = 120 / 25.4, height = 125 / 25.4, family = "Arial"); print(p); dev.off()
agg_png(paste0(stem, ".png"), width = 120, height = 125, units = "mm", res = 300, background = "white"); print(p); dev.off()
stopifnot(identical(before, vapply(paths, hash_file, "")))
write_json(list(status = "EXPORTED", source_paths = paths, source_sha256 = as.list(before),
  source_values_unchanged = TRUE, model_refits = 0, plots = "boxplots and individual city points",
  sample_sizes = c(outside = 108, whole_city = 108, other_city_land = 85, reclaimed = 85),
  boxes = "Median, interquartile range; whiskers extend to most extreme observations within 1.5 IQR",
  points = "Each point is one city, jittered horizontally with a fixed seed",
  p_values = "Six two-sided paired t tests, Holm-adjusted across all six comparisons",
  pairwise_table = "all_108_six_pairwise_tests.csv"),
  file.path(root, "figures", "original_surface_boxplot_receipt.json"), pretty = TRUE, auto_unbox = TRUE)
print(pairwise)
message("Boxplot exported: ", stem)
