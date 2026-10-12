# Original source: make_userpalette_and_diff.R
# Original source location: ${NON_NATIVE_ROOT}/temp/make_userpalette_and_diff.R
# Original source SHA-256: ec3aa973d82db98887ff6970d9a2f6f97b3483a203e48da19ea2bf8189f43c67
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(ragg); library(svglite)
  library(terra)   # data preparation only
})
FIG <- "<NON_NATIVE_ROOT>/publication_figures"
BASE <- "<WORK_ROOT>/results"

theme_set(theme_classic(base_size = 6.5, base_family = "Helvetica") +
            theme(axis.line = element_line(linewidth = 0.35, colour = "black"),
                  axis.ticks = element_line(linewidth = 0.35, colour = "black"),
                  legend.title = element_text(size = 6.2),
                  legend.text = element_text(size = 5.8),
                  panel.grid = element_blank()))

save_pub <- function(plot, name, w_mm, h_mm) {
  w <- w_mm / 25.4; h <- h_mm / 25.4
  svglite::svglite(file.path(FIG, paste0(name, ".svg")), width = w, height = h); print(plot); dev.off()
  grDevices::pdf(file.path(FIG, paste0(name, ".pdf")), width = w, height = h, family = "Helvetica"); print(plot); dev.off()
  ragg::agg_png(file.path(FIG, paste0(name, ".png")), width = w, height = h, units = "in", res = 600); print(plot); dev.off()
  cat("saved", name, "\n")
}

# ---- user colour scale (white -> green -> gold -> orange -> firebrick) ----
USER_PAL <- c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222")

prep_df <- function(r, fact = 12, fun = "mean") {
  a <- aggregate(r, fact = fact, fun = fun, na.rm = TRUE)
  d <- as.data.frame(a, xy = TRUE, na.rm = TRUE)
  names(d)[3] <- "value"
  d
}

map_theme <- function(graticule = TRUE) {
  theme_void(base_size = 6.5, base_family = "Helvetica") +
    theme(axis.line = element_line(linewidth = 0.35),
          axis.ticks = element_line(linewidth = 0.3),
          axis.text = element_text(size = 5.5, colour = "grey30"),
          axis.title = element_blank(),
          legend.title = element_text(size = 6.2),
          legend.text = element_text(size = 5.8),
          legend.key.size = unit(0.32, "cm"),
          panel.grid = if (graticule) element_line(colour = "gray60", linetype = 3, linewidth = 0.2) else element_blank(),
          plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 5.5, hjust = 0.5, colour = "grey30"))
}

# ================= 1. Figure 4 with user palette (lon/lat + graticule) =================
cur <- rast(file.path(BASE, "aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif"))
dcur <- prep_df(cur)
g_cur <- ggplot(dcur, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = USER_PAL, na.value = "white",
                       name = "Mean normalized\nsuitability", limits = c(0, 0.765)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme(graticule = TRUE) +
  labs(title = "Mean normalized suitability across 813 invasive plant taxa",
       caption = "user palette: white -> green -> gold -> orange -> firebrick")
save_pub(g_cur, "figure4_aggregate_map_userpalette_v4", 183, 110)

# ================= 2. Diff maps: current vs previous builds =================
prev <- rast(file.path(BASE, "aggregation_publication_fullcoverage_v1_20260810/hotspot_mean_suitability_normalized.tif"))
hist <- rast(file.path(BASE, "aggregation_813_final_20260729/hotspot_mean_suitability_normalized.tif"))

diff_prev <- cur - prev
diff_hist <- cur - hist
gd_prev <- global(diff_prev, fun = c("mean", "min", "max"), na.rm = TRUE)
gd_hist <- global(diff_hist, fun = c("mean", "min", "max"), na.rm = TRUE)

dd_prev <- prep_df(diff_prev)
dd_hist <- prep_df(diff_hist)

DIVERGING <- c("#2166AC", "#F7F7F7", "#B2182B")

lim_prev <- max(abs(c(gd_prev[1, 2], gd_prev[1, 3])))
lim_hist <- max(abs(c(gd_hist[1, 2], gd_hist[1, 3])))

p1 <- ggplot(dd_prev, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = DIVERGING, na.value = "white",
                       name = "Current \u2212\n2026-08-10", limits = c(-lim_prev, lim_prev)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "vs previous build (2026-08-10)")

p2 <- ggplot(dd_hist, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = DIVERGING, na.value = "white",
                       name = "Current \u2212\n2026-07-29", limits = c(-lim_hist, lim_hist)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "vs historical build (2026-07-29)")

g_diff <- p1 / p2 + plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Aggregate heatmap differences (current clean build minus earlier builds)",
                  caption = sprintf("left: mean diff %.2e (r = 0.9999968)   right: mean diff %.4f (r = 0.9927)",
                                    gd_prev[1, 1], gd_hist[1, 1]),
                  theme = theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
                                plot.caption = element_text(size = 5.5, hjust = 0.5, colour = "grey30")))
save_pub(g_diff, "qa_aggregate_heatmap_diff_v1", 183, 210)

cat(sprintf("diff vs 2026-08-10: mean=%.2e min=%.5f max=%.5f\n", gd_prev[1,1], gd_prev[1,2], gd_prev[1,3]))
cat(sprintf("diff vs 2026-07-29: mean=%.5f min=%.5f max=%.5f\n", gd_hist[1,1], gd_hist[1,2], gd_hist[1,3]))
cat("DONE\n")
