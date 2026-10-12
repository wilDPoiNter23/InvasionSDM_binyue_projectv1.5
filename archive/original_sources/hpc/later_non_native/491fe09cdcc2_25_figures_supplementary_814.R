# Original source: 25_figures_supplementary_814.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/25_figures_supplementary_814.R
# Original source SHA-256: 491fe09cdcc23a3aeca1e41e93b92a059ebf915f7a40637ecbd24e74102e97e7
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 25_figures_supplementary_814.R — TASK D: figures only (reads finished rasters)
# Colour scale: power-0.65 transform (between linear and sqrt) — low values near-white.
# Outputs (publication_figures/):
#   figure_supplement_814_userpalette_v1.{svg,pdf,png}  (a) 814 map (b) 814-813 (c) 813nn-global
#   figure4_non_native_813_userpalette_v2.{svg,pdf,png} (813 map, adjusted scale)
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(ragg); library(svglite); library(scales); library(terra)
})
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

FIG <- "<NON_NATIVE_ROOT>/publication_figures"
SUPP <- file.path(DIR_AGG_RASTERS, "supplementary_814")
DIFF <- file.path(SUPP, "differences")

USER_PAL <- c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222")
DIVERGING <- c("#2166AC", "#F7F7F7", "#B2182B")
pow65 <- scales::trans_new("pow65",
  transform = function(x) x^0.65,
  inverse = function(x) x^(1/0.65))

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

map_theme <- function() {
  theme_void(base_size = 6.5, base_family = "Helvetica") +
    theme(axis.line = element_line(linewidth = 0.35),
          axis.ticks = element_line(linewidth = 0.3),
          axis.text = element_text(size = 5.5, colour = "grey30"),
          axis.title = element_blank(),
          legend.title = element_text(size = 6.2),
          legend.text = element_text(size = 5.8),
          legend.key.size = unit(0.32, "cm"),
          panel.grid = element_line(colour = "gray60", linetype = 3, linewidth = 0.2),
          plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 5.5, hjust = 0.5, colour = "grey30"))
}

prep_df <- function(r, fact = 12) {
  a <- aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
  d <- as.data.frame(a, xy = TRUE, na.rm = TRUE)
  names(d)[3] <- "value"
  d
}

pal_fill <- function(rmax) {
  scale_fill_gradientn(colours = USER_PAL, na.value = "white",
                       name = "Mean normalized\nsuitability",
                       limits = c(0, rmax), trans = pow65,
                       breaks = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, rmax),
                       labels = function(b) sprintf("%.2f", b))
}

# ---- data ----
nn814 <- rast(file.path(SUPP, "multispecies_non_native_invasion_potential_814.tif"))
nn813 <- rast(AGG_OUT_RASTER)
d_814_813 <- rast(file.path(DIFF, "diff_814_minus_813.tif"))
d_nn_glob <- rast(file.path(DIFF, "diff_813_non_native_minus_global.tif"))

rmax814 <- minmax(nn814)[2]
rmax813 <- minmax(nn813)[2]
lim1 <- max(abs(minmax(d_814_813)))
lim2 <- max(abs(minmax(d_nn_glob)))

# ---- 3-panel supplement figure ----
g1 <- ggplot(prep_df(nn814), aes(x = x, y = y, fill = value)) +
  geom_raster() + pal_fill(rmax814) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "a) Multi-species non-native invasion potential, 814 taxa",
       caption = "power-0.65 colour scale: white -> green -> gold -> orange -> firebrick")

g2 <- ggplot(prep_df(d_814_813), aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = DIVERGING, na.value = "white",
                       name = "814 \u2212 813", limits = c(-lim1, lim1)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "b) Contribution of Solanum rostratum add-on")

g3 <- ggplot(prep_df(d_nn_glob), aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = DIVERGING, na.value = "white",
                       name = "Non-native\n\u2212 global", limits = c(-lim2, lim2)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "c) Native-domain removal effect (813 non-native \u2212 global)")

fig <- g1 / (g2 | g3) + plot_layout(heights = c(1.15, 1)) +
  plot_annotation(
    title = "Supplementary: 814-taxon non-native invasion potential and decomposition of differences",
    theme = theme(plot.title = element_text(size = 7.5, face = "bold", hjust = 0.5)))
save_pub(fig, "figure_supplement_814_userpalette_v1", 183, 210)

# ---- 813 map with adjusted (sqrt) colour scale ----
g813 <- ggplot(prep_df(nn813), aes(x = x, y = y, fill = value)) +
  geom_raster() + pal_fill(rmax813) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "Multi-species non-native invasion potential (813 taxa)",
       caption = "power-0.65 colour scale: white -> green -> gold -> orange -> firebrick")
save_pub(g813, "figure4_non_native_813_userpalette_v3b", 183, 110)
cat("DONE\n")
