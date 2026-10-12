# Original source: 22_figures_814_userpalette.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/22_figures_814_userpalette.R
# Original source SHA-256: cdf06da9d83c9a389f4e3245b01f7f5b8186182caf17c5fe0bd836474dec63a6
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 22_figures_814_userpalette.R — supplementary figure, user colour scale
# Figure contract:
#   Core conclusion: the 814-taxon non-native aggregate (813 + Solanum rostratum
#   add-on) is virtually identical to the 813 product (rostratum shifts only its
#   non-native range), while native-domain removal lowers suitability relative to
#   the global aggregate, mainly in native-rich regions.
#   Panels: a) hero map NN814 (user palette)  b) diff NN814-NN813 (rostratum)
#           c) diff NN813-GLOBAL (native-domain removal)
#   Archetype: quantitative grid. Backend: R (ggplot2/patchwork/ragg/svglite).
#   Export: SVG + PDF + 600-dpi PNG at 183 mm width.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(ragg); library(svglite); library(terra)
})
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

FIG <- "<NON_NATIVE_ROOT>/publication_figures"
SUPP <- file.path(DIR_AGG_RASTERS, "supplementary_814")

USER_PAL <- c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222")
DIVERGING <- c("#2166AC", "#F7F7F7", "#B2182B")

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

nn814 <- rast(file.path(SUPP, "multispecies_non_native_invasion_potential_814.tif"))
nn813 <- rast(AGG_OUT_RASTER)
glob  <- rast(FROZEN_AGG_RASTER)

mm814 <- minmax(nn814)[2]
d814 <- prep_df(nn814)
g1 <- ggplot(d814, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = USER_PAL, na.value = "white",
                       name = "Mean normalized\nsuitability", limits = c(0, mm814)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "Multi-species non-native invasion potential, 814 taxa (813 + Solanum rostratum)",
       caption = "user palette: white -> green -> gold -> orange -> firebrick")

# diffs
d_814_813 <- nn814 - nn813
d_nn_glob <- nn813 - glob
lim1 <- max(abs(minmax(d_814_813)))
lim2 <- max(abs(minmax(d_nn_glob)))
dd1 <- prep_df(d_814_813); dd2 <- prep_df(d_nn_glob)

g2 <- ggplot(dd1, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = DIVERGING, na.value = "white",
                       name = "814 \u2212 813", limits = c(-lim1, lim1)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "Contribution of Solanum rostratum add-on")

g3 <- ggplot(dd2, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = DIVERGING, na.value = "white",
                       name = "Non-native\n\u2212 global", limits = c(-lim2, lim2)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "Native-domain removal effect (non-native \u2212 global)")

fig <- g1 / (g2 | g3) + plot_layout(heights = c(1.15, 1)) +
  plot_annotation(
    title = "Non-native invasion potential: 814-taxon product and decomposition of differences",
    caption = "a: 814-taxon aggregate (user palette, capped at product max). b: 814 vs 813 (rostratum). c: 813 non-native vs global (native-domain removal).",
    theme = theme(plot.title = element_text(size = 7.5, face = "bold", hjust = 0.5),
                  plot.caption = element_text(size = 5.5, hjust = 0.5, colour = "grey30")))
save_pub(fig, "figure_non_native_814_userpalette_v1", 183, 210)
cat("DONE\n")
