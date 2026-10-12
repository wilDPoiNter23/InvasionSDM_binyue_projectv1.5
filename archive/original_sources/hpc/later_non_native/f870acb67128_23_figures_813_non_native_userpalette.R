# Original source: 23_figures_813_non_native_userpalette.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/23_figures_813_non_native_userpalette.R
# Original source SHA-256: f870acb67128941cc962ae27c0fde5a6b978bdb7ff6e72552034ef82c67e3974
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 23_figures_813_non_native_userpalette.R — plot the EXISTING frozen 813 non-native
# aggregate with the user colour scale (no re-aggregation).
# Figure contract: hero map; one claim — the 813-taxon non-native invasion potential
# is concentrated outside mapped native domains (systematically lower than global).
# Backend: R (ggplot2 + patchwork + ragg + svglite). Export 183 x 110 mm.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(ragg); library(svglite); library(terra)
})
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

FIG <- "<NON_NATIVE_ROOT>/publication_figures"
USER_PAL <- c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222")

theme_set(theme_classic(base_size = 6.5, base_family = "Helvetica") +
            theme(axis.line = element_line(linewidth = 0.35, colour = "black"),
                  axis.ticks = element_line(linewidth = 0.35, colour = "black"),
                  legend.title = element_text(size = 6.2),
                  legend.text = element_text(size = 5.8),
                  panel.grid = element_blank()))

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

r <- rast(AGG_OUT_RASTER)
rmax <- minmax(r)[2]
a <- aggregate(r, fact = 12, fun = "mean", na.rm = TRUE)
d <- as.data.frame(a, xy = TRUE, na.rm = TRUE)
names(d)[3] <- "value"

g <- ggplot(d, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colours = USER_PAL, na.value = "white",
                       name = "Mean normalized\nsuitability", limits = c(0, rmax)) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-60, 60, 30)) +
  map_theme() +
  labs(title = "Multi-species non-native invasion potential (813 taxa)",
       caption = "user palette: white -> green -> gold -> orange -> firebrick")

w <- 183 / 25.4; h <- 110 / 25.4
name <- "figure4_non_native_813_userpalette_v1"
svglite::svglite(file.path(FIG, paste0(name, ".svg")), width = w, height = h); print(g); dev.off()
grDevices::pdf(file.path(FIG, paste0(name, ".pdf")), width = w, height = h, family = "Helvetica"); print(g); dev.off()
ragg::agg_png(file.path(FIG, paste0(name, ".png")), width = w, height = h, units = "in", res = 600); print(g); dev.off()
cat("saved", name, "max=", rmax, "\n")
