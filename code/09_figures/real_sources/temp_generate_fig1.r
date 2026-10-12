# Original source: temp_generate_fig1.r
# Original source location: <LOCAL_CODE_ROOT>/temp_generate_fig1.r
# Original source SHA-256: a5c9f065c72de12359d77b12a4bd60f8e07cc48c3a27869f92f29ca0ce37c5f5
# Public-safe original snapshot: archive/original_sources/local/temp_generate_fig1.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(ggplot2)
library(patchwork)
library(dplyr)

res_all <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/city_vs_background_buffer_sensitivity.csv"))
wide <- reshape(res_all[, c("shapeName", "buffer_km", "DeltaP")],
  idvar = "shapeName",
  timevar = "buffer_km",
  direction = "wide"
)

plot_corr <- function(data, x_col, y_col, x_lab, y_lab, rho_val) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.6, color = "#2e63d6", size = 2) +
    geom_smooth(method = "lm", color = "#dd6e70", fill = "#dd6e70", alpha = 0.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    annotate("text",
      x = -Inf, y = Inf,
      label = paste0("Spearman's rho = ", round(rho_val, 2)),
      hjust = -0.1, vjust = 1.5, size = 4, fontface = "bold"
    ) +
    labs(x = x_lab, y = y_lab) +
    theme_bw(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      aspect.ratio = 1
    )
}

rho_10_50 <- cor(wide$DeltaP.10, wide$DeltaP.50, method = "spearman", use = "complete.obs")
rho_50_100 <- cor(wide$DeltaP.50, wide$DeltaP.100, method = "spearman", use = "complete.obs")
rho_10_100 <- cor(wide$DeltaP.10, wide$DeltaP.100, method = "spearman", use = "complete.obs")

p_a <- plot_corr(
  wide, "DeltaP.50", "DeltaP.10",
  expression(paste(Delta, "Pressure (50 km buffer)")),
  expression(paste(Delta, "Pressure (10 km buffer)")), rho_10_50
)

p_b <- plot_corr(
  wide, "DeltaP.50", "DeltaP.100",
  expression(paste(Delta, "Pressure (50 km buffer)")),
  expression(paste(Delta, "Pressure (100 km buffer)")), rho_50_100
)

p_c <- plot_corr(
  wide, "DeltaP.10", "DeltaP.100",
  expression(paste(Delta, "Pressure (10 km buffer)")),
  expression(paste(Delta, "Pressure (100 km buffer)")), rho_10_100
)

res_all$buffer_km_factor <- factor(res_all$buffer_km, levels = c(10, 50, 100), labels = c("10 km", "50 km", "100 km"))

p_d <- ggplot(res_all, aes(x = buffer_km_factor, y = DeltaP)) +
  geom_boxplot(fill = "#A0CBE8", alpha = 0.8, outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#d73027", linewidth = 0.8) +
  labs(
    x = "Buffer Radius",
    y = expression(paste(Delta, "Pressure (City - Background)"))
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
windows()
p_d
# Combine
final_plot <- (p_a + p_b + p_c) / p_d +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

dir.create(publication_path("LOCAL_ANALYSIS_ROOT", "figure/Fig.S"), showWarnings = FALSE)
ggsave(publication_path("LOCAL_ANALYSIS_ROOT", "figure/Fig.S/Extended_Data_Fig_1_Robustness.pdf"), final_plot, width = 12, height = 10, bg = "transparent")
ggsave(publication_path("LOCAL_ANALYSIS_ROOT", "figure/Fig.S/Extended_Data_Fig_1_Robustness.png"), final_plot, width = 12, height = 10, dpi = 600, bg = "transparent")
