# Original source: 0501results_anaysis.r
# Original source location: <LOCAL_CODE_ROOT>/0501results_anaysis.r
# Original source SHA-256: 809ed9df6789d99b955ce04f9ebe6fca8518e76a513b07527872e5fd26a2c745
# Public-safe original snapshot: archive/original_sources/local/0501results_anaysis.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.

# ==== 📚 加载包 ====
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

out_dir <- "<LOCAL_DATABASE_ROOT>/continent_preview_risk_levels"
tif_dir <- "<LOCAL_DATABASE_ROOT>/continent_risk_tifs"

# ==== 📂 List all TIFs ====
tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

# ==== 📊 Store summary results ====
area_summary <- data.frame()
# ==== 🎨 Define color palette for risk classes ====
risk_colors <- c("1" = "#91bfdb", "2" = "#fee08b", "3" = "#d73027")
risk_labels <- c("Low", "Medium", "High")

# ==== 🔁 Loop over each file ====
for (tif in tif_files) {
  cat("🔎 Processing:", basename(tif), "\n")
  
  r <- rast(tif)
  continent <- tools::file_path_sans_ext(basename(tif))
  
  # # 🧮 Area calculation (per pixel, km²)
  r_area <- cellSize(r, unit = "km")
  
  # Extract values
  vals_r <- values(r)
  vals_a <- values(r_area)
  
  # Combine and remove NAs
  df <- data.frame(class = vals_r, area = vals_a)

  print(names(df))
 df <- df %>%
  rename(class = `Abutilon.grandifolium_PA1_RUN1_GLM`) %>%
  mutate(class = as.integer(class)) %>%
  filter(class %in% 1:3)

  # Summarize area by class
  df_sum <- df %>%
    group_by(class) %>%
    summarise(area_km2 = sum(area, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      class = as.integer(class),
      level = factor(class, levels = 1:3, labels = risk_labels),
      Continent = continent
    ) %>%
    select(Continent, level, area_km2)
  
  area_summary <- bind_rows(area_summary, df_sum)
  
  # # ==== 🖼️ Map output ====
  # # convert to data.frame for ggplot
  # df_r <- as.data.frame(r, xy = TRUE)
  # colnames(df_r)[3] <- "class"
  # df_r <- df_r %>% filter(!is.na(class))
  
  # p <- ggplot(df_r) +
  #   geom_raster(aes(x = x, y = y, fill = factor(class))) +
  #   scale_fill_manual(
  #     values = risk_colors,
  #     name = "Risk Level",
  #     labels = risk_labels,
  #     drop = FALSE
  #   ) +
  #   coord_equal() +
  #   labs(
  #     title = paste("Invasion Suitability -", continent),
  #     subtitle = "Risk classification: Low (1), Medium (2), High (3)",
  #     x = NULL, y = NULL
  #   ) +
  #   theme_minimal(base_family = "Times") +
  #   theme(
  #     axis.text = element_blank(),
  #     axis.ticks = element_blank(),
  #     legend.position = "bottom"
  #   )
  
  # ggsave(
  #   filename = file.path(out_dir, paste0(continent, "_risk_map.pdf")),
  #   plot = p, width = 8, height = 6
  # )
}

# === 转为数据框后统计 ===
print(area_summary)
cat("dataframe completed")
# ==== 📊 面积统计 ====
str(area_summary)
head(area_summary)
names(area_summary)
# ==== 🖼️ 出图 ====
# 先准备颜色（红-黄-绿，低=绿，高=红）

# ✅ 颜色设置：绿色-黄色-红色（和 RdYlGn 顺序一致）
# 颜色设定：与你地图配色保持一致
cols <- c("High" = "#D73027",     # 红色
          "Medium" = "#FEC44F",   # 橙色
          "Low" = "#66BD63")      # 绿色
# 保证等级顺序（红色在最上）
area_summary$level <- factor(area_summary$level, levels = c("High", "Medium", "Low"))

# 出图
p <- p1+ ggplot(area_summary, aes(x = Continent, y = area_km2 / 1e6, fill = level)) +
  geom_bar(stat = "identity", position = "stack", color = NA, linewidth = 0.3, width = 0.6, alpha = 0.7) +
  scale_fill_manual(values = cols, name = "Suitability Level", drop = FALSE) +
  labs(
    title = "Continental Invasion Suitability Area",
    x = NULL,
    y = "Area (million km²)"
  ) +
  theme_minimal(base_family = "Times") +
  scale_y_continuous(expand = c(0, 0)) +  # Y轴从0开始，无额外留白
  theme_minimal(base_family = "Times") +
theme(
  panel.background = element_rect(fill = "transparent", color = NA),   # 图形区域透明
  plot.background  = element_rect(fill = "transparent", color = NA),   # 整个图背景透明
  panel.grid       = element_blank(),                                  # 去掉网格线
  axis.line        = element_line(color = "black"),                    # 显示坐标轴
  axis.text.x      = element_text(angle = 30, hjust = 1, size = 12),
  axis.text.y      = element_text(size = 12),
  axis.title.y     = element_text(size = 13),
  plot.title       = element_text(size = 16, face = "bold", hjust = 0.5),
  legend.title     = element_text(size = 12),
  legend.text      = element_text(size = 11),
  legend.position  = "right"
)

p
  
# ==== 💾 输出为 PDF ====
ggsave("<LOCAL_DATABASE_ROOT>/continent_suitability_distribution.pdf", plot = p, width = 10, height = 6, dpi = 300, bg = "transparent")
cat("✅ Done! Plot saved to: continent_suitability_distribution.pdf\n")

# ==== 🌍 获取国家边界 ====
world <- ne_countries(scale = "medium", returnclass = "sf")

# ==== 🖼️ 绘图 ====
p1 <- ggplot(data = world) +
  geom_sf(fill = NA, color = "grey40", linewidth = 0.3) +
  theme_void() +
  coord_sf(expand = FALSE) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
p1+p
ggsave("<LOCAL_DATABASE_ROOT>/countries_distribution.pdf", plot = p1, width = 10, height = 6, dpi = 300, bg = "transparent")
