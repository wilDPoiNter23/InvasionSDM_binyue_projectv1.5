# Original source: 030601projectPlot.r
# Original source location: <LOCAL_CODE_ROOT>/030601projectPlot.r
# Original source SHA-256: fd9eb057d86983fcf5d3f7a49900c279516d60ebb8cc7e6ee34a4d5afa501bcf
# Public-safe original snapshot: archive/original_sources/local/030601projectPlot.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(biomod2)
ensemble_projections <- load(publication_path("LOCAL_DATABASE_ROOT", "Spartina.alterniflora.ensemble_projection.ensemble.projection.out"))
ensemble_projections
plot(ensemble_projections)
library(terra)
library(rasterVis)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(sp)
library(ggExtra)
library(dplyr)
library(grid)
library(cowplot)
library(sf)
library(spdep)
library(spData)

r <- terra::rast(publication_path("LOCAL_DATABASE_ROOT", "evaluations/hotspot_mean_suitability_normalized2.0.tif"))  # or .tif
r <- terra::rast(publication_path("LOCAL_DATABASE_ROOT", "evaluations/richness_binary_map.tif"))  # or .tif

# 高风险区多边形
high_risk_poly <- vect("high_risk_poly.shp")

# 围填海区域 shapefile
reclamation_area <- vect(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/globalreclamation/reclamation.shp"))

setwd(publication_path("LOCAL_DATABASE_ROOT", "evaluations/"))
r_rescaled[r == 0] <- NA
r_rescaled <- projectRaster(r_rescaled, crs = CRS("+proj=longlat +datum=WGS84"))

r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
colnames(r_df)[3] <- "suitability"
colnames(r_df)[3] <- "richness"
colnames(r_df)[3] <- "value"
col_fun <- colorRampPalette(c("white", "#4CAF50", "#FFD700", "#FF8C00", "#B22222"))
cols <- col_fun(100)

pdf("hotspot_mean_with_normalized2.0.pdf", width = 6.67, height = 4.67) 
p <- plot(r, col = cols, main = "Habitat richness Hotspot",
     axes = TRUE,       # 显示经纬度坐标轴
     colNA = NA,
     box = TRUE)

# 添加经纬网格（graticule）
grid(nx = 10, ny = 10, col = "gray60", lty = 3)


grid.newpage()
plot(r, col = cols, main = "Habitat richness Hotspot", axes = TRUE, box = TRUE)
grid(nx = 10, ny = 10, col = "gray60", lty = 3)
p_grob <- grid.grab()


dev.off()
summary(r_df)

r <- terra::rast(publication_path("LOCAL_DATABASE_ROOT", "Spartina.alterniflora_EMcaByTSS_mergedData_mergedRun_mergedAlgo_TSSfilt.grd"))


world_map <- map_data("world")

SA_sdm <- ggplot() +
 geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
 fill = "gray80", color = "gray85", size = 0.2) +
 geom_raster(data = r_df, aes(x = x, y = y, fill = suitability)) +
 scale_fill_gradientn( colours = c( "#4CAF50", "#FFD700", "#FF8C00", "#B22222"),
  values = scales::rescale(c(0.01, 0.3, 0.6, 0.9, 1)),
   limits = c(0, 1),
    na.value = "transparent",
     name = "Suitability" ) +
 coord_fixed() +
 labs(
 title = "Species Distribution Model",
 x = "Longitude", y = "Latitude"
 ) +
 theme_minimal() +
 theme(
 panel.grid = element_blank(),
 legend.position = "right" )
SA_sdm


# 颜色设置
lon_fill <- "#A6CEE3"  # 蓝色系
lon_line <- "#1F78B4"
lat_fill <- "#FDB9B9"  # 红色系
lat_line <- "#E31A1C"

# 经度密度图
p1 <- ggplot(r_df, aes(x = x, weight = suitability)) +
  geom_density(fill = lon_fill, color = lon_line, alpha = 0.5, linewidth = 0.7, adjust = 1) +
  labs(x = "Longitude (°)", y = "Weighted Density", title = "Invasion Risk Density by Longitude") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "plain", size = 12)
  )
p1
ggsave("lon_density.pdf", plot = p1, width = 5, height = 3, units = "in", dpi = 300)

# 纬度密度图
p2 <- ggplot(r_df, aes(x = y, weight = suitability)) +
  geom_density(fill = lat_fill, color = lat_line, alpha = 0.5, linewidth = 0.7, adjust = 1) +
  labs(x = "Latitude (°)", y = "Weighted Density", title = "Invasion Risk Density by Latitude") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "plain", size = 12)
  )
ggsave("latitude_density.pdf", plot = p2, width = 5, height = 3, units = "in", dpi = 300)

 ggsave(publication_path("LOCAL_DATABASE_ROOT", "SA_sdm.png"), plot = SA_sdm, width = 10, height = 5, dpi = 300)


jenks <- classInt::classIntervals(r_df$value, n = n_class, style = "fisher")
r_df$class <- cut(r_df$value, breaks = jenks$brks, include.lowest = TRUE, labels = FALSE)
r_df$class_label <- cut(r_df$value, breaks = jenks$brks, include.lowest = TRUE)

# 4. 可视化
ggplot(r_df) +
  geom_raster(aes(x = x, y = y, fill = as.factor(class))) +
  scale_fill_brewer(palette = "YlOrRd", name = "Suitability\n(Jenks Class)") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Global Invasion Risk Map (Jenks Classification)",
       x = "Longitude", y = "Latitude")
       
values <- values(r, na.rm = TRUE)
jenks <- classInt::classIntervals(values, n = 4, style = "fisher")$brks



# 分类映射：0 为 NA，1-3 为三类
r_classified <- classify(r, rbind(
  c(-Inf, jenks[2], NA),   # 类别 0：低于最低阈值 → 设为 NA（透明）
  c(jenks[2], jenks[3], 1),
  c(jenks[3], jenks[4], 2),
  c(jenks[4], Inf, 3)
))

writeRaster(r_classified, "Fisher-Jenks Classification.tif", datatype = "INT1U", overwrite = TRUE)
# 设置颜色：类别 1–3 对应绿黄红，类别 0（NA）为透明
cols <- rev(RColorBrewer::brewer.pal(3, "RdYlGn"))  # 低风险→高风险

png("Fisher-Jenks Classification.png", width = 1800, height = 1400, res = 150)
pdf("Fisher-Jenks Classification.pdf", width = 6.67, height = 4.67, bg = "transparent")
# 绘图

# ==== 获取国界线 ====
world <- ne_countries(scale = "medium", returnclass = "sf")

# 转换为 terra 矢量对象（用于 base plot）
world_vect <- vect(world)

# ==== 绘图 ====
plot(r_classified,
     col = cols,
     legend = TRUE,
     axes = FALSE,
     main = "Global Invasion Risk (Fisher-Jenks Classification)",
     mar = c(2, 2, 3, 2))

# ==== 叠加国界线（灰色线，细线条） ====
lines(world_vect, col = "grey50", lwd = 0.5)

dev.off()
# 每个像元面积（默认单位为度，建议你是投影坐标）
freq_table <- freq(r_classified, digits = 0)
print(freq_table)


crs(r_classified)
res(r_classified)
area_raster <- terra::cellSize(r_classified, unit = "km")

area_by_class <- tapply(values(area_raster), values(r_classified), sum, na.rm = TRUE)

total_area <- sum(area_by_class)
round(100 * area_by_class / total_area, 2)
df_area <- data.frame(
  risk_level = factor(names(area_by_class), levels = c("1", "2", "3"),
                      labels = c("Low", "Medium", "High")),
  area_km2 = as.numeric(area_by_class)
)
df_area$percentage <- round(100 * df_area$area_km2 / sum(df_area$area_km2), 2)
ggplot(df_area, aes(x = risk_level, y = area_km2, fill = risk_level)) +
  geom_col() +
  scale_fill_manual(values = c("green", "yellow", "red")) +
  labs(title = "Area by Invasion Risk Level", x = "Risk Level", y = "Area (km²)") +
  theme_minimal()
print(jenks)


high_risk <- r_classified
high_risk[r_classified != 3] <- NA   # 非3设为NA
high_risk[r_classified == 3] <- 1    # 3设为1
high_risk_poly <- as.polygons(high_risk, dissolve = TRUE)
png("high_risk_poly.png", width = 1800, height = 1400, res = 150)
plot(high_risk_poly, col = "red", main = "High Invasion Risk Zones (Class 3)")
dev.off()

freq(r_classified)
output_shp <- publication_path("LOCAL_DATABASE_ROOT", "evaluations/high_risk_poly.shp")
r_classified_poly <- as.polygons(r_classified, dissolve = TRUE)
writeVector(high_risk_poly, output_shp, overwrite = TRUE)


### Gi*（Getis-Ord Gi*）
# 转成点数据（每个像元中心点）
r_lowres <- aggregate(r, fact = 4, fun = mean, na.rm = TRUE)

r_pts <- as.points(r_lowres)
# --- 3. 转换为 sf 点对象 ---
r_df <- as.data.frame(r_lowres, xy = TRUE, na.rm = TRUE)
r_sf <- st_as_sf(r_df, coords = c("x", "y"), crs = crs(r))

# --- 4. 构建邻接权重矩阵（基于 k 邻居） ---
coords <- st_coordinates(r_sf)
k_nb <- knearneigh(coords, k = 8)
nb <- knn2nb(k_nb)
lw <- nb2listw(nb, style = "W")

# --- 5. 计算 Getis-Ord Gi* 指数 ---
gi <- localG(r_sf[[1]], lw)  # r_sf[[1]] 是第一个字段，即 risk 值

# --- 6. 合并 Gi* 结果 ---
r_sf$Gi_star <- as.numeric(gi)

# --- 7. 可视化热点地图 ---
ggplot(r_sf) +
  geom_sf(aes(color = Gi_star), size = 0.8) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, name = "Gi*"
  ) +
  labs(title = "Hotspot Analysis (Gi*) of Invasion Risk") +
  theme_minimal()

####proximity analysis with reclamation area
high_risk_clean <- makeValid(high_risk_poly)

reclaim_clean <- makeValid(reclamation_area)

crs(high_risk_proj)

crs(reclamation_area)

high_risk_proj <- terra::project(high_risk_clean, crs(reclaim_clean))

# 再做交集
intersection <- intersect(high_risk_proj, reclaim_clean)

# 如果是 list，则合并
if (inherits(intersection, "list")) {
  intersection <- do.call(rbind, intersection)
}

# 计算面积
area_km2 <- expanse(intersection, unit = "km")
total_area_km2 <- sum(area_km2, na.rm = TRUE)
print(total_area_km2)

pdf("reclaim.pdf", width = 6.67, height = 4.67)
plot(reclaim_clean, border = "blue", main = "Projected Overlay")
plot(high_risk_proj, col = "red", add = TRUE)

dev.off()
