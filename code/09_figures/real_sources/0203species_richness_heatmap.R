# Original source: 0203species_richness_heatmap.R
# Original source location: <LOCAL_CODE_ROOT>/0203species_richness_heatmap.R
# Original source SHA-256: e55f75e4f35b0d9d0c9c14439091dbe5e86b21ac8308d8dba4c05b7801f1945c
# Public-safe original snapshot: archive/original_sources/local/0203species_richness_heatmap.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")

library(remotes)
install_github("Rdatatable/data.table")

install_github("tidyverse/ggplot2")

install_github("r-spatial/sf")

install_github("cran/doParallel")

install_github("rspatial/terra")
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
  library(parallel)
  library(doParallel)
  library(pbapply)
})
setwd(publication_path("HISTORICAL_PATH_003"))
library(scales)
library(ggrepel)
library(ggnewscale)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(svglite)
library(systemfonts)
library(Cairo)
library(ggspatial)
library(forcats)
library(stringr)
packageVersion("systemfonts")
package_version()

install.packages("Cairo")

input_file <- "data/species_geodatapoint_cleaned.csv" 
output_file <- "data/species_richness.csv"           
heatmap_file <- "data/species_richness_heatmap.png"


grid_resolution <- 0.1

num_cores <- detectCores() - 1 

species_geodatapoint_cleaned <- fread(publication_path("LOCAL_AUX_ROOT", "data/species_geodatapoint_cleaned.csv"))
city_df <- fread(publication_path("LOCAL_ANALYSIS_ROOT", "data/cityRisk_nonORrecalim/city_risk_mean_by_zone_fromGIS.csv"))

### create_grid 

species_geodatapoint_cleaned[, lon_bin := floor(decimalLongitude / grid_resolution) * grid_resolution + grid_resolution / 2]
species_geodatapoint_cleaned[, lat_bin := floor(decimalLatitude / grid_resolution) * grid_resolution + grid_resolution / 2]

unique_grids <- unique(species_geodatapoint_cleaned[, .(lon_bin, lat_bin)])
unique_grids[1, lon_bin]
richness <- uniqueN(species_geodatapoint_cleaned[lon_bin == unique_grids[2, lon_bin] & lat_bin ==unique_grids[2, lat_bin], species])

richness_results <- pblapply(seq_len(nrow(unique_grids)), function(i) {
  grid_lon <- unique_grids[i, lon_bin]  
  grid_lat <- unique_grids[i, lat_bin] 
  
  # 选择当前栅格的数据并计算丰富度
  richness <- uniqueN(species_geodatapoint_cleaned[lon_bin ==  grid_lon & lat_bin == grid_lat, species])
  
  
  return(data.table(lon_bin = grid_lon, lat_bin = grid_lat, species_richness = richness))
  
  rm(grid_lon)
  rm(grid_lat)
  gc() 
})

# 合并结果
richness_results <- rbindlist(richness_results)

saveRDS(richness_results, publication_path("LOCAL_DATABASE_ROOT", "RDS/richness_results.rds"))

richness_results <- readRDS(publication_path("LOCAL_DATABASE_ROOT", "RDS/richness_results.rds"))
  
# 颜色：低-中-高（与前文一致）
cols_risk <- c("1" = "#66BD63", "2" = "#FEC44F", "3" = "#D73027")

# —— 清洗城市点数据（按你表头改名，防止大小写/拼写问题）——
# 世界国界（sf）
world <- ne_countries(scale = "medium", returnclass = "sf")

# 整理城市表
city_df <- city_df |>
  rename(lon = Longitude, lat = Latitude, reclam_km2 = reclamation_area) |>
  mutate(
    risk_class = factor(risk_class),
    reclam_km2 = as.numeric(reclam_km2)
  ) |>
  filter(!is.na(lon), !is.na(lat), !is.na(reclam_km2))
# —— 叠加绘图 —— 
city_df$reclamation_level <- cut(city_df$reclam_km2,
                                 breaks = c(-Inf, 550, 1700, 6300, 12800, Inf),
                                 labels = c("1 - 550", "550 - 1700", "1700 - 6300", "6300 - 12800", ">12800"),
                                 right = TRUE)

top_cities <- city_df |> 
  dplyr::arrange(desc(reclam_km2)) |> 
  dplyr::slice(1:30)
top_cities
CairoPDF(publication_path("LOCAL_ANALYSIS_ROOT", "figure/study_area.pdf"), width = 12, height = 8)

# 精心挑的高级配色（低值浅紫到高值橙红）
rich_cols <- c(
  "#455F9E",  # 蓝灰
  "#6266b3",  # 蓝灰→暖棕过渡
  "#64a3b6",  # 中性灰褐
  "#B6AF64",  # 暖棕过渡
  "#CF8E4A",  # 暖棕
  "#e2592f"   # 红
)
# x 向量是你用于着色的连续变量，比如 richness_results$species_richness
x <- richness_results$species_richness

# 构造一个基于样本的 ECDF 变换
make_ecdf_trans <- function(x) {
  x <- x[is.finite(x)]
  Fwd <- stats::ecdf(x)
  Inv <- function(p) stats::quantile(x, probs = p, na.rm = TRUE, type = 7)
  scales::trans_new("ecdf", transform = function(z) Fwd(z), inverse = Inv,
                    domain = range(x, na.rm = TRUE))
}

ecdf_trans <- make_ecdf_trans(x)

p <- ggplot() +
  # —— 热图（连续 fill）——
  geom_tile(data = richness_results,
            aes(x = lon_bin, y = lat_bin, fill = species_richness)) +  scale_fill_gradientn(
    colours = rich_cols,
    trans   = ecdf_trans,         # 关键：把数据映射到均匀分布
    limits  = range(x, na.rm = TRUE),
    oob     = squish,
    breaks  = quantile(x, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE),
    labels  = label_number(accuracy = 1),
    name    = "Species richness"
  ) +

  # —— 国界线（更细、更淡）——
  geom_sf(data = world, fill = NA, color = "#5a5c6d", linewidth = 0.2) +

  # —— 开第二套 fill 给点图 —— 
  ggnewscale::new_scale_fill() +

  # —— 城市气泡（更明显）——
  geom_point(data = city_df,
           aes(x = lon, y = lat, size = reclamation_level),
           shape = 21, colour = "#423e3e", fill = "#E1C59D", stroke = 0.2, alpha = 0.75) +
scale_size_manual(name = "Total reclaimed land 2000–2020 by city (ha)",
                  values = c(2, 4, 6, 8, 10)) + # 可根据视觉效果调整
  scale_fill_brewer(palette = "Set2", name = "Risk class") +
  # geom_text(data = top_cities,
  #         aes(x = lon, y = lat, label = city),
  #         size = 3.5, family = "", fontface = "bold", color = "#f40707", check_overlap = TRUE)+


  # —— 坐标系 —— 
  coord_sf(crs = sf::st_crs(4326), xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +

  theme_minimal(base_family = "") +
  labs(title = "Species Richness with Reclamation Cities",
       x = "Longitude", y = "Latitude") +
  theme(
    panel.grid.major = element_blank(),  # 去除主网格线
    panel.grid.minor = element_blank(),  # 去除次网格线
    axis.text = element_text(size = 10, color = "black"),         # 去除坐标轴文字
    axis.title = element_blank(),        # 去除坐标轴标题
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # 黑色边框
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )
# —— 轴刻度标签函数（新增）——
lab_lon <- function(x) paste0(abs(x), "°", ifelse(x >= 0, "E", "W"))
lab_lat <- function(y) paste0(abs(y), "°", ifelse(y >= 0, "N", "S"))

lon_breaks <- seq(105, 130, by = 15)   # 经线（你可以改成 10/15）
lat_breaks <- seq(15,  45,  by = 10)   # 纬线

p2 <- ggplot() +
  # —— 热图（连续 fill）——
  geom_tile(data = richness_results,
            aes(x = lon_bin, y = lat_bin, fill = species_richness)) +   scale_fill_gradientn(
    colours = rich_cols,
    trans   = ecdf_trans,         # 关键：把数据映射到均匀分布
    limits  = range(x, na.rm = TRUE),
    oob     = squish,
    breaks  = quantile(x, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE),
    labels  = label_number(accuracy = 1),
    name    = "Species richness"
  )+

  # —— 国界线（更细、更淡）——
  geom_sf(data = world, fill = NA, color = "#5a5c6d", linewidth = 0.2) +
  # —— 经纬线（新增，放在国界线之后，点图之前）——
  geom_vline(xintercept = lon_breaks, colour = "grey85", linewidth = 0.25) +
  geom_hline(yintercept = lat_breaks, colour = "grey85", linewidth = 0.25) +

  # —— 开第二套 fill 给点图 —— 
  ggnewscale::new_scale_fill() +

  # —— 城市气泡（更明显）——
  geom_point(data = city_df,
           aes(x = lon, y = lat, size = reclamation_level),
           shape = 21,  colour = "#423e3e", fill = "#E1C59D", stroke = 0.2, alpha = 0.75) +
scale_size_manual(name = "Total reclaimed land 2000–2020 by city (ha)",
                  values = c(2, 4, 6, 8, 10)) + # 可根据视觉效果调整

  # geom_text(data = top_cities,
  #         aes(x = lon, y = lat, label = city),
  #         size = 3.5, family = "", fontface = "bold", color = "#f40707",, check_overlap = TRUE)+


  # —— 坐标系 —— 
  coord_sf(crs = sf::st_crs(4326), xlim = c(105, 130), ylim = c(15, 45), expand = FALSE) +

  # —— 轴刻度显示成 °E/°N（修改）——
  scale_x_continuous(breaks = lon_breaks, labels = lab_lon, expand = c(0,0)) +
  scale_y_continuous(breaks = lat_breaks, labels = lab_lat, expand = c(0,0)) +


  theme_minimal(base_family = "") +
  theme(
    panel.grid.major = element_blank(),  # 去除主网格线
    panel.grid.minor = element_blank(),  # 去除次网格线
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),        # 去除坐标轴标题
    panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),  # 黑色边框
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )




# 将两个图组合，并合并图例
combined_plot <- p + p2 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")  # 设置统一的图例位置

combined_plot
print(combined_plot)
dev.off()

  # 保存热图
  ggsave(publication_path("LOCAL_DATABASE_ROOT", "evaluations/Species_Richness_Heatmap.png"), plot = combined_plot, width = 12, height = 8, dpi = 300)
  

ggsave(publication_path("LOCAL_ANALYSIS_ROOT", "figure/study_area_nontext.pdf"), combined_plot, width = 12, height =8, device = cairo_pdf)

ggsave(publication_path("LOCAL_ANALYSIS_ROOT", "figure/study_area.svg"),
       plot = combined_plot,
       device = "svg",
       width = 12, height = 8,
       units = "in",
       dpi = 300)

#=====study_area_barchart ===============

# ---- 5) 颜色（左：城市数=深蓝；右：面积=琥珀橙）----
col_cities <- "#d1b7d3"  # 你之前喜欢的深蓝
col_area   <- "#E1C59D"  # 和之前提出的更搭的高级橙
#=======1) 数据清洗：去掉无洲别/南极/公海，去掉面积<=0  =====

recode_continent <- function(x){
  x0 <- tolower(trimws(x))
  dplyr::case_when(
    grepl("africa", x0) ~ "Africa",
    grepl("europe", x0) ~ "Europe",
    grepl("oceania|australia|zealand", x0) ~ "Oceania",
    grepl("south\\s*america", x0) ~ "South America",
    grepl("north\\s*america|caribbean|central\\s*america", x0) ~ "North America",
    grepl("asia|east asia|south asia|southeast|west asia|central asia|middle east", x0) ~ "Asia",
    TRUE ~ NA_character_
  )
}
CONT_LEVELS <- c("Africa","Asia","Europe","North America","South America","Oceania")

# city_df 需包含: city, Regions, reclamation_area(hm²)
df <- city_df %>%
  mutate(Continent = recode_continent(Regions)) %>%
  filter(!is.na(Continent)) %>%
  mutate(Continent = factor(Continent, levels = CONT_LEVELS),
         reclamation_area = as.numeric(reclamation_area)) %>%
  filter(is.finite(reclamation_area), reclamation_area >= 0)

# ---- 2) 左柱：各洲城市数（与你一致）----
count_by_cont <- df %>% distinct(city, Continent) %>% count(Continent, name = "n_cities")


# ---- 3) 右柱：各洲“总”围填海面积（hm²），不再堆叠 ----
area_total <- df %>%
  group_by(Continent) %>%
  summarise(total_hm2 = sum(reclamation_area), .groups = "drop")

# ---- 4) 缩放与右轴（分位数控制上限，避免极端值拉爆）----
# —— 缩放系数：把面积压缩到和城市数同一主轴范围 —— 
max_cities <- max(count_by_cont$n_cities, na.rm = TRUE)
max_area   <- max(area_total$total_hm2,    na.rm = TRUE)
s <- (max_cities * 0.95) / (max_area * 1.05)   # 顶部留 5% 余量；右轴从 0 开始

# —— 作图 —— 

p1 <- ggplot() +
  geom_col(
    data = area_total,
    aes(x = Continent, y = total_hm2 * s, fill = "Reclaimed area"),
    width = 0.42, position = position_nudge(x = +0.22),
    colour = "#00000000", linewidth = 0.35, alpha = 0.95
  ) +
  geom_text(
    data = area_total,
    aes(x = Continent, y = total_hm2 * s,
        label = scales::label_number(accuracy = 1, big.mark = ",")(total_hm2)),
    position = position_nudge(x = +0.22),
    vjust = -0.55, size = 3.3, colour = col_area, fontface = "bold"
  ) +
  geom_col(
    data = count_by_cont,
    aes(x = Continent, y = n_cities, fill = "Cities"),
    width = 0.42, position = position_nudge(x = -0.22),
    colour = "#00000000", linewidth = 0.35
  ) +
  geom_text(
    data = count_by_cont,
    aes(x = Continent, y = n_cities, label = n_cities),
    position = position_nudge(x = -0.22),
    vjust = -0.55, size = 3.3, colour = col_cities, fontface = "bold"
  ) +
  scale_y_continuous(
    name = "Cities",
    expand = expansion(mult = c(0, .12)),
    sec.axis = sec_axis(
      ~ . / s,
      name   = "Reclaimed area (hm\u00B2)",
      labels = function(x) scales::label_number(big.mark = ",", accuracy = 1)(x)
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Cities" = col_cities, "Reclaimed area" = col_area)
  ) +
  labs(x = NULL, title = "Cities vs. reclaimed area by continent (hm\u00B2)") +
  theme_classic(base_family = "") +
  theme(
    legend.position      = "top",
    panel.border         = element_rect(colour = "black", fill = NA, linewidth = .6),
    axis.title.y.left    = element_text(color = col_cities, face = "bold"),
    axis.title.y.right   = element_text(color = col_area,   face = "bold"),
    plot.title           = element_text(size = 13, face = "bold", hjust = 0)
  ) +
  coord_cartesian(clip = "off")


p1
# ggsave("con

sessionInfo()
find.package("scales"); packageVersion("scales")
getAnywhere(scales::train_continuous)
