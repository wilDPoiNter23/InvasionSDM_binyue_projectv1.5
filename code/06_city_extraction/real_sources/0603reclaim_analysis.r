# Original source: 0603reclaim_analysis.r
# Original source location: <LOCAL_CODE_ROOT>/0603reclaim_analysis.r
# Original source SHA-256: ee31305c93396b7c48499ebe619967871b2bf4ff6ee1feb4546ee1a8d1186553
# Public-safe original snapshot: archive/original_sources/local/0603reclaim_analysis.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(sf)
library(terra)
library(data.table)
library(ggrepel)    # 可选：智能标注
library(patchwork)  # 可选：拼图
 library(dplyr)
 library(ggplot2)
 library(gghalves)
 library(ggalt)
install.packages('ggalt')
r <- terra::rast(publication_path("LOCAL_DATABASE_ROOT", "evaluations/hotspot_mean_suitability_normalized2.0.tif"))  # or .tif
reclamation_city_shp <- st_read(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/108finalCities.shp"))
cilp_Reclaim <- st_read(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/globalreclamation/cityClipToReclaim.shp"))
difference_Reclaim <-st_read(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/globalreclamation/difference_Reclaim.shp"))

csv_1  <- fread(publication_path("LOCAL_ANALYSIS_ROOT", "data/cityRisk_nonORrecalim/city_risk_mean_by_zone_fromGIS.csv"))
csv_2 <- fread(publication_path("LOCAL_ANALYSIS_ROOT", "data/10_03_108city_mean_variables.csv"))
csv_2$city <-  tolower(csv_2$city)
csv_1$city  <-  tolower(csv_1$city)
setdiff(csv_2$city, csv_1$city)
length(unique(csv_2$city))

# 统计每个城市出现的次数
city_counts <- table(csv_2$city)
#======caculate cities area ============
# 手动设置 Mollweide 投影字符串
moll_crs <- "+proj=moll +datum=WGS84 +units=m +no_defs"

# 转换投影
reclamation_city_proj <- st_transform(reclamation_city_shp, crs = moll_crs)

reclamation_city_proj <- reclamation_city_proj %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)
names(reclamation_city_proj)
area_table <- reclamation_city_proj %>%
  st_drop_geometry() %>%
  select(shapeName, area_km2)
write.csv(area_table, publication_path("LOCAL_ANALYSIS_ROOT", "data/reclamation_city_area.csv"), row.names = FALSE)
# 找出出现次数大于 1 的城市
duplicated_cities <- city_counts[city_counts > 1]

# 显示结果
print(duplicated_cities)

# 2) CRS 对齐 + 几何清理（只保留面更稳）
target_crs <- terra::crs(r)
cilp_Reclaim       <- st_transform(cilp_Reclaim, target_crs)       |> st_make_valid() |> st_collection_extract("POLYGON")
difference_Reclaim <- st_transform(difference_Reclaim, target_crs) |> st_make_valid() |> st_collection_extract("POLYGON")

# 3) 字段检查（要求两层都有 shapeName 作为城市名）
stopifnot("shapeName" %in% names(cilp_Reclaim), "shapeName" %in% names(difference_Reclaim))

# 4) 转成 terra 矢量以便提取
v_rec  <- terra::vect(cilp_Reclaim)         # 有多个要素，每个要素带 shapeName
v_non  <- terra::vect(difference_Reclaim)

# 5) 提取（注意：extract 返回的数据第1列是 ID，后面是栅格值列）
val_col <- names(r)[1]

rec_df <- terra::extract(r, v_rec, na.rm = TRUE, ID = TRUE)          # cols: ID, <raster_value>
if (!is.null(rec_df) && nrow(rec_df) > 0) {
  rec_df$city <- v_rec$shapeName[rec_df$ID]                           # 用 ID 映射回城市名
  rec_df$zone <- "Reclamation"
  rec_df$risk <- rec_df[[val_col]]
  rec_df <- rec_df[!is.na(rec_df$risk), c("city","zone","risk")]
} else {
  rec_df <- data.frame(city = character(), zone = character(), risk = numeric())
}

non_df <- terra::extract(r, v_non, na.rm = TRUE, ID = TRUE)
if (!is.null(non_df) && nrow(non_df) > 0) {
  non_df$city <- v_non$shapeName[non_df$ID]
  non_df$zone <- "NonReclamation"
  non_df$risk <- non_df[[val_col]]
  non_df <- non_df[!is.na(non_df$risk), c("city","zone","risk")]
} else {
  non_df <- data.frame(city = character(), zone = character(), risk = numeric())
}

# 6) 合并为像元级长表（与之前相同结构）
final_data <- rbindlist(list(rec_df, non_df), use.names = TRUE, fill = TRUE)

# === NEW: 6.5) 严格对齐 city × zone（每城两类都出现，即使像元为 0） ===
library(data.table)

# 完整城市清单（优先用你原始城市表；若对象不存在则退化为两层 union）
if (exists("reclamation_city_shp")) {
  all_cities <- sort(unique(reclamation_city_shp$shapeName))
} else {
  all_cities <- sort(unique(c(cilp_Reclaim$shapeName, difference_Reclaim$shapeName)))
}
ZONES <- c("Reclamation","NonReclamation")
base_cz <- CJ(city = all_cities, zone = ZONES, unique = TRUE)

# 像元级长表补齐缺失的 city-zone 组合（risk 为 NA 的占位行）
have_cz <- unique(final_data[, .(city, zone)])
need_cz <- fsetdiff(base_cz, have_cz)
if (nrow(need_cz) > 0) {
  add_rows <- need_cz[, .(city, zone, risk = as.numeric(NA))]
  final_data <- rbindlist(list(final_data, add_rows), use.names = TRUE, fill = TRUE)
}

# 7) 分区统计（像元数、均值、中位数），并与 base_cz 左连接补齐
stats_by_zone <- final_data[, .(
  n_pixels    = sum(!is.na(risk)),
  mean_risk   = if (sum(!is.na(risk)) > 0) round(mean(risk, na.rm = TRUE), 4) else NA_real_,
  median_risk = if (sum(!is.na(risk)) > 0) round(median(risk, na.rm = TRUE), 4) else NA_real_
), by = .(city, zone)]
stats_by_zone <- merge(base_cz, stats_by_zone, by = c("city","zone"), all.x = TRUE)
stats_by_zone[is.na(n_pixels), n_pixels := 0]

# 8) 生成与你之前一致的城市级汇总（保证每城都有两类像元数与均值列）
overall <- final_data[, .(
  total_pixels = sum(!is.na(risk)),
  mean_risk    = if (sum(!is.na(risk)) > 0) round(mean(risk, na.rm = TRUE), 4) else NA_real_,
  median_risk  = if (sum(!is.na(risk)) > 0) round(median(risk, na.rm = TRUE), 4) else NA_real_
), by = city]

cnt_wide  <- dcast(stats_by_zone, city ~ zone, value.var = "n_pixels", fill = 0)
setnames(cnt_wide, c("Reclamation","NonReclamation"),
                 c("reclamation_pixels","non_reclamation_pixels"))

mean_wide <- dcast(stats_by_zone, city ~ zone, value.var = "mean_risk")
setnames(mean_wide, c("Reclamation","NonReclamation"),
                   c("mean_risk_reclamation","mean_risk_non_reclamation"))

city_stats <- Reduce(function(x,y) merge(x,y, by = "city", all = TRUE),
                     list(overall, cnt_wide, mean_wide))

city_stats[, reclamation_percentage :=
             ifelse(total_pixels > 0,
                    round(100 * reclamation_pixels / total_pixels, 2), 0)]
city_stats[, risk_difference :=
             round(mean_risk_reclamation - mean_risk_non_reclamation, 4)]

# 9) 导出（文件名沿用你原来的；若想保留旧版可改为 *_complete.csv）
fwrite(final_data,   publication_path("LOCAL_ANALYSIS_ROOT", "data/city_risk_pixel_data_fromGIS.csv"))
fwrite(city_stats,   publication_path("LOCAL_ANALYSIS_ROOT", "data/city_risk_summary_fromGIS.csv"))
fwrite(mean_wide,    publication_path("LOCAL_ANALYSIS_ROOT", "data/city_risk_mean_by_zone_fromGIS.csv"))
mean_wide <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/cityRisk_nonORrecalim/city_risk_mean_by_zone_fromGIS.csv"))
cat("Done. Files saved:\n",
    " - city_risk_pixel_data_fromGIS.csv (像元级，已补齐 city×zone)\n",
    " - city_risk_summary_fromGIS.csv (城市汇总，含两类像元数/均值/差值)\n",
    " - city_risk_mean_by_zone_fromGIS.csv (分区均值宽表)\n", sep = "")


df <- mean_wide
df2 <- df %>%
  mutate(
    diff = mean_risk_reclamation - mean_risk_non_reclamation
  ) %>%
  arrange(desc(diff))

head(df2, 10)  # 围填海均值明显更高的前十
tail(df2, 10)  # 非围填海更高的城市

ggplot(df, aes(x = mean_risk_non_reclamation, y = mean_risk_reclamation)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(alpha = 0.7) +
  labs(x = "Non-reclamation mean risk",
       y = "Reclamation mean risk",
       title = "City-wise mean risk: Reclamation vs Non-reclamation") +
  theme_minimal()
df_pair <- df %>% filter(!is.na(mean_risk_reclamation), !is.na(mean_risk_non_reclamation))


# 兼容不同列名：可能是 Reclamation/NonReclamation 或 mean_risk_* 命名
# 统一为 city, mean_risk_reclamation, mean_risk_non_reclamation
if (!"mean_risk_reclamation" %in% names(df) || !"mean_risk_non_reclamation" %in% names(df)) {
  # 尝试从 Reclamation / NonReclamation 重命名
  if (all(c("Reclamation", "NonReclamation") %in% names(df))) {
    df <- df %>% rename(mean_risk_reclamation = Reclamation,
                        mean_risk_non_reclamation = NonReclamation)
  } else {
    stop("找不到列：请确保表中包含 'mean_risk_reclamation' 和 'mean_risk_non_reclamation'（或 'Reclamation'/'NonReclamation'）。")
  }
}

# 保险起见，再做一次数值化（去掉可能的千分位/空格等）
numize <- function(x) readr::parse_number(as.character(x))
df <- df %>%
  mutate(
    mean_risk_reclamation     = numize(mean_risk_reclamation),
    mean_risk_non_reclamation = numize(mean_risk_non_reclamation)
  )

# 成对数据
df_pair <- df %>%
  filter(!is.na(mean_risk_reclamation), !is.na(mean_risk_non_reclamation)) %>%
  mutate(diff = mean_risk_reclamation - mean_risk_non_reclamation)
  
# 配对 t 检验
tt <- t.test(df_pair$mean_risk_reclamation, df_pair$mean_risk_non_reclamation, paired = TRUE)

# Wilcoxon
wt <- suppressWarnings(wilcox.test(df_pair$mean_risk_reclamation, df_pair$mean_risk_non_reclamation, paired = TRUE, exact = FALSE))



# 现在计算 Cohen's d（配对：差值的均值 / 差值的标准差）
cohens_d <- mean(df_pair$diff, na.rm = TRUE) / sd(df_pair$diff, na.rm = TRUE)

# 方向一致性
prop_rec_lower  <- mean(df_pair$diff < 0)  # Reclamation < NonReclamation
prop_rec_higher <- mean(df_pair$diff > 0)

# === 3) 导出一页摘要表 ===
summary_tbl <- tibble::tibble(
  n_cities_paired = nrow(df_pair),
  mean_diff       = mean(df_pair$diff),
  sd_diff         = sd(df_pair$diff),
  t_stat          = unname(tt$statistic),
  df_t            = unname(tt$parameter),
  p_t             = tt$p.value,
  ci_low          = tt$conf.int[1],
  ci_high         = tt$conf.int[2],
  p_wilcox        = wt$p.value,
  cohens_d        = cohens_d,
  prop_rec_lower  = prop_rec_lower,
  prop_rec_higher = prop_rec_higher
)

summary_tbl
readr::write_csv(summary_tbl, file.path(publication_path("LOCAL_ANALYSIS_ROOT", "data/10_04analysis_summary_city_mean_by_zone.csv")))

# === 4) 可视化 ===
# -------- 1) 统一的期刊风主题 --------

theme_journal <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid = element_blank(),
      axis.line  = element_line(linewidth = 0.4, colour = "black"),
      axis.ticks = element_line(linewidth = 0.4, colour = "black"),
      plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 6)),
      legend.position = "top",
      legend.title = element_blank()
    )
}

# 假设 df_pair 已有列：
# city, mean_risk_reclamation, mean_risk_non_reclamation
df_pair$diff <- df_pair$mean_risk_reclamation - df_pair$mean_risk_non_reclamation
# 取差异绝对值最大的前 N，避免过密
N <- min(30, nrow(df_pair))
plot_df <- df_pair[order(-abs(df_pair$diff)), ][1:N, ]
plot_df$city <- factor(plot_df$city, levels = rev(plot_df$city))  # 竖轴按排序

p_dumbbell <- ggplot(plot_df) +
  geom_segment(aes(x = mean_risk_non_reclamation, xend = mean_risk_reclamation,
                   y = city, yend = city),
               linewidth = 0.6, colour = "grey60") +
  geom_point(aes(x = mean_risk_non_reclamation, y = city),
             shape = 21, size = 2.6, stroke = 0.5, fill = "white", colour = "black") +
  geom_point(aes(x = mean_risk_reclamation, y = city),
             shape = 21, size = 2.6, stroke = 0.5, fill = "grey30", colour = "black") +
  labs(x = NULL, y = NULL,
       title = sprintf("Top %d cities: mean risk (Dumbbell)", N),
       subtitle = "White = Non-reclamation, Dark = Reclamation") +
  theme_journal()
p_dumbbell

ggsave("fig_dumbbell_topN.pdf", p_dumbbell, width = 4.6, height = 6.5, device = cairo_pdf)

# -------- 3) 差值排序条形图（|diff| 最大的前 30） --------
plot_df2 <- df_pair[order(df_pair$diff), ]
plot_df2$city <- factor(plot_df2$city, levels = plot_df2$city)

p_cleveland <- ggplot(plot_df2, aes(x = diff, y = city)) +
  geom_segment(aes(x = 0, xend = diff, yend = city), colour = "grey65", linewidth = 0.5) +
  geom_point(shape = 21, size = 2.4, stroke = 0.5,
             fill = "white", colour = "black") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4, colour = "grey50") +
  labs(x = "Mean difference (Rec − Nonrec)", y = NULL,
       title = "City-wise mean difference (Cleveland dot)") +
  theme_journal()
p_cleveland

ggsave("fig_cleveland_diff.pdf", p_cleveland, width = 4.2, height = 8.0, device = cairo_pdf)


# -------- 4) Bland–Altman 图（一致性/系统性偏差） --------
df_long <- reshape2::melt(
  df_pair[, c("city", "mean_risk_reclamation", "mean_risk_non_reclamation")],
  id.vars = "city",
  variable.name = "zone", value.name = "mean_risk"
)
df_long$zone <- factor(df_long$zone,
                       levels = c("mean_risk_non_reclamation","mean_risk_reclamation"),
                       labels = c("Non-reclamation","Reclamation"))

p_ecdf <- ggplot(df_long, aes(x = mean_risk, colour = zone)) +
  stat_ecdf(linewidth = 0.8) +
  scale_colour_manual(values = c("grey20","grey60")) +
  labs(x = "City mean risk", y = "ECDF",
       title = "Distribution of city-wise means (ECDF)") +
  theme_journal() +
  theme(legend.position = "right")
p_ecdf

pB <- ggplot(df_long, aes(x = zone, y = mean_risk, fill = zone)) +
  geom_half_violin(side = "l", alpha = 0.4, colour = NA) +
  geom_half_boxplot(width = 0.1, side = "r", outlier.shape = NA) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1.5) +
  labs(y = "Mean risk", x = NULL) +
  scale_fill_manual(values = c("Reclamation" = "#D95F02", "NonReclamation" = "#1B9E77")) +
  theme_minimal(base_size = 14)

  ggplot(df_pair, aes(y = reorder(city, mean_risk_non_reclamation))) +
  geom_dumbbell(aes(x = mean_risk_non_reclamation, xend = mean_risk_reclamation),
                size = 1.5, color = "grey80", 
                colour_x = "steelblue", colour_xend = "firebrick") +
  labs(x = "Mean Risk", y = "City", title = "Risk Change (Non vs Reclamation)") +
  theme_minimal(base_size = 12)

ggplot(df_diff_by_zone, aes(x = climate_zone, y = cohens_d)) +
  geom_col(fill = "tomato") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Cohen's d", x = NULL, title = "Effect size per climate zone") +
  theme_minimal()


df_slope <- df_pair |> 
  mutate(city = factor(city, levels = city[order(mean_risk_non_reclamation)]))

ggplot(df_slope, aes(x = "Non", xend = "Rec", y = mean_risk_non_reclamation, yend = mean_risk_reclamation)) +
  geom_segment(aes(group = city), colour = "grey60", alpha = 0.5) +
  geom_point(aes(x = "Non", y = mean_risk_non_reclamation), shape = 21, fill = "steelblue") +
  geom_point(aes(x = "Rec", y = mean_risk_reclamation), shape = 21, fill = "firebrick") +
  labs(x = NULL, y = "Mean Risk", title = "Risk Shift per City") +
  theme_minimal(base_size = 13)
# -------- 5) 可选：合并成一页（投稿草图排版用） --------



# ====== 统一期刊风主题 ======
theme_journal <- function(base_size = 11){
  theme_minimal(base_size = base_size) %+replace% theme(
    panel.grid = element_blank(),
    axis.line  = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks = element_line(linewidth = 0.4, colour = "black"),
    plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 6)),
    legend.position = "top",
    legend.title = element_blank()
  )
}

# ====== A. 配对斜线（Slope）=====
# 为了更整齐，按 NonReclamation 排序
df_slope <- df_pair %>%
  arrange(mean_risk_non_reclamation) %>%
  mutate(city = factor(city, levels = city))

pA <- ggplot() +
  geom_segment(data = df_slope,
               aes(x = 1, xend = 2,
                   y = mean_risk_non_reclamation, yend = mean_risk_reclamation),
               colour = "grey60", linewidth = 0.5, alpha = 0.6) +
  geom_point(data = df_slope, aes(x = 1, y = mean_risk_non_reclamation),
             shape = 21, size = 2.2, stroke = 0.5, fill = "white", colour = "black") +
  geom_point(data = df_slope, aes(x = 2, y = mean_risk_reclamation),
             shape = 21, size = 2.2, stroke = 0.5, fill = "grey30", colour = "black") +
  scale_x_continuous(breaks = c(1,2), labels = c("NonReclamation","Reclamation")) +
  labs(x = NULL, y = "Mean risk", title = "A. Paired comparison (slope)") +
  theme_journal()

# ====== B. 箱线 + 抖动 + 显著性标注 ======
ymax <- max(df_long$mean_risk, na.rm = TRUE)
pB <- ggplot(df_long, aes(x = zone, y = mean_risk)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, fill = "grey88", colour = "black") +
  geom_jitter(width = 0.08, alpha = 0.45, size = 1.6, shape = 16, colour = "grey25") +
  annotate("segment", x = 1, xend = 2, y = ymax + 0.02, yend = ymax + 0.02) +
  annotate("text", x = 1.5, y = ymax + 0.035, label = p_lab, size = 3.4) +
  labs(x = NULL, y = "City mean risk", title = "B. Distribution & significance") +
  coord_cartesian(ylim = c(min(df_long$mean_risk, na.rm = TRUE), ymax + 0.06)) +
  theme_journal()

# ====== C. Cleveland 差值图（全部城市，按差值排序）=====
df_clev <- df_pair %>% arrange(diff) %>% mutate(city = factor(city, levels = city))
pC <- ggplot(df_clev, aes(x = diff, y = city)) +
  geom_segment(aes(x = 0, xend = diff, yend = city), colour = "grey65", linewidth = 0.5) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5, fill = "white", colour = "black") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4, colour = "grey50") +
  labs(x = "Mean difference (Rec − Nonrec)", y = NULL, title = "C. City-wise differences") +
  theme_journal()

# ====== D. ECDF 分布对比（总体形状）=====
pD <- ggplot(df_long, aes(x = mean_risk, colour = zone)) +
  stat_ecdf(linewidth = 0.8) +
  scale_colour_manual(values = c("NonReclamation" = "grey20", "Reclamation" = "firebrick")) +
  labs(x = "City mean risk", y = "ECDF", title = "D. Distribution (ECDF)") +
  theme_journal() + theme(legend.position = "right")

# ====== 组合版式（Patchwork）======
layout <- "
AB
CD
"
combined <- pA + pB + pC + pD + plot_layout(design = layout)
combined
ggsave(publication_path("LOCAL_ANALYSIS_ROOT", "figure/cities_reclaim_non_diff.pdf"), combined, width = 4.2, height = 8.0, device = cairo_pdf)
ggsave(publication_path("LOCAL_ANALYSIS_ROOT", "figure/cities_reclaim_non_diff.jpg"), combined, width = 8.2, height = 20.0)



# >>> PRESSURE_FIGURES_START (updated 2026-03-15)
# ========= Publication-ready city-level pressure models and paired distributions ==========
model_df <- csv_1 %>%
  transmute(
    city = tolower(city),
    city_title = tools::toTitleCase(city),
    mean_pressure_non_reclaimed = readr::parse_number(as.character(mean_risk_non_reclamation)),
    mean_pressure_reclaimed = readr::parse_number(as.character(mean_risk_reclamation)),
    reclamation_area_ha = readr::parse_number(as.character(reclamation_area)),
    area_km2 = readr::parse_number(as.character(area_km2)),
    pressure = readr::parse_number(as.character(risk)),
    Regions = Regions,
    climate_zone = climate_zone
  ) %>%
  distinct(city, .keep_all = TRUE) %>%
  mutate(
    log_reclamation_area = log1p(reclamation_area_ha),
    log_city_area = log1p(area_km2),
    # Convert reclamation area from ha to km2 before calculating the fraction.
    reclamation_fraction = (reclamation_area_ha / 100) / area_km2,
    log_reclamation_fraction = log1p(reclamation_fraction),
    pressure_difference = mean_pressure_reclaimed - mean_pressure_non_reclaimed,
    diff_direction = ifelse(
      pressure_difference > 0,
      "Reclaimed > non-reclaimed",
      "Reclaimed <= non-reclaimed"
    ),
    gap_label = ifelse(
      rank(-abs(pressure_difference), ties.method = "first") <= 8,
      city_title,
      NA_character_
    ),
    fraction_flag = ifelse(reclamation_fraction > 1, "Fraction > 1", "Fraction <= 1")
  ) %>%
  filter(
    is.finite(mean_pressure_non_reclaimed),
    is.finite(mean_pressure_reclaimed),
    is.finite(reclamation_area_ha),
    is.finite(area_km2),
    area_km2 > 0,
    is.finite(reclamation_fraction),
    reclamation_fraction >= 0
  )

fraction_issue_tbl <- as.data.table(model_df) %>%
  .[reclamation_fraction > 1, .(
    city,
    city_title,
    reclamation_area_ha,
    area_km2,
    reclamation_fraction
  )]

model_df_fraction_sens <- model_df %>%
  filter(reclamation_fraction <= 1)

model_df_no_alex <- model_df %>%
  filter(city != "alexandria")

m_reclaim_area <- lm(mean_pressure_reclaimed ~ log_reclamation_area, data = model_df)
m_overall_reclaim_area <- lm(pressure ~ log_reclamation_area, data = model_df)
m_nonrec_city_area <- lm(mean_pressure_non_reclaimed ~ log_city_area, data = model_df)
m_overall_city_area <- lm(pressure ~ log_city_area, data = model_df)
m_diff_area <- lm(pressure_difference ~ log_reclamation_area, data = model_df)
m_diff_log_fraction <- lm(pressure_difference ~ log_reclamation_fraction, data = model_df)
m_diff_log_fraction_sens <- lm(
  pressure_difference ~ log_reclamation_fraction,
  data = model_df_fraction_sens
)
m_reclaimed_log_fraction <- lm(
  mean_pressure_reclaimed ~ log_reclamation_fraction,
  data = model_df
)
m_nonreclaimed_log_fraction <- lm(
  mean_pressure_non_reclaimed ~ log_reclamation_fraction,
  data = model_df
)
m_pressure_log_fraction <- lm(pressure ~ log_reclamation_fraction, data = model_df)

model_df <- model_df %>%
  mutate(
    pressure_fraction_resid = resid(m_pressure_log_fraction),
    pressure_fraction_label = ifelse(
      rank(-abs(pressure_fraction_resid), ties.method = "first") <= 6 |
        city == "alexandria",
      city_title,
      NA_character_
    )
  )

m_reclaimed_log_fraction_no_alex <- lm(
  mean_pressure_reclaimed ~ log_reclamation_fraction,
  data = model_df_no_alex
)
m_nonreclaimed_log_fraction_no_alex <- lm(
  mean_pressure_non_reclaimed ~ log_reclamation_fraction,
  data = model_df_no_alex
)
m_pressure_log_fraction_no_alex <- lm(
  pressure ~ log_reclamation_fraction,
  data = model_df_no_alex
)

model_df_no_alex <- model_df_no_alex %>%
  mutate(
    pressure_fraction_resid = resid(m_pressure_log_fraction_no_alex),
    pressure_fraction_label = ifelse(
      rank(-abs(pressure_fraction_resid), ties.method = "first") <= 6,
      city_title,
      NA_character_
    )
  )

model_specs <- list(
  list(
    model = m_reclaim_area,
    model_id = "mean_pressure_reclaimed ~ log1p(reclamation_area_ha)",
    response = "mean_pressure_reclaimed",
    predictor = "log1p(reclamation_area_ha)",
    dataset = "all_108_cities"
  ),
  list(
    model = m_overall_reclaim_area,
    model_id = "pressure ~ log1p(reclamation_area_ha)",
    response = "pressure",
    predictor = "log1p(reclamation_area_ha)",
    dataset = "all_108_cities"
  ),
  list(
    model = m_nonrec_city_area,
    model_id = "mean_pressure_non_reclaimed ~ log1p(area_km2)",
    response = "mean_pressure_non_reclaimed",
    predictor = "log1p(area_km2)",
    dataset = "all_108_cities"
  ),
  list(
    model = m_overall_city_area,
    model_id = "pressure ~ log1p(area_km2)",
    response = "pressure",
    predictor = "log1p(area_km2)",
    dataset = "all_108_cities"
  ),
  list(
    model = m_diff_area,
    model_id = "pressure_difference ~ log1p(reclamation_area_ha)",
    response = "pressure_difference",
    predictor = "log1p(reclamation_area_ha)",
    dataset = "all_108_cities"
  ),
  list(
    model = m_diff_log_fraction,
    model_id = "pressure_difference ~ log_reclamation_fraction",
    response = "pressure_difference",
    predictor = "log_reclamation_fraction",
    dataset = "all_108_cities"
  ),
  list(
    model = m_diff_log_fraction_sens,
    model_id = "pressure_difference ~ log_reclamation_fraction",
    response = "pressure_difference",
    predictor = "log_reclamation_fraction",
    dataset = "fraction_le_1_sensitivity"
  ),
  list(
    model = m_reclaimed_log_fraction,
    model_id = "mean_pressure_reclaimed ~ log_reclamation_fraction",
    response = "mean_pressure_reclaimed",
    predictor = "log_reclamation_fraction",
    dataset = "all_108_cities"
  ),
  list(
    model = m_nonreclaimed_log_fraction,
    model_id = "mean_pressure_non_reclaimed ~ log_reclamation_fraction",
    response = "mean_pressure_non_reclaimed",
    predictor = "log_reclamation_fraction",
    dataset = "all_108_cities"
  ),
  list(
    model = m_pressure_log_fraction,
    model_id = "pressure ~ log_reclamation_fraction",
    response = "pressure",
    predictor = "log_reclamation_fraction",
    dataset = "all_108_cities"
  )
)

tidy_lm_table <- function(model, model_id, response, predictor, dataset) {
  sm <- summary(model)
  ci <- confint(model)
  coef_tab <- as.data.frame(sm$coefficients)
  coef_tab$term <- rownames(coef_tab)
  rownames(coef_tab) <- NULL

  data.table(
    model_id = model_id,
    response = response,
    predictor = predictor,
    dataset = dataset,
    term = coef_tab$term,
    estimate = coef_tab$Estimate,
    std_error = coef_tab$`Std. Error`,
    statistic = coef_tab$`t value`,
    p_value = coef_tab$`Pr(>|t|)`,
    conf_low = ci[coef_tab$term, 1],
    conf_high = ci[coef_tab$term, 2],
    n = nobs(model),
    r_squared = sm$r.squared,
    adj_r_squared = sm$adj.r.squared,
    sigma = sm$sigma,
    f_statistic = unname(sm$fstatistic[1]),
    model_p_value = pf(
      sm$fstatistic[1],
      sm$fstatistic[2],
      sm$fstatistic[3],
      lower.tail = FALSE
    )
  )
}

glance_lm_table <- function(model, model_id, response, predictor, dataset) {
  sm <- summary(model)
  data.table(
    model_id = model_id,
    response = response,
    predictor = predictor,
    dataset = dataset,
    n = nobs(model),
    r_squared = sm$r.squared,
    adj_r_squared = sm$adj.r.squared,
    sigma = sm$sigma,
    f_statistic = unname(sm$fstatistic[1]),
    df_model = unname(sm$fstatistic[2]),
    df_residual = unname(sm$fstatistic[3]),
    model_p_value = pf(
      sm$fstatistic[1],
      sm$fstatistic[2],
      sm$fstatistic[3],
      lower.tail = FALSE
    ),
    aic = AIC(model),
    bic = BIC(model)
  )
}

model_coef_table <- rbindlist(
  lapply(
    model_specs,
    function(x) tidy_lm_table(x$model, x$model_id, x$response, x$predictor, x$dataset)
  ),
  use.names = TRUE,
  fill = TRUE
)

model_fit_table <- rbindlist(
  lapply(
    model_specs,
    function(x) glance_lm_table(x$model, x$model_id, x$response, x$predictor, x$dataset)
  ),
  use.names = TRUE,
  fill = TRUE
)

paired_tt <- t.test(
  model_df$mean_pressure_reclaimed,
  model_df$mean_pressure_non_reclaimed,
  paired = TRUE
)
paired_wt <- suppressWarnings(
  wilcox.test(
    model_df$mean_pressure_reclaimed,
    model_df$mean_pressure_non_reclaimed,
    paired = TRUE,
    exact = FALSE
  )
)

distribution_summary_tbl <- data.table(
  n_cities = nrow(model_df),
  n_fraction_le_1 = nrow(model_df_fraction_sens),
  mean_diff = mean(model_df$pressure_difference, na.rm = TRUE),
  sd_diff = sd(model_df$pressure_difference, na.rm = TRUE),
  cohens_d = mean(model_df$pressure_difference, na.rm = TRUE) /
    sd(model_df$pressure_difference, na.rm = TRUE),
  t_stat = unname(paired_tt$statistic),
  df_t = unname(paired_tt$parameter),
  p_t = paired_tt$p.value,
  ci_low = paired_tt$conf.int[1],
  ci_high = paired_tt$conf.int[2],
  p_wilcox = paired_wt$p.value,
  max_reclamation_fraction = max(model_df$reclamation_fraction, na.rm = TRUE),
  max_fraction_city = model_df$city_title[which.max(model_df$reclamation_fraction)]
)

coef_out <- publication_path("LOCAL_ANALYSIS_ROOT", "data/10_04_108city_pressure_model_coefficients.csv")
fit_out <- publication_path("LOCAL_ANALYSIS_ROOT", "data/10_04_108city_pressure_model_fit.csv")
dist_out <- publication_path("LOCAL_ANALYSIS_ROOT", "data/10_04_108city_pressure_distribution_summary.csv")
fraction_diag_out <- publication_path("LOCAL_ANALYSIS_ROOT", "data/10_04_108city_reclamation_fraction_diagnostics.csv")
fwrite(model_coef_table, coef_out)
fwrite(model_fit_table, fit_out)
fwrite(distribution_summary_tbl, dist_out)
fwrite(as.data.table(model_df), fraction_diag_out)

format_p_value <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  if (x < 0.001) {
    return("<0.001")
  }
  formatC(x, format = "f", digits = 3)
}

model_label <- function(model) {
  sm <- summary(model)
  slope <- sm$coefficients[2, "Estimate"]
  r2 <- sm$r.squared
  p_val <- sm$coefficients[2, "Pr(>|t|)"]
  paste0(
    "Slope = ", sprintf("%.3f", slope),
    "\nR2 = ", sprintf("%.3f", r2),
    "\nP = ", format_p_value(p_val)
  )
}

theme_nature_like <- function(base_size = 8) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      axis.line = element_line(linewidth = 0.4, colour = "black"),
      axis.ticks = element_line(linewidth = 0.4, colour = "black"),
      axis.ticks.length = grid::unit(1.5, "mm"),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.4),
      panel.background = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.box = "horizontal",
      legend.key = element_blank(),
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      plot.tag = element_text(face = "bold"),
      plot.margin = margin(6, 6, 6, 6)
    )
}

save_nature_figure <- function(plot_obj, base_name, width, height, fig_dir = publication_path("LOCAL_ANALYSIS_ROOT", "figure/city_reclaimed_vs_nonreclaimed")) {
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    file.path(fig_dir, paste0(base_name, ".pdf")),
    plot = plot_obj,
    width = width,
    height = height,
    bg = "white"
  )
  tryCatch(
    {
      ggsave(
        file.path(fig_dir, paste0(base_name, ".png")),
        plot = plot_obj,
        width = width,
        height = height,
        dpi = 600,
        bg = "white"
      )
    },
    error = function(e) {
      message("PNG export skipped for ", base_name, ": ", conditionMessage(e))
    }
  )
  tryCatch(
    {
      grDevices::jpeg(
        filename = file.path(fig_dir, paste0(base_name, ".jpg")),
        width = width,
        height = height,
        units = "in",
        res = 600,
        quality = 100,
        bg = "white"
      )
      print(plot_obj)
      dev.off()
    },
    error = function(e) {
      message("JPG export skipped for ", base_name, ": ", conditionMessage(e))
    }
  )
}

make_reg_panel <- function(df, x_var, y_var, x_lab, y_lab, model, point_fill, label_var = NULL) {
  panel_df <- df
  panel_df$x_value <- panel_df[[x_var]]
  panel_df$y_value <- panel_df[[y_var]]

  p <- ggplot(panel_df, aes(x = x_value, y = y_value)) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      linewidth = 0.7,
      colour = "black",
      fill = "grey85"
    ) +
    geom_point(
      shape = 21,
      size = 2.3,
      stroke = 0.35,
      colour = "black",
      fill = point_fill,
      alpha = 0.9
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = model_label(model),
      hjust = 1.02,
      vjust = 1.05,
      size = 2.5,
      colour = "black",
      lineheight = 1.0
    ) +
    labs(x = x_lab, y = y_lab) +
    theme_nature_like()

  if (!is.null(label_var)) {
    label_df <- subset(panel_df, !is.na(panel_df[[label_var]]))
    p <- p +
      ggrepel::geom_text_repel(
        data = label_df,
        aes(label = .data[[label_var]]),
        size = 2.3,
        family = "sans",
        min.segment.length = 0,
        box.padding = 0.18,
        point.padding = 0.12,
        segment.size = 0.25,
        seed = 123,
        max.overlaps = Inf
      )
  }

  p
}

make_gap_panel <- function(df, x_var, x_lab, model) {
  panel_df <- df
  panel_df$x_value <- panel_df[[x_var]]

  ggplot(panel_df, aes(x = x_value, y = pressure_difference)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4,
      colour = "grey50"
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      linewidth = 0.7,
      colour = "black",
      fill = "grey85"
    ) +
    geom_point(
      aes(fill = diff_direction),
      shape = 21,
      size = 2.4,
      stroke = 0.35,
      colour = "black",
      alpha = 0.9
    ) +
    ggrepel::geom_text_repel(
      data = subset(panel_df, !is.na(gap_label)),
      aes(label = gap_label),
      size = 2.3,
      family = "sans",
      min.segment.length = 0,
      box.padding = 0.18,
      point.padding = 0.12,
      segment.size = 0.25,
      seed = 123,
      max.overlaps = Inf
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = model_label(model),
      hjust = 1.02,
      vjust = 1.08,
      size = 2.6,
      colour = "black",
      lineheight = 1.0
    ) +
    scale_fill_manual(
      values = c(
        "Reclaimed > non-reclaimed" = "#C54A43",
        "Reclaimed <= non-reclaimed" = "#4C78A8"
      )
    ) +
    labs(
      x = x_lab,
      y = "Within-city pressure difference\n(reclaimed - non-reclaimed)"
    ) +
    theme_nature_like()
}

legacy_linear_fig <- (
  make_reg_panel(
    model_df,
    "log_reclamation_area",
    "mean_pressure_reclaimed",
    "Log reclamation area",
    "Mean reclaimed-zone pressure",
    m_reclaim_area,
    "#C59A6D"
  ) |
    make_reg_panel(
    model_df,
    "log_reclamation_area",
    "pressure",
    "Log reclamation area",
    "Overall mean city pressure",
    m_overall_reclaim_area,
    "#8FA3B6"
  )
) / (
  make_reg_panel(
    model_df,
    "log_city_area",
    "mean_pressure_non_reclaimed",
    "Log city area",
    "Mean non-reclaimed pressure",
    m_nonrec_city_area,
    "#8FB996"
  ) |
    make_reg_panel(
    model_df,
    "log_city_area",
    "pressure",
    "Log city area",
    "Overall mean city pressure",
    m_overall_city_area,
    "#B88C9E"
  )
) / (
  make_reg_panel(
    model_df_no_alex,
    "log_reclamation_fraction",
    "mean_pressure_reclaimed",
    "Log reclamation fraction",
    "Mean reclaimed-zone pressure",
    m_reclaimed_log_fraction_no_alex,
    "#D2B48C"
  ) |
    make_reg_panel(
      model_df_no_alex,
      "log_reclamation_fraction",
      "pressure",
      "Log reclamation fraction",
      "Overall mean city pressure",
      m_pressure_log_fraction_no_alex,
      "#7FA7C9",
      label_var = "pressure_fraction_label"
    )
) +
  plot_annotation(tag_levels = "A")

risk_gap_fig <- (
  make_gap_panel(
    model_df,
    "log_reclamation_area",
    "Log reclamation area",
    m_diff_area
  ) |
    make_gap_panel(
      model_df,
      "log_reclamation_fraction",
      "Log reclamation fraction",
      m_diff_log_fraction
    ) |
    make_gap_panel(
      model_df_fraction_sens,
      "log_reclamation_fraction",
      "Log reclamation fraction\n(outlier-adjusted)",
      m_diff_log_fraction_sens
    )
) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "top")

paired_long <- melt(
  as.data.table(
    model_df[, c("city", "mean_pressure_non_reclaimed", "mean_pressure_reclaimed")]
  ),
  id.vars = "city",
  variable.name = "zone",
  value.name = "mean_pressure"
)
paired_long[, zone := factor(
  zone,
  levels = c("mean_pressure_non_reclaimed", "mean_pressure_reclaimed"),
  labels = c("Non-reclaimed", "Reclaimed")
)]

ymax_box <- max(paired_long$mean_pressure, na.rm = TRUE)

box_jitter_fig <- ggplot(
  paired_long,
  aes(x = zone, y = mean_pressure, fill = zone)
) +
  geom_boxplot(
    width = 0.48,
    outlier.shape = NA,
    colour = "black",
    linewidth = 0.4,
    alpha = 0.85
  ) +
  geom_jitter(
    width = 0.08,
    height = 0,
    size = 1.4,
    shape = 21,
    stroke = 0.25,
    colour = "black",
    alpha = 0.6
  ) +
  annotate(
    "segment",
    x = 1,
    xend = 2,
    y = ymax_box + 0.02,
    yend = ymax_box + 0.02,
    linewidth = 0.4
  ) +
  annotate(
    "text",
    x = 1.5,
    y = ymax_box + 0.04,
    label = paste0("Paired t P = ", format_p_value(paired_tt$p.value)),
    size = 2.8
  ) +
  scale_fill_manual(
    values = c("Non-reclaimed" = "#5f90e9", "Reclaimed" = "#cf7ebb")
  ) +
  labs(x = NULL, y = "City mean pressure") +
  coord_cartesian(
    ylim = c(min(paired_long$mean_pressure, na.rm = TRUE), ymax_box + 0.06)
  ) +
  theme_nature_like()

ecdf_fig <- ggplot(
  paired_long,
  aes(x = mean_pressure, colour = zone)
) +
  stat_ecdf(linewidth = 0.9) +
  scale_colour_manual(
    values = c("Non-reclaimed" = "#5f90e9", "Reclaimed" = "#cf7ebb")
  ) +
  labs(x = "City mean pressure", y = "ECDF") +
  theme_nature_like() +
  theme(legend.position = "top")

distribution_fig <- (box_jitter_fig | ecdf_fig) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "top")

log_fraction_no_alex_fig <- (
  make_reg_panel(
    model_df_no_alex,
    "log_reclamation_fraction",
    "mean_pressure_reclaimed",
    "Log reclamation fraction",
    "Mean reclaimed-zone pressure",
    m_reclaimed_log_fraction_no_alex,
    "#D2B48C"
  ) |
    make_reg_panel(
      model_df_no_alex,
      "log_reclamation_fraction",
      "mean_pressure_non_reclaimed",
      "Log reclamation fraction",
      "Mean non-reclaimed pressure",
      m_nonreclaimed_log_fraction_no_alex,
      "#8FB996"
    ) |
    make_reg_panel(
      model_df_no_alex,
      "log_reclamation_fraction",
      "pressure",
      "Log reclamation fraction",
      "Overall mean city pressure",
      m_pressure_log_fraction_no_alex,
      "#7FA7C9",
      label_var = "pressure_fraction_label"
    )
) +
  plot_annotation(
    tag_levels = "A",
    title = "Log reclamation fraction models (Alexandria excluded)"
  )

save_nature_figure(
  legacy_linear_fig,
  "nature_pressure_models_legacy_4panel",
  width = 7.4,
  height = 6.4
)
save_nature_figure(
  risk_gap_fig,
  "nature_pressure_difference_models_3panel",
  width = 10.8,
  height = 3.8
)
save_nature_figure(
  distribution_fig,
  "nature_reclaim_nonreclaim_pressure_distribution",
  width = 7.2,
  height = 3.8
)
save_nature_figure(
  log_fraction_no_alex_fig,
  "nature_log_reclamation_fraction_3models_no_alexandria",
  width = 10.8,
  height = 3.8,
  fig_dir = publication_path("LOCAL_ANALYSIS_ROOT", "figure/city_reclaimed_vs_nonreclaimed")
)

cat("\n===== 108-city pressure models =====\n")
print(model_fit_table)
print(model_coef_table)
print(distribution_summary_tbl)
print(fraction_issue_tbl)
print(legacy_linear_fig)
print(risk_gap_fig)
print(distribution_fig)
print(log_fraction_no_alex_fig)
# <<< PRESSURE_FIGURES_END


# ========== 划分围填海等级=======
# 假设你的数据叫 csv_2
# 使用自然断点法（Jenks）分为 3 类
jenks_breaks <- classInt::classIntervals(csv_2$reclamation_area, n = 3, style = "jenks")

# 查看分段断点
print(jenks_breaks$brks)

# 将每个值分到对应等级（Low = 1, Medium = 2, High = 3）
csv_2 <- csv_2 %>%
  mutate(reclamation_level = cut(reclamation_area,
                                 breaks = jenks_breaks$brks,
                                 include.lowest = TRUE,
                                 labels = c("Low", "Medium", "High")))
table(csv_2$reclamation_level)


# 原始值
area_values <- csv_2$reclamation_area

# K-Means 聚类（设置中心数 k = 3）
set.seed(123)  # 保证结果可重复
kmeans_result <- kmeans(area_values, centers = 3)

# 提取聚类标签（1, 2, 3），并按中心值从小到大排序映射为 Low / Medium / High
cluster_labels <- kmeans_result$cluster
cluster_centers <- kmeans_result$centers

# 映射标签为等级（可选：按中心从小到大映射为 Low, Medium, High）
label_map <- data.frame(cluster = 1:3, center = as.vector(cluster_centers)) %>%
  arrange(center) %>%
  mutate(level = c("Low", "Medium", "High"))

# 将聚类结果合并到原数据
csv_2 <- csv_2 %>%
  mutate(
    cluster = cluster_labels
  ) %>%
  left_join(label_map, by = c("cluster"))

# 查看结果
table(csv_2$level)
ggplot(csv_2, aes(x = reclamation_area, fill = level)) +
  geom_histogram(bins = 30, color = "black") +
  scale_fill_manual(values = c("Low" = "skyblue", "Medium" = "orange", "High" = "red")) +
  labs(title = "K-Means Clustering on Reclamation Area",
       x = "Reclamation Area", y = "Count") +
  theme_minimal()
write.csv(csv_2, publication_path("LOCAL_ANALYSIS_ROOT", "data/10_03_108city_mean_variables.csv"))
hist(csv_2$reclamation_area, breaks = 30, main = "Distribution of Reclamation Area")
csv_2 <- csv_2 %>%
  mutate(reclamation_level_manual = case_when(
    reclamation_area < 3000 ~ "Low",
    reclamation_area < 10000 ~ "Medium",
    TRUE ~ "High"
  ))
table(csv_2$reclamation_level_manual)

#==== accroding to reclamation fraction to classify
jenks_breaks <- classInt::classIntervals(city_df$recl_frac, n = 3, style = "jenks")
# 查看分段断点
print(jenks_breaks$brks)

# 将每个值分到对应等级（Low = 1, Medium = 2, High = 3）
city_df <- city_df %>%
  mutate(recla_level_byfraction = cut(recl_frac,
                                 breaks = jenks_breaks$brks,
                                 include.lowest = TRUE,
                                 labels = c("Low", "Medium", "High")))
table(city_df$recla_level_byfraction)
