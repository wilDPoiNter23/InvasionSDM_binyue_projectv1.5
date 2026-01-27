library(sf)
library(terra)
library(data.table)
library(ggrepel)    # Optional: smart labeling
library(patchwork)  # Optional: plot assembly
 library(dplyr)
 library(ggplot2)
 library(gghalves)
 library(ggalt)
install.packages('ggalt')
r <- terra::rast("E:/RcodeforTest/database/evaluations/hotspot_mean_suitability_normalized2.0.tif")  # or .tif
reclamation_city_shp <- st_read('E:/k_PhD_NKU/202407biomod/data/data/108finalCities.shp')
cilp_Reclaim <- st_read('E:/k_PhD_NKU/202407biomod/data/data/globalreclamation/cityClipToReclaim.shp')
difference_Reclaim <-st_read('E:/k_PhD_NKU/202407biomod/data/data/globalreclamation/difference_Reclaim.shp')

csv_1  <- fread("E:/k_PhD_NKU/202407biomod/data/cityRisk_nonORrecalim/city_risk_mean_by_zone_fromGIS.csv")
csv_2 <- fread('E:/k_PhD_NKU/202407biomod/data/10_03_108city_mean_variables.csv')
csv_2$city <-  tolower(csv_2$city)
csv_1$city  <-  tolower(csv_1$city)
setdiff(csv_2$city, csv_1$city)
length(unique(csv_2$city))

# Count occurrences of each city
city_counts <- table(csv_2$city)
#======caculate cities area ============
# Manually set Mollweide projection string
moll_crs <- "+proj=moll +datum=WGS84 +units=m +no_defs"

# Transform projection
reclamation_city_proj <- st_transform(reclamation_city_shp, crs = moll_crs)

reclamation_city_proj <- reclamation_city_proj %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)
names(reclamation_city_proj)
area_table <- reclamation_city_proj %>%
  st_drop_geometry() %>%
  select(shapeName, area_km2)
write.csv(area_table, "E:/k_PhD_NKU/202407biomod/data/reclamation_city_area.csv", row.names = FALSE)
# Find cities appearing more than once
duplicated_cities <- city_counts[city_counts > 1]

# Display results
print(duplicated_cities)

# 2) CRS alignment + geometry cleaning (keeping only polygons is more stable)
target_crs <- terra::crs(r)
cilp_Reclaim       <- st_transform(cilp_Reclaim, target_crs)       |> st_make_valid() |> st_collection_extract("POLYGON")
difference_Reclaim <- st_transform(difference_Reclaim, target_crs) |> st_make_valid() |> st_collection_extract("POLYGON")

# 3) Field check (require both layers to have shapeName as city name)
stopifnot("shapeName" %in% names(cilp_Reclaim), "shapeName" %in% names(difference_Reclaim))

# 4) Convert to terra vectors for extraction
v_rec  <- terra::vect(cilp_Reclaim)         # Multiple features, each with shapeName
v_non  <- terra::vect(difference_Reclaim)

# 5) Extract (note: extract returns data with column 1 as ID, followed by raster value columns)
val_col <- names(r)[1]

rec_df <- terra::extract(r, v_rec, na.rm = TRUE, ID = TRUE)          # cols: ID, <raster_value>
if (!is.null(rec_df) && nrow(rec_df) > 0) {
  rec_df$city <- v_rec$shapeName[rec_df$ID]                           # Map ID back to city name
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

# 6) Merge into pixel-level long table (same structure as before)
final_data <- rbindlist(list(rec_df, non_df), use.names = TRUE, fill = TRUE)

# === NEW: 6.5) Strictly align city × zone (both types appear for each city, even if pixels = 0) ===
library(data.table)

# Complete city list (prioritize your original city table; fallback to union of both layers if object doesn't exist)
if (exists("reclamation_city_shp")) {
  all_cities <- sort(unique(reclamation_city_shp$shapeName))
} else {
  all_cities <- sort(unique(c(cilp_Reclaim$shapeName, difference_Reclaim$shapeName)))
}
ZONES <- c("Reclamation","NonReclamation")
base_cz <- CJ(city = all_cities, zone = ZONES, unique = TRUE)

# Fill missing city-zone combinations in pixel-level long table (placeholder rows with risk = NA)
have_cz <- unique(final_data[, .(city, zone)])
need_cz <- fsetdiff(base_cz, have_cz)
if (nrow(need_cz) > 0) {
  add_rows <- need_cz[, .(city, zone, risk = as.numeric(NA))]
  final_data <- rbindlist(list(final_data, add_rows), use.names = TRUE, fill = TRUE)
}

# 7) Zone statistics (pixel count, mean, median), and left join with base_cz to fill gaps
stats_by_zone <- final_data[, .(
  n_pixels    = sum(!is.na(risk)),
  mean_risk   = if (sum(!is.na(risk)) > 0) round(mean(risk, na.rm = TRUE), 4) else NA_real_,
  median_risk = if (sum(!is.na(risk)) > 0) round(median(risk, na.rm = TRUE), 4) else NA_real_
), by = .(city, zone)]
stats_by_zone <- merge(base_cz, stats_by_zone, by = c("city","zone"), all.x = TRUE)
stats_by_zone[is.na(n_pixels), n_pixels := 0]

# 8) Generate city-level summary consistent with your previous format (ensure each city has both types of pixel counts and mean columns)
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

# 9) Export (filename follows your original; can change to *_complete.csv if you want to keep old version)
fwrite(final_data,   "E:/k_PhD_NKU/202407biomod/data/city_risk_pixel_data_fromGIS.csv")
fwrite(city_stats,   "E:/k_PhD_NKU/202407biomod/data/city_risk_summary_fromGIS.csv")
fwrite(mean_wide,    "E:/k_PhD_NKU/202407biomod/data/city_risk_mean_by_zone_fromGIS.csv")

cat("Done. Files saved:\n",
    " - city_risk_pixel_data_fromGIS.csv (pixel-level, filled city×zone)\n",
    " - city_risk_summary_fromGIS.csv (city summary, contains both types of pixel counts/means/differences)\n",
    " - city_risk_mean_by_zone_fromGIS.csv (zone mean wide table)\n", sep = "")


df <- mean_wide
df2 <- df %>%
  mutate(
    diff = mean_risk_reclamation - mean_risk_non_reclamation
  ) %>%
  arrange(desc(diff))

head(df2, 10)  # Top 10 cities where reclamation mean is significantly higher
tail(df2, 10)  # Cities where non-reclamation is higher

ggplot(df, aes(x = mean_risk_non_reclamation, y = mean_risk_reclamation)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(alpha = 0.7) +
  labs(x = "Non-reclamation mean risk",
       y = "Reclamation mean risk",
       title = "City-wise mean risk: Reclamation vs Non-reclamation") +
  theme_minimal()
df_pair <- df %>% filter(!is.na(mean_risk_reclamation), !is.na(mean_risk_non_reclamation))


# Compatible with different column names: may be Reclamation/NonReclamation or mean_risk_* naming
# Standardize to city, mean_risk_reclamation, mean_risk_non_reclamation
if (!"mean_risk_reclamation" %in% names(df) || !"mean_risk_non_reclamation" %in% names(df)) {
  # Try renaming from Reclamation / NonReclamation
  if (all(c("Reclamation", "NonReclamation") %in% names(df))) {
    df <- df %>% rename(mean_risk_reclamation = Reclamation,
                        mean_risk_non_reclamation = NonReclamation)
  } else {
    stop("Columns not found: Please ensure table contains 'mean_risk_reclamation' and 'mean_risk_non_reclamation' (or 'Reclamation'/'NonReclamation').")
  }
}

# For safety, convert to numeric again (remove possible thousand separators/spaces)
numize <- function(x) readr::parse_number(as.character(x))
df <- df %>%
  mutate(
    mean_risk_reclamation     = numize(mean_risk_reclamation),
    mean_risk_non_reclamation = numize(mean_risk_non_reclamation)
  )

# Paired data
df_pair <- df %>%
  filter(!is.na(mean_risk_reclamation), !is.na(mean_risk_non_reclamation)) %>%
  mutate(diff = mean_risk_reclamation - mean_risk_non_reclamation)
# Paired t-test
tt <- t.test(df_pair$mean_risk_reclamation, df_pair$mean_risk_non_reclamation, paired = TRUE)

# Wilcoxon
wt <- suppressWarnings(wilcox.test(df_pair$mean_risk_reclamation, df_pair$mean_risk_non_reclamation, paired = TRUE, exact = FALSE))



# Now calculate Cohen's d (paired: mean of differences / standard deviation of differences)
cohens_d <- mean(df_pair$diff, na.rm = TRUE) / sd(df_pair$diff, na.rm = TRUE)

# Direction consistency
prop_rec_lower  <- mean(df_pair$diff < 0)  # Reclamation < NonReclamation
prop_rec_higher <- mean(df_pair$diff > 0)

# === 3) Export one-page summary table ===
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


readr::write_csv(summary_tbl, file.path('E:/k_PhD_NKU/202407biomod/data/10_04analysis_summary_city_mean_by_zone.csv'))

# === 4) Visualization ===
# -------- 1) Unified journal-style theme --------

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

# Assume df_pair already has columns:
# city, mean_risk_reclamation, mean_risk_non_reclamation
df_pair$diff <- df_pair$mean_risk_reclamation - df_pair$mean_risk_non_reclamation
# Take top N with largest absolute difference to avoid overcrowding
N <- min(30, nrow(df_pair))
plot_df <- df_pair[order(-abs(df_pair$diff)), ][1:N, ]
plot_df$city <- factor(plot_df$city, levels = rev(plot_df$city))  # Sort vertical axis

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

# -------- 3) Difference-sorted bar chart (top 30 by |diff|) --------
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


# -------- 4) Bland–Altman plot (consistency/systematic bias) --------
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
# -------- 5) Optional: Combine into one page (for manuscript draft layout) --------



# ====== Unified journal-style theme ======
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

# ====== A. Paired slope =====
# For better alignment, sort by NonReclamation
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

# ====== B. Boxplot + jitter + significance annotation ======
ymax <- max(df_long$mean_risk, na.rm = TRUE)
pB <- ggplot(df_long, aes(x = zone, y = mean_risk)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, fill = "grey88", colour = "black") +
  geom_jitter(width = 0.08, alpha = 0.45, size = 1.6, shape = 16, colour = "grey25") +
  annotate("segment", x = 1, xend = 2, y = ymax + 0.02, yend = ymax + 0.02) +
  annotate("text", x = 1.5, y = ymax + 0.035, label = p_lab, size = 3.4) +
  labs(x = NULL, y = "City mean risk", title = "B. Distribution & significance") +
  coord_cartesian(ylim = c(min(df_long$mean_risk, na.rm = TRUE), ymax + 0.06)) +
  theme_journal()

# ====== C. Cleveland difference plot (all cities, sorted by difference) =====
df_clev <- df_pair %>% arrange(diff) %>% mutate(city = factor(city, levels = city))
pC <- ggplot(df_clev, aes(x = diff, y = city)) +
  geom_segment(aes(x = 0, xend = diff, yend = city), colour = "grey65", linewidth = 0.5) +
  geom_point(shape = 21, size = 2.0, stroke = 0.5, fill = "white", colour = "black") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4, colour = "grey50") +
  labs(x = "Mean difference (Rec − Nonrec)", y = NULL, title = "C. City-wise differences") +
  theme_journal()

# ====== D. ECDF distribution comparison (overall shape) =====
pD <- ggplot(df_long, aes(x = mean_risk, colour = zone)) +
  stat_ecdf(linewidth = 0.8) +
  scale_colour_manual(values = c("NonReclamation" = "grey20", "Reclamation" = "firebrick")) +
  labs(x = "City mean risk", y = "ECDF", title = "D. Distribution (ECDF)") +
  theme_journal() + theme(legend.position = "right")

# ====== Combined layout (Patchwork) ======
layout <- "
AB
CD
"
combined <- pA + pB + pC + pD + plot_layout(design = layout)
combined
ggsave("E:/k_PhD_NKU/202407biomod/figure/cities_reclaim_non_diff.pdf", combined, width = 4.2, height = 8.0, device = cairo_pdf)
ggsave("E:/k_PhD_NKU/202407biomod/figure/cities_reclaim_non_diff.jpg", combined, width = 8.2, height = 20.0)



#========= risk and area relationship==========

# Reclamation area vs reclamation risk
ggplot(csv_1, aes(x = reclamation_area, y = mean_risk_reclamation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +  # log may be more appropriate
  labs(title = "Reclamation Area vs Reclamation Risk", x = "Reclamation Area (log)", y = "Mean Reclamation Risk")

# City area vs non-reclamation risk
ggplot(csv_1, aes(x = area_km2, y = mean_risk_non_reclamation)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  labs(title = "City Area vs Non-reclamation Risk", x = "City Area (log)", y = "Mean Non-Reclamation Risk")
# Effect of reclamation area on risk
lm_reclamation <- lm(mean_risk_reclamation ~ log1p(reclamation_area), data = csv_1)
summary(lm_reclamation)

lm_reclamationTotal <- lm(risk ~ log1p(reclamation_area), data = csv_1)
summary(lm_reclamationTotal)



# Effect of total city area on non-reclamation risk
lm_non_reclamation <- lm(mean_risk_non_reclamation ~ log1p(area_km2), data = csv_1)
summary(lm_non_reclamation)
# Effect of total city area on city risk
lm_reclamation_cities <- lm(risk ~ log1p(area_km2), data = csv_1)
summary(lm_reclamation_cities)
# First plot: Reclamation area vs reclamation risk
p1 <- ggplot(csv_1, aes(x = log1p(reclamation_area), y = mean_risk_reclamation)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(
    title = "A. Reclamation Area vs. Reclamation Risk",
    x = "log(Reclamation Area + 1)",
    y = "Mean Reclamation Risk"
  ) +
  theme_minimal()

# Second plot: City area vs non-reclamation risk
p2 <- ggplot(csv_1, aes(x = log1p(area_km2), y = mean_risk_non_reclamation)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    title = "B. City Area vs. Non-Reclamation Risk",
    x = "log(City Area + 1)",
    y = "Mean Non-Reclamation Risk"
  ) +
  theme_minimal()

# Third plot: City area vs overall risk
p3 <- ggplot(csv_1, aes(x = log1p(area_km2), y = risk)) +
  geom_point(color = "tomato") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "C. City Area vs. Overall Risk",
    x = "log(City Area + 1)",
    y = "Overall Mean Risk"
  ) +
  theme_minimal()
p4 <- ggplot(csv_1, aes(x = log1p(reclamation_area), y = risk)) +
  geom_point(color = "#675b59") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "C. Reclamation Area vs. Overall Risk",
    x = "log(Reclamation Area + 1)",
    y = "Overall Mean Risk"
  ) +
  theme_minimal()


# Combine plots
combined_plot <- (p1 | p2) / (p3 | p4)

# Display plot
print(combined_plot)


# ========== Classify reclamation levels =======
# Assume your data is called csv_2
# Use natural breaks method (Jenks) to divide into 3 classes
jenks_breaks <- classInt::classIntervals(csv_2$reclamation_area, n = 3, style = "jenks")

# View breakpoints
print(jenks_breaks$brks)

# Assign each value to corresponding level (Low = 1, Medium = 2, High = 3)
csv_2 <- csv_2 %>%
  mutate(reclamation_level = cut(reclamation_area,
                                 breaks = jenks_breaks$brks,
                                 include.lowest = TRUE,
                                 labels = c("Low", "Medium", "High")))
table(csv_2$reclamation_level)


# Original values
area_values <- csv_2$reclamation_area

# K-Means clustering (set number of centers k = 3)
set.seed(123)  # Ensure reproducible results
kmeans_result <- kmeans(area_values, centers = 3)

# Extract cluster labels (1, 2, 3), and map to Low / Medium / High by sorting center values from small to large
cluster_labels <- kmeans_result$cluster
cluster_centers <- kmeans_result$centers

# Map labels to levels (optional: map from small to large centers to Low, Medium, High)
label_map <- data.frame(cluster = 1:3, center = as.vector(cluster_centers)) %>%
  arrange(center) %>%
  mutate(level = c("Low", "Medium", "High"))

# Merge clustering results to original data
csv_2 <- csv_2 %>%
  mutate(
    cluster = cluster_labels
  ) %>%
  left_join(label_map, by = c("cluster"))

# View results
table(csv_2$level)
ggplot(csv_2, aes(x = reclamation_area, fill = level)) +
  geom_histogram(bins = 30, color = "black") +
  scale_fill_manual(values = c("Low" = "skyblue", "Medium" = "orange", "High" = "red")) +
  labs(title = "K-Means Clustering on Reclamation Area",
       x = "Reclamation Area", y = "Count") +
  theme_minimal()
write.csv(csv_2, 'E:/k_PhD_NKU/202407biomod/data/10_03_108city_mean_variables.csv')
hist(csv_2$reclamation_area, breaks = 30, main = "Distribution of Reclamation Area")
csv_2 <- csv_2 %>%
  mutate(reclamation_level_manual = case_when(
    reclamation_area < 3000 ~ "Low",
    reclamation_area < 10000 ~ "Medium",
    TRUE ~ "High"
  ))
table(csv_2$reclamation_level_manual)

#==== according to reclamation fraction to classify
jenks_breaks <- classInt::classIntervals(city_df$recl_frac, n = 3, style = "jenks")
# View breakpoints
print(jenks_breaks$brks)

# Assign each value to corresponding level (Low = 1, Medium = 2, High = 3)
city_df <- city_df %>%
  mutate(recla_level_byfraction = cut(recl_frac,
                                 breaks = jenks_breaks$brks,
                                 include.lowest = TRUE,
                                 labels = c("Low", "Medium", "High")))
table(city_df$recla_level_byfraction)
