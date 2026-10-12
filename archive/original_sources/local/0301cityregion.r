# Original source: 0301cityregion.r
# Original source location: <LOCAL_CODE_ROOT>/0301cityregion.r
# Original source SHA-256: 5ea6ddabf4d209651323b12f6832bd9ca3e94df875d8ba4ca07f6c7b7255fb07
# Public-safe original snapshot: archive/original_sources/local/0301cityregion.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)
library(sf)
library(osmdata)
# Define the city of interest

CN_city <- st_read("<LOCAL_ANALYSIS_ROOT>/data/data/2019ChinaAdiminDivsion/市.shp")
names(CN_city) <- c("Province_Code", "Province", "City_Code", "City", "Type", "geometry")

# List of city names as a single pattern string for grepl
cities_pattern <- '台湾|儋州|三亚|大连|东营|广州|海口|嘉兴|宁波|莆田|青岛|秦皇岛|泉州|日照|上海|汕头|深圳|唐山|天津|威海|温州|厦门|烟台|营口|湛江|舟山|珠海'

recaLCN_SHP <- CN_city %>%
  filter(grepl(cities_pattern, City))

translation <- c("天津市" = "Tianjin", "河北省" = "Hebei", "辽宁省" = "Liaoning", 
                 "上海市" = "Shanghai", "浙江省" = "Zhejiang", "福建省" = "Fujian", 
                 "山东省" = "Shandong", "广东省" = "Guangdong", "海南省" = "Hainan", 
                 "台湾省" = "Taiwan")

recaLCN_SHP$Province <- recode(recaLCN_SHP$Province, !!!translation) %>% tolower()
# save the recalcn_shp file
recaLCN_SHP <- recaLCN_SHP %>%
  mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "ASCII//TRANSLIT")))

st_write(recaLCN_SHP, "<LOCAL_ANALYSIS_ROOT>/data/data/recalCN.shp", delete_layer = TRUE)


recaLCN_SHP <- st_read("<LOCAL_ANALYSIS_ROOT>/data/data/recalCN.shp")
selected_city <-  recaLCN_SHP %>% filter(Provinc == 'guangdong')

ggplot() +
  geom_sf(data = recaLCN_SHP, fill = "lightblue", color = "blue", size = 0.5) +
  ggtitle("Boundary of tianjin") +
  theme_minimal()

# ======= downlad city shp ============ 
city_df <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/05onlyinvasion_city_duodu.csv", stringsAsFactors = FALSE)
city_df$City

city_shapes <- list()

for (i in seq_len(nrow(city_df))) {
  city <- city_df$City[i]
  country <- city_df$Country[i]

  # ✅ 跳过中国城市
  if (tolower(country) == "china") {
    cat(sprintf("[%02d/%02d] ⏭️ Skipping Chinese city: %s\n", i, nrow(city_df), city))
    next
  }

  query <- paste(city, country, sep = ", ")
  cat(sprintf("[%02d/%02d] 🔍 Downloading boundary for: %s\n", i, nrow(city_df), query))

  result <- tryCatch({
    opq(query) %>%
      add_osm_feature(key = "boundary", value = "administrative") %>%
      osmdata_sf()
  }, error = function(e) {
    cat("❌ Request failed:", query, "\n")
    return(NULL)
  })

  if (!is.null(result)) {
    poly <- NULL
    if (!is.null(result$osm_multipolygons) && nrow(result$osm_multipolygons) > 0) {
      poly <- result$osm_multipolygons
    } else if (!is.null(result$osm_polygons) && nrow(result$osm_polygons) > 0) {
      poly <- result$osm_polygons
    }

    if (!is.null(poly)) {
      match_row <- poly[grepl(city, poly$name, ignore.case = TRUE), ]
      if (nrow(match_row) > 0) {
        city_shape <- st_make_valid(match_row[1, ])
        city_shape$City <- city
        city_shape$Country <- country
        city_shapes[[length(city_shapes) + 1]] <- city_shape
        cat("✅ Success:", city, "\n")
      } else {
        cat("⚠️ No match by name in geometry:", city, "\n")
      }
    } else {
      cat("⚠️ No polygon result found for:", city, "\n")
    }
  }
}
city_shapes

common_cols <- Reduce(intersect, lapply(city_shapes, names))


city_shp_clean <- city_shp %>%
  dplyr::select(name, City, osm_id, Country, geometry)


city_shp <- do.call(rbind, city_shapes_clean)

names(city_shp)

st_write(city_shp_clean, "<LOCAL_ANALYSIS_ROOT>/data/data/reclamation_cities.shp")
# 合并导出
if (length(city_shapes) > 0) {
  city_shp <- do.call(rbind, city_shapes)
  st_write(city_shp, "<LOCAL_ANALYSIS_ROOT>/data/data/reclamation_cities.shp", delete_layer = TRUE)
  cat("✅ Exported city shapefile.\n")
} else {
  cat("❌ No valid city boundary retrieved.\n")
}

shp_folder <-  "<LOCAL_ANALYSIS_ROOT>/data/data/research_cities"

shp_files <- list.files(shp_folder, pattern = "\\.shp$", full.names = TRUE)

shp_list <- lapply(shp_files, st_read)

names(shp_list[[2]])

shp_list


shp_list_clean <- lapply(shp_files, function(file) {
  shp <- st_read(file, quiet = TRUE)
  
  # 尝试识别城市字段
  city_field <- names(shp)[grepl("City|NAME_2|shapeName|name", names(shp), ignore.case = TRUE)][1]
  country_field <- names(shp)[grepl("COUNTRY|Provinc|shapeGroup", names(shp), ignore.case = TRUE)][1]
  
  if (is.null(city_field) || is.null(shp[[city_field]])) return(NULL)
  
  shp_out <- shp %>%
    transmute(
      city = !!sym(city_field),
      country = if (!is.null(country_field)) !!sym(country_field) else NA,
      geometry = geometry
    )
  
  st_transform(shp_out, crs = 4326)
})

# 移除无效项
shp_list_clean <- Filter(Negate(is.null), shp_list_clean)

# 合并
merged_shp <- do.call(rbind, shp_list_clean)


st_write(merged_shp, "<LOCAL_ANALYSIS_ROOT>/data/data/reclaimedCities.shp")

