# Original source: temp_buffer.r
# Original source location: <LOCAL_CODE_ROOT>/temp_buffer.r
# Original source SHA-256: 2c09815178c1117cae6f3aaa734a1dbaf2935f18fa8723e0fe11410d89c7cf19
# Public-safe original snapshot: archive/original_sources/local/temp_buffer.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(terra)
library(sf)

print("Starting buffer calculation script...")

press <- rast("<LOCAL_ANALYSIS_ROOT>/data/data/hotspot_mean_suitability_normalized2.0.tif")
cities <- vect("<LOCAL_ANALYSIS_ROOT>/data/data/108finalCities.shp")
cities_v <- makeValid(cities)

print("Projecting...")
press_m  <- project(press, 'EPSG:6933')
cities_m <- project(cities_v, 'EPSG:6933')

calc_city_bg <- function(press_m, cities_m, r_km = 50, id_field = 'shapeName', fun_stat = mean, na_rm = TRUE) {
  print(paste("Running buffer", r_km))
  if (!same.crs(press_m, cities_m)) {
    cities_m <- project(cities_m, crs(press_m))
  }
  cities_v <- makeValid(cities_m)
  cities_v <- buffer(cities_v, 0)
  city_stat <- extract(press_m, cities_v, fun = fun_stat, na.rm = na_rm)
  colnames(city_stat)[2] <- 'P_city'
  
  r_m <- r_km * 1000
  cities_buf <- buffer(cities_v, width = r_m)
  cities_buf <- makeValid(cities_buf)
  cities_buf <- buffer(cities_buf, 0)
  buffer_stat <- extract(press_m, cities_buf, fun = fun_stat, na.rm = na_rm)
  colnames(buffer_stat)[2] <- 'P_buffer'
  
  cities_ring <- erase(cities_buf, cities_v)
  cities_ring <- makeValid(cities_ring)
  cities_ring <- buffer(cities_ring, 0)
  bg_stat <- extract(press_m, cities_ring, fun = fun_stat, na.rm = na_rm)
  colnames(bg_stat)[2] <- 'P_bg'
  
  out <- data.frame(
    buffer_km = r_km,
    city_id   = cities_v[[id_field]],
    P_city    = city_stat$P_city,
    P_buffer  = buffer_stat$P_buffer,
    P_bg      = bg_stat$P_bg
  )
  out$DeltaP   <- out$P_city - out$P_bg
  out$RelDelta <- out$DeltaP / out$P_bg
  out
}

buffers <- c(10, 50, 100)
res_list <- list()
for (bk in buffers) {
  res_list[[as.character(bk)]] <- calc_city_bg(
    press_m  = press_m,
    cities_m = cities_m,
    r_km     = bk,
    id_field = 'shapeName',
    fun_stat = mean
  )
}
res_all <- do.call(rbind, res_list)

dir.create("<LOCAL_ANALYSIS_ROOT>/data/organized_table", recursive = TRUE, showWarnings = FALSE)
write.csv(res_all, "<LOCAL_ANALYSIS_ROOT>/data/organized_table/city_vs_background_buffer_sensitivity.csv", row.names = FALSE)
print("Saved!")
