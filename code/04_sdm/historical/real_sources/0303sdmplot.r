# Original source: 0303sdmplot.r
# Original source location: <LOCAL_CODE_ROOT>/0303sdmplot.r
# Original source SHA-256: ee3b11a4a67091ebf7619227fb3da5ade413ccf1e234a63a0897978ca39be9f0
# Public-safe original snapshot: archive/original_sources/local/0303sdmplot.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(terra)
library(ggplot2)
library(tmap)
library(viridisLite)
library(biomod2)
library(randomForest)
install.packages('randomForest')
install.packages("viridisLite")
proj_raster <- rast(publication_path("RECOVERED_CODE_ROOT", "Spartina.alterniflora/proj_current/proj_current_Spartina.alterniflora.grd"))
china_shp   <- vect(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/ChinaAdminDivisonSHP-24.02.06/1. Country/country.shp"))


proj_raster <- rast(publication_path("HISTORICAL_PATH_004"))
# Convert to a data frame
raster_df <- as.data.frame(proj_raster, xy = TRUE)

print(crs(proj_raster))

head(raster_df)

     # Mask NA values if necessary
proj_raster <- mask(proj_raster, proj_raster)
proj_raster <- crop(proj_raster, extent(-180, 180, -90, 90))
png("spatina_raster_plot.png", width = 800, height = 600, res = 150)
terra::plot(proj_raster,1,
     xlim = c(-180, 180),  # Longitude range
     ylim = c(-90, 90),
     axes = TRUE,    # Latitude range
     main = "spatrina",
     col = rev(terrain.colors(10000)))


 dev.off()

# 1. Crop to China bounding box
r_cropped <- crop(proj_raster, china_shp)

# 2. Mask so that only pixels within the China shapefile remain
r_clipped <- mask(r_cropped, china_shp)


png("spartina_CNraster_plot.png", width = 800, height = 600, res = 150)
terra::plot(r_clipped,1,
     xlim = c(-180, 180),  # Longitude range
     ylim = c(-90, 90),
     axes = TRUE,    # Latitude range
     main = "spatina",
     col = rev(terrain.colors(10000)))

 dev.off()


#  Rescale from 0–1000 to 0–1
r_01 <- r_clipped / 1000

# 3) Classify into intervals
m <- matrix(c(
  0,    0.25, 1,
  0.25, 0.50, 2,
  0.50, 0.75, 3,
  0.75, 1.00, 4
), ncol = 3, byrow = TRUE)

r_class <- classify(r_01, m, include.lowest=TRUE, right=FALSE)

# 4) Optional: Convert to factor with meaningful labels
r_class <- as.factor(r_class)
cats <- data.frame(ID = 1:4,
                   category = c("Not suitable (0–0.25)",
                                "Low (0.25–0.50)",
                                "Moderate (0.50–0.75)",
                                "High (0.75–1.00)"))
levels(r_class) <- cats

png("classified_spartina_CNraster_plot.png", width = 800, height = 600, res = 150)
plot(r_class,1, ylim = c(10, 60), col = rev(terrain.colors(5)),
     main = "Spartina Suitability", 
     type = "classes",       # optional, if your raster is categorical/factor
     plg = list(x = "bottomright", title = "Suitability", cex = 1.5),
     axes = TRUE)
 dev.off()

myBiomodModelOut <- readRDS(publication_path("RECOVERED_CODE_ROOT", "Spartina.alterniflora/biomod_model_out.rds"))
# Get evaluation scores & variables importance
df_eval <- get_evaluations(myBiomodModelOut)
get_variables_importance(myBiomodModelOut)


tss_rows <- df_eval[df_eval$metric.eval == "TSS", ]
roc_rows <- df_eval[df_eval$metric.eval == "ROC", ]
# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut)
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'run'))

# Represent response curves

png("PlotResponseCurves.png", width = 1600, height = 1800, res = 150)
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[4],
                      fixed.var = 'median')
 dev.off()

png("PlotResponseCurves.png", width = 1600, height = 1800, res = 150)
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
 dev.off()
