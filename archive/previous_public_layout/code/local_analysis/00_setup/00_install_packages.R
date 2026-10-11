# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 2729e48a90e8310b072aae4f20b5aad28ed8177c0e74645f77fd4045fba90fe3
# find the environmental file
file.edit("~/.Renviron")
#set the vpn port
Sys.getenv("http_proxy")
Sys.getenv("https_proxy")
#restart
.rs.restartR()

setwd('${LOCAL_ANALYSIS_ROOT}/data')
install.packages("ENMeval")
devtools::install_github("brshipley/megaSDM", build_vignettes = TRUE)

library(biomod2)
library(rgbif)
library(BIEN)
library(raster)
library(geodata)
library(tidyterra)
library(ggtext)
library(xgboost)
library(caret)
library(doParallel)
library(pbapply)
library(ENMeval)
# register for parallel working
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# check the number of cluster
detectCores()
cluster_size <- getDoParWorkers()
print(paste("Number of parallel workers:", cluster_size))
# search species in the database
occ_data <- occ_search(scientificName = "Spartina alterniflora")
bien_data <- BIEN_occurrence_species("Spartina alterniflora")
write.csv(occ_data$data, "Spartina_alterniflora_gbif.csv", row.names = FALSE)
# download environmental data
worldclim_data <- worldclim_global(var='bio', res=2.5, path='data/')
bioclim_names <- c(
  "Annual_Mean_Temperature",
  "Mean_Diurnal_Range",
  "Isothermality",
  "Temperature_Seasonality",
  "Max_Temperature_of_Warmest_Month",
  "Min_Temperature_of_Coldest_Month",
  "Temperature_Annual_Range",
  "Mean_Temperature_of_Wettest_Quarter",
  "Mean_Temperature_of_Driest_Quarter",
  "Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Coldest_Quarter",
  "Annual_Precipitation",
  "Precipitation_of_Wettest_Month",
  "Precipitation_of_Driest_Month",
  "Precipitation_Seasonality",
  "Precipitation_of_Wettest_Quarter",
  "Precipitation_of_Driest_Quarter",
  "Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Coldest_Quarter"
)

names(worldclim_data) <- bioclim_names
print(names(worldclim_data))
# check for every parameter
bio1 <- worldclim_data[[1]]  # Bioclimatic variable 1 (Annual Mean Temperature)
# visualization
plot(bio1, main = "Annual Mean Temperature")
# environmental layers for biomod
env_files <- list.files(path = "${LOCAL_ANALYSIS_ROOT}/data/data/climate/wc2.1_2.5m", pattern = ".tif$", full.names = TRUE)
env_stack <- stack(env_files)
# read species data
species_data <- read.csv("Spartina_alterniflora_gbif.csv")
resp_var <- rep(1, nrow(species_data))
resp_xy <- species_data[, c("decimalLongitude", "decimalLatitude")]

formatted_data <- BIOMOD_FormatingData(
  resp.var = resp_var,
  expl.var = worldclim_data,
  resp.xy = resp_xy,
  resp.name = "Spartina_alterniflora",
  PA.nb.rep = 1,  # generate one group pesdo-absence
  PA.nb.absences = 1000,  # num=1000
  PA.strategy = 'random'
)
plot(formatted_data)
# # Select multiple sets of pseudo-absences

# # Transform true absences into potential pseudo-absences
 myResp.PA <- ifelse(resp_var == 1, 1, NA)
# 
# # Format Data with pseudo-absences : random method
myBiomodData.multi <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                            expl.var = worldclim_data,
                                           resp.xy = resp_xy,
                                           resp.name = "Spartina_alterniflora",
                                           PA.nb.rep = 4,
                                           PA.nb.absences = c(1000, 500, 500, 200),
                                           PA.strategy = 'random')

summary(myBiomodData.multi)
plot(myBiomodData.multi)


# # tuned parameters with formated data
model_options <- bm_ModelingOptions(
  GLM = list(type = 'quadratic', interaction.level = 0),
  GBM = list(n.trees = 1000, interaction.depth = 5),
  RF = list(ntree = 500)
)


# train model (# models = c("ANN", "CTA", "FDA", "GLM", "MARS", "MAXENT", "MAXNET", "RF","SRE", "XGBOOST")
pboptions(type = "timer")
biomod_model_out <- BIOMOD_Modeling(
  formatted_data,
  models = c( "MAXENT", "MAXNET", "RF", "XGBOOST"),
  CV.strategy = 'random',
  CV.nb.rep = 2,
  CV.perc = 0.8,
  OPT.strategy = 'tuned',
  var.import = 3,
  metric.eval = c('TSS','ROC'),
  nb.cpu = 11)
# Model ensemble models
biomod_model_out
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = biomod_model_out,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      em.algo = c('EMmean', 'EMcv', 'EMci', 'EMmedian', 'EMca', 'EMwmean'),
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.7),
                                      metric.eval = c('TSS', 'ROC'),
                                      var.import = 3,
                                      EMci.alpha = 0.05,
                                      EMwmean.decay = 'proportional')
myBiomodEM
# projection
biomod_projection <- BIOMOD_Projection(
  modeling.output = biomod_model_out,
  new.env = env_stack,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = TRUE,
  output.format = '.grd'
)

stopCluster(cl)
registerDoSEQ()

# Visualize results
proj_raster <- raster("Pinus_sylvestris/proj_current/proj_current_Pinus_sylvestris.grd")
plot(proj_raster, main = "Pinus sylvestris Distribution Prediction")

# Model evaluation
eval <- get_evaluations(biomod_model_out)
print(eval)

# Variable importance
var_import <- get_variables_importance(biomod_model_out)
print(var_import)