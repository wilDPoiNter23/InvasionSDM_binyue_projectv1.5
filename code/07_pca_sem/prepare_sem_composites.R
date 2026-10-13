# City-variable composite scores
# Input: transformed and standardized city variables
# Output: full-city composite table for SEM
# Use the archived composite table to reproduce the reported coefficient analysis.

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
dir.create(publication_path("SEM_OUTPUT"), recursive=TRUE, showWarnings=FALSE)
library(ggplot2)
library(piecewiseSEM)
library(glmmTMB)
dt1 <- read.csv(publication_path("SEM_BASE_INPUT"),header = T)


drop_bad_cols <- function(dat, cols, max_na = 0.5){
  keep <- cols[sapply(cols, function(v){
    if(!v %in% names(dat)) return(FALSE)
    na_rate <- mean(is.na(dat[[v]]))
    na_rate <= max_na && na_rate < 1
  })]
  keep
}

# Construct the Climate composite and fit Climate_model.
Climate_model <- lm(risk ~ wc2.1_2.5m_bio_1+
                      wc2.1_2.5m_bio_2+
                      wc2.1_2.5m_bio_3+
                      wc2.1_2.5m_bio_4+
                      wc2.1_2.5m_bio_5+
                      wc2.1_2.5m_bio_6+

                      wc2.1_2.5m_bio_8+
                      wc2.1_2.5m_bio_9+
                      wc2.1_2.5m_bio_10+
                      wc2.1_2.5m_bio_11+
                      wc2.1_2.5m_bio_12+
                      wc2.1_2.5m_bio_13+
                      wc2.1_2.5m_bio_14+
                      wc2.1_2.5m_bio_15+
                      wc2.1_2.5m_bio_16+
                      wc2.1_2.5m_bio_17+
                      wc2.1_2.5m_bio_18+
                      wc2.1_2.5m_bio_19 , dt1)
# Print model results.
summary(Climate_model)
coefs(Climate_model, standardize = 'scale')

# Extract coefficients to calculate composite scores for predicting the response.
beta_wc2.1_2.5m_bio_1 <-  summary(Climate_model)$coefficients[2, 1]
beta_wc2.1_2.5m_bio_2 <-  summary(Climate_model)$coefficients[3, 1]
beta_wc2.1_2.5m_bio_3 <-  summary(Climate_model)$coefficients[4, 1]
beta_wc2.1_2.5m_bio_4 <-  summary(Climate_model)$coefficients[5, 1]
beta_wc2.1_2.5m_bio_5 <-  summary(Climate_model)$coefficients[6, 1]
beta_wc2.1_2.5m_bio_6 <-  summary(Climate_model)$coefficients[7, 1]

beta_wc2.1_2.5m_bio_8 <-  summary(Climate_model)$coefficients[8, 1]
beta_wc2.1_2.5m_bio_9 <-  summary(Climate_model)$coefficients[9, 1]
beta_wc2.1_2.5m_bio_10 <-  summary(Climate_model)$coefficients[10, 1]
beta_wc2.1_2.5m_bio_11 <-  summary(Climate_model)$coefficients[11, 1]
beta_wc2.1_2.5m_bio_12 <-  summary(Climate_model)$coefficients[12, 1]
beta_wc2.1_2.5m_bio_13 <-  summary(Climate_model)$coefficients[13, 1]
beta_wc2.1_2.5m_bio_14 <-  summary(Climate_model)$coefficients[14, 1]
beta_wc2.1_2.5m_bio_15 <-  summary(Climate_model)$coefficients[15, 1]
beta_wc2.1_2.5m_bio_16 <-  summary(Climate_model)$coefficients[16, 1]
beta_wc2.1_2.5m_bio_17 <-  summary(Climate_model)$coefficients[17, 1]
beta_wc2.1_2.5m_bio_18 <-  summary(Climate_model)$coefficients[18, 1]
beta_wc2.1_2.5m_bio_19 <-  summary(Climate_model)$coefficients[19, 1]



Climate<- beta_wc2.1_2.5m_bio_1*dt1$wc2.1_2.5m_bio_1+
  beta_wc2.1_2.5m_bio_2*dt1$wc2.1_2.5m_bio_2+
  beta_wc2.1_2.5m_bio_3*dt1$wc2.1_2.5m_bio_3+
  beta_wc2.1_2.5m_bio_4*dt1$wc2.1_2.5m_bio_4+
  beta_wc2.1_2.5m_bio_5*dt1$wc2.1_2.5m_bio_5+
  beta_wc2.1_2.5m_bio_6*dt1$wc2.1_2.5m_bio_6+

  beta_wc2.1_2.5m_bio_8*dt1$wc2.1_2.5m_bio_8+
  beta_wc2.1_2.5m_bio_9*dt1$wc2.1_2.5m_bio_9+
  beta_wc2.1_2.5m_bio_10*dt1$wc2.1_2.5m_bio_10+
  beta_wc2.1_2.5m_bio_11*dt1$wc2.1_2.5m_bio_11+
  beta_wc2.1_2.5m_bio_12*dt1$wc2.1_2.5m_bio_12+
  beta_wc2.1_2.5m_bio_13*dt1$wc2.1_2.5m_bio_13+
  beta_wc2.1_2.5m_bio_14*dt1$wc2.1_2.5m_bio_14+
  beta_wc2.1_2.5m_bio_15*dt1$wc2.1_2.5m_bio_15+
  beta_wc2.1_2.5m_bio_16*dt1$wc2.1_2.5m_bio_16+
  beta_wc2.1_2.5m_bio_17*dt1$wc2.1_2.5m_bio_17+
  beta_wc2.1_2.5m_bio_18*dt1$wc2.1_2.5m_bio_18+
  beta_wc2.1_2.5m_bio_19*dt1$wc2.1_2.5m_bio_19



dt1$Climate<-Climate
summary(lm(risk~ Climate, dt1))

# Obtain standardized coefficients with piecewiseSEM Coefs.
coefs(lm(risk~ Climate, dt1))










# Construct the Water composite and fit Water_model.
Water_model <- lm(risk ~ salinity_mean+
                    salinity_range+
                    temperature_range+
                    temperature_mean
                  , dt1)
# Print model results.
summary(Water_model)
coefs(Water_model, standardize = 'scale')

# Extract coefficients to calculate composite scores for predicting the response.
beta_salinity_mean <-  summary(Water_model)$coefficients[2, 1]
beta_salinity_range <-  summary(Water_model)$coefficients[3, 1]
beta_temperature_range <-  summary(Water_model)$coefficients[4, 1]
beta_temperature_mean <-  summary(Water_model)$coefficients[5, 1]
Water <- beta_salinity_mean*dt1$salinity_mean+ beta_salinity_range*dt1$salinity_range+
  beta_temperature_range*dt1$temperature_range+ beta_temperature_mean*dt1$temperature_mean
dt1$Water <- Water
summary(lm(risk~ Water, dt1))


# Obtain standardized coefficients with piecewiseSEM Coefs.
coefs(lm(risk~ Water, dt1))









# Construct the reclamation composite and fit reclamation_model.
reclamation_model <- lm(risk ~ reclamation_area+area_ratio
                        , dt1)
# Print model results.
summary(reclamation_model)
coefs(reclamation_model, standardize = 'scale')

# Extract coefficients to calculate composite scores for predicting the response.
beta_reclamation_area <-  summary(reclamation_model)$coefficients[2, 1]
beta_area_ratio <-  summary(reclamation_model)$coefficients[3, 1]
reclamation <- beta_reclamation_area*dt1$reclamation_area+ beta_area_ratio*dt1$area_ratio
dt1$reclamation <- reclamation
summary(lm(risk~ reclamation, dt1))


# Obtain standardized coefficients with piecewiseSEM Coefs.
coefs(lm(risk~ reclamation, dt1))






# Construct the remaining composites and fit their linear models.
# Construct the Soil composite and fit Soil_model.
# The 9.24 revision removed soc.
Soil_model <- lm(risk ~ bdod_mean_2024_2_5min_aligned+
                   cfvo_mean_2024_2_5min_aligned+

                   nitrogen_mean_2024_2_5min_aligned+
                   ocd_mean_2024_2_5min_aligned+
                   phh2o_mean_2024_2_5min_aligned+
                   sand_mean_2024_2_5min_aligned+
                   silt_mean_2024_2_5min_aligned
                 , dt1)
summary(Soil_model)
coefs(Soil_model, standardize = 'scale')
# Extract coefficients to calculate composite scores for predicting the response.
beta_bdod_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[2, 1]
beta_cfvo_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[3, 1]

beta_nitrogen_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[4, 1]
beta_ocd_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[5, 1]
beta_phh2o_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[6, 1]
beta_sand_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[7, 1]
beta_silt_mean_2024_2_5min_aligned<-  summary(Soil_model)$coefficients[8, 1]


Soil<-beta_bdod_mean_2024_2_5min_aligned*dt1$bdod_mean_2024_2_5min_aligned +
  beta_cfvo_mean_2024_2_5min_aligned*dt1$cfvo_mean_2024_2_5min_aligned+

  beta_nitrogen_mean_2024_2_5min_aligned*dt1$nitrogen_mean_2024_2_5min_aligned +
  beta_ocd_mean_2024_2_5min_aligned*dt1$ocd_mean_2024_2_5min_aligned+
  beta_phh2o_mean_2024_2_5min_aligned*dt1$phh2o_mean_2024_2_5min_aligned +
  beta_sand_mean_2024_2_5min_aligned*dt1$sand_mean_2024_2_5min_aligned +
  beta_silt_mean_2024_2_5min_aligned*dt1$silt_mean_2024_2_5min_aligned

dt1$Soil<- Soil
summary(lm(risk~ Soil, dt1))

# Obtain standardized coefficients with piecewiseSEM Coefs.
coefs(lm(risk~ Soil, dt1))
















# Construct the socioeconomic composite and fit climate_model.
socioeconomic_model <- lm(risk ~ BUILT_S_E2025_2.5min+
                            GDP2020_2.5min+
                            VIIRS_2023_2.5min+
                            Pop2020_2.5min+
                            area_km2
                          , dt1)
summary(socioeconomic_model)
coefs(socioeconomic_model, standardize = 'scale')
# Extract coefficients to calculate composite scores for predicting the response.
beta_BUILT_S_E2025_2.5min<-  summary(socioeconomic_model)$coefficients[2, 1]
beta_GDP2020_2.5min<-  summary(socioeconomic_model)$coefficients[3, 1]
beta_VIIRS_2023_2.5min<-  summary(socioeconomic_model)$coefficients[4, 1]
beta_Pop2020_2.5min<-  summary(socioeconomic_model)$coefficients[5, 1]
beta_area_km2<-  summary(socioeconomic_model)$coefficients[6, 1]
socioeconomic <- beta_BUILT_S_E2025_2.5min*dt1$BUILT_S_E2025_2.5min+
  beta_GDP2020_2.5min*dt1$GDP2020_2.5min +
  beta_VIIRS_2023_2.5min*dt1$VIIRS_2023_2.5min +
  beta_Pop2020_2.5min*dt1$Pop2020_2.5min +
  beta_area_km2*dt1$area_km2
dt1$socioeconomic <- socioeconomic
summary(lm(risk~ socioeconomic, dt1))

# Obtain standardized coefficients with piecewiseSEM Coefs.
coefs(lm(risk~ socioeconomic, dt1))



















# The 9.17 revision removed water -> risk.

write.csv(dt1, publication_path("SEM_OUTPUT", "city_sem_composites.csv"), row.names=FALSE)
