# Original source: 0105analysis.R
# Original source location: <LOCAL_CODE_ROOT>/0105analysis.R
# Original source SHA-256: ec3fa1fad2bf33e1715feabeb4af0b5bf6c6d6805d1f565d20185cb261fbb031
# Public-safe original snapshot: archive/original_sources/local/0105analysis.R
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
library(ggplot2)
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
library(duckdb)
setwd(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/pic"))
taxa_count_per_city <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/04onlycity_duodu.csv"))

taxa_count_per_city$reclamation_area <- as.numeric(taxa_count_per_city$reclamation_area)

reclamation_global <- ggplot(taxa_count_per_city, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "gray85", fill = "gray80") +  # Add world map background
  geom_point(aes(size = reclamation_area, alpha = reclamation_area), shape = 21, stroke = 1, fill = '#9DC3E7', color = "black") +  # Add points with size based on 'Value'
  scale_size_continuous(range = c(1, 10)) +  # Adjust size range
  scale_color_viridis_c(option = "A") + 
  scale_alpha_continuous(range = c(0.5, 1)) +# Use a color scale for the values
  theme_minimal() +  # Apply minimal theme
  labs(title = "Project area in 43 Reclamation cities",
       x = "Longitude", y = "Latitude",  alpha = "reclamation_area") +
  coord_quickmap()
reclamation_global

ggsave("reclamation_global_43.png", plot = reclamation_global,  width = 10, height = 11.5, dpi = 300) 
