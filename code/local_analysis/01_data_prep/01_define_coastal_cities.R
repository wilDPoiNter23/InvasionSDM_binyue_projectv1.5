# Load required libraries
library(httr)
library(dplyr)
library(pbapply)

# Your GeoNames username
username <- 'binyue'  # Replace with your GeoNames username

# Load the dataset
df <- read.csv('E:/k_PhD_NKU/202407biomod/data/GCC_socioeconomic.csv')

# Ensure latitude and longitude are numeric
df$lat <- as.numeric(df$lat)
df$lon <- as.numeric(df$lon)


# Define the function to query GeoNames API
get_city <- function(lat, lon, username) {
  if (is.na(lat) || is.na(lon)) {
    return(NA)
  }
  
  url <- paste0("http://api.geonames.org/findNearbyPlaceNameJSON?lat=", lat, "&lng=", lon, "&username=", username)
  
  response <- try(GET(url), silent = TRUE)
  
  if (inherits(response, "try-error")) {
    Sys.sleep(1)  # Delay before retrying
    response <- try(GET(url), silent = TRUE)
    if (inherits(response, "try-error")) {
      return(NA)
    }
  }
  
  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    if (length(content$geonames) > 0) {
      return(content$geonames[[1]]$name)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# Batch processing
batch_size <- 10000
num_batches <- ceiling(nrow(df) / batch_size)

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = num_batches, style = 3)

# Create an empty vector to store the results
df$city <- NA

# Process in batches
for (batch_num in 1:num_batches) {
  start_idx <- (batch_num - 1) * batch_size + 1
  end_idx <- min(batch_num * batch_size, nrow(df))
  
  # Apply the function to each row in the current batch and unlist the results
  cities <- unlist(pbapply::pblapply(start_idx:end_idx, function(i) {
    get_city(df$lat[i], df$lon[i], username)
  }))
  
  # Assign the results to the corresponding rows in the 'city' column
  df$city[start_idx:end_idx] <- cities
  
  # Update progress bar
  setTxtProgressBar(pb, batch_num)
}

# Close progress bar
close(pb)



# Save the result to a CSV file
write.csv(df, "E:/k_PhD_NKU/202407biomod/data/GCC_socioeconomic_with_cities.csv", row.names = FALSE)

