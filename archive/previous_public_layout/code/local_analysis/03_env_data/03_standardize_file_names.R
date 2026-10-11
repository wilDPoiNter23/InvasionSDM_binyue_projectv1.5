# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 96392f0188b6a95333733162eed01a9a0bdf798f001f8a74ecad013e35fcc446
library(stringr)

# Set file path
file_path <- "${WORK_ROOT}/results/projections"

# Get all .rds files in directory
file_list <- list.files(
  path = file_path,
  pattern = "\\.rds$",
  full.names = FALSE
)

# Create empty dataframe to store results
results <- data.frame(
  species = character(),
  type = character(),
  stringsAsFactors = FALSE
)

# Iterate through each filename and extract information
for (file_name in file_list) {
  # Remove .rds extension
  base_name <- gsub("\\.rds$", "", file_name)

  # Process filename format: Genus species_type_projection
  # Example: Rhaphiolepis bibas_ensemble_projection.rds
  # Or: Typha domingensis_fallback_projection.rds

  # Use regex to separate species name and type
  # Match format: species name (may contain spaces) followed by _type_projection
  pattern <- "^(.+?)_(ensemble|fallback)_projection$"

  if (grepl(pattern, base_name)) {
    # Extract matched parts
    matches <- regmatches(base_name, regexec(pattern, base_name))[[1]]

    if (length(matches) >= 3) {
      species_part <- matches[2] # Species name part
      type <- matches[3] # Type part

      # Process species name: replace underscores with spaces, keep only first two words
      species_words <- str_split(species_part, "[_\\s]+")[[1]]

      if (length(species_words) >= 2) {
        species_name <- paste(species_words[1], species_words[2], sep = " ")

        # Add to results dataframe
        results <- rbind(results, data.frame(
          species = species_name,
          type = type,
          stringsAsFactors = FALSE
        ))
      }
    }
  } else {
    # If doesn't match standard format, output warning
    cat("Warning: filename doesn't match expected format:", file_name, "\n")
  }
}

# Remove duplicate rows
results <- unique(results)

# Sort by species name
results <- results[order(results$species), ]

# Reset row names
rownames(results) <- NULL

# Display results
print("Extracted species information:")
print(results)

# Save as CSV file
output_file <- "${WORK_ROOT}/results/evaluations/projection_summary.csv"
write.csv(results, file = output_file, row.names = FALSE, quote = FALSE)

cat("Results saved to:", output_file, "\n")
cat("Total processed", nrow(results), "species\n")

# Display type statistics
cat("\nType statistics:\n")
print(table(results$type))
