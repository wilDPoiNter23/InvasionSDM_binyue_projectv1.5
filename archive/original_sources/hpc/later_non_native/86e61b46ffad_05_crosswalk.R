# Original source: 05_crosswalk.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/05_crosswalk.R
# Original source SHA-256: 86e61b46ffadb0e4123cbfedb44ba9be463fa4123adbd904ed97043067ccb918
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 05_crosswalk.R — PHASE 5: TDWG-level3 area token -> ISO3 country crosswalk
# Method:
#  1. name reconciliation (exact/normalised) against WGSRPD level-3 names
#     + a small documented reconciliation table for GloNAF-specific names
#  2. geometric attribution: WGSRPD level-3 polygon x geoBoundaries country
#     polygons (dissolved ADM1). Countries overlapping >=2% of the L3 area,
#     or containing the L3 centroid, are attributed.
#  Every fuzzy/name-normalisation decision is written to the crosswalk CSV.
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

mp   <- read.csv(MAPPING_CSV, stringsAsFactors = FALSE)
clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
                  stringsAsFactors = FALSE)

w3 <- vect(WGSRPD_L3_SHP)
cat("L3 units read:", nrow(w3), "\n")
iv <- is.valid(w3)
cat("invalid L3 geometries in WGSRPD source:", sum(!iv), "\n")
if (any(!iv)) {
  cat("invalid unit names:", paste(trimws(w3$LEVEL3_NAM[!iv]), collapse = "; "), "\n")
  w3 <- makeValid(w3)
  cat("after makeValid invalid:", sum(!is.valid(w3)), " features:", nrow(w3), "\n")
}
l3_names <- trimws(w3$LEVEL3_NAM)

countries <- vect(COUNTRY_GPKG)
countries <- makeValid(countries)

norm <- function(x) tolower(gsub("[^a-z]", "", iconv(x, to = "ASCII//TRANSLIT")))
l3_norm <- norm(l3_names)

# ---- reconciliation table: GloNAF token -> WGSRPD level-3 unit name(s) ----
recon <- list(
  "Central American Pacific Is." = "C. American Pacific Is.",
  "Cocos (Keeling) Is."          = "Cocos (Keeling) I.",
  "Kirgizstan"                   = "Kirgizistan",
  "Leeward Is."                  = "Leeward Is. AB Ant",
  "Yakutiya"                     = "Yakutskiya",
  "Suriname"                     = "Surinam",
  "Czechia-Slovakia"             = "Czechoslovakia",
  "Eswatini"                     = "Swaziland",
  "NW. Balkan Pen."              = "Yugoslavia",
  "Sudan-South Sudan"            = c("Sudan", "South Sudan")   # WGSRPD has "Sudan" only (pre-2011); add South Sudan by ISO
)

# native tokens only (introduced==1 rows are excluded from native mapping)
tok_nat <- sort(unique(trimws(unlist(strsplit(trimws(mp$area[mp$introduced == 0]), ";")))))

find_l3 <- function(token) {
  if (token %in% names(recon)) return(recon[[token]])
  n <- norm(token)
  hit <- which(l3_norm == n)
  if (length(hit) >= 1) return(l3_names[hit])   # norm-matched (handles accents etc.)
  character(0)
}

tok_to_l3 <- lapply(tok_nat, find_l3)
names(tok_to_l3) <- tok_nat
unmatched <- names(tok_to_l3)[vapply(tok_to_l3, length, integer(1)) == 0]
cat("tokens:", length(tok_nat), "unmatched:", length(unmatched), "\n")
if (length(unmatched)) print(unmatched)

# ---- geometric attribution ----
l3_needed <- sort(unique(unlist(tok_to_l3)))
cat("L3 units needed:", length(l3_needed), "\n")

attr_rows <- list()
for (u in l3_needed) {
  if (u == "South Sudan") {
    # WGSRPD L3 "Sudan" includes both; explicitly add SSD via geoBoundaries
    piece <- countries[countries$iso3 == "SSD", ]
    frac <- data.frame(iso3 = "SSD", frac = 1.0, centroid = TRUE)
  } else {
    idx <- which(l3_names == u)
    if (length(idx) != 1) { cat("L3 lookup problem:", u, "->", length(idx), "\n"); next }
    l3p <- w3[idx, ]
    isec <- tryCatch(intersect(l3p, countries), error = function(e) NULL)
    if (is.null(isec) || nrow(isec) == 0) {
      # try centroid containment
      ctr <- centroids(l3p)
      cont <- tryCatch(which(relate(ctr, countries, "intersects")[, 1]), error = function(e) integer(0))
      if (length(cont)) {
        frac <- data.frame(iso3 = countries$iso3[cont], frac = 1.0, centroid = TRUE)
      } else {
        frac <- data.frame(iso3 = character(0), frac = numeric(0), centroid = logical(0))
      }
    } else {
      areas <- expanse(isec, unit = "m")
      total <- sum(areas)
      agg <- tapply(areas, isec$iso3, sum)
      frac <- data.frame(iso3 = names(agg), frac = as.numeric(agg) / total, centroid = FALSE)
      ctr <- centroids(l3p)
      cont <- tryCatch(which(relate(ctr, countries, "intersects")[, 1]), error = function(e) integer(0))
      if (length(cont)) {
        ciso <- countries$iso3[cont]
        for (ci in ciso) if (!(ci %in% frac$iso3)) frac <- rbind(frac, data.frame(iso3 = ci, frac = 0, centroid = TRUE))
      }
    }
  }
  if (nrow(frac)) {
    attr_rows[[u]] <- cbind(l3_unit = u, frac)
  } else {
    attr_rows[[u]] <- data.frame(l3_unit = u, iso3 = NA_character_, frac = NA_real_, centroid = FALSE)
  }
}
attr_df <- do.call(rbind, attr_rows)
row.names(attr_df) <- NULL

cat("L3 units with no country attribution:\n")
print(attr_df[is.na(attr_df$iso3), ])

# ---- build crosswalk CSV (one row per token x ISO3) ----
cw <- list()
for (tk in tok_nat) {
  units <- tok_to_l3[[tk]]
  if (length(units) == 0) {
    cw[[length(cw) + 1]] <- data.frame(
      native_country_original = tk,
      native_country_standardised = NA_character_,
      native_country_code = NA_character_,
      boundary_country_name = NA_character_,
      boundary_country_code = NA_character_,
      match_type = "UNMATCHED",
      match_confidence = NA_real_,
      overlap_fraction = NA_real_,
      manual_review_required = TRUE,
      notes = "No WGSRPD level-3 unit and no documented reconciliation.",
      stringsAsFactors = FALSE)
    next
  }
  rows_u <- list()
  for (u in units) {
    sub <- attr_df[attr_df$l3_unit == u & !is.na(attr_df$iso3), ]
    if (nrow(sub) == 0) {
      rows_u[[length(rows_u) + 1]] <- data.frame(
        native_country_original = tk,
        native_country_standardised = u,
        native_country_code = NA_character_,
        boundary_country_name = NA_character_,
        boundary_country_code = NA_character_,
        match_type = "UNMATCHED",
        match_confidence = NA_real_,
        overlap_fraction = NA_real_,
        manual_review_required = TRUE,
        notes = paste("L3 unit", u, "has no overlapping country geometry."),
        stringsAsFactors = FALSE)
      next
    }
    keep <- sub$frac >= 0.02 | sub$centroid
    sub <- sub[keep, , drop = FALSE]
    if (nrow(sub) == 0) sub <- attr_df[attr_df$l3_unit == u & !is.na(attr_df$iso3), , drop = FALSE]
    for (j in seq_len(nrow(sub))) {
      iso <- sub$iso3[j]
      nm  <- countries$country_name[match(iso, countries$iso3)]
      mt  <- if (tk %in% names(recon)) "NORMALISED_NAME" else "EXACT_NAME"
      if (tk == "Sudan-South Sudan") mt <- "MANUAL_CONFIRMED"
      if (!(tk %in% names(recon)) && norm(tk) != norm(u)) mt <- "NORMALISED_NAME"
      conf <- if (is.na(sub$frac[j])) 0.5 else min(1, max(0, sub$frac[j]))
      rows_u[[length(rows_u) + 1]] <- data.frame(
        native_country_original = tk,
        native_country_standardised = u,
        native_country_code = iso,
        boundary_country_name = nm,
        boundary_country_code = iso,
        match_type = mt,
        match_confidence = conf,
        overlap_fraction = sub$frac[j],
        manual_review_required = (conf < 0.5) || (tk %in% names(recon)),
        notes = if (tk %in% names(recon)) {
          paste0("GloNAF name '", tk, "' reconciled to WGSRPD level-3 '", u, "'.")
        } else if (norm(tk) != norm(u)) {
          paste0("Accent/punctuation-normalised match: '", tk, "' -> '", u, "'.")
        } else {
          ""
        },
        stringsAsFactors = FALSE)
    }
  }
  cw[[length(cw) + 1]] <- do.call(rbind, rows_u)
}
crosswalk <- do.call(rbind, cw)
write.csv(crosswalk, file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"),
          row.names = FALSE)
cat("Wrote:", file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"),
    "rows:", nrow(crosswalk), "\n")
cat("match_type counts:\n"); print(table(crosswalk$match_type))

# ---- patch species_native_mapping_clean.csv with country codes ----
# NOTE: rebuild the clean table from the AUTHORITATIVE source (manifest v3 +
# mapping CSV) so this step is robust to any concurrent edits of the clean file
# (a parallel session may have replaced tokens with reconciled L3 names).
tok_iso <- split(crosswalk$native_country_code[crosswalk$match_type != "UNMATCHED"],
                 crosswalk$native_country_original[crosswalk$match_type != "UNMATCHED"])
tok_iso <- lapply(tok_iso, function(x) sort(unique(x[!is.na(x) & x != ""])))

# reverse map: reconciled L3 name -> original GloNAF token
rev_map <- setNames(crosswalk$native_country_original,
                    crosswalk$native_country_standardised)
rev_map <- rev_map[!duplicated(rev_map) & !is.na(rev_map)]

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
synonym_map <- c("Spartina alterniflora" = "Sporobolus alterniflorus")

clean <- data.frame(
  accepted_taxon          = mani$accepted_taxon,
  original_name_in_mapping = NA_character_,
  taxonomic_match_type    = NA_character_,
  native_country_original = NA_character_,
  native_country_standardised = NA_character_,
  native_country_code     = NA_character_,
  mapping_status          = NA_character_,
  notes                   = "",
  stringsAsFactors        = FALSE)

row_by_name <- match(clean$accepted_taxon, mp$species_name)
for (i in seq_len(nrow(clean))) {
  tx <- clean$accepted_taxon[i]
  if (!is.na(row_by_name[i])) {
    mr <- mp[row_by_name[i], ]
    clean$original_name_in_mapping[i] <- mr$species_name
    clean$native_country_original[i]  <- mr$area
    toks <- trimws(unlist(strsplit(mr$area, ";")))
    toks <- toks[toks != ""]
    clean$native_country_standardised[i] <- paste(sort(unique(toks)), collapse = "; ")
    if (mr$introduced == 1) {
      clean$taxonomic_match_type[i] <- "EXACT_NAME"
      clean$mapping_status[i]        <- "MISSING_NATIVE_MAPPING"
      clean$notes[i] <- paste0("Mapping row exists but lists introduced-only distribution (introduced=1, '",
                               mr$note, "'); no native-range list available in source. Excluded from native masking.")
    } else {
      clean$taxonomic_match_type[i] <- "EXACT_NAME"
      # resolve codes from ORIGINAL tokens (fallback: reconciled L3 names)
      codes <- unique(unlist(tok_iso[toks]))
      bad   <- toks[!(toks %in% names(tok_iso))]
      if (length(bad)) {
        via <- unique(unlist(tok_iso[unname(rev_map[bad])]))
        codes <- sort(unique(c(codes, via)))
        still <- setdiff(bad, names(rev_map))
        if (length(still)) {
          clean$notes[i] <- paste0("UNRESOLVED area tokens: ", paste(still, collapse = ", "))
          clean$mapping_status[i] <- "UNMATCHED_COUNTRY"
        } else {
          clean$mapping_status[i] <-
            if (length(toks) > 1) "MULTIPLE_NATIVE_COUNTRIES" else "EXACT_MATCH"
        }
      } else {
        clean$mapping_status[i] <-
          if (length(toks) > 1) "MULTIPLE_NATIVE_COUNTRIES" else "EXACT_MATCH"
      }
      clean$native_country_code[i] <- paste(sort(unique(codes)), collapse = ";")
    }
  } else if (tx %in% names(synonym_map)) {
    src <- synonym_map[[tx]]
    mr  <- mp[mp$species_name == src, ]
    stopifnot(nrow(mr) == 1)
    clean$original_name_in_mapping[i] <- src
    clean$native_country_original[i]  <- mr$area
    toks <- trimws(unlist(strsplit(mr$area, ";")))
    toks <- toks[toks != ""]
    clean$native_country_standardised[i] <- paste(sort(unique(toks)), collapse = "; ")
    codes <- unique(unlist(tok_iso[toks]))
    bad   <- toks[!(toks %in% names(tok_iso))]
    if (length(bad)) {
      codes <- sort(unique(c(codes, unique(unlist(tok_iso[unname(rev_map[bad])])))))
      still <- setdiff(bad, names(rev_map))
      if (length(still)) {
        clean$notes[i] <- paste0("UNRESOLVED area tokens: ", paste(still, collapse = ", "))
        clean$mapping_status[i] <- "UNMATCHED_COUNTRY"
      } else {
        clean$mapping_status[i] <- "SYNONYM_RECONCILED"
      }
    } else {
      clean$mapping_status[i] <- "SYNONYM_RECONCILED"
    }
    clean$native_country_code[i] <- paste(sort(unique(codes)), collapse = ";")
    clean$taxonomic_match_type[i] <- "SYNONYM"
    clean$notes[i] <- paste0("Accepted taxon '", tx, "' reconciled to mapping row '", src,
                             "' (POWO synonym).")
  } else {
    clean$taxonomic_match_type[i] <- "NONE"
    clean$mapping_status[i]        <- "AMBIGUOUS_MAPPING"
    clean$notes[i] <- "No mapping row and no documented synonym found."
  }
}
stopifnot(!any(is.na(clean$mapping_status)))

write.csv(clean, file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
          row.names = FALSE)
cat("Patched clean mapping with ISO3 codes.\n")
cat("status counts now:\n"); print(table(clean$mapping_status))

saveRDS(list(tok_iso = tok_iso, crosswalk = crosswalk),
        file.path(DIR_PROVENANCE, "crosswalk_internal.rds"))
cat("Done.\n")
