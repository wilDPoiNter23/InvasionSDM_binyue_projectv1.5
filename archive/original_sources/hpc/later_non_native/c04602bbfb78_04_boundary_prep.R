# Original source: 04_boundary_prep.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/04_boundary_prep.R
# Original source SHA-256: c04602bbfb782f8d0ae9a1f25fdfa288d2949c53cc6057906a7a0593d4e172fa
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 04_boundary_prep.R — PHASE 4: dissolve geoBoundaries ADM1 to country level
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages({ library(sf); library(rnaturalearth) })
sf_use_s2(FALSE)   # planar geometry operations (avoid s2 edge-crossing failures)

# IMPORTANT SOURCE ISSUE (documented, 2026-08-14):
# geoBoundariesCGAZ_ADM1.shp contains one corrupt shape record (shape 263,
# Crooked Island, BHS) and 508 empty feature geometries when read by GDAL.
# The sibling *_packed.shp (same feature order, verified 60/60 sampled rows
# have identical coordinates) has 3224 complete, valid geometries but no DBF.
# Fix: geometry from *_packed.shp + attributes (shapeGroup=ISO3, shapeName)
# from the original .shp DBF. Source files are NOT modified.
v_geom  <- st_read(file.path(dirname(BOUNDARY_SHP), "geoBoundariesCGAZ_ADM1_packed.shp"), quiet = TRUE)
v_attr  <- suppressWarnings(st_read(BOUNDARY_SHP, quiet = TRUE))
stopifnot(nrow(v_geom) == nrow(v_attr))

v <- v_geom
v$shapeGroup <- v_attr$shapeGroup
v$shapeName  <- v_attr$shapeName
v$shapeID    <- v_attr$shapeID
v$shapeType  <- v_attr$shapeType
# packed shapefile ships without .prj -> CRS must be set explicitly (WGS84)
v <- st_set_crs(v, 4326)

# Disputed zones (shapeGroup is numeric, shapeType DISP) are excluded from the
# country dissolve used for masks; they are recorded in the inventory as not usable.
is_disp <- grepl("^[0-9]+$", v$shapeGroup)
cat("DISP (disputed) features excluded from country layer:", sum(is_disp), "\n")

cat("ADM1 features:", nrow(v), "ISO3 groups:", length(unique(v$shapeGroup[!is_disp])), "\n")
cat("empty geometries:", sum(st_is_empty(v)), " invalid:", sum(!st_is_valid(v)), "\n")
v <- st_make_valid(v)   # repair any self-intersections before union

# dissolve by ISO3 (base-R split/union to avoid extra dependencies)
v_use <- v[!is_disp, ]
iso <- v_use$shapeGroup
spl <- split(seq_len(nrow(v_use)), iso)
geoms <- unlist(lapply(spl, function(i) st_union(st_geometry(v_use[i, ]))),
                recursive = FALSE)
countries <- st_sf(
  iso3 = names(spl),
  geometry = st_sfc(geoms, crs = st_crs(v_use)),
  stringsAsFactors = FALSE)
countries <- st_make_valid(countries)
cat("country features after dissolve:", nrow(countries),
    " invalid:", sum(!st_is_valid(countries)), "\n")

# ---- territories absent from the CGAZ ADM1 export (documented augmentation) ----
# The ADM1 DBF contains no rows for ~19 small territories (e.g. ABW, CYM, REU,
# SHN, PYF, COK...). Their polygons are taken from the WGSRPD level-3 shapes
# (already an auxiliary input of this workflow; same TDWG scheme as the mapping
# table). These rows are flagged SOURCE=WGSRPD_L3 in the inventory.
w3 <- suppressWarnings(st_read(WGSRPD_L3_SHP, quiet = TRUE))
w3 <- st_set_crs(w3, 4326)
w3 <- st_make_valid(w3)
l3_unit <- function(nms) {
  i <- which(trimws(w3$LEVEL3_NAM) %in% nms)
  st_make_valid(st_union(st_geometry(w3[i, ])))
}

territory_map <- list(
  ABW = c("Aruba"),
  SHN = c("St.Helena", "Ascension", "Tristan da Cunha"),
  BMU = c("Bermuda"),
  CYM = c("Cayman Is."),
  IOT = c("Chagos Archipelago"),
  COK = c("Cook Is."),
  FLK = c("Falkland Is."),
  FRO = c("Føroyar"),
  UMI = c("Wake I.", "Howland-Baker Is."),
  PYF = c("Marquesas", "Society Is.", "Tuamotu", "Tubuai Is."),
  NCL = c("New Caledonia"),
  NIU = c("Niue"),
  PCN = c("Pitcairn Is."),
  REU = c("Réunion"),
  TCA = c("Turks-Caicos Is."),
  TKL = c("Tokelau-Manihiki"),
  WLF = c("Wallis-Futuna Is."),
  SJM = c("Svalbard"),
  ATF = c("Mozambique Channel Is."),
  ANT = c("Netherlands Antilles"),      # Curacao, Sint Maarten, BES islands
  SCS = c("South China Sea")            # disputed islands; kept as auxiliary unit
)
augment_map <- list(
  NZL = c("Kermadec Is.", "Antipodean Is."),
  JPN = c("Marcus I."),
  PRT = c("Selvagens")
)

territory_rows <- list()
for (code in names(territory_map)) {
  territory_rows[[code]] <- data.frame(
    iso3 = code, src = "WGSRPD_L3", stringsAsFactors = FALSE)
}
new_rows <- lapply(names(territory_map), function(code) {
  st_sf(iso3 = code, geometry = l3_unit(territory_map[[code]]),
        src = "WGSRPD_L3", stringsAsFactors = FALSE)
})
if (length(new_rows)) {
  new_sf <- do.call(rbind, new_rows)
  countries$src <- "CGAZ_ADM1"
  countries <- rbind(countries, new_sf)
}

# augment existing countries with WGSRPD island parts
for (code in names(augment_map)) {
  i <- which(countries$iso3 == code)
  if (!length(i)) next
  add <- l3_unit(augment_map[[code]])
  countries$geometry[i] <- st_make_valid(st_union(countries$geometry[i], add))
}
countries <- st_make_valid(countries)
cat("country features after territory augmentation:", nrow(countries), "\n")

# country name lookup: rnaturalearth (installed data) + manual fallback
ne <- tryCatch({
  s <- ne_countries(scale = 50, returnclass = "sf")
  data.frame(iso_a3 = s$iso_a3, name = s$name, stringsAsFactors = FALSE)
}, error = function(e) NULL)

manual_names <- c(
  "XKX" = "Kosovo",
  "ATA" = "Antarctica", "TWN" = "Taiwan", "PSE" = "Palestine", "KOS" = "Kosovo",
  "SOL" = "Somaliland", "ZAN" = "Zanzibar", "ESB" = "Somaliland (east)", "NKR" = "Nagorno-Karabakh",
  "XSP" = "Spratly Islands", "XPI" = "Paracel Islands", "XDS" = "Dhekelia", "XAK" = "Akrotiri",
  "XAD" = "Abyei", "XJK" = "Jammu and Kashmir", "XBI" = "Bir Tawil", "IOA" = "Indian Ocean Territories",
  "XCL" = "Clipperton Island", "BES" = "Bonaire, Sint Eustatius and Saba", "CUW" = "Curaçao",
  "SXM" = "Sint Maarten", "BVT" = "Bouvet Island", "HMD" = "Heard Island and McDonald Islands",
  "UMI" = "United States Minor Outlying Islands", "IOT" = "British Indian Ocean Territory",
  "SGS" = "South Georgia and the South Sandwich Islands", "NFK" = "Norfolk Island",
  "CXR" = "Christmas Island", "CCK" = "Cocos (Keeling) Islands", "PCN" = "Pitcairn",
  "TKL" = "Tokelau", "SHN" = "Saint Helena, Ascension and Tristan da Cunha",
  "ATF" = "French Southern Territories", "GLP" = "Guadeloupe", "MTQ" = "Martinique",
  "GUF" = "French Guiana", "REU" = "Réunion", "MYT" = "Mayotte", "SPM" = "Saint Pierre and Miquelon",
  "NCL" = "New Caledonia", "PYF" = "French Polynesia", "WLF" = "Wallis and Futuna",
  "MAF" = "Saint Martin", "BLM" = "Saint Barthélemy", "SJM" = "Svalbard and Jan Mayen",
  "COK" = "Cook Islands", "NIU" = "Niue", "REU" = "Réunion", "ANT" = "Former Netherlands Antilles islands",
  "SCS" = "South China Sea islands (disputed)",
  "ALA" = "Åland Islands", "GGY" = "Guernsey", "JEY" = "Jersey", "IMN" = "Isle of Man",
  "FRO" = "Faroe Islands", "GIB" = "Gibraltar", "ABW" = "Aruba", "AIA" = "Anguilla",
  "BMU" = "Bermuda", "VGB" = "British Virgin Islands", "CYM" = "Cayman Islands",
  "MSR" = "Montserrat", "TCA" = "Turks and Caicos Islands", "FLK" = "Falkland Islands",
  "GRL" = "Greenland", "PRI" = "Puerto Rico", "VIR" = "U.S. Virgin Islands", "GUM" = "Guam",
  "MNP" = "Northern Mariana Islands", "ASM" = "American Samoa", "COK" = "Cook Islands",
  "NIU" = "Niue", "SMR" = "San Marino", "MCO" = "Monaco", "VAT" = "Vatican City",
  "LIE" = "Liechtenstein", "AND" = "Andorra", "MLT" = "Malta", "BVT" = "Bouvet Island",
  "CHN" = "China", "KOR" = "South Korea", "PRK" = "North Korea", "RUS" = "Russia",
  "USA" = "United States of America", "VNM" = "Vietnam", "TLS" = "Timor-Leste",
  "COD" = "Democratic Republic of the Congo", "COG" = "Republic of the Congo",
  "CIV" = "Côte d'Ivoire", "TZA" = "Tanzania", "MKD" = "North Macedonia", "SYR" = "Syria",
  "LAO" = "Laos", "BRN" = "Brunei", "BOL" = "Bolivia", "VEN" = "Venezuela", "TTO" = "Trinidad and Tobago",
  "MDA" = "Moldova", "GNB" = "Guinea-Bissau", "SSD" = "South Sudan", "STP" = "São Tomé and Príncipe",
  "CPV" = "Cape Verde", "FSM" = "Federated States of Micronesia", "MHL" = "Marshall Islands",
  "PLW" = "Palau", "NRU" = "Nauru", "KIR" = "Kiribati", "TUV" = "Tuvalu", "SLB" = "Solomon Islands",
  "VUT" = "Vanuatu", "FJI" = "Fiji", "TON" = "Tonga", "WSM" = "Samoa", "PNG" = "Papua New Guinea",
  "SYC" = "Seychelles", "MUS" = "Mauritius", "COM" = "Comoros", "MDG" = "Madagascar",
  "BHR" = "Bahrain", "QAT" = "Qatar", "ARE" = "United Arab Emirates", "KWT" = "Kuwait",
  "OMN" = "Oman", "YEM" = "Yemen", "SAU" = "Saudi Arabia", "JOR" = "Jordan", "ISR" = "Israel",
  "LBN" = "Lebanon", "IRQ" = "Iraq", "IRN" = "Iran", "AFG" = "Afghanistan", "PAK" = "Pakistan",
  "IND" = "India", "LKA" = "Sri Lanka", "MDV" = "Maldives", "NPL" = "Nepal", "BTN" = "Bhutan",
  "BGD" = "Bangladesh", "MMR" = "Myanmar", "THA" = "Thailand", "KHM" = "Cambodia", "MYS" = "Malaysia",
  "SGP" = "Singapore", "IDN" = "Indonesia", "PHL" = "Philippines", "AUS" = "Australia",
  "NZL" = "New Zealand", "EGY" = "Egypt", "LBY" = "Libya", "TUN" = "Tunisia", "DZA" = "Algeria",
  "MAR" = "Morocco", "ESH" = "Western Sahara", "MRT" = "Mauritania", "MLI" = "Mali", "NER" = "Niger",
  "TCD" = "Chad", "SDN" = "Sudan", "ERI" = "Eritrea", "DJI" = "Djibouti", "SOM" = "Somalia",
  "ETH" = "Ethiopia", "KEN" = "Kenya", "UGA" = "Uganda", "RWA" = "Rwanda", "BDI" = "Burundi",
  "TGO" = "Togo", "BEN" = "Benin", "NGA" = "Nigeria", "GHA" = "Ghana", "BFA" = "Burkina Faso",
  "SEN" = "Senegal", "GMB" = "Gambia", "GNB" = "Guinea-Bissau", "GIN" = "Guinea", "SLE" = "Sierra Leone",
  "LBR" = "Liberia", "CMR" = "Cameroon", "GAB" = "Gabon", "GNQ" = "Equatorial Guinea",
  "CAF" = "Central African Republic", "AGO" = "Angola", "ZMB" = "Zambia", "ZWE" = "Zimbabwe",
  "MWI" = "Malawi", "MOZ" = "Mozambique", "BWA" = "Botswana", "NAM" = "Namibia", "ZAF" = "South Africa",
  "LSO" = "Lesotho", "SWZ" = "Eswatini", "MDG" = "Madagascar", "ARG" = "Argentina", "BOL" = "Bolivia",
  "BRA" = "Brazil", "CHL" = "Chile", "COL" = "Colombia", "ECU" = "Ecuador", "GUY" = "Guyana",
  "PRY" = "Paraguay", "PER" = "Peru", "SUR" = "Suriname", "URY" = "Uruguay",
  "CAN" = "Canada", "MEX" = "Mexico", "GTM" = "Guatemala", "BLZ" = "Belize", "SLV" = "El Salvador",
  "HND" = "Honduras", "NIC" = "Nicaragua", "CRI" = "Costa Rica", "PAN" = "Panama", "CUB" = "Cuba",
  "HTI" = "Haiti", "DOM" = "Dominican Republic", "JAM" = "Jamaica", "BHS" = "Bahamas",
  "ATG" = "Antigua and Barbuda", "DMA" = "Dominica", "GRD" = "Grenada", "KNA" = "Saint Kitts and Nevis",
  "LCA" = "Saint Lucia", "VCT" = "Saint Vincent and the Grenadines", "BRB" = "Barbados",
  "GBR" = "United Kingdom", "IRL" = "Ireland", "FRA" = "France", "ESP" = "Spain", "PRT" = "Portugal",
  "DEU" = "Germany", "NLD" = "Netherlands", "BEL" = "Belgium", "LUX" = "Luxembourg",
  "CHE" = "Switzerland", "AUT" = "Austria", "ITA" = "Italy", "GRC" = "Greece", "DNK" = "Denmark",
  "NOR" = "Norway", "SWE" = "Sweden", "FIN" = "Finland", "ISL" = "Iceland", "EST" = "Estonia",
  "LVA" = "Latvia", "LTU" = "Lithuania", "POL" = "Poland", "CZE" = "Czechia", "SVK" = "Slovakia",
  "HUN" = "Hungary", "ROU" = "Romania", "BGR" = "Bulgaria", "SRB" = "Serbia", "HRV" = "Croatia",
  "BIH" = "Bosnia and Herzegovina", "SVN" = "Slovenia", "MNE" = "Montenegro", "ALB" = "Albania",
  "UKR" = "Ukraine", "BLR" = "Belarus", "MDA" = "Moldova", "RUS" = "Russia", "GEO" = "Georgia",
  "ARM" = "Armenia", "AZE" = "Azerbaijan", "TUR" = "Turkey", "CYP" = "Cyprus", "KAZ" = "Kazakhstan",
  "KGZ" = "Kyrgyzstan", "TJK" = "Tajikistan", "TKM" = "Turkmenistan", "UZB" = "Uzbekistan",
  "MNG" = "Mongolia", "JPN" = "Japan", "CHN" = "China", "PRK" = "North Korea", "KOR" = "South Korea",
  "GRC" = "Greece", "MKD" = "North Macedonia", "KOS" = "Kosovo", "BIH" = "Bosnia and Herzegovina",
  "ATA" = "Antarctica", "AZE" = "Azerbaijan", "KIR" = "Kiribati", "TUV" = "Tuvalu", "ASM" = "American Samoa",
  "WLF" = "Wallis and Futuna", "CUW" = "Curaçao", "SXM" = "Sint Maarten", "ABW" = "Aruba",
  "TCA" = "Turks and Caicos Islands", "FLK" = "Falkland Islands", "ZWE" = "Zimbabwe", "ZMB" = "Zambia"
)

iso3s <- sort(unique(countries$iso3))
names_found <- character(length(iso3s))
if (!is.null(ne)) {
  idx <- match(iso3s, ne$iso_a3)
  names_found <- ifelse(!is.na(idx), ne$name[idx], NA_character_)
}
missing <- is.na(names_found)
names_found[missing] <- unname(manual_names[iso3s[missing]])
still_missing <- iso3s[is.na(names_found)]
cat("ISO3 without resolved name:", length(still_missing), "\n")
if (length(still_missing)) print(still_missing)

countries$country_name <- names_found[match(countries$iso3, iso3s)]
# names for augmented territories (manual_names may already cover most)
countries$country_name[countries$iso3 == "ANT"] <- "Former Netherlands Antilles islands"
countries$country_name[countries$iso3 == "SCS"] <- "South China Sea islands (disputed)"
countries <- st_make_valid(countries)
countries <- st_set_crs(countries, 4326)
cat("invalid country geometries after dissolve (fixed):", sum(!st_is_valid(countries)), "\n")

st_write(countries, COUNTRY_GPKG, delete_dsn = TRUE, quiet = TRUE)

# ---- country_boundary_inventory.csv ----
adm1_counts <- table(v$shapeGroup)
inv <- data.frame(
  country_name = countries$country_name,
  source_country_code = countries$iso3,
  standardised_country_code = countries$iso3,
  ADM1_feature_count = as.integer(adm1_counts[countries$iso3]),
  geometry_valid = st_is_valid(countries),
  dissolved_country_geometry_valid = st_is_valid(countries),
  mapping_usable = TRUE,
  notes = ifelse(countries$src == "WGSRPD_L3",
    "Territory absent from geoBoundariesCGAZ_ADM1; geometry sourced from WGSRPD level-3 (auxiliary input).",
    "Dissolved from geoBoundariesCGAZ_ADM1 by shapeGroup (ISO3). Geometry taken from the *_packed.shp sibling (complete, valid); attributes from the original .shp DBF (508 empty geometries in original file documented in script header)."),
  stringsAsFactors = FALSE)

disp_names <- v$shapeName[is_disp]
disp_codes <- v$shapeGroup[is_disp]
if (length(disp_codes)) {
  inv <- rbind(inv, data.frame(
    country_name = paste0("DISPUTED: ", disp_names),
    source_country_code = disp_codes,
    standardised_country_code = NA_character_,
    ADM1_feature_count = 1L,
    geometry_valid = st_is_valid(v[is_disp, ]),
    dissolved_country_geometry_valid = NA,
    mapping_usable = FALSE,
    notes = "geoBoundaries DISP (disputed) zone; no ISO3 country attribution, excluded from country boundary layer used for native masks.",
    stringsAsFactors = FALSE))
}
write.csv(inv, file.path(DIR_INPUTS, "country_boundary_inventory.csv"), row.names = FALSE)

cat("Wrote:", COUNTRY_GPKG, "\n")
cat("Wrote:", file.path(DIR_INPUTS, "country_boundary_inventory.csv"), "\n")
