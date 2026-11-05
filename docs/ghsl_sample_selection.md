# Urban-boundary sensitivity for source-city selection

This supplementary workflow compares linked and unlinked records in the
135-city source frame under a common GHSL urban-centre definition. It does not
replace manuscript boundaries, refit SDMs, or change the original analyses.

## Inputs and definitions

- GHS-UCDB R2024A V1.2, fixed 2025 urban-centre polygons. The general,
  geography, GHSL and socioeconomic theme dictionaries accompany the release.
- A country- and name-based source crosswalk, checked against coordinates and
  documented principal-city locations. Shared centres are retained explicitly;
  nearest-centroid substitution is not used for unmatched cities.
- Provider population totals and built-up surface for 2020. Built-up surface
  is converted from square metres to square kilometres before calculating its
  share of the urban-centre polygon.
- Published SC_GDP_AVG_2020, the zonal average of GDP PPP grid values. This
  should not be labelled municipal GDP or GDP per capita without additional
  verification of the underlying raster. The release dictionary names the
  source paper on GDP per capita, but that title alone does not establish the
  units of this derived field.
- Existing project GDP and population grids for 2020, built-up grid for 2025,
  and VIIRS night lights for 2023. Exact polygon-cell overlaps multiplied by
  geodesic cell area weight their means. Coverage is reported separately.
- Independent coastal reclamation polygons with five-year endpoints from
  1990 to 2020. Intersecting components are unioned before measuring area to
  avoid duplicate counting. Zero means no mapped intersection in this boundary.
- GBIF Plantae records (taxon key 6), with coordinates, no flagged geospatial
  issue and occurrence status PRESENT. Queries retain all dates, 2000-2024,
  and 2000-2024 with reported coordinate uncertainty of 0-1000 m. The latter
  excludes unspecified uncertainty. Counts reflect recording volume, not
  inventory completeness or survey visits. Historical occurrence years are
  not historical database snapshots. Save requests and responses with hashes
  and UTC retrieval times because the index changes.

## Execution

All scripts take external JSON configurations except the figure script, which
takes comparison and figure directories. No private paths are embedded in code.

1. Run `prepare_ghsl_city_crosswalk.py` with source pool, general geopackage,
   WGS84 centroid CSV and reviewed name links. The provider's centroid layer
   must be transformed from its declared projected CRS before exporting lon/lat.
2. Run `extract_ghsl_city_covariates.R` with the crosswalk, general geopackage,
   provider CSVs, predictor stack and coastal-change shapefile. It writes
   atomic GBIF query geometries, independent urban units, source-row summaries,
   coverage, input hashes and a spatial receipt.
3. Run `query_gbif_city_record_counts.py` with `geometry_csv`, `output_root` and
   `windows` set to `["all_available", "years2000_2024", "precise2000_2024"]`
   for the complete comparison below. The query client's two-window default
   omits the precision sensitivity required by the comparison script.
   Cached compatible responses are reused; failed requests remain
   missing and are never replaced by zero. Full MULTIPOLYGON geometries use
   anticlockwise outer rings and clockwise holes.
4. Run `compare_ghsl_selection.R` with `unit_covariates`, `crosswalk`,
   `gbif_counts`, `raster_coverage` and `output_root`.
5. Run `render_ghsl_selection.R` with the comparison and figure directories.

Crosswalk preparation, spatial extraction and group comparisons require fresh
output directories. The GBIF query step alone resumes compatible saved requests.

## Comparison design

The source membership is 106 linked and 29 unlinked records; this is not a
reconstructed 108-versus-27 occurrence-based exclusion experiment. One source
city, Tokoname, lacks a defensible match in this urban-centre product and remains
missing in the 135-row roster. The other 134 records correspond to 131 distinct
urban units: 102 linked and 29 unlinked. Guangzhou, Shenzhen and Zhongshan share
one centre; Dubai and Sharjah share another. The compound Tampa-St Petersburg
source record uses the union of two GHSL centres. Units never cross groups.

Primary comparisons cover continent, latitude, urban-centre area, mapped
reclamation area and share, GDP, population, built-up share, night lights and
all-plant record counts. All numeric differences except latitude use log1p
before calculating standardized mean differences. Intervals use 1999
group-stratified bootstrap resamples of distinct urban units. Two-sided
Wilcoxon rank-sum tests and a Monte Carlo Fisher test for continent are adjusted
together by Benjamini-Hochberg across these ten comparisons. Secondary
indicators form a separate adjustment family. Raw medians, interquartile ranges,
coverage and source-record-weighted descriptive summaries are retained.

Shared centres are not treated as independent observations in inference.
Source-row results have no p-values or bootstrap intervals. An additional
coverage check restricts project-grid comparisons to units with at least 80%
valid spatial coverage. A nonsignificant result does not establish equivalence;
differences and uncertainty should be reported together.

The analysis describes sample composition. It cannot alone reconstruct whether
each historical exclusion was caused by occurrence availability or demonstrate
that the ecological association is unbiased. Those questions require the
original filtering sequence or an analysis of outcomes across the source frame.
