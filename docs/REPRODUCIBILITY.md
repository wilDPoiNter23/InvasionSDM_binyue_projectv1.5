# Reproducibility status

The repository supplies analysis implementations and statistical figure components. A syntax pass establishes that source can be parsed; reproducing manuscript numbers additionally requires the matching external inputs, saved model metadata, package environment and figure/table version.

## Static validation

Run from any directory with Python 3, Rscript and Bash available:

```sh
python tools/validate_publication.py
```

If those executables are outside PATH, supply `--rscript /path/to/Rscript` and `--bash /path/to/bash`. This command parses R and Python, checks Bash syntax, checks local Markdown links, verifies literal `publication_path` aliases against the example configuration and checks the eleven preserved source checksums. It does not install packages, source R scripts, load scientific data, submit jobs or run models. Missing checking executables produce a failed check rather than a false pass.

## Source coverage

The paired sensitivity has eleven production-source files in the preserved source archive. Their SHA-256 values are recorded in [archived_source_checksums.csv](../metadata/archived_source_checksums.csv); the public files matched those archived original bytes at the 2026-10-13 review. These include input freezing, input validation, species fitting/helpers, shared-denominator aggregation, independent sampled-cell checking, raster/city comparison, attrition diagnostics, the city association and two submission templates. This is a source-preservation claim, not a new acceptance of results.

The original archive also preserves scientific and audit sources outside this curated public workflow. It is not a substitute for the public analysis entrypoints, and the total archive file count is not the number of manuscript analyses. Historical scripts and later sensitivities must retain their different response definitions and species sets.

[METHODS_CODE_MAP](METHODS_CODE_MAP.md) lists the main analyses and figure components. Coverage has these practical limits:

| Component | Available implementation or supplied input | Remaining requirement |
|---|---|---|
| Species SDM and projections | Three-fold batch and separate five-fold implementation | Choose the saved design/options and package versions for the corresponding fitted objects |
| Historical manuscript aggregate | Aggregation implementation and staged normalized raster | Bind the exact historical writer, successful contributor list and projection versions to the deposited raster |
| PCA, SEM and city effects | Loading/composite equations, SEM fit and coefficient-based effects | Use archived composite/coefficient tables; the precise upstream SEM transformations and standardization are not fully specified |
| GAM and sensitivity components | Model, surface, curve, raw-proxy and spatial scripts | Supply the exact score table and figure-specific summaries; no analysis was re-fitted during this review |
| Maps and multi-panel figures | Statistical components and listed source data | Final cartography, manual panel arrangement and figure/table version selection remain separate |
| Supplementary Table 6 and Extended Data Table 3 | Manuscript source-data tables identified in the map | Their final tables are not reconstructed by substituting a different script or model |

## Open scientific and release requirements

The historical contributor candidate manifest contains 737 rows, whereas `metadata/species_list.csv` contains 813 taxonomic names from the later preservation pool. Neither roster size proves the exact successful contributors to the historical manuscript raster. A writer/contributor/projection binding is still required; do not replace the historical denominator with 813 or with the 684 paired sensitivity taxa. The paired sensitivity begins with 814 candidates and uses its own frozen accepted set.

The SEM composite input has undergone upstream response/covariate preprocessing. The retained composite equations cannot establish every transformation choice or its order. Reconstructing a missing historical preprocessing script by guessing would create a new analysis. Use the archived input for downstream reproduction and retain this limitation until original preprocessing evidence is located.

Exact external dataset editions, GBIF download identifiers, redistribution permissions, a data deposit identifier and the compatible R/package environment remain release requirements described in [DATA](DATA.md) and the [deposit plan](../zenodo/README.md). There is no locked package environment or declared code licence in this repository; the authors must select the code reuse licence. Passing static validation does not establish numerical reproduction, scientific acceptance or publication readiness.
