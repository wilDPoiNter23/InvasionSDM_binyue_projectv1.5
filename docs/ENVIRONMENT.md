# Observed HPC environment

[hpc_environment_observed_20261013.csv](../metadata/hpc_environment_observed_20261013.csv)
combines the paired-release session record with installed package descriptions
read on 2026-10-13.

## Recorded paired-release session

The paired release's `logs/session_information.txt` reports R 4.4.1
(2024-06-14), `aarch64-unknown-linux-gnu`, terra 1.7-78, mgcv 1.9-1,
digest 0.6.37 and nlme 3.1-164. Loaded namespace versions are transcribed into
the CSV. The original file SHA-256 is
`cb64eb27d9f1a0fb789787d43859afbd38306aab43e48a329b548790978a6f43`.
Its modification time was 2026-10-12 12:31:53 UTC; the session capture time
was not independently established.

This session applies to the paired release. Historical species fits, local city
analyses and figures require their own environment records. Package versions
omitted from the session output remain unspecified in the CSV.

## Installed package descriptions

The `installed_DESCRIPTION` rows cover the configured HPC user library,
`renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu`, and the
R 4.4.1 base library. The CSV records package versions, build information,
DESCRIPTION SHA-256 values, observation time and script references. These
references include optional components; install the dependencies for the
selected workflow.

Of 73 referenced packages, 52 had a DESCRIPTION file in the two libraries and
21 were marked `NOT_FOUND_IN_SCANNED_LIBRARIES`. That status is specific to
those locations. Versions in other local or server environments remain
unrecorded.

Use the paired-session versions to identify a compatible downstream setup.
Execution also depends on compiled libraries, GDAL/GEOS/PROJ, Java/MaxEnt and
the R library search path. The CSV records observed metadata rather than a
complete dependency lock or package-loading test. The repository currently
has no `renv.lock`; preserve the environment and random-number records for
each historical fit.

## Local implementation checks on 2026-11-03

Static and small synthetic checks used Windows R 4.5.1. The installed terra and
sf packages reported builds under R 4.5.2. Native-exclusion, polygon-partition,
whole-city weighting and synthetic raster-comparison checks exited successfully.
The pseudo-absence helper's `--unit-only` checks also exited successfully.

The optional synthetic BIOMOD formatting check passed its count and membership
assertions, then terminated with Windows native exit code `-1073741819`.
The process failed, so formatting needs a repeat check in a compatible runtime
before the preparation workflow is used. The test used synthetic inputs and
performed no model fitting.
