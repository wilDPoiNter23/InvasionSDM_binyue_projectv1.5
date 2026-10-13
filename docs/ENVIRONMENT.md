# Observed HPC environment

[hpc_environment_observed_20261013.csv](../metadata/hpc_environment_observed_20261013.csv) records two different evidence sources read on 2026-10-13. No packages were installed and no scientific scripts were executed to produce this profile.

## Recorded paired-release session

The paired release's existing `logs/session_information.txt` reports R 4.4.1 (2024-06-14), `aarch64-unknown-linux-gnu`, terra 1.7-78, mgcv 1.9-1, digest 0.6.37 and nlme 3.1-164. Loaded namespace versions are also transcribed into the CSV. Its original file SHA-256 is `cb64eb27d9f1a0fb789787d43859afbd38306aab43e48a329b548790978a6f43`; its observed modification time is 2026-10-12 12:31:53 UTC. Modification time is file metadata, not proof of the exact session capture time.

This record identifies a paired-release analysis session. It does not establish the environment of every historical species fit, local city analysis or manuscript figure. Base packages listed without versions in the session output are not assigned inferred package versions.

## Installed package descriptions

The `installed_DESCRIPTION` rows describe the current package metadata in two explicitly scanned locations: the configured HPC user library with layout `renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu`, and the R 4.4.1 base library. The CSV supplies package versions, recorded build information, DESCRIPTION SHA-256 values, observation time and the public scripts that contain literal library/require/namespace references. References include optional components and are not a minimal installation specification.

Of 73 referenced package names, 52 have an observed DESCRIPTION and 21 were not found in those two libraries. `NOT_FOUND_IN_SCANNED_LIBRARIES` does not mean a package is absent from every machine or every library. In particular, local city/SEM/GAM/figure workflows may use other environments. Their package versions remain unrecorded here; no missing version is guessed.

An installed DESCRIPTION establishes package metadata presence, not successful loading or compatible compiled dependencies. The profile does not capture the complete dependency graph, GDAL/GEOS/PROJ, Java/MaxEnt, system libraries, every R library search path or historical random-number state. The library directory's `renv` name does not establish that this repository contains an `renv.lock` or a restored environment.

Use the recorded paired-session versions as concrete evidence when identifying a compatible downstream environment. Use the current DESCRIPTION snapshot to inspect available packages and remaining requirements. Preserve the historical fit's own environment records whenever available. This CSV is an observed environment profile, not a historical full-fit lockfile or a guarantee of numerical reproduction.
