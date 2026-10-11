# Loads paths, never installs packages, creates directories, or runs an analysis.
project <- Sys.getenv("INVASIONSDM_PROJECT_ROOT", unset = getwd())
project <- normalizePath(project, mustWork = TRUE)
cfg <- Sys.getenv("INVASIONSDM_CONFIG", file.path(project, "config", "config.local.R"))
if (!file.exists(cfg)) stop("Missing local configuration: copy config/config.example.R to config/config.local.R and set external paths")
source(cfg, local = .GlobalEnv)
if (!exists("PATHS") || !is.list(PATHS)) stop("Configuration must define a named PATHS list")
publication_path <- function(alias, ...) {
  p <- PATHS[[alias]]
  if (is.null(p) || length(p) != 1L || is.na(p) || !nzchar(p) || grepl("SET_EXTERNAL", p))
    stop("Configure path alias: ", alias)
  file.path(p, ...)
}
publication_require_workspace <- function(required) {
  absent <- required[!vapply(required, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
  if (length(absent) && exists("WORKSPACE_RDS") && file.exists(WORKSPACE_RDS)) {
    objects <- readRDS(WORKSPACE_RDS)
    if (!is.list(objects) || is.null(names(objects))) stop("WORKSPACE_RDS must contain a named list")
    list2env(objects, envir = .GlobalEnv)
    absent <- required[!vapply(required, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
  }
  if (length(absent)) stop("Historical workspace dependency unresolved: ", paste(absent, collapse = ", "),
                           ". See docs/REPRODUCIBILITY.md; do not invent these objects.")
}
