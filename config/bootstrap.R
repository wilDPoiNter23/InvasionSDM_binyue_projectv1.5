# Load external data and output paths.
project <- normalizePath(Sys.getenv("INVASIONSDM_PROJECT_ROOT", getwd()), mustWork=TRUE)
config_file <- Sys.getenv("INVASIONSDM_CONFIG", file.path(project, "config", "config.local.R"))
if (!file.exists(config_file)) stop("Copy config/config.example.R to config/config.local.R and set the data paths.")
source(config_file, local=.GlobalEnv)
publication_path <- function(alias, ...) {
  base <- PATHS[[alias]]
  if (is.null(base) || length(base)!=1L || is.na(base) || !nzchar(base) || grepl("SET_PATH",base))
    stop("Set the configuration path: ", alias)
  path <- file.path(base, ...)
  if (alias == "OUTPUT_ROOT") {
    parent <- if (grepl("\\.[A-Za-z0-9]{1,6}$", basename(path))) dirname(path) else path
    dir.create(parent, recursive=TRUE, showWarnings=FALSE)
  }
  path
}
