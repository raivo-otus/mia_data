library(rix)

path_default_nix <- "."

rix(
  r_ver = "latest-upstream",
  r_pkgs = c(
    "Matrix",
    "TreeSummarizedExperiment",
    "data.table",
    "dplyr",
    "httr",
    "jsonlite",
    "mia", # Not strictly needed, just useful to test
    "rix" # Can be removed from final
  ),
  system_pkgs = NULL,
  git_pkgs = NULL,
  ide = "radian",
  project_path = path_default_nix,
  overwrite = TRUE,
  print = FALSE
)
