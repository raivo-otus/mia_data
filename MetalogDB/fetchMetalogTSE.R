# This function downloads metaphlan profiles and associated metadata
# from the Metalog database. The option exists to filter to a subset
# if a samplelist is provided, downloaded from the Metalog webUI. 
# 
# Author: Rasmus Hindström
# Date: ---

# Libraries
library(mia)
library(data.table)
library(dplyr)
library(httr)

# Source helpers
sapply(
  list.files(path = "R/", pattern = "\\.R$", full.names = TRUE),
  source
)

# -------------
# Main function
# -------------

fetchMetalogTSE <- function(
  collection,		# One of "human", "animal", "ocean", "other_environment"
  metadata = "core",	# One of "core", "partially_harmonized", "all"
  samplelist = NULL,
  use_cache = TRUE
) {

  # Validate inputs
  .validate_inputs(
    collection = collection,
    metadata = metadata,
    samplelist = samplelist,
    use_cache = use_cache
  )

  # Construct download URLs, download and cache
  data_files <- .resolve_metalog_url(collection, metadata, use_cache)

  # Data ingest

  # Filtering

  # TSE construction

  # License injection

  # Explicit return of tse object

}

# Export function to namespace?
