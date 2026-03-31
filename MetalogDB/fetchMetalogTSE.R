# This function downloads metaphlan profiles and associated metadata
# from the Metalog database. The option exists to filter to a subset
# if a samplelist is provided, downloaded from the Metalog webUI. 
# 
# Author: Rasmus Hindström
# Date: ---

# Libraries
library(mia)
library(data.table)
library(httr2)

# Source helpers
sapply(
  list.files(path = "R/", pattern = "\\.R$", full.names = TRUE),
  source
)

# -------------
# Main function
# -------------

fetchMetalogTSE <- function(
  collection,		# One of "human", "animal", "ocean_water", "other_environ"
  metadata = "core",	# One of "core", "partially_harmonized", "all"
  samplelist = NULL,
  cache = TRUE
) {

  # Validate inputs
  .validate_inputs(
    collection = collection,
    metadata = metadata,
    samplelist = samplelist,
    cache = cache
  )
  
  # Consruct download URLs, download and cache

  # Data ingest

  # Filtering

  # TSE construction

  # License injection

  # Explicit return of tse object

}

# Export function to namespace?
