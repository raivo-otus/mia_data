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

# -------------
# Helpers FUNs
# -------------

# Function to check that inputs to fetchMetalogTSE
.validate_inputs <- function(collection, metadata, samplelist, use_cache) {
	allowed_collections <- c("human", "animal", "ocean_water", "other_environmental")
	if (missing(collection) || !collection %in% allowed_collections) {
		stop(
			"Validation Error: 'collection' must be one of: ",
			paste(paste0('""', allowed_collections, '""'), collapse = ", ")
		)
	}
	
	allowed_metadata <- c("core", "partially_harmonized", "all")
	if (!metadata %in% allowed_metadata) {
		stop(
			"Validation Error: 'metadata' must be one of: ",
			paste(paste0('""', allowed_metadata, '""'), collapse = ", ")
		)
	}

	if (!is.null(samplelist)) {
		ext <- tolower(tools::file_ext(samplelist))
		allowed_exts <- c("csv", "tsv", "txt", "json")
		if (!ext %in% allowed_exts) {
			stop(
				"Validation Error: samplelist file type must be one of: ",
				paste(allowed_exts, collapse = ", ")
			)
		}
	}

	if (!is.logical(use_cache) || length(use_cache) != 1 || is.na(use_cache)) {
		stop("Validation Error: 'use_cache' must be a single logical value (TRUE or FALSE)")
	}

	invisible(NULL)
}

# Function to download datafiles, adapted from Metalog's example script
.download_if_missing <- function(
  target_url,
  download_dir = ".data_cache",
  use_cache = TRUE
  ) {
  
  base_filename <- basename(target_url)
  
  # Caching Logic
  if (use_cache) {
    # Replace "latest" with a date regex pattern (YYYY-MM-DD)
    pattern <- sub("latest", "[0-9]{4}-[0-9]{2}-[0-9]{2}", base_filename)
    matching_files <- list.files(download_dir, pattern = pattern, full.names = TRUE)
    
    if (length(matching_files) > 0) {
      latest_file <- max(matching_files)
      message("Loaded cached file: ", latest_file)
      return(latest_file)
    }
  } else {
    message("Skipping cache. Forcing download for: ", base_filename)
  }
  
  # We make a simple GET request first just to see where "latest" redirects us
  message("Fetching file from Metalog...")
  response <- httr::GET(target_url, httr::config(followlocation = TRUE))
  
  if (httr::status_code(response) != 200) {
    stop("Error fetching the file! Status code: ", httr::status_code(response))
  }
  
  # Extract the final URL and save
  url_with_date <- response$url
  filename <- basename(url_with_date)
  destfile <- file.path(download_dir, filename)
  
  message("Downloading to: ", destfile)
  httr::GET(url_with_date, httr::write_disk(destfile, overwrite = TRUE))
  
  return(destfile)
}

# Function constructs download URL for requested metalog data from DB,
# If cache, checks if files already exist in datadir, and downloads if not.
# Otherwise downloads files to datadir.
.resolve_metalog_url <- function(collection, metadata, use_cache) {

	cache_dir <- ".data_cache"
	if (!dir.exists(cache_dir)) {
		dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
	}

	# Construct download URL's into target list
	base_url <- "https://metalog.embl.de/static/download"
	
	profile <- dplyr::case_when(
		collection == "human" ~ "human",
		collection == "animal" ~ "animal",
		collection == "ocean" ~ "ocean",
		collection == "other_environment" ~ "environmental"
	)
	assay_url <- sprintf("%s/profiles/%s_metaphlan4_latest.tsv.gz", base_url, profile)

	md_type <- dplyr::case_when(
		metadata == "core" ~ "core",
		metadata == "partially_harmonized" ~ "extended",
		metadata == "all" ~ "all"
	)
	md_url <- sprintf("%s/metadata/%s_%s_long_latest.tsv.gz", base_url, profile, md_type)

	# Get files
	assay_file <- .download_if_missing(
		target_url = assay_url,
		download_dir = cache_dir,
		use_cache = use_cache
	)

	md_file <- .download_if_missing(
		target_url = md_url,
		download_dir = cache_dir,
		use_cache = use_cache
	)

	return(list(
		assay = assay_file,
		md = md_file
	))
}

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

  # Injest data
  

  # Filtering

  # TSE construction

  # License injection

  # Explicit return of tse object

}

# Export function to namespace?
