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
