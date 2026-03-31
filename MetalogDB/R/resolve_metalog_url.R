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
