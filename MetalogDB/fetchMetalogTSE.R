# This function downloads metaphlan profiles and associated metadata
# from the Metalog database. The option exists to filter to a subset
# if a samplelist is provided, downloaded from the Metalog webUI.
#
# Author: Rasmus Hindström
# Date: ---

# Libraries
library(Matrix)
library(TreeSummarizedExperiment)
library(data.table)
library(dplyr)
library(httr)

# -------------
# Helpers FUNs
# -------------

# Function to check that inputs to fetchMetalogTSE
.validate_inputs <- function(collection, metadata, samplelist, use_cache) {
  allowed_collections <- c("human", "animal", "ocean", "environmental")
  if (missing(collection) || !collection %in% allowed_collections) {
    stop(
      "Validation Error: 'collection' must be one of: ",
      paste(paste0('""', allowed_collections, '""'), collapse = ", ")
    )
  }

  allowed_metadata <- c("core", "extended", "all")
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
# Function to download datafiles, adapted to handle broken HTTP redirects
.download_if_missing <- function(
  target_url,
  download_dir = ".data_cache",
  use_cache = TRUE
) {
  base_filename <- basename(target_url)

  # Caching Logic
  if (use_cache) {
    pattern <- sub("latest", "[0-9]{4}-[0-9]{2}-[0-9]{2}", base_filename)
    matching_files <- list.files(download_dir, pattern = pattern, full.names = TRUE)

    if (length(matching_files) > 0) {
      latest_file <- max(matching_files)
      message("Loaded cached file: ", latest_file)
      return(latest_file)
    }
  }

  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  }

  message("Fetching file from: ", target_url)

  ### Masquarade as a browser, in case of anti-scraping measures
  ua <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")

  ### Metalog server downgrades protocol from https >> http, 
  ### Since 1st of April 2026. This janky solution is needed to force https

  # Catch the redirect
  initial_req <- httr::GET(target_url, ua, httr::config(followlocation = FALSE))
  
  # Check if we got a redirect to HTTP
  if (initial_req$status_code >= 300 && initial_req$status_code < 400) {
    # Extract the redirected URL
    final_url <- initial_req$headers$location
    
    # Force HTTPS protocol
    final_url <- sub("^http://", "https://", final_url)
    message("Intercepted redirect. Forcing HTTPS: ", final_url)
    
  } else if (initial_req$status_code == 200) {
    final_url <- target_url # No redirect occurred
  } else {
    stop("Initial request failed with status: ", initial_req$status_code)
  }

  # Download the actual file from the corrected URL
  filename <- basename(final_url)
  destfile <- file.path(download_dir, filename)
  
  message("Downloading to: ", destfile)
  final_req <- httr::GET(
    final_url, 
    ua,
    httr::write_disk(destfile, overwrite = TRUE)
  )

  if (httr::status_code(final_req) != 200) {
    if (file.exists(destfile)) file.remove(destfile)
    stop("Error downloading the file! Status code: ", httr::status_code(final_req))
  }

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
  
  profile <- collection
  assay_url <- sprintf("%s/profiles/%s_metaphlan4_latest.tsv.gz", base_url, profile)

  md_type <- metadata
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

# Function loads assay profile as sparse matrix
.load_assay <- function(path, sep = "\t") {
  # Read and subset to SGB only
  dt <- fread(path, sep = sep)
  dt <- dt[startsWith(clade_name, "t__SGB"), ]

  # Ensure expected names and types
  setnames(dt, c("sample_alias", "clade_name", "rel_abund"))
  dt[, rel_abund := as.numeric(rel_abund)]
  dt <- dt[!is.na(rel_abund) & rel_abund != 0]

  # Aggregate duplicates (taxon, sample) -> sum(rel_abund)
  setkey(dt, clade_name, sample_alias)
  dt <- dt[, .(rel_abund = sum(rel_abund)), by = .(clade_name, sample_alias)]
  # Map taxa and samples
  taxa <- sort(unique(dt$clade_name))
  samples <- sort(unique(dt$sample_alias))
  i <- match(dt$clade_name, taxa)
  j <- match(dt$sample_alias, samples)
  x <- dt$rel_abund

  # Build sparse matrix (rows = taxa, cols = samples)
  X <- sparseMatrix(
    i = i, j = j, x = x,
    dims = c(length(taxa), length(samples)),
    dimnames = list(taxa, samples)
  )
  assay <- list(assay = X, taxa = taxa, samples = samples)
  return(assay)
}

# Loads the metadata and filteres to samples found in assay
.load_metadata <- function(meta_df, samples, sep = "\t") {
  dt <- fread(meta_df, sep = sep, na.strings = c("", "NA"))

  # Pivot to wide
  wide <- dcast(
    dt,
    sample_alias ~ metadata_item,
    value.var = "value",
    fill = NA_character_
  )

  meta_df <- as.data.frame(wide)
  rownames(meta_df) <- meta_df$sample_alias

  # Output Dataframe subset to samples
  meta_df <- meta_df[samples, ]
  return(meta_df)
}

# Maps full lineage names levels
.construct_taxmap <- function(database, taxa) {
  taxmap <- fread(database, sep = "\t", header = TRUE)
  taxmap <- taxmap[startsWith(clade_name, "t__SGB")]

  taxmap <- taxmap %>%
    distinct(clade_name, .keep_all = TRUE) %>%
    filter(clade_name %in% taxa)

  idx <- match(taxa, taxmap$clade_name)
  taxmap <- taxmap[idx, ]

  rownames(taxmap) <- taxmap$clade_name

  taxmap <- taxmap %>%
    filter(clade_name %in% taxa) %>%
    tidyr::separate(
      col = lineage,
      into = c(
        "Kingdom", "Phylum", "Class", "Order",
        "Family", "Genus", "Species", "SGB"
      ),
      sep = "\\|",
      fill = "right"
    ) %>%
    select(-NCBI_taxids, -clade_name) %>%
    as.data.frame()

  # Subset to taxa in present
  taxmap <- taxmap[taxa, ]
  return(taxmap)
}

# Function filters profile data down to provided samples
.filter_datasets <- function(assay_list, samplelist) {
  # Resolve the samplelist into a vector of target IDs
  ext <- tolower(tools::file_ext(samplelist))

  # Read file based on extension
  if (ext %in% c("csv", "tsv")) {
    sl_df <- data.table::fread(samplelist)
  } else if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("The 'jsonlite' package is required to read JSON sample lists.")
    }
    sl_df <- as.data.frame(jsonlite::fromJSON(samplelist))
  }
  # Grab samples
  target_samples <- sl_df[["sample_alias"]]

  # Intersect requested samples with available samples
  available_samples <- assay_list[["samples"]]
  keep_samples <- intersect(target_samples, available_samples)

  if (length(keep_samples) == 0) {
    stop("Filtering Error: None of the provided samples were found in the dataset.")
  }

  # Subset the sparse matrix (columns = samples)
  assay_list$assay <- assay_list$assay[, keep_samples, drop = FALSE]
  assay_list$samples <- keep_samples

  # Drop taxa that now have 0 abundance across all remaining samples
  row_sums <- Matrix::rowSums(assay_list$assay)
  keep_taxa <- names(row_sums[row_sums > 0])
  assay_list$assay <- assay_list$assay[keep_taxa, , drop = FALSE]
  assay_list$taxa <- keep_taxa

  return(assay_list)
}

# -------------
# Main function
# -------------

fetchMetalogTSE <- function(
  collection, # One of "human", "animal", "ocean", "other_environment"
  metadata = "core", # One of "core", "partially_harmonized", "all"
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
  # Lastest database file for tax mapping
  mapping_db <- .download_if_missing(
    "https://metalog.embl.de/static/download/profiles/metaphlan4_clades.tsv.gz",
    use_cache = use_cache
  )

  # Injest data
  assay_list <- .load_assay(data_files[["assay"]])

  # Filtering
  if (!is.null(samplelist)) {
    message("Filtering datasets to include only the requested samples...")
    assay_list <- .filter_datasets(assay_list, samplelist)
  }
  md_dt <- .load_metadata(data_files[["md"]], assay_list[["samples"]])

  # Map SGB's to full lineage
  tax <- .construct_taxmap(mapping_db, assay_list[["taxa"]])

  tse <- TreeSummarizedExperiment(
    assays  = SimpleList("relabundance" = assay_list[["assay"]]),
    colData = DataFrame(md_dt),
    rowData = DataFrame(tax)
  )

  # License injection
  metadata(tse)$license <- "https://metalog.embl.de/ - Open Database License (ODbL) v1.0"

  return(tse)
}
