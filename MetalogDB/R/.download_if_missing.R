# Adapted from Metalog's example script

.download_if_missing <- function(target_url, download_dir = ".data_cache", use_cache = TRUE) {
  
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
  response <- httr::GET(base_url, httr::config(followlocation = TRUE))
  
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
