#' @export
nightlight_estimates <- function(years, polygons, identifier, fun = "sum",
                                 quiet = FALSE,
                                 project_raster = FALSE,
                                 cache_dir = ".nightlight_cache",
                                 keep_cache = TRUE) {
  require(sf)
  require(raster)
  require(stars)
  require(terra)
  require(fassf)
  require(tidyverse)
  
  if (!identifier %in% colnames(polygons)) {
    return("missing identifier from polygon names")
  }
  
  options(timeout = 9999999)
  
  # Harmonized DMSP–VIIRS URLs (1992–2024)
  urls <- data.frame(
    year = 1992:2024,
    urls = c(
      "https://figshare.com/ndownloader/files/17626052",
      "https://figshare.com/ndownloader/files/17626055",
      "https://figshare.com/ndownloader/files/17626061",
      "https://figshare.com/ndownloader/files/17626067",
      "https://figshare.com/ndownloader/files/17626070",
      "https://figshare.com/ndownloader/files/17626073",
      "https://figshare.com/ndownloader/files/17626079",
      "https://figshare.com/ndownloader/files/17626082",
      "https://figshare.com/ndownloader/files/17626085",
      "https://figshare.com/ndownloader/files/17626088",
      "https://figshare.com/ndownloader/files/17626091",
      "https://figshare.com/ndownloader/files/17626094",
      "https://figshare.com/ndownloader/files/17626097",
      "https://figshare.com/ndownloader/files/17626100",
      "https://figshare.com/ndownloader/files/17626103",
      "https://figshare.com/ndownloader/files/17626109",
      "https://figshare.com/ndownloader/files/17626016",
      "https://figshare.com/ndownloader/files/17626019",
      "https://figshare.com/ndownloader/files/17626022",
      "https://figshare.com/ndownloader/files/17626025",
      "https://figshare.com/ndownloader/files/17626031",
      "https://figshare.com/ndownloader/files/17626034",
      "https://figshare.com/ndownloader/files/57065276",
      "https://figshare.com/ndownloader/files/57065321",
      "https://figshare.com/ndownloader/files/57065291",
      "https://figshare.com/ndownloader/files/57065282",
      "https://figshare.com/ndownloader/files/57065288",
      "https://figshare.com/ndownloader/files/57065285",
      "https://figshare.com/ndownloader/files/57065297",
      "https://figshare.com/ndownloader/files/57065294",
      "https://figshare.com/ndownloader/files/57065303",
      "https://figshare.com/ndownloader/files/57065300",
      "https://figshare.com/ndownloader/files/57065306"
    ),
    stringsAsFactors = FALSE
  )
  
  raster_polygon_values_contained <- function(raster, polygons, quiet = FALSE, fun = NULL, drop_na = TRUE, project_raster = project_raster){{
      require(sf)
      require(raster)
      require(stars)
      require(terra)
      raster_file <- rast(raster)
      if (!quiet == TRUE) {
        "re-projecting raster file"
      }
      if(project_raster){
        raster_file <- tryCatch({
          terra::project(raster_file, crs(polygons))
        }, error = function(e) {
          return(e)
        })
      }else{
        polygons <- st_transform(polygons, st_crs(raster_file))
      }
      if (grepl(raster_file, pattern = "incorrect number of values")) {
        polygons <- st_transform(polygons, crs = "EPSG:3857")
        raster_file <- rast(raster)
        raster_file <- tryCatch({
          project(raster_file, crs(polygons))
        }, error = function(e) {
          return(e)
        })
      }
      if (!quiet == TRUE) {
        "obtaining estimates"
      }
      polygon_geometry <- st_geometry(polygons)
      get_values <- function(i, polygons, raster_file, quiet, fun, 
                             drop_na) {
        if (!quiet == TRUE) {
          cat("\r", i/length(polygon_geometry))
        }
        polygon <- st_as_sf(st_make_valid(polygon_geometry[i]))
        x <- tryCatch(crop(raster_file, ext(polygon)), error = function(e) {
          NA
        })
        if (typeof(x) == "logical") {
          return(NA)
        }
        y <- mask(x, polygon)
        if (is.null(fun)) {
          return(values(y, na.rm = drop_na)[,1])
        }
        else {
          return(lapply(list(values(y, na.rm = drop_na)), get(fun))[[1]])
        }
      }
    }
    library(pbapply)
    final_values <- pbsapply(FUN = get_values, X = 1:length(polygon_geometry), 
                             polygons = polygons, raster_file = raster_file, quiet = quiet, 
                             fun = fun, drop_na = drop_na)
    return(final_values)
  }
    
  # Validate requested years against available URLs
  available_years <- urls$year[!is.na(urls$urls) & nzchar(urls$urls)]
  unsupported <- setdiff(years, available_years)
  if (length(unsupported) > 0) {
    stop(
      sprintf(
        "Unsupported year(s): %s. Supported range is %d–%d.",
        paste(unsupported, collapse = ", "),
        min(available_years),
        max(available_years)
      )
    )
  }
  
  # Prepare cache root
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  all_out <- list()
  
  for (i in seq_along(years)) {
    yr <- years[i]
    if (isFALSE(quiet)) {
      cat(sprintf("\nProcessing year %d (%d/%d)\n", yr, i, length(years)))
    }
    
    # Per-year cache subdir
    year_dir <- file.path(cache_dir, as.character(yr))
    tif_files <- list.files(year_dir, pattern = "\\.tif$", full.names = TRUE)
    
    # Download if not cached
    if (length(tif_files) == 0) {
      if (isFALSE(quiet)) cat("   Data not found in cache. Downloading...\n")
      dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)
      
      current_url <- urls$urls[urls$year == yr]
      if (length(current_url) == 0 || is.na(current_url) || current_url == "") {
        warning(paste("No URL found for year", yr))
        if (!keep_cache) unlink(year_dir, recursive = TRUE)
        next
      }
      
      tif_path <- file.path(year_dir, paste0(yr, ".tif"))
      download.file(current_url, destfile = tif_path, mode = "wb")
      tif_files <- tif_path
    } else {
      if (isFALSE(quiet)) cat("   Found cached data at:", year_dir, "\n")
    }
    
    # Safety check
    if (length(tif_files) == 0 || !file.exists(tif_files[1])) {
      warning(paste("No .tif file found for year", yr, "after attempting download."))
      if (!keep_cache) unlink(year_dir, recursive = TRUE)
      next
    }
    
    # Extract polygon values (keeps original function's behavior)
    values <- raster_polygon_values_contained(
      tif_files[1],
      st_geometry(polygons),
      quiet = quiet,
      fun = fun,
      project_raster = project_raster
    )
    
    if(!is.null(fun)){
      out <- data.frame(
        year = rep(yr, length(values)),
        identifier = polygons[[identifier]],
        nightlight_means = values
      )
    }else{
      out <- data.frame(
        year = rep(yr, length(values)),
        identifier = polygons[[identifier]],
        nightlight_means = I(values)
      )
    }
    names(out)[names(out) == "identifier"] <- identifier
    
    all_out[[as.character(yr)]] <- out
    
    # Optional cleanup (mirrors first function's keep_cache behavior)
    if (!keep_cache) {
      if (isFALSE(quiet)) cat("   keep_cache is FALSE, cleaning:", year_dir, "\n")
      unlink(year_dir, recursive = TRUE)
    }
  }
  
  if (length(all_out) == 0) {
    return(data.frame())
  } else {
    return(dplyr::bind_rows(all_out))
  }
}
