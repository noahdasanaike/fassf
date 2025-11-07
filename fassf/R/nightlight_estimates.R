#' @export
nightlight_estimates <- function(
  years,
  polygons,
  identifier,
  fun = "sum",
  quiet = FALSE,
  cache_dir = ".nightlight_cache",
  keep_cache = TRUE
) {
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

  available_years <- urls$year[!is.na(urls$urls) & nzchar(urls$urls)]
  unsupported <- setdiff(years, available_years)
  if (length(unsupported) > 0) {
    stop(sprintf(
      "Unsupported year(s): %s. Supported range is %d–%d.",
      paste(unsupported, collapse = ", "),
      min(available_years), max(available_years)
    ))
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  all_out <- list()

  for (i in seq_along(years)) {
    yr <- years[i]
    if (isFALSE(quiet)) cat(sprintf("\nProcessing year %d (%d/%d)\n", yr, i, length(years)))

    year_dir <- file.path(cache_dir, as.character(yr))
    tif_files <- list.files(year_dir, pattern = "\\.tif$", full.names = TRUE)

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

    if (length(tif_files) == 0 || !file.exists(tif_files[1])) {
      warning(paste("No .tif file found for year", yr, "after attempting download."))
      if (!keep_cache) unlink(year_dir, recursive = TRUE)
      next
    }

    if (is.null(fun)) {
      # ---- RAW VALUES PATH: one call per polygon, returns a vector per polygon ----
      ids <- polygons[[identifier]]
      raw_list <- vector("list", length = nrow(polygons))

      for (j in seq_len(nrow(polygons))) {
        vals_j <- try(
          raster_polygon_values(
            raster   = tif_files[1],
            polygons = polygons[j, , drop = FALSE],
            quiet    = quiet,
            fun      = NULL
          ),
          silent = TRUE
        )
        if (inherits(vals_j, "try-error") || is.null(vals_j)) vals_j <- NA_real_
        raw_list[[j]] <- vals_j
      }

      out <- data.frame(
        year = rep(yr, nrow(polygons)),
        identifier = ids,
        raw_values = I(raw_list)
      )
      names(out)[names(out) == "identifier"] <- identifier
      all_out[[as.character(yr)]] <- out

    } else {
      # ---- SUMMARY PATH: original behavior (one scalar per polygon) ----
      vals <- raster_polygon_values(
        raster   = tif_files[1],
        polygons = polygons,
        quiet    = quiet,
        fun      = fun
      )

      out <- data.frame(
        year = rep(yr, nrow(polygons)),
        identifier = polygons[[identifier]],
        nightlight_means = vals
      )
      names(out)[names(out) == "identifier"] <- identifier
      all_out[[as.character(yr)]] <- out
    }

    if (!keep_cache) {
      if (isFALSE(quiet)) cat("   keep_cache is FALSE, cleaning:", year_dir, "\n")
      unlink(year_dir, recursive = TRUE)
    }
  }

  if (length(all_out) == 0) {
    data.frame()
  } else {
    dplyr::bind_rows(all_out)
  }
}
