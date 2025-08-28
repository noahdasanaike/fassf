#' @export

nightlight_uncalibrated_estimates <- function(years, polygons, identifier, fun = "mean", quiet = FALSE, cache_dir = ".nightlight_cache", keep_cache = TRUE) {
    # Load required packages
    require(sf)
    require(raster)
    require(stars)
    require(terra)
    require(fassf)
    require(tidyverse)
    require(archive)
    
    # https://www.nature.com/articles/s41597-024-04228-6

    # Check that the identifier exists in the polygon data
    if (!identifier %in% colnames(polygons)) {
        stop("Missing identifier from polygon names")
    }

    # Increase the timeout in case downloads take a while
    options(timeout = 9999999)

    # Data frame of years and corresponding URLs
    urls <- data.frame(
        year = c(
            1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
            2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
            2014, 2021, 2022, 2023, 2015, 2016, 2017, 2018, 2019, 2020
        ),
        urls = c(
            "https://figshare.com/ndownloader/files/39569938",
            "https://figshare.com/ndownloader/files/39569971",
            "https://figshare.com/ndownloader/files/39569983",
            "https://figshare.com/ndownloader/files/39569995",
            "https://figshare.com/ndownloader/files/39570142",
            "https://figshare.com/ndownloader/files/39570277",
            "https://figshare.com/ndownloader/files/39570424",
            "https://figshare.com/ndownloader/files/39570427",
            "https://figshare.com/ndownloader/files/39571273",
            "https://figshare.com/ndownloader/files/39570433",
            "https://figshare.com/ndownloader/files/39571327",
            "https://figshare.com/ndownloader/files/39574615",
            "https://figshare.com/ndownloader/files/39574783",
            "https://figshare.com/ndownloader/files/39574975",
            "https://figshare.com/ndownloader/files/39574987",
            "https://figshare.com/ndownloader/files/39575020",
            "https://figshare.com/ndownloader/files/39575035",
            "https://figshare.com/ndownloader/files/39575068",
            "https://figshare.com/ndownloader/files/39575092",
            "https://figshare.com/ndownloader/files/39575098",
            "https://figshare.com/ndownloader/files/39575752",
            "https://figshare.com/ndownloader/files/39575773",
            "https://figshare.com/ndownloader/files/39576709",
            "https://figshare.com/ndownloader/files/47986051",
            "https://figshare.com/ndownloader/files/47986054",
            "https://figshare.com/ndownloader/files/47986057",
            "https://figshare.com/ndownloader/files/39576721",
            "https://figshare.com/ndownloader/files/39576733",
            "https://figshare.com/ndownloader/files/39577360",
            "https://figshare.com/ndownloader/files/39577372",
            "https://figshare.com/ndownloader/files/39577894",
            "https://figshare.com/ndownloader/files/39577912"
        ),
        stringsAsFactors = FALSE
    )

    # Create the main cache directory if it doesn't exist
    dir.create(cache_dir, showWarnings = FALSE)

    # Initialize a list to hold results per year
    all_out <- list()

    # Process one year at a time
    for (yr in years) {
        if (!quiet) {
            cat("\nProcessing year", yr, "\n")
        }

        # Get the URL for the current year
        current_url <- urls$urls[urls$year == yr]
        if (length(current_url) == 0) {
            warning(paste("No URL found for year", yr))
            next
        }

        # Create a subdirectory path for the current year
        year_dir <- file.path(cache_dir, as.character(yr))

        # Check for cached .tif files first
        tif_files <- list.files(year_dir, pattern = "\\.tif$", full.names = TRUE)

        # If no .tif files are found in the cache, download and extract them
        if (length(tif_files) == 0) {
            if (!quiet) {
                cat("   Data not found in cache. Downloading and extracting...\n")
            }
            dir.create(year_dir, showWarnings = FALSE)

            # Download the zip file
            zip_path <- file.path(year_dir, paste0(yr, ".zip"))
            download.file(current_url, destfile = zip_path, mode = "wb")

            # Unzip the file
            archive::archive_extract(zip_path, dir = year_dir)
            unlink(zip_path, recursive = TRUE) # remove the zip file

            # Locate the .tif file(s) that were extracted
            tif_files <- list.files(year_dir, pattern = "\\.tif$", full.names = TRUE)
        } else {
            if (!quiet) {
                cat("   Found cached data, loading from:", year_dir, "\n")
            }
        }

        # If still no .tif file, something is wrong; warn and skip
        if (length(tif_files) == 0) {
            warning(paste("No .tif file found for year", yr, "after checking cache and attempting download."))
            # Clean up empty year-specific folder if not keeping cache
            if (!keep_cache) {
                unlink(year_dir, recursive = TRUE)
            }
            next
        }

        # Process each .tif file individually
        values_list <- list()
        for (tif in tif_files) {
            # Extract raster values for the given polygons
            values <- raster_polygon_values_faster(tif, polygons, quiet = quiet, fun = fun)
            values_list[[tif]] <- values
        }

        # If multiple .tif files exist, aggregate the results per polygon
        if (length(values_list) > 1) {
            val_mat <- do.call(cbind, values_list)
            combined_values <- apply(val_mat, 1, fun)
        } else {
            combined_values <- values_list[[1]]
        }

        # Build a data frame for this year
        year_out <- data.frame(
            year = rep(yr, length(combined_values)),
            nightlight_means = combined_values
        )
        year_out[[identifier]] <- polygons[[identifier]]

        # Save the result for this year
        all_out[[as.character(yr)]] <- year_out

        # Clean up the year-specific cache folder if specified
        if (!keep_cache) {
            if (!quiet) {
                cat("   keep_cache is FALSE, cleaning up directory:", year_dir, "\n")
            }
            unlink(year_dir, recursive = TRUE)
        }
    }

    # The main cache directory is intentionally not removed.

    # Combine all yearly results and return
    final_out <- dplyr::bind_rows(all_out)
    return(final_out)
}
