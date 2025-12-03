# Equations taken from Chapter 4, Geometry, CRC Standard Mathematical Tables and Formulae, 30th Edition
# Function to reproject 3D cartesian to 3D spherical to 2D polar scaled to pi/2
# 4.9.4 Relations between Cartesian, Cylindrical, and spherical coordinates, p. 299.
lidar2hemi <- function(x, y, z) {
  # spherical coordinates (rho, theta, phi)
  rho <- sqrt(x^2 + y^2 + z^2) # distance from origin (0,0,0) to pt(x,y,z)
  theta <- atan2(y, x) # angle in radians from east = 0, CCW convention
  phi <- acos(z / rho) # zenith angle in radians from positive z-axis
  # reproject 3D spherical coordinates onto 2D polar image plane scaled to pi/2
  x <- phi * cos(theta) * -1 # multiple by -1 to flip image along N/S axis
  y <- phi * sin(theta)
  # Return data frame of output variables
  data.frame(rho, theta, phi, x, y)
}


#' Process lidar data for hemispherical photo creation
#' @keywords internal
gla_transform_lidar <- function(
  las_input,
  x_meters,
  y_meters,
  elev_m,
  cam_ht = 1.37,
  min_dist = 1
) {
  # Validate inputs
  if (missing(x_meters) || missing(y_meters) || missing(elev_m)) {
    stop("x_meters, y_meters, and elev_m must all be provided")
  }

  if (!is.numeric(x_meters) || !is.numeric(y_meters) || !is.numeric(elev_m)) {
    stop("x_meters, y_meters, and elev_m must be numeric")
  }

  # Read LAS or LAZ formatted file (only keep point classes 1 - non-ground and 2 - ground)
  las <- lidR::readLAS(las_input, select = "xyzc", filter = "-keep_class 1 2")
  las <- as.data.frame(attributes(las)$data)

  # Set plot center in map projection coordinates (m)
  x_cnt <- round(x_meters, digits = 2)
  y_cnt <- round(y_meters, digits = 2)
  z_cnt <- round(elev_m, digits = 2)

  # Camera elevation (m)
  cam_elev <- z_cnt + cam_ht

  # Eliminate all laser points at or below elevation of camera
  las <- las[(las$Z > cam_elev), ]

  # Transform points to (0,0) origin and absolute height above camera
  x_new <- las$X - x_cnt
  y_new <- las$Y - y_cnt
  z_new <- las$Z - cam_elev

  # Reproject point cloud to hemispherical coordinates
  df <- lidar2hemi(x_new, y_new, z_new)

  # Remove all points within fixed distance of camera
  df[(df$rho >= min_dist), ]
}

#' Create virtual plots from LiDAR data
#'
#' Clips circular plots from a LiDAR catalog at specified point locations.
#' Processes points in batches to avoid memory exhaustion with large datasets.
#'
#' @param points sf object with point locations
#' @param folder Directory containing LAS/LAZ files
#' @param output_dir Directory to save clipped plot files
#' @param plot_radius Radius of circular plots in meters
#' @param filter LAS filter string (default: "-keep_class 1 2 9")
#' @param chunk_size Chunk size for LAScatalog processing (default: 0)
#' @param batch_size Number of plots to process per batch (default: 1000).
#'   Reduce if encountering memory errors with large datasets.
#' @param resume Skip points with existing output files (default: TRUE)
#' @param check_las_catalog Run las_check on catalog (default: FALSE)
#' @param ... Additional arguments passed to readLAScatalog
#'
#' @return sf object with added las_files column containing paths to clipped plots
#'
#' @details
#' Processes plots in batches to prevent memory exhaustion. With large datasets
#' (e.g., 5000+ plots), processing all at once can fail. The batch_size parameter
#' controls how many plots are clipped simultaneously. Default of 1000 works well
#' for most systems. Reduce to 500 or lower if still encountering memory issues.
#'
#' @export
gla_create_virtual_plots <- function(
  points,
  folder,
  output_dir,
  plot_radius,
  filter = "-keep_class 1 2 9",
  chunk_size = 0,
  batch_size = 1000,
  resume = TRUE,
  check_las_catalog = FALSE,
  ...
) {
  # Validate inputs
  if (!dir.exists(folder)) {
    stop("folder does not exist: ", folder)
  }

  if (!inherits(points, "sf")) {
    stop("points must be an sf object")
  }

  if (!dir.exists(output_dir)) {
    message("Creating output directory: ", output_dir)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Extract coordinates
  coordinates <- sf::st_coordinates(points)

  # Check for existing output files if resume = TRUE
  existing_mask <- rep(FALSE, nrow(points))

  if (resume && dir.exists(output_dir)) {
    existing_files <- list.files(
      output_dir,
      pattern = "\\.las$",
      full.names = TRUE
    )

    if (length(existing_files) > 0) {
      # Parse existing filenames using shared helper
      existing_info <- parse_las_filenames(existing_files)

      # Match existing files to input points (exact match)
      # Note: filenames use round(x, 0) and round(y, 1)
      for (i in seq_len(nrow(points))) {
        matches <- existing_info$x_meters == round(coordinates[i, "X"], 0) &
          existing_info$y_meters == round(coordinates[i, "Y"], 1)
        if (any(matches)) {
          existing_mask[i] <- TRUE
        }
      }

      n_existing <- sum(existing_mask)
      if (n_existing > 0) {
        message(
          "Found ",
          n_existing,
          " existing output files, skipping those points"
        )
      }
    }
  }

  # Determine which points need processing
  points_to_process_mask <- !existing_mask
  n_to_process <- sum(points_to_process_mask)

  # If all points already processed, return early
  if (n_to_process == 0) {
    message("All ", nrow(points), " plots already exist, skipping processing")
    # Add las_files column to existing points
    all_files <- list.files(output_dir, pattern = "\\.las$", full.names = TRUE)
    points <- add_las_filename(points, all_files)
    return(invisible(points))
  }

  if (resume && sum(existing_mask) > 0) {
    message(
      "Processing ",
      n_to_process,
      " new plots (",
      sum(existing_mask),
      " already exist)"
    )
  }

  # Extract only coordinates that need processing
  coordinates_to_process <- coordinates[points_to_process_mask, , drop = FALSE]

  # Read catalog
  ctg <- lidR::readLAScatalog(folder, ...)

  if (check_las_catalog) {
    lidR::las_check(ctg)
  }

  # Check CRS match - strict validation to prevent spatial errors
  ctg_crs <- sf::st_crs(ctg)
  pts_crs <- sf::st_crs(points)

  validate_crs_match(pts_crs, ctg_crs, "Points", "LiDAR catalog")

  # Set options
  lidR::opt_select(ctg) <- "xyzc"
  lidR::opt_chunk_buffer(ctg) <- 0
  lidR::opt_progress(ctg) <- FALSE
  lidR::opt_filter(ctg) <- filter
  lidR::opt_chunk_size(ctg) <- chunk_size # Chunk size to balance memory usage across workers

  # Set output files with template
  lidR::opt_output_files(ctg) <- paste0(
    output_dir,
    "/",
    "{XCENTER}_{YCENTER}"
  )

  # Clip circles in batches to avoid memory exhaustion
  message(
    "Clipping ",
    n_to_process,
    " circular plots with radius ",
    plot_radius,
    "m"
  )

  # Split points into batches
  n_batches <- ceiling(n_to_process / batch_size)
  all_new_files <- character(0)

  for (batch_idx in seq_len(n_batches)) {
    batch_start <- (batch_idx - 1) * batch_size + 1
    batch_end <- min(batch_idx * batch_size, n_to_process)
    batch_indices <- batch_start:batch_end

    message(
      "Processing batch ",
      batch_idx,
      "/",
      n_batches,
      " (",
      length(batch_indices),
      " plots)"
    )

    batch_coords <- coordinates_to_process[batch_indices, , drop = FALSE]

    rois <- tryCatch(
      {
        # Suppress progress bars via option
        old_progress <- getOption("lidR.progress")
        options(lidR.progress = FALSE)

        result <- lidR::clip_circle(
          ctg,
          batch_coords[, "X"],
          batch_coords[, "Y"],
          radius = plot_radius
        )

        # Restore option
        options(lidR.progress = old_progress)

        result
      },
      error = function(e) {
        warning(
          "Error during clip_circle (batch ",
          batch_idx,
          "): ",
          e$message,
          "\nContinuing with next batch..."
        )
        NULL
      }
    )

    # Accumulate output files from this batch
    if (!is.null(rois)) {
      batch_files <- rois@data$filename
      all_new_files <- c(all_new_files, batch_files)
    }

    # Clean up memory between batches
    rm(rois)
    gc()
  }

  # Report results
  if (length(all_new_files) > 0) {
    message("Created ", length(all_new_files), " new plot files")
  } else {
    message("No new plot files created")
  }

  # Get all files from output directory
  all_files <- list.files(output_dir, pattern = "\\.las$", full.names = TRUE)

  # Initialize columns if they don't exist
  if (!"las_files" %in% names(points)) {
    points$las_files <- NA_character_
  }

  # Only parse and match if there are files
  if (length(all_files) > 0) {
    # Parse all filenames
    all_files_info <- parse_las_filenames(all_files)

    # Match files to all points by coordinates (exact match)
    # Note: filenames use round(x, 0) and round(y, 1)
    for (i in seq_len(nrow(points))) {
      match_idx <- which(
        all_files_info$x_meters == round(coordinates[i, "X"], 0) &
          all_files_info$y_meters == round(coordinates[i, "Y"], 1)
      )
      if (length(match_idx) > 0) {
        points$las_files[i] <- all_files_info$las_files[match_idx[1]]
      }
    }
  } else {
    message("No LAS files created - all plots may have failed")
  }

  # Ensure geometry column is last
  points <- move_geom_col_to_end(points)

  if (resume && sum(existing_mask) > 0) {
    message(
      "Total: ",
      nrow(points),
      " plots (",
      n_to_process,
      " new, ",
      sum(existing_mask),
      " existing)"
    )
  }

  # Return summary
  invisible(points)
}
