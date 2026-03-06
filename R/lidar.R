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
  camera_height_m,
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
  cam_elev <- z_cnt + camera_height_m

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

  validate_sf_object(points)

  if (!dir.exists(output_dir)) {
    message("Creating output directory: ", output_dir)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  validate_required_columns(
    points,
    "point_id",
    hint = "Run gla_load_points() first"
  )

  # Extract coordinates
  coordinates <- sf::st_coordinates(points)

  # Check for existing output files if resume = TRUE
  existing_mask <- rep(FALSE, nrow(points))

  if (resume) {
    existing_mask <- file.exists(
      file.path(output_dir, paste0(points$point_id, ".las"))
    )

    n_existing <- sum(existing_mask)
    if (n_existing > 0) {
      message(
        "Found ",
        n_existing,
        " existing output files, skipping those points"
      )
    }
  }

  # Determine which points need processing
  points_to_process_mask <- !existing_mask
  n_to_process <- sum(points_to_process_mask)

  # If all points already processed, return early
  if (n_to_process == 0) {
    message("All ", nrow(points), " plots already exist, skipping processing")
    points$las_files <- file.path(output_dir, paste0(points$point_id, ".las"))
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

  # Extract only coordinates that need processing, retaining original row indices
  # to map batch positions back to point_id values after clipping.
  points_to_process_original_idx <- which(points_to_process_mask)
  coordinates_to_process <- coordinates[points_to_process_mask, , drop = FALSE]

  # Read catalog
  ctg <- lidR::readLAScatalog(folder, ...)

  if (check_las_catalog) {
    lidR::las_check(ctg)
  }

  validate_crs_match(
    sf::st_crs(points),
    sf::st_crs(ctg),
    "Points",
    "LiDAR catalog"
  )

  lidR::opt_select(ctg) <- "xyzc"
  lidR::opt_chunk_buffer(ctg) <- 0
  lidR::opt_progress(ctg) <- FALSE
  lidR::opt_filter(ctg) <- filter
  lidR::opt_chunk_size(ctg) <- chunk_size

  lidR::opt_output_files(ctg) <- paste0(output_dir, "/", "{XCENTER}_{YCENTER}")

  message(
    "Clipping ",
    n_to_process,
    " circular plots with radius ",
    plot_radius,
    "m"
  )

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

    # Snapshot directory before clipping so we can identify exactly which files
    # lidR writes for this batch. We cannot rely on rois@data$filename being
    # order-preserving with respect to input coordinates when processing a
    # LAScatalog, so we match files back to points by coordinate instead.
    before_files <- list.files(
      output_dir,
      pattern = "\\.las$",
      full.names = TRUE
    )

    rois <- tryCatch(
      {
        invisible(utils::capture.output(
          result <- lidR::clip_circle(
            ctg,
            batch_coords[, "X"],
            batch_coords[, "Y"],
            radius = plot_radius
          ),
          file = nullfile()
        ))

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

    if (!is.null(rois)) {
      after_files <- list.files(
        output_dir,
        pattern = "\\.las$",
        full.names = TRUE
      )
      new_raw_files <- setdiff(after_files, before_files)

      # Rename each lidR output file from {XCENTER}_{YCENTER}.las to {point_id}.las.
      # Tolerances are calibrated to lidR's actual coordinate formatting: X is
      # rounded to the nearest integer (up to 0.5m error) and Y to one decimal
      # place (up to 0.05m error). These bounds are tight enough to avoid
      # cross-matching two distinct points in normal usage.
      for (k in seq_along(batch_indices)) {
        orig_idx <- points_to_process_original_idx[batch_indices[k]]
        pid <- points$point_id[orig_idx]

        match_idx <- which(vapply(
          new_raw_files,
          function(f) {
            parsed <- parse_lidr_las_filename(f)
            abs(parsed$x - batch_coords[k, "X"]) < 0.5 &&
              abs(parsed$y - batch_coords[k, "Y"]) < 0.05
          },
          logical(1)
        ))

        if (length(match_idx) == 1) {
          new_path <- file.path(output_dir, paste0(pid, ".las"))
          if (file.rename(new_raw_files[match_idx], new_path)) {
            all_new_files <- c(all_new_files, new_path)
            # Remove from candidates so the same file is not matched twice.
            new_raw_files <- new_raw_files[-match_idx]
          } else {
            warning(
              "Failed to rename ",
              new_raw_files[match_idx],
              " to ",
              new_path
            )
          }
        }
      }
    }

    rm(rois)
    gc()
  }

  if (length(all_new_files) > 0) {
    message("Created ", length(all_new_files), " new plot files")
  } else {
    message("No new plot files created")
  }

  # Assign las_files by point_id - exact, unambiguous lookup.
  expected_paths <- file.path(output_dir, paste0(points$point_id, ".las"))
  points$las_files <- ifelse(
    file.exists(expected_paths),
    expected_paths,
    NA_character_
  )

  if (all(is.na(points$las_files))) {
    message("No LAS files created - all plots may have failed")
  }

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

  invisible(points)
}
