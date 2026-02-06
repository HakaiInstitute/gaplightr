# Pure R implementation of horizon angle calculation using terra
# Replicates GRASS GIS r.horizon algorithm without requiring GRASS

#' Save horizon data to CSV file
#'
#' @param horizon_df Data frame with horizon data (azimuth, horizon_height, x_msk, y_msk)
#' @param x_meters X coordinate in meters
#' @param y_meters Y coordinate in meters
#' @param output_dir Directory to save CSV file
#'
#' @return File path where CSV was saved
#' @keywords internal
save_horizon_csv <- function(horizon_df, x_meters, y_meters, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  file_path <- file.path(
    output_dir,
    sprintf("%d_%.1f_horizon.csv", round(x_meters, 0), round(y_meters, 1))
  )

  write.csv(horizon_df, file_path, row.names = FALSE)

  return(file_path)
}

#' Load horizon data from CSV file
#'
#' @param x_meters X coordinate in meters
#' @param y_meters Y coordinate in meters
#' @param output_dir Directory containing CSV file
#'
#' @return Data frame with horizon data, or NULL if file doesn't exist
#' @keywords internal
load_horizon_csv <- function(x_meters, y_meters, output_dir) {
  file_path <- file.path(
    output_dir,
    sprintf("%d_%.1f_horizon.csv", round(x_meters, 0), round(y_meters, 1))
  )

  if (!file.exists(file_path)) {
    return(NULL)
  }

  read.csv(file_path)
}

#' Find which points have existing horizon CSV files
#'
#' @param points sf object with points (must have x_meters and y_meters columns)
#' @param output_dir Directory containing horizon CSV files
#'
#' @return Logical vector indicating which points have cached horizons
#' @keywords internal
find_existing_horizons <- function(points, output_dir) {
  if (!dir.exists(output_dir)) {
    return(rep(FALSE, nrow(points)))
  }

  sapply(seq_len(nrow(points)), function(i) {
    file_path <- file.path(
      output_dir,
      sprintf(
        "%d_%.1f_horizon.csv",
        round(points$x_meters[i], 0),
        round(points$y_meters[i], 1)
      )
    )
    file.exists(file_path)
  })
}

#' Extract horizon angles from DEM using terra (GRASS r.horizon replacement)
#'
#' Calculates the horizon elevation angle for each azimuth direction from a given
#' observation point using a digital elevation model (DEM). This is a pure R
#' implementation that replicates GRASS GIS r.horizon algorithm.
#'
#' @param dem_rast SpatRaster object from terra package containing the DEM
#' @param x_meters Numeric X coordinate in meters (in the DEM's coordinate system)
#' @param y_meters Numeric Y coordinate in meters (in the DEM's coordinate system)
#' @param step Numeric value specifying the azimuth step size in degrees (default: 5)
#' @param max_search_distance Maximum search distance in meters (default: NULL, uses full extent)
#' @param distance_step Distance step size for sampling along line of sight in meters
#'   (default: NULL, uses raster resolution)
#' @param dem_max Maximum elevation in the DEM for early termination optimization
#'   (default: NULL, will be computed). When processing multiple points with the same
#'   DEM, pass this value to avoid recomputing (expensive on large DEMs).
#' @param cam_ht Camera height above ground in meters (default: 1.37). The observer
#'   elevation is calculated as ground elevation (from DEM) plus this height. Use the
#'   same value as in \code{\link{gla_transform_lidar}} for consistent reference frames.
#' @param verbose Logical indicating whether to print progress messages (default: FALSE)
#'
#' @return Data frame with two columns:
#'   - azimuth: azimuth angles in degrees (0-360, East=0, counterclockwise)
#'   - horizon_height: horizon elevation angles in degrees
#'
#' @details The algorithm replicates GRASS r.horizon using the exact algorithm:
#'   1. Uses tanh (tan of horizon angle) for tracking maximum
#'   2. Compares each point against current horizon line
#'   3. Updates horizon when: z_point > z_origin + curvature + distance * tanh
#'   4. Applies Earth curvature correction: 0.5 * distance^2 / 6371000
#'   5. Terminates when reaching max elevation or max distance
#'
#'   The observer elevation is computed as: ground_elev (from DEM) + cam_ht.
#'   This matches the camera position used in \code{\link{gla_transform_lidar}},
#'   ensuring consistent reference frames between horizon masking and point cloud
#'   transformation.
#'
#' @examples
#' \dontrun{
#'   dem <- terra::rast("path/to/dem.tif")
#'   horizon_data <- gla_extract_horizon_terra(
#'     dem_rast = dem,
#'     x_meters = 500000,
#'     y_meters = 5500000,
#'     step = 5
#'   )
#' }
gla_extract_horizon_terra <- function(
  dem_rast,
  x_meters,
  y_meters,
  step = 5,
  max_search_distance = NULL,
  distance_step = NULL,
  dem_max = NULL,
  cam_ht = 1.37,
  verbose = FALSE
) {
  # Validate inputs
  if (!inherits(dem_rast, "SpatRaster")) {
    stop("dem_rast must be a SpatRaster object from terra package")
  }

  if (missing(x_meters) || missing(y_meters)) {
    stop("Both x_meters and y_meters must be provided")
  }

  if (!is.numeric(x_meters) || !is.numeric(y_meters)) {
    stop("x_meters and y_meters must be numeric")
  }

  # Use projected coordinates directly
  obs_x <- x_meters
  obs_y <- y_meters

  if (verbose) {
    cat(
      "Using observation point at (",
      obs_x,
      ", ",
      obs_y,
      ")\n"
    )
  }

  # Extract observer elevation from DEM
  obs_point <- terra::vect(
    matrix(c(obs_x, obs_y), ncol = 2),
    crs = terra::crs(dem_rast)
  )
  obs_elev <- terra::extract(dem_rast, obs_point)[1, 2]

  if (is.na(obs_elev)) {
    stop("Observer location is outside DEM extent or has no data")
  }

  # Camera elevation = ground elevation + camera height above ground

  cam_elev <- obs_elev + cam_ht

  if (verbose) {
    cat("Ground elevation:", obs_elev, "m\n")
    cat("Camera elevation:", cam_elev, "m (ground +", cam_ht, "m)\n")
  }

  # Get global maximum elevation for early termination (GRASS does this)
  # Only compute if not provided (expensive operation on large DEMs)
  if (is.null(dem_max)) {
    dem_max <- terra::global(dem_rast, "max", na.rm = TRUE)[1, 1]
    if (verbose) {
      cat("Computed DEM maximum elevation:", dem_max, "m\n")
    }
  } else {
    if (verbose) {
      cat("Using cached DEM maximum elevation:", dem_max, "m\n")
    }
  }

  # Set distance step to raster resolution if not specified
  if (is.null(distance_step)) {
    distance_step <- mean(terra::res(dem_rast))
    if (verbose) {
      cat("Using distance step:", distance_step, "m\n")
    }
  }

  # Set maximum distance if not specified
  if (is.null(max_search_distance)) {
    # Use distance to furthest corner of DEM
    dem_ext <- terra::ext(dem_rast)
    corners <- rbind(
      c(dem_ext$xmin, dem_ext$ymin),
      c(dem_ext$xmax, dem_ext$ymin),
      c(dem_ext$xmin, dem_ext$ymax),
      c(dem_ext$xmax, dem_ext$ymax)
    )
    distances <- sqrt((corners[, 1] - obs_x)^2 + (corners[, 2] - obs_y)^2)
    max_search_distance <- max(distances)
    if (verbose) {
      cat(
        "Using maximum search distance:",
        round(max_search_distance, 1),
        "m\n"
      )
    }
  }

  # Generate azimuth angles (0 = East, counterclockwise)
  # GRASS r.horizon uses: 0=East, 90=North, 180=West, 270=South
  azimuths <- seq(0, 360 - step, by = step)

  # Earth radius in meters (same as GRASS: 6371000)
  earth_radius <- 6371000
  inv_earth <- 1.0 / earth_radius

  # Conversion factors
  deg_to_rad_factor <- deg_to_rad()
  rad_to_deg_factor <- rad_to_deg()

  # Pre-compute all sample distances
  distances <- seq(distance_step, max_search_distance, by = distance_step)
  n_distances <- length(distances)

  # Pre-compute curvature corrections for all distances (vectorized once)
  # Earth curvature: vertical drop = d²/(2R) for small angles
  curvature_corrections <- 0.5 * distances * distances * inv_earth

  # Build matrix of ALL points for ALL azimuths (batch extraction)
  n_azimuths <- length(azimuths)
  total_points <- n_azimuths * n_distances

  # Pre-allocate matrices
  all_points <- matrix(nrow = total_points, ncol = 2)
  azimuth_index <- integer(total_points)
  distance_index <- integer(total_points)

  # Fill matrices with coordinates for all azimuths and distances
  idx <- 1
  for (i in seq_along(azimuths)) {
    azi_rad <- azimuths[i] * deg_to_rad_factor
    dx <- cos(azi_rad)
    dy <- sin(azi_rad)

    for (j in seq_along(distances)) {
      all_points[idx, 1] <- obs_x + distances[j] * dx
      all_points[idx, 2] <- obs_y + distances[j] * dy
      azimuth_index[idx] <- i
      distance_index[idx] <- j
      idx <- idx + 1
    }
  }

  if (verbose) {
    cat("Extracting", total_points, "elevation values in single batch...\n")
  }

  # Single batch extraction for all points
  all_elevations <- terra::extract(dem_rast, all_points)[, 1]

  if (verbose) {
    cat("Processing horizon angles for", n_azimuths, "azimuths...\n")
  }

  # Process each azimuth using the pre-extracted elevations
  horizon_angles <- numeric(n_azimuths)

  for (i in seq_along(azimuths)) {
    # Get elevations for this azimuth
    azimuth_mask <- azimuth_index == i
    elevations <- all_elevations[azimuth_mask]

    # Initialize horizon tracking (GRASS uses tan of horizon angle)
    tanh0 <- 0.0

    # Process points along line of sight
    for (j in seq_along(distances)) {
      elev <- elevations[j]

      # Stop if outside DEM or NA
      if (is.na(elev)) {
        break
      }

      # Calculate projected height on current horizon line
      z_horizon <- cam_elev + curvature_corrections[j] + distances[j] * tanh0

      # Update horizon if this point is above current horizon line
      if (elev > z_horizon) {
        tanh0 <- (elev - cam_elev - curvature_corrections[j]) / distances[j]
      }

      # Early termination: if horizon line is above global max elevation
      if (
        cam_elev + curvature_corrections[j] + distances[j] * tanh0 >= dem_max
      ) {
        break
      }
    }

    # Convert tan to angle in degrees
    horizon_angles[i] <- atan(tanh0) * rad_to_deg_factor

    if (verbose && i %% 10 == 0) {
      cat("Processed azimuth", azimuths[i], "degrees\n")
    }
  }

  # Create output dataframe (same format as GRASS r.horizon)
  result <- data.frame(
    azimuth = azimuths,
    horizon_height = horizon_angles
  )

  if (verbose) {
    cat(
      "Horizon analysis completed. Returned",
      nrow(result),
      "data points\n"
    )
  }

  return(result)
}

#' Prepare horizon mask for fisheye photo creation
#'
#' Converts horizon elevation angles to polar projected coordinates suitable
#' for masking fisheye photographs.
#'
#' @param horizon_data Data frame with horizon data. Must contain at least
#'   two columns: azimuth (degrees, 0-360) and elevation angle (degrees)
#' @param radial_distortion Lens projection method. Use "equidistant" (default)
#'   for standard equidistant polar projection, or provide custom lens calibration
#'   data (see \code{\link{gla_lens_sigma_8mm}} for format).
#' @param verbose Logical indicating whether to print the processed data
#'   (default: FALSE)
#'
#' @return Data frame with original columns plus:
#'   \describe{
#'     \item{x_msk}{X-coordinates in polar projection}
#'     \item{y_msk}{Y-coordinates in polar projection}
#'   }
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Converts horizon elevation angles to zenith angles
#'   \item Converts to radians
#'   \item Projects to Cartesian coordinates using polar projection
#'   \item Flips x-axis so East is on left (camera convention)
#' }
#'
#' @keywords internal
prepare_horizon_mask <- function(
  horizon_data,
  radial_distortion = "equidistant",
  verbose = FALSE
) {
  # Validate input
  if (
    missing(horizon_data) || is.null(horizon_data) || nrow(horizon_data) == 0
  ) {
    stop("horizon_data parameter is required and must contain data")
  }

  if (ncol(horizon_data) < 2) {
    stop("horizon_data must have at least 2 columns (azimuth, elevation)")
  }

  # Validate radial_distortion
  validate_radial_distortion(radial_distortion)

  # Standardize column names - assume first two columns are azimuth and elevation
  names(horizon_data)[1:2] <- c("azimuth", "horizon_height")

  # Sort data by azimuth
  dat <- horizon_data[order(horizon_data$azimuth), ]

  if (verbose) {
    print(dat)
  }

  # Convert azimuth to radians
  azi_rad <- dat$azimuth * deg_to_rad()

  # Apply radial distortion if custom calibration provided
  if (!identical(radial_distortion, "equidistant")) {
    # horizon_height is elevation angle in degrees
    elev_rad <- dat$horizon_height * deg_to_rad()

    # Forward direction: elevation -> normalized radius (0-1)
    norm_radius <- apply_radial_distortion_mapping(
      elev_rad,
      from = radial_distortion$elevation,
      to = radial_distortion$radius
    )

    # Scale to image coordinates
    zen_rad_distorted <- norm_radius * rad_90()
    x_msk <- zen_rad_distorted * cos(azi_rad) * -1
    y_msk <- zen_rad_distorted * sin(azi_rad)
  } else {
    # Equidistant projection (default)
    zen_rad <- (90 - dat$horizon_height) * deg_to_rad()
    x_msk <- zen_rad * cos(azi_rad) * -1 # flip image along N/S axis (left = EAST; right = WEST)
    y_msk <- zen_rad * sin(azi_rad)
  }

  # Append new columns to dataframe
  result <- as.data.frame(cbind(dat, x_msk, y_msk))

  return(result)
}


#' Extract horizon masks for multiple points
#'
#' Extracts terrain horizon elevation angles from a DEM for multiple observation
#' points and converts them to horizon masks suitable for fisheye photo creation.
#' Supports caching to disk for efficient resume of interrupted workflows.
#'
#' @param points sf object containing observation points with point geometry.
#'   Must have x_meters and y_meters columns. Coordinates will be extracted from
#'   the geometry and transformed to WGS84 if necessary.
#' @param dem_path Character path to the DEM raster file (GeoTIFF)
#' @param output_dir Character path to directory for caching horizon CSV files.
#'   Each point's horizon data is saved as x_y_horizon.csv. Required parameter.
#' @param step Numeric azimuth step size in degrees for horizon calculation
#'   (default: 5)
#' @param max_search_distance Maximum search distance in meters for horizon detection
#'   (default: NULL, uses full DEM extent)
#' @param distance_step Distance step size in meters for sampling along line of
#'   sight (default: NULL, uses raster resolution)
#' @param cam_ht Camera height above ground in meters (default: 1.37). The observer
#'   elevation is calculated as ground elevation (from DEM) plus this height. Use the
#'   same value as in \code{\link{gla_transform_lidar}} for consistent reference frames.
#' @param parallel Logical indicating whether to process points in parallel
#'   (default: TRUE). When TRUE, uses the future backend configured with
#'   \code{future::plan()}. When FALSE, processes sequentially.
#' @param resume Logical indicating whether to skip points that already have
#'   cached horizon files (default: TRUE). When FALSE, recomputes all horizons.
#' @param verbose Logical indicating whether to print progress messages
#'   (default: FALSE)
#'
#' @return The input sf object with an added horizon_mask list-column. Each element
#'   is a list containing:
#'   \describe{
#'     \item{x_msk}{Numeric vector of x-coordinates for horizon mask polygon}
#'     \item{y_msk}{Numeric vector of y-coordinates for horizon mask polygon}
#'   }
#'
#' @details
#' This function:
#' \enumerate{
#'   \item For sequential (parallel=FALSE): Loads DEM once, processes all points
#'   \item For parallel (parallel=TRUE): Each worker loads DEM independently from file
#'   \item Extracts horizon angles using \code{gla_extract_horizon_terra}
#'   \item Converts to polar projection mask using \code{prepare_horizon_mask}
#'   \item Cleans up DEM from memory
#' }
#'
#' Memory usage: Each worker loads its own copy of the DEM from disk.
#' For N points with a DEM of size M GB and W workers:
#' \itemize{
#'   \item Sequential (parallel=FALSE): Peak memory ~M GB, Time ~40-60 sec/point
#'   \item Parallel: Peak memory ~W×M GB, Time ~(40-60 sec/point)/W
#' }
#'
#' Set up parallel processing before calling this function:
#' \code{future::plan(future::multisession, workers = 3)}
#'
#' @examples
#' \dontrun{
#'   # Load stream points
#'   stream_points <- gla_load_stream_network(
#'     stream_network_path,
#'     dem_path,
#'     HydroID = NULL
#'   )
#'
#'   # Extract horizons with parallel processing and caching
#'   future::plan(future::multisession, workers = 3)
#'   stream_points <- gla_extract_horizons(
#'     points = stream_points,
#'     dem_path = dem_path,
#'     output_dir = "output/horizons",
#'     step = 5,
#'     max_search_distance = NULL,
#'     parallel = TRUE,
#'     resume = TRUE
#'   )
#'
#'   # Horizon masks are now stored in stream_points$horizon_mask
#'   # Use in fisheye photo creation
#'   stream_points <- gla_create_fisheye_photos(
#'     points = stream_points,
#'     output_dir = "output/fisheye_photos"
#'   )
#' }
#'
#' @seealso
#' \code{\link{gla_extract_horizon_terra}} for single-point horizon extraction,
#' \code{\link{gla_create_fisheye_photos}} for using extracted horizons
#'
#' @export
gla_extract_horizons <- function(
  points,
  dem_path,
  output_dir,
  step = 5,
  max_search_distance = NULL,
  distance_step = NULL,
  cam_ht = 1.37,
  parallel = TRUE,
  resume = TRUE,
  verbose = FALSE
) {
  # Validate inputs
  validate_sf_object(points)

  if (!file.exists(dem_path)) {
    stop("DEM file not found: ", dem_path)
  }

  n_points <- nrow(points)
  message(
    "Extracting horizons for ",
    n_points,
    " locations using terra method..."
  )

  # Ensure x_meters and y_meters columns exist (needed for caching filenames)
  if (!("x_meters" %in% names(points))) {
    coords <- sf::st_coordinates(points)
    points$x_meters <- coords[, 1]
    points$y_meters <- coords[, 2]
  }

  # Check for cached horizons if resume=TRUE
  has_cached <- rep(FALSE, n_points)
  cached_horizons <- vector("list", n_points)

  if (resume) {
    has_cached <- find_existing_horizons(points, output_dir)
    n_cached <- sum(has_cached)

    if (n_cached > 0) {
      message("Found ", n_cached, " cached horizon files, loading...")

      for (i in which(has_cached)) {
        horizon_df <- load_horizon_csv(
          points$x_meters[i],
          points$y_meters[i],
          output_dir
        )

        if (!is.null(horizon_df)) {
          # Convert cached horizon angles to cartesian coordinates
          # Use first two columns which are azimuth and horizon_height
          horizon_processed <- prepare_horizon_mask(
            horizon_df[, 1:2],
            radial_distortion = "equidistant",
            verbose = FALSE
          )

          cached_horizons[[i]] <- list(
            azimuth = horizon_processed$azimuth,
            horizon_height = horizon_processed$horizon_height,
            x_msk = horizon_processed$x_msk,
            y_msk = horizon_processed$y_msk
          )
        }
      }

      message(
        "Loaded ",
        n_cached,
        " cached horizons, will compute ",
        n_points - n_cached,
        " new ones"
      )
    }
  }

  # Indices of points that need computation
  points_to_compute <- which(!has_cached)
  n_to_compute <- length(points_to_compute)

  # If all points are cached, return early
  if (n_to_compute == 0) {
    message("All horizons already cached, skipping computation")
    points$horizon_mask <- cached_horizons
    return(move_geom_col_to_end(points))
  }

  # Load DEM and validate CRS matches points
  message("Loading DEM and validating CRS...")
  dem_rast <- terra::rast(dem_path)

  # Strict CRS validation to prevent spatial errors
  dem_crs <- get_raster_crs(dem_rast)
  pts_crs <- sf::st_crs(points)

  validate_crs_match(pts_crs, dem_crs, "Points", "DEM")

  # Compute DEM max once for early termination optimization
  message("Computing DEM maximum elevation for early termination...")
  dem_max <- terra::global(dem_rast, "max", na.rm = TRUE)[1, 1]
  message("DEM max elevation: ", round(dem_max, 1), " m")

  if (parallel) {
    message("Using parallel processing")

    # Clean up DEM in main process (workers will load their own)
    rm(dem_rast)
    gc()

    # Compute only the points that need it
    computed_horizons <- future.apply::future_lapply(
      points_to_compute,
      function(
        i,
        dem_path,
        lats,
        lons,
        step,
        max_search_distance,
        dist_step,
        dem_max,
        cam_ht,
        verbose,
        x_meters,
        y_meters,
        output_dir
      ) {
        # Load DEM in worker (each worker loads independently)
        dem_rast <- terra::rast(dem_path)

        if (verbose && i %% 10 == 0) {
          message("Processing point ", i)
        }

        # Extract horizon (pass cached dem_max)
        horizon_df <- gla_extract_horizon_terra(
          dem_rast = dem_rast,
          x_meters = x_meters[i],
          y_meters = y_meters[i],
          step = step,
          max_search_distance = max_search_distance,
          distance_step = dist_step,
          dem_max = dem_max,
          cam_ht = cam_ht,
          verbose = FALSE
        ) |>
          prepare_horizon_mask(
            radial_distortion = "equidistant",
            verbose = FALSE
          )

        # Save to CSV
        save_horizon_csv(horizon_df, x_meters[i], y_meters[i], output_dir)

        list(
          azimuth = horizon_df$azimuth,
          horizon_height = horizon_df$horizon_height,
          x_msk = horizon_df$x_msk,
          y_msk = horizon_df$y_msk
        )
      },
      dem_path = dem_path,
      step = step,
      max_search_distance = max_search_distance,
      dist_step = distance_step,
      dem_max = dem_max,
      cam_ht = cam_ht,
      verbose = verbose,
      x_meters = points$x_meters,
      y_meters = points$y_meters,
      output_dir = output_dir,
      future.seed = TRUE
    )

    # Merge cached and computed horizons
    horizon_list <- cached_horizons
    horizon_list[points_to_compute] <- computed_horizons
  } else {
    message("Using sequential processing")

    # DEM already loaded above for dem_max calculation
    # Extract horizons sequentially for points that need computation
    computed_horizons <- lapply(points_to_compute, function(i) {
      if (verbose && i %% 10 == 0) {
        message("Processing point ", i, " of ", n_points)
      }

      horizon_df <- gla_extract_horizon_terra(
        dem_rast = dem_rast,
        x_meters = points$x_meters[i],
        y_meters = points$y_meters[i],
        step = step,
        max_search_distance = max_search_distance,
        distance_step = distance_step,
        dem_max = dem_max,
        cam_ht = cam_ht,
        verbose = FALSE
      ) |>
        prepare_horizon_mask(
          radial_distortion = "equidistant",
          verbose = FALSE
        )

      # Save to CSV
      save_horizon_csv(
        horizon_df,
        points$x_meters[i],
        points$y_meters[i],
        output_dir
      )

      list(
        azimuth = horizon_df$azimuth,
        horizon_height = horizon_df$horizon_height,
        x_msk = horizon_df$x_msk,
        y_msk = horizon_df$y_msk
      )
    })

    # Merge cached and computed horizons
    horizon_list <- cached_horizons
    horizon_list[points_to_compute] <- computed_horizons
  }

  # Clean up
  gc()
  message("Horizon extraction complete, DEM removed from memory")

  # Add horizon masks as list-column to points
  points$horizon_mask <- horizon_list
  move_geom_col_to_end(points)
}
