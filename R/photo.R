# Construct the output filename for a synthetic fisheye photo. The name encodes
# all parameters that affect pixel content so filenames are unambiguous and
# caching semantics remain correct when any parameter changes.
fisheye_filename <- function(
  site_id,
  pointsize,
  max_cex,
  min_cex,
  min_dist,
  max_dist,
  res,
  width,
  radial_distortion
) {
  fmt <- function(x) {
    if (is.numeric(x)) {
      if (all(is.finite(x)) && all(x == as.integer(x))) {
        s <- formatC(as.integer(x), format = "d")
      } else {
        s <- formatC(x, format = "f", digits = 6)
      }
    } else {
      s <- as.character(x)
    }
    gsub("\\.", "pt", s)
  }
  safe <- function(x) gsub("[^A-Za-z0-9_-]", "_", as.character(x))
  distortion_label <- if (identical(radial_distortion, "equidistant")) {
    "equidistant"
  } else if (is.list(radial_distortion) && !is.null(radial_distortion$name)) {
    safe(radial_distortion$name)
  } else {
    "custom"
  }
  sprintf(
    "%s_ps%s_cex%s-%s_dist%s-%s_%sdpi_%spx_%s.bmp",
    safe(site_id),
    fmt(pointsize),
    fmt(max_cex),
    fmt(min_cex),
    fmt(min_dist),
    fmt(max_dist),
    fmt(res),
    fmt(width),
    distortion_label
  )
}


#' Create fisheye image (single point)
#' @param radial_distortion Lens projection method. Use "equidistant" (default)
#'   for standard equidistant polar projection, or provide custom lens calibration
#'   data (see \code{\link{gla_lens_sigma_8mm}} for format).
#' @keywords internal
gla_create_fisheye_photo_single <- function(
  processed_lidar,
  x_msk,
  y_msk,
  site_id,
  img_path,
  max_cex,
  min_cex,
  min_dist,
  max_dist,
  width, # Required: image width for bmp() and filename
  pointsize, # Required: point size for bmp() and filename
  res, # Required: resolution for bmp() and filename
  radial_distortion = "equidistant",
  ...
) {
  # Create variable point size for plotting using linear distance decay function
  # Clamp to min_cex for points beyond max_dist
  pt_size <- pmax(
    (max_cex - min_cex) *
      (1 - (processed_lidar$rho - min_dist) / (max_dist - min_dist)) +
      min_cex,
    min_cex
  )

  # Apply radial distortion if custom calibration provided
  if (!identical(radial_distortion, "equidistant")) {
    # Convert zenith angle to elevation angle
    elev_rad <- rad_90() - processed_lidar$phi

    # Forward direction: elevation -> normalized radius (0-1)
    norm_radius <- apply_radial_distortion_mapping(
      elev_rad,
      from = radial_distortion$elevation,
      to = radial_distortion$radius
    )

    # Scale to image coordinates and convert to cartesian
    # (matches Gord's phi.sigma approach)
    phi_distorted <- norm_radius * rad_90()
    processed_lidar$x <- phi_distorted * cos(processed_lidar$theta) * -1
    processed_lidar$y <- phi_distorted * sin(processed_lidar$theta)
  }

  img_file <- fisheye_filename(
    site_id,
    pointsize,
    max_cex,
    min_cex,
    min_dist,
    max_dist,
    res,
    width,
    radial_distortion
  )
  out_file <- paste0(img_path, "/", img_file)

  # Convert R plot to bitmap and save as image file
  bmp(
    filename = out_file,
    width = width,
    pointsize = pointsize,
    res = res,
    bg = "white",
    type = "cairo",
    ...
  )

  # Set plotting window dimensions
  par(mai = c(0, 0, 0, 0))

  # create plot to hold topo mask and transformed lidar point cloud
  plot(
    x_msk,
    y_msk,
    cex = 0,
    xlim = c(-pi / 2, pi / 2),
    ylim = c(-pi / 2, pi / 2),
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    xaxs = "i",
    yaxs = "i",
    xpd = TRUE,
    asp = 1
  )

  # draw 180 FOV
  plotrix::draw.circle(
    0,
    0,
    rad = pi / 2,
    nv = 1000,
    border = NULL,
    col = "black"
  )

  # draw topo mask
  polygon(x_msk, y_msk, border = "white", col = "white")

  # add canopy points to plot
  points(
    processed_lidar$x,
    processed_lidar$y,
    pch = 16,
    cex = pt_size,
    xlim = c(-pi / 2, pi / 2),
    ylim = c(-pi / 2, pi / 2),
    col = "black"
  )

  dev.off()

  invisible(out_file)
}


#' Compute solar positions and radiation values
#' @keywords internal
gla_compute_solar_positions <- function(
  lat_deg,
  long_deg,
  elev,
  clearsky_coef,
  time_step_min,
  day_start,
  day_end,
  day_res,
  elev_res,
  azi_res,
  solar_constant
) {
  # Convert coordinates to radians
  lat_rad <- lat_deg * deg_to_rad()
  long_rad <- long_deg * deg_to_rad()
  time_zone <- timezone(long_deg = long_deg)[1]
  std_meridian <- timezone(long_deg = long_deg)[2]

  # Solar time step parameters
  time_step_rad <- time_step_min * (two_pi() / 1440)
  time_sample_rad <- seq(
    from = 0,
    to = 1440 - time_step_min,
    by = time_step_min
  ) *
    (two_pi() / 1440)
  time_sample_ha <- hrangle(solar_time_rad = time_sample_rad)

  # Sky mask resolution
  nRings <- 90 / elev_res
  nSectors <- 360 / azi_res

  # Conversion factors
  c1 <- 1 / (two_pi() / time_step_rad)
  c2 <- 3.6 / 1000 * 24 / two_pi() * time_step_rad

  # Day parameters
  day_numbers <- seq(from = day_start, to = day_end, by = day_res)
  num_days <- length(day_numbers)

  # Create arrays for solar calculations
  beam_array <- array(0, dim = c(nRings, nSectors))

  # Create storage matrices
  nrow_max <- 24 * 60 / time_step_min * num_days
  solar_mat <- matrix(
    NA,
    nrow = nrow_max,
    ncol = 11,
    dimnames = list(
      c(1:nrow_max),
      c(
        "DAY_NUM",
        "ZENITH",
        "AZIMUTH",
        "X_SUN",
        "Y_SUN",
        "EoT_MIN",
        "TIME_CORR_MIN",
        "SOLAR_TIME_HR",
        "LOCAL_STD_TIME_HR",
        "EXTRA_Wm2",
        "REL_BEAM"
      )
    )
  )
  day_mat <- data.frame(
    day_number = integer(num_days),
    day_length = numeric(num_days),
    wsr_lat = numeric(num_days),
    wss_lat = numeric(num_days),
    numSolarPos = integer(num_days),
    Ho_Wm2 = numeric(num_days),
    Ho_MJm2 = numeric(num_days)
  )

  # Initialize variables
  Total_rbi <- 0
  k <- 0

  # Process each day
  for (i in 1:length(day_numbers)) {
    day_angle <- da(day_number = day_numbers[i])
    ecf_dat <- ecf(day_angle_rad = day_angle)
    sol_dec <- soldec(day_angle_rad = day_angle)
    eot_dat <- eot(day_angle_rad = day_angle, long_deg = long_deg)
    etm <- eot_dat[1]
    time_offset <- eot_dat[2]
    hour_angle <- sshourangle(
      lat_rad = lat_rad,
      solar_declination_rad = sol_dec
    )
    cos_ws <- hour_angle[1]
    wsr <- hour_angle[2]
    wss <- hour_angle[3]

    # Check for polar night (sun never rises)
    if (is.na(wsr)) {
      day_mat$day_number[i] <- day_numbers[i]
      day_mat$day_length[i] <- 0
      day_mat$wsr_lat[i] <- NA_real_
      day_mat$wss_lat[i] <- NA_real_
      day_mat$numSolarPos[i] <- 0
      day_mat$Ho_Wm2[i] <- 0
      day_mat$Ho_MJm2[i] <- 0
      next # Skip to next day
    }

    wsr_lat <- 12 - (wsr * rad_to_deg()) / 15
    wss_lat <- 12 - (wss * rad_to_deg()) / 15
    day_length <- 2 * wsr * rad_to_deg() / 15
    ha_sample_pts <- time_sample_ha[which(
      time_sample_ha >= wss & time_sample_ha <= wsr
    )]
    numSolarPos <- length(ha_sample_pts)
    Daily_Io <- 0

    # Process each solar position
    for (j in 1:length(ha_sample_pts)) {
      ha <- ha_sample_pts[j]
      solar_lat <- (180 - ha * rad_to_deg()) / 15
      solar_lst <- solar_lat - time_offset / 60
      spd <- solpos(
        solar_declination_rad = sol_dec,
        lat_rad = lat_rad,
        hour_angle_rad = ha
      )
      sza <- spd[1]
      sea <- spd[2]
      saa <- spd[4]
      x_sun_pos <- spd[6]
      y_sun_pos <- spd[7]
      SR <- solrad(
        solar_constant = solar_constant,
        eccentricity_correction = ecf_dat,
        solar_zenith_angle = sza,
        site_elevation = elev,
        clearsky_transmission = clearsky_coef
      )
      Io <- SR[1]
      Daily_Io <- Daily_Io + Io
      rel_beam_int <- SR[2]
      Total_rbi <- Total_rbi + rel_beam_int

      k <- k + 1
      solar_mat[k, 1] <- day_numbers[i]
      solar_mat[k, 2] <- sza * rad_to_deg()
      solar_mat[k, 3] <- saa * rad_to_deg()
      solar_mat[k, 4] <- x_sun_pos
      solar_mat[k, 5] <- y_sun_pos
      solar_mat[k, 6] <- etm
      solar_mat[k, 7] <- time_offset
      solar_mat[k, 8] <- solar_lat
      solar_mat[k, 9] <- solar_lst
      solar_mat[k, 10] <- Io
      solar_mat[k, 11] <- rel_beam_int

      beam_idx <- skyregidx(
        solar_elevation_rad = sea,
        solar_azimuth_rad = saa,
        n_elevation_rings = nRings,
        n_azimuth_sectors = nSectors
      )
      beam_array[beam_idx[1], beam_idx[2]] <- beam_array[
        beam_idx[1],
        beam_idx[2]
      ] +
        rel_beam_int
    }

    # Store daily results
    Ho_Wm2 <- Daily_Io * c1
    Ho_MJm2 <- Daily_Io * c2
    day_mat$day_number[i] <- day_numbers[i]
    day_mat$day_length[i] <- day_length
    day_mat$wsr_lat[i] <- wsr_lat
    day_mat$wss_lat[i] <- wss_lat
    day_mat$numSolarPos[i] <- numSolarPos
    day_mat$Ho_Wm2[i] <- Ho_Wm2
    day_mat$Ho_MJm2[i] <- Ho_MJm2
  }

  # Return list with all computed values
  # Handle case where k=0 (no solar positions calculated, e.g., polar night)
  # Return 0-row matrix to maintain consistent structure with column names
  if (k == 0) {
    solar_mat_result <- solar_mat[0, , drop = FALSE]
  } else {
    solar_mat_result <- solar_mat[1:k, ]
  }

  list(
    solar_mat = solar_mat_result,
    beam_array = beam_array,
    day_mat = day_mat,
    Total_rbi = Total_rbi
  )
}


#' Extract gap fraction from fisheye image
#'
#' Processes a fisheye image and computes gap fraction (proportion of sky visible)
#' in elevation and azimuth bins.
#'
#' @param img_file Path to fisheye image file
#' @param elev_res Elevation resolution in degrees (default 5)
#' @param azi_res Azimuth resolution in degrees (default 5)
#' @param rotation_deg Rotation angle in degrees to align image with true north (default 0)
#' @param radial_distortion Lens projection method. Use "equidistant" (default)
#'   for standard equidistant polar projection, or provide custom lens calibration
#'   data (see \code{\link{gla_lens_sigma_8mm}} for format).
#' @param threshold Threshold value for converting image to binary. Can be:
#'   \itemize{
#'     \item Numeric value (0-1): pixels below threshold become 0, above become 1. Default is 0 (matches original behavior).
#'     \item "auto": automatic threshold detection using Otsu's method
#'     \item "XX\%": percentile-based threshold (e.g., "95\%")
#'   }
#'
#' @return A list with gap fraction matrix and metadata
#'
#' @examples
#' \dontrun{
#' # Default behavior (threshold = 0, any non-zero pixel becomes white)
#' gla_extract_gap_fraction("photo.jpg")
#'
#' # Automatic threshold detection
#' gla_extract_gap_fraction("photo.jpg", threshold = "auto")
#'
#' # Custom numeric threshold
#' gla_extract_gap_fraction("photo.jpg", threshold = 0.5)
#'
#' # Use lens calibration
#' sigma_cal <- gla_lens_sigma_8mm()
#' gla_extract_gap_fraction("photo.jpg", radial_distortion = sigma_cal)
#' }
#'
#' @export
gla_extract_gap_fraction <- function(
  img_file,
  elev_res = 5,
  azi_res = 5,
  rotation_deg = 0,
  radial_distortion = "equidistant",
  threshold = 0
) {
  # Sky mask resolution
  nRings <- 90 / elev_res
  nSectors <- 360 / azi_res

  # Load and process fisheye image
  img <- imager::load.image(img_file)
  if (imager::spectrum(img) > 1) {
    img <- imager::channel(img, 1)
  }

  # Apply thresholding to convert to binary
  # threshold() returns a pixset, convert back to cimg
  fisheye_pixset <- imager::threshold(img, thr = threshold)
  fisheye <- imager::as.cimg(fisheye_pixset)

  # Validate result is binary
  if (!is_img_binary(fisheye)) {
    stop(
      "Thresholding failed to produce binary image. ",
      "This should not happen - please report as a bug.",
      call. = FALSE
    )
  }

  # Compute canopy gap fraction
  image_width <- imager::width(fisheye)
  image_height <- imager::height(fisheye)
  radius <- mean(c(image_width / 2, image_height / 2))

  # Convert to centered Cartesian coordinates
  x_index <- c(1:image_width)
  y_index <- rev(1:image_height)
  x_coord <- x_index - mean(x_index)
  y_coord <- y_index - mean(y_index)

  # Initialize pixel count arrays
  total_pixels <- array(0, dim = c(nRings, nSectors))
  gap_pixels <- array(0, dim = c(nRings, nSectors))

  # Vectorized approach for pixel processing
  x_grid <- rep(x_coord, image_height)
  y_grid <- rep(y_coord, each = image_width)
  dist_grid <- sqrt(x_grid^2 + y_grid^2)
  circle_mask <- dist_grid <= radius

  x_circle <- x_grid[circle_mask]
  y_circle <- y_grid[circle_mask]
  dist_circle <- dist_grid[circle_mask]

  img_values <- as.vector(fisheye)
  pixel_values <- img_values[circle_mask]

  # Calculate elevation angles with optional custom calibration
  if (!identical(radial_distortion, "equidistant")) {
    # Custom lens calibration (reverse direction: radius -> elevation)
    norm_radius <- dist_circle / radius
    elev_rad <- apply_radial_distortion_mapping(
      norm_radius,
      from = radial_distortion$radius,
      to = radial_distortion$elevation
    )
  } else {
    # Default: polar (equidistant) projection
    zen_rad <- (dist_circle / radius) * rad_90()
    elev_rad <- rad_90() - zen_rad
  }

  # Calculate azimuth angles
  azi_rad <- atan2(x_circle, y_circle)

  # Apply rotation angle (default 0 = no rotation)
  # Positive angle = east of north, negative angle = west of north
  rot_azi <- azi_rad + (rotation_deg * deg_to_rad())

  # Convert to geographic direction (East/West flipped for skyward fisheye view)
  # North at 0 degrees, South at 180 degrees
  azi_rad <- convert_to_geographic_azimuth(rot_azi)

  # Compute sky region indices using elevation angle
  elev_bin_idx <- angular_bin_idx(elev_rad, rad_90(), nRings)
  azi_bin_idx <- angular_bin_idx(azi_rad, two_pi(), nSectors)

  # Count pixels in each bin
  for (i in 1:length(elev_bin_idx)) {
    total_pixels[elev_bin_idx[i], azi_bin_idx[i]] <- total_pixels[
      elev_bin_idx[i],
      azi_bin_idx[i]
    ] +
      1
    if (pixel_values[i] == 1) {
      gap_pixels[elev_bin_idx[i], azi_bin_idx[i]] <- gap_pixels[
        elev_bin_idx[i],
        azi_bin_idx[i]
      ] +
        1
    }
  }

  # Compute gap fractions
  gap_fraction <- gap_pixels / total_pixels

  list(
    gap_fraction = gap_fraction,
    total_pixels = total_pixels,
    gap_pixels = gap_pixels,
    nRings = nRings,
    nSectors = nSectors
  )
}


#' Get Sigma 8mm f/3.5 EX DG lens radial calibration
#'
#' Returns the radial distortion calibration data for a Sigma 8mm f/3.5 EX DG
#' circular fisheye lens. This calibration maps normalized image radius to
#' elevation angle.
#'
#' @return A list with two components:
#'   \item{radius}{Normalized radial distance from image center (0 to 1)}
#'   \item{elevation}{Elevation angle in radians (0 at horizon, pi/2 at zenith)}
#'
#' @return A list with components \code{radius} (normalized image radius),
#'   \code{elevation} (elevation angle in radians), and \code{name} (lens
#'   identifier for this calibration; currently \code{"sigma8mm"}).
#' @export
gla_lens_sigma_8mm <- function() {
  # Image radius (mm) for Sigma 8mm lens sequenced by 0.5
  sigma_radius <- seq(0, 11.5, 0.5)
  norm_sigma_radius <- sigma_radius / max(sigma_radius)

  # Optical zenith angle (degrees)
  sigma_zen_deg <- c(
    0,
    3.5,
    7,
    10.6,
    14.1,
    17.6,
    21.2,
    24.8,
    28.4,
    32.1,
    35.7,
    39.4,
    43.2,
    47,
    50.9,
    54.8,
    58.8,
    62.9,
    67,
    71.3,
    75.7,
    80.2,
    84.9,
    90
  )

  # Convert to radians and then to elevation angles
  sigma_zen_rad <- sigma_zen_deg * deg_to_rad()
  sigma_elev_rad <- rad_90() - sigma_zen_rad

  list(
    radius = norm_sigma_radius,
    elevation = sigma_elev_rad,
    name = "sigma8mm"
  )
}


#' Apply radial distortion mapping in either direction
#'
#' Performs linear interpolation for lens distortion calibration. Used
#' bidirectionally: forward (elevation → radius) for synthetic photo creation,
#' reverse (radius → elevation) for photo analysis.
#'
#' @param input_values Vector of input values to map
#' @param from Vector of input reference values (from calibration)
#' @param to Vector of output reference values (from calibration)
#' @return Vector of mapped values
#' @keywords internal
apply_radial_distortion_mapping <- function(input_values, from, to) {
  approx(
    from,
    to,
    xout = input_values,
    method = "linear",
    rule = 2
  )$y
}


#' Validate radial distortion calibration data
#'
#' Checks that radial_distortion is either "equidistant" (default) or a valid
#' calibration list with required components and normalized radius values.
#'
#' @param radial_distortion Either "equidistant" string or calibration list
#'   with radius and elevation components
#' @return TRUE invisibly if valid, otherwise stops with error
#' @keywords internal
validate_radial_distortion <- function(radial_distortion) {
  # Allow "equidistant" as valid string value
  if (is.character(radial_distortion) && length(radial_distortion) == 1) {
    if (radial_distortion == "equidistant") {
      return(invisible(TRUE))
    } else {
      stop(
        "radial_distortion string must be 'equidistant'.\n",
        "For custom calibration, provide a list with 'radius' and 'elevation' components.",
        call. = FALSE
      )
    }
  }

  # Validate list structure
  if (!is.list(radial_distortion)) {
    stop(
      "radial_distortion must be 'equidistant' or a calibration list",
      call. = FALSE
    )
  }

  if (!all(c("radius", "elevation") %in% names(radial_distortion))) {
    stop(
      "radial_distortion must have 'radius' and 'elevation' components.\n",
      "See ?gla_lens_sigma_8mm for example format.",
      call. = FALSE
    )
  }

  # Check radius is normalized (0-1)
  if (any(radial_distortion$radius < 0 | radial_distortion$radius > 1)) {
    stop(
      "radial_distortion$radius must be normalized to 0-1 range.\n",
      "Found values outside [0, 1]: min = ",
      min(radial_distortion$radius),
      ", max = ",
      max(radial_distortion$radius),
      "\n",
      "Normalize by dividing by maximum physical radius.",
      call. = FALSE
    )
  }

  # Check vectors are same length
  if (length(radial_distortion$radius) != length(radial_distortion$elevation)) {
    stop(
      "radial_distortion$radius and radial_distortion$elevation must have same length",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Process single fisheye photo and calculate SSR
#' @keywords internal
gla_process_fisheye_photo_single <- function(
  solar_data,
  img_file,
  lat_deg,
  long_deg,
  Kt,
  elev_res,
  azi_res,
  rotation_deg,
  keep_gap_fraction_data = FALSE,
  radial_distortion = "equidistant",
  threshold = 0
) {
  # Extract computed values from solar_data
  solar_mat <- solar_data$solar_mat
  beam_array <- solar_data$beam_array
  day_mat <- solar_data$day_mat
  Total_rbi <- solar_data$Total_rbi

  # Daily Kd ~ Kt decomposition model calibration
  daily_kt_calib <- seq(0, 1, 0.05)
  daily_kd_calib <- c(
    0.9956545,
    0.9891128,
    0.9763180,
    0.9577567,
    0.9305954,
    0.8929988,
    0.8441450,
    0.7817907,
    0.7096211,
    0.6301739,
    0.5463198,
    0.4610067,
    0.3780925,
    0.3029890,
    0.2391789,
    0.1869482,
    0.1658167,
    0.1658167,
    0.1658167,
    0.1658167,
    0.1658167
  )

  # Compute gap fractions
  gap_data <- gla_extract_gap_fraction(
    img_file,
    elev_res,
    azi_res,
    rotation_deg,
    radial_distortion,
    threshold
  )
  gap_frac <- gap_data$gap_fraction
  nRings <- gap_data$nRings
  nSectors <- gap_data$nSectors
  norm_sky_area <- skyarea(
    n_elevation_rings = nRings,
    n_azimuth_sectors = nSectors
  )
  CO <- sum(gap_frac * norm_sky_area) * 100

  # Build isotropic UOC sky irradiance model
  sky_rad <- uoc(
    n_elevation_rings = nRings,
    n_azimuth_sectors = nSectors
  )
  # Sky view factor (also known as indirect site factor)
  svf <- sum(sky_rad * gap_frac)

  # Calculate final solar radiation values from day_mat
  mean_Ho_Wm2 <- mean(day_mat$Ho_Wm2)
  mean_Ho_MJm2 <- mean(day_mat$Ho_MJm2)
  mean_H_MJm2 <- mean_Ho_MJm2 * Kt

  Kd <- approx(
    daily_kt_calib,
    daily_kd_calib,
    xout = Kt,
    method = "linear",
    rule = 2,
    ties = "ordered"
  )$y
  mean_Hd_MJm2 <- mean_H_MJm2 * Kd
  mean_Hb_MJm2 <- mean_H_MJm2 - mean_Hd_MJm2

  # Calculate transmitted radiation
  norm_rbi_wt <- beam_array / Total_rbi
  trans_Hb_MJm2 <- sum(mean_Hb_MJm2 * norm_rbi_wt * gap_frac)
  trans_Hd_MJm2 <- sum(mean_Hd_MJm2 * sky_rad * gap_frac)
  trans_H_MJm2 <- trans_Hb_MJm2 + trans_Hd_MJm2

  pct_trans_Hb <- trans_Hb_MJm2 / mean_Hb_MJm2 * 100
  pct_trans_Hd <- trans_Hd_MJm2 / mean_Hd_MJm2 * 100
  pct_trans_H <- trans_H_MJm2 / mean_H_MJm2 * 100

  # Return results as dataframe
  result <- data.frame(
    fisheye_photo_path = img_file,
    canopy_openness_pct = CO,
    mean_daily_extraterrestrial_irradiance_Wm2 = mean_Ho_Wm2,
    mean_daily_direct_irradiation_MJm2d = mean_Hb_MJm2,
    mean_daily_diffuse_irradiation_MJm2d = mean_Hd_MJm2,
    mean_daily_global_irradiation_MJm2d = mean_H_MJm2,
    transmitted_direct_irradiation_MJm2d = trans_Hb_MJm2,
    transmitted_diffuse_irradiation_MJm2d = trans_Hd_MJm2,
    transmitted_global_irradiation_MJm2d = trans_H_MJm2,
    transmitted_direct_irradiation_pct = pct_trans_Hb,
    transmitted_diffuse_irradiation_pct = pct_trans_Hd,
    transmitted_global_irradiation_pct = pct_trans_H,
    subcanopy_solar_radiation_MJm2d = trans_H_MJm2,
    light_penetration_index = (trans_H_MJm2 / mean_H_MJm2),
    row.names = NULL
  )

  # Add gap fraction data if requested
  if (keep_gap_fraction_data) {
    result$nRings <- nRings
    result$nSectors <- nSectors
    result$gap_pixels <- list(gap_data$gap_pixels)
    result$total_pixels <- list(gap_data$total_pixels)
    result$gap_fraction <- list(gap_data$gap_fraction)
  }

  result
}


#' Process fisheye photos for solar radiation analysis
#'
#' Batch processes fisheye photos for multiple points, computing gap fractions and
#' transmitted solar radiation. This is the main workflow function for analyzing
#' hemispherical fisheye photos.
#'
#' @param points An sf object with point locations. Must contain columns:
#'   \code{fisheye_photo_path}, \code{lat}, \code{lon}, and \code{elevation}.
#' @param clearsky_coef Clear-sky transmission coefficient (default 0.65).
#'   Proportion of extraterrestrial radiation reaching the surface under clear skies.
#' @param time_step_min Time step in minutes for computing solar positions (default 2).
#' @param day_start Start day of year (default 1). Use day-of-year format (1-365).
#' @param day_end End day of year (default 365).
#' @param day_res Day resolution - compute every N days (default 1 for daily).
#' @param Kt Mean cloudiness index (Kt = H/Ho) for the period of interest (default 0.45).
#'   Ratio of measured to extraterrestrial radiation.
#' @param parallel Use parallel processing (default TRUE)
#' @param keep_gap_fraction_data Include gap fraction matrix in output (default FALSE).
#'   If TRUE, adds \code{gap_fraction_data} column to output.
#' @param solar_constant Solar constant in W/m² (default 1367). The total solar
#'   electromagnetic radiation per unit area at the top of Earth's atmosphere.
#' @inheritParams gla_extract_gap_fraction
#'
#' @return An sf object with computed solar radiation metrics:
#'   \item{canopy_openness_pct}{Canopy openness percentage}
#'   \item{mean_daily_extraterrestrial_irradiance_Wm2}{Mean daily extraterrestrial irradiance (W/m²)}
#'   \item{mean_daily_direct_irradiation_MJm2d}{Mean daily direct irradiation (MJ/m²/day)}
#'   \item{mean_daily_diffuse_irradiation_MJm2d}{Mean daily diffuse irradiation (MJ/m²/day)}
#'   \item{mean_daily_global_irradiation_MJm2d}{Mean daily global irradiation (MJ/m²/day)}
#'   \item{transmitted_direct_irradiation_MJm2d}{Transmitted direct irradiation (MJ/m²/day)}
#'   \item{transmitted_diffuse_irradiation_MJm2d}{Transmitted diffuse irradiation (MJ/m²/day)}
#'   \item{transmitted_global_irradiation_MJm2d}{Transmitted global irradiation (MJ/m²/day)}
#'   \item{transmitted_direct_irradiation_pct}{Transmitted direct irradiation percentage}
#'   \item{transmitted_diffuse_irradiation_pct}{Transmitted diffuse irradiation percentage}
#'   \item{transmitted_global_irradiation_pct}{Transmitted global irradiation percentage}
#'
#' @examples
#' \dontrun{
#' # Process fisheye photos for August (days 213-243)
#' results <- gla_process_fisheye_photos(
#'   points = stream_points,
#'   day_start = 213,
#'   day_end = 243,
#'   Kt = 0.45
#' )
#'
#' # Use automatic thresholding for real photos
#' results <- gla_process_fisheye_photos(
#'   points = stream_points,
#'   threshold = "auto"
#' )
#'
#' # Keep gap fraction data for further analysis
#' results <- gla_process_fisheye_photos(
#'   points = stream_points,
#'   keep_gap_fraction_data = TRUE
#' )
#' }
#'
#' @export
gla_process_fisheye_photos <- function(
  points,
  clearsky_coef = 0.65,
  time_step_min = 2,
  day_start = 1,
  day_end = 365,
  day_res = 1,
  elev_res = 5,
  azi_res = 5,
  Kt = 0.45,
  rotation_deg = 0,
  parallel = TRUE,
  keep_gap_fraction_data = FALSE,
  radial_distortion = "equidistant",
  threshold = 0,
  solar_constant = 1367
) {
  # Validate inputs
  validate_sf_object(points)

  # Check if already processed
  solar_rad_cols <- c(
    "canopy_openness_pct",
    "transmitted_global_irradiation_MJm2d",
    "light_penetration_index"
  )
  already_processed <- any(solar_rad_cols %in% names(points))

  if (already_processed) {
    stop(
      "Points appear to be already processed (contains solar radiation columns). ",
      "Remove existing solar radiation columns before reprocessing."
    )
  }

  required_cols <- c("fisheye_photo_path", "lat", "lon", "elevation")
  validate_required_columns(
    points,
    required_cols,
    hint = "Use gla_create_fisheye_photos() to add fisheye_photo_path column"
  )

  invalid_bmp <- !vapply(points$fisheye_photo_path, is_valid_bmp, logical(1))
  if (any(invalid_bmp)) {
    stop(
      sum(invalid_bmp),
      " point(s) have missing or corrupted BMP files: ",
      paste(points$fisheye_photo_path[invalid_bmp], collapse = ", ")
    )
  }

  message("Processing ", nrow(points), " fisheye photos for solar radiation...")

  # Extract only the minimal data needed by workers (reduces memory footprint)
  pts_minimal <- data.frame(
    fisheye_photo_path = points$fisheye_photo_path,
    lat = points$lat,
    lon = points$lon,
    elevation = points$elevation,
    stringsAsFactors = FALSE
  )

  # Process each fisheye photo
  if (parallel) {
    results_list <- future.apply::future_lapply(
      seq_len(nrow(points)),
      function(
        i,
        pts,
        clearsky_coef,
        time_step_min,
        day_start,
        day_end,
        day_res,
        elev_res,
        azi_res,
        Kt,
        rotation_deg,
        keep_gap_fraction_data,
        radial_distortion
      ) {
        # Compute solar data in worker (avoids passing large object)
        solar_data <- gla_compute_solar_positions(
          lat_deg = pts$lat[i],
          long_deg = pts$lon[i],
          elev = pts$elevation[i],
          clearsky_coef = clearsky_coef,
          time_step_min = time_step_min,
          day_start = day_start,
          day_end = day_end,
          day_res = day_res,
          elev_res = elev_res,
          azi_res = azi_res,
          solar_constant = solar_constant
        )

        gla_process_fisheye_photo_single(
          solar_data = solar_data,
          img_file = pts$fisheye_photo_path[i],
          lat_deg = pts$lat[i],
          long_deg = pts$lon[i],
          Kt = Kt,
          elev_res = elev_res,
          azi_res = azi_res,
          rotation_deg = rotation_deg,
          keep_gap_fraction_data = keep_gap_fraction_data,
          radial_distortion = radial_distortion,
          threshold = threshold
        )
      },
      pts = pts_minimal,
      clearsky_coef = clearsky_coef,
      time_step_min = time_step_min,
      day_start = day_start,
      day_end = day_end,
      day_res = day_res,
      elev_res = elev_res,
      azi_res = azi_res,
      Kt = Kt,
      keep_gap_fraction_data = keep_gap_fraction_data,
      radial_distortion = radial_distortion,
      rotation_deg = rotation_deg,
      future.seed = TRUE
    )
  } else {
    results_list <- lapply(seq_len(nrow(points)), function(i) {
      # Compute solar data
      solar_data <- gla_compute_solar_positions(
        lat_deg = points$lat[i],
        long_deg = points$lon[i],
        elev = points$elevation[i],
        clearsky_coef = clearsky_coef,
        time_step_min = time_step_min,
        day_start = day_start,
        day_end = day_end,
        day_res = day_res,
        elev_res = elev_res,
        azi_res = azi_res,
        solar_constant = solar_constant
      )

      gla_process_fisheye_photo_single(
        solar_data = solar_data,
        img_file = points$fisheye_photo_path[i],
        lat_deg = points$lat[i],
        long_deg = points$lon[i],
        Kt = Kt,
        elev_res = elev_res,
        azi_res = azi_res,
        rotation_deg = rotation_deg,
        keep_gap_fraction_data = keep_gap_fraction_data,
        radial_distortion = radial_distortion,
        threshold = threshold
      )
    })
  }

  # Combine results into dataframe
  results_df <- do.call(rbind, results_list)

  # Add results to points dataframe
  merged <- merge(points, results_df, by = "fisheye_photo_path")
  class(merged) <- c("sf", "tbl_df", "tbl", "data.frame")

  if (nrow(merged) != nrow(points)) {
    stop(sprintf(
      "Merge failed: expected %d rows, got %d",
      nrow(points),
      nrow(merged)
    ))
  }

  message("Completed processing ", nrow(merged), " fisheye photos")

  merged
}


#' Create fisheye photos for multiple points
#'
#' Batch process LiDAR data to create synthetic hemispherical (fisheye)
#' photographs for multiple spatial points. Supports parallel processing
#' and can resume from previous runs.
#'
#' @param points An sf object containing spatial points with required columns:
#'   \code{las_files}, \code{lat}, \code{lon}, \code{elevation}, and
#'   \code{horizon_mask}. Use \code{gla_extract_horizons()} to add the
#'   horizon_mask column.
#' @param output_dir Directory path where fisheye photo BMP files will be saved
#' @param camera_height_m Camera height above ground in meters. Default is 1.37m
#' @param min_dist Minimum distance from camera to include LiDAR points (meters).
#'   Points closer than this distance are excluded. Default is 1m
#' @param max_dist Distance at which point symbols reach minimum size (meters).
#'   Point size (CEX) decays linearly from max_cex at min_dist to min_cex at
#'   max_dist. Points beyond max_dist are plotted with min_cex (smallest size).
#'   Default is 220m
#' @param img_res Image resolution in pixels (width and height). Default is 2800
#' @param max_cex Maximum symbol size for plotting points (CEX value). Controls
#'   the size of points closest to the camera (at min_dist). Default is 0.2
#' @param min_cex Minimum symbol size for plotting points (CEX value). Points at
#'   or beyond max_dist are plotted with this size. Default is 0.05
#' @param pointsize Point size parameter for bitmap graphics device. Default is 10
#' @param dpi Resolution in dots per inch for output image. Default is 300
#' @param parallel Logical. If TRUE (default), use parallel processing via
#'   \code{future.apply}. Set up parallel plan with \code{future::plan()} before
#'   calling this function
#' @param resume Logical. If TRUE (default), skip points that already have
#'   fisheye photos in the output directory
#' @param radial_distortion Lens projection method. Use "equidistant" (default)
#'   for standard equidistant polar projection, or provide custom lens calibration
#'   data with \code{radius} (normalized 0-1) and \code{elevation} (radians) components.
#'   See \code{\link{gla_lens_sigma_8mm}} for example format. Applied to both LiDAR
#'   points and horizon mask during photo creation.
#'
#' @return The input \code{points} sf object with an added column
#'   \code{fisheye_photo_path} containing the file paths to the generated
#'   fisheye photos
#'
#' @details
#' This function processes multiple points in batch, transforming LiDAR data
#' into synthetic hemispherical photographs. For each point, it:
#' \enumerate{
#'   \item Reads and transforms the LiDAR point cloud
#'   \item Projects points onto a hemispherical image plane
#'   \item Creates a bitmap image with distance-weighted point sizes
#'   \item Saves the result as a BMP file
#' }
#'
#' Parallel processing can significantly speed up processing for many points.
#' Set up a parallel plan before calling this function:
#' \code{future::plan(future::multisession, workers = 4)}
#'
#' The resume feature allows you to interrupt and restart processing without
#' re-creating existing photos.
#'
#' @seealso
#' \code{\link{gla_extract_horizons}} for extracting horizon masks
#'
#' @examples
#' \dontrun{
#'   # Assuming you have points with horizon masks already extracted
#'   future::plan(future::multisession, workers = 4)
#'
#'   points_with_photos <- gla_create_fisheye_photos(
#'     points = stream_points,
#'     output_dir = "output/fisheye_photos",
#'     camera_height_m = 1.37,
#'     max_dist = 220,
#'     parallel = TRUE,
#'     resume = TRUE
#'   )
#' }
#' @export
gla_create_fisheye_photos <- function(
  points,
  output_dir,
  camera_height_m = 1.37,
  min_dist = 1,
  max_dist = 220,
  img_res = 2800,
  max_cex = 0.2,
  min_cex = 0.05,
  pointsize = 10,
  dpi = 300,
  parallel = TRUE,
  resume = TRUE,
  radial_distortion = "equidistant"
) {
  # Validate inputs
  validate_sf_object(points)

  required_cols <- c(
    "point_id",
    "las_files",
    "lat",
    "lon",
    "elevation",
    "horizon_mask"
  )
  validate_required_columns(
    points,
    required_cols,
    hint = "Use gla_load_points() then gla_extract_horizons() to prepare points"
  )

  # Check for missing, non-existent, or empty LAS files.
  invalid_files <- !vapply(points$las_files, is_valid_las_file, logical(1))
  if (any(invalid_files)) {
    stop(
      sum(invalid_files),
      " point(s) have missing, non-existent, or empty LAS files: ",
      paste(points$las_files[invalid_files], collapse = ", ")
    )
  }

  # Validate radial_distortion
  validate_radial_distortion(radial_distortion)
  if (!identical(radial_distortion, "equidistant")) {
    message(
      "Using custom lens calibration for radial distortion (non-equidistant projection)"
    )
  }

  if (!dir.exists(output_dir)) {
    message("Creating output directory: ", output_dir)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Check for existing fisheye photos if resume = TRUE
  points_to_process_indices <- seq_len(nrow(points))
  existing_photo_paths <- character(nrow(points))

  if (resume && dir.exists(output_dir)) {
    existing_photos <- list.files(
      output_dir,
      pattern = "\\.bmp$",
      full.names = TRUE
    )

    if (length(existing_photos) > 0) {
      expected_filenames <- fisheye_filename(
        paste0(points$point_id, "_h", camera_height_m),
        pointsize,
        max_cex,
        min_cex,
        min_dist,
        max_dist,
        dpi,
        img_res,
        radial_distortion
      )

      # Check which photos already exist
      existing_mask <- file.path(output_dir, expected_filenames) %in%
        existing_photos

      if (any(existing_mask)) {
        n_existing <- sum(existing_mask)
        message(
          "Found ",
          n_existing,
          " existing fisheye photos, skipping those points"
        )
        existing_photo_paths[existing_mask] <- file.path(
          output_dir,
          expected_filenames[existing_mask]
        )
        points_to_process_indices <- which(!existing_mask)
      }
    }
  }

  # If all photos already exist, return early
  if (length(points_to_process_indices) == 0) {
    message(
      "All ",
      nrow(points),
      " fisheye photos already exist, skipping processing"
    )
    points$fisheye_photo_path <- existing_photo_paths
    return(invisible(points))
  }

  if (resume && length(points_to_process_indices) < nrow(points)) {
    message(
      "Processing ",
      length(points_to_process_indices),
      " new fisheye photos (",
      nrow(points) - length(points_to_process_indices),
      " already exist)"
    )
  }

  # Extract horizon masks for points that need processing
  # (if resume=TRUE, some points may be skipped, so we subset from points$horizon_mask)
  horizon_list <- points$horizon_mask[points_to_process_indices]

  # Reprocess horizon masks with current radial_distortion if needed
  # Horizon extraction always uses equidistant projection, but photo creation
  # may use custom lens calibration
  if (!identical(radial_distortion, "equidistant")) {
    horizon_list <- lapply(horizon_list, function(horizon) {
      # Reprocess angular data with current distortion
      horizon_df <- data.frame(
        azimuth = horizon$azimuth,
        horizon_height = horizon$horizon_height
      )
      horizon_processed <- prepare_horizon_mask(
        horizon_df,
        radial_distortion = radial_distortion,
        verbose = FALSE
      )
      list(
        azimuth = horizon$azimuth,
        horizon_height = horizon$horizon_height,
        x_msk = horizon_processed$x_msk,
        y_msk = horizon_processed$y_msk
      )
    })
  }

  message(
    "Processing ",
    length(points_to_process_indices),
    " fisheye photos..."
  )

  # Build a minimal data frame for parallel workers - geometry and list-columns like
  # horizon_mask are not needed and would be serialized to every worker needlessly.
  pts_worker <- data.frame(
    las_files = points$las_files,
    point_id = points$point_id,
    x_meters = points$x_meters,
    y_meters = points$y_meters,
    elevation = points$elevation,
    stringsAsFactors = FALSE
  )

  # Process each point that needs processing
  if (parallel) {
    new_fisheye_paths <- future.apply::future_lapply(
      seq_along(points_to_process_indices),
      function(idx) {
        i <- points_to_process_indices[idx]

        # Transform lidar
        processed_lidar <- gla_transform_lidar(
          las_input = pts_worker$las_files[i],
          x_meters = pts_worker$x_meters[i],
          y_meters = pts_worker$y_meters[i],
          elev_m = pts_worker$elevation[i],
          camera_height_m = camera_height_m,
          min_dist = min_dist
        )

        site_id <- as.character(pts_worker$point_id[i])

        # Create fisheye photo with error handling
        tryCatch(
          {
            gla_create_fisheye_photo_single(
              processed_lidar = processed_lidar,
              x_msk = horizon_list[[idx]]$x_msk,
              y_msk = horizon_list[[idx]]$y_msk,
              site_id = site_id,
              img_path = output_dir,
              max_cex = max_cex,
              min_cex = min_cex,
              min_dist = min_dist,
              max_dist = max_dist,
              width = img_res,
              pointsize = pointsize,
              res = dpi,
              radial_distortion = radial_distortion,
              height = img_res,
              units = "px"
            )
          },
          error = function(e) {
            warning(
              "Failed to create fisheye photo for point ",
              i,
              " (",
              site_id,
              "): ",
              e$message
            )
            NA_character_
          }
        )
      },
      future.seed = TRUE
    )
  } else {
    new_fisheye_paths <- lapply(
      seq_along(points_to_process_indices),
      function(idx) {
        i <- points_to_process_indices[idx]
        # Transform lidar
        processed_lidar <- gla_transform_lidar(
          las_input = pts_worker$las_files[i],
          x_meters = pts_worker$x_meters[i],
          y_meters = pts_worker$y_meters[i],
          elev_m = pts_worker$elevation[i],
          camera_height_m = camera_height_m,
          min_dist = min_dist
        )

        site_id <- as.character(pts_worker$point_id[i])

        # Create fisheye photo with error handling
        tryCatch(
          {
            gla_create_fisheye_photo_single(
              processed_lidar = processed_lidar,
              x_msk = horizon_list[[idx]]$x_msk,
              y_msk = horizon_list[[idx]]$y_msk,
              site_id = site_id,
              img_path = output_dir,
              max_cex = max_cex,
              min_cex = min_cex,
              min_dist = min_dist,
              max_dist = max_dist,
              width = img_res,
              pointsize = pointsize,
              res = dpi,
              radial_distortion = radial_distortion,
              height = img_res,
              units = "px"
            )
          },
          error = function(e) {
            warning(
              "Failed to create fisheye photo for point ",
              i,
              " (",
              site_id,
              "): ",
              e$message
            )
            NA_character_
          }
        )
      }
    )
  }

  # Merge new paths with existing paths
  all_photo_paths <- existing_photo_paths
  all_photo_paths[points_to_process_indices] <- unlist(new_fisheye_paths)

  points$fisheye_photo_path <- all_photo_paths

  # Move geometry column to end
  points <- move_geom_col_to_end(points)

  message(
    "Completed processing ",
    length(points_to_process_indices),
    " new fisheye photos"
  )
  if (resume && length(points_to_process_indices) < nrow(points)) {
    message(
      "Total: ",
      nrow(points),
      " photos (",
      length(points_to_process_indices),
      " new, ",
      nrow(points) - length(points_to_process_indices),
      " existing)"
    )
  }

  points
}
