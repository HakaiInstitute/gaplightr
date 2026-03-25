# DEFINE ALL REQUIRED CUSTOM R FUNCTIONS

#' NORMALIZED AREA FOR EACH ZENITH BY AZIMUTH SKY-REGION SECTOR
#'
#' Note: Uses elevation angle (complement of zenith) for calculation
#'
#' @param n_elevation_rings no. of elevation rings
#' @param n_azimuth_sectors no. of azimuth sectors
#' @keywords internal
#' @noRd
skyarea <- function(n_elevation_rings, n_azimuth_sectors) {
  # Define ring and sector limits for sky region segments
  # Using elevation angle (90 degrees - zenith angle)
  elev_lim <- seq(0, rad_90(), rad_90() / n_elevation_rings)
  # Create array for sky-region area calculations
  sky_area <- array(0, dim = c(n_elevation_rings, n_azimuth_sectors))
  # Normalized sky area by ring
  for (i in 1:n_elevation_rings) {
    # Ring area using elevation angle and sin()
    ring_area <- sin(elev_lim[i + 1]) - sin(elev_lim[i])
    # Sector area
    for (j in 1:n_azimuth_sectors) {
      # compute sky area per sky sector
      seg_area <- ring_area / n_azimuth_sectors
      sky_area[i, j] <- seg_area
    }
  }
  return(sky_area)
}
# TEST: Compute sky region areas (note: must sum to one)
# norm_sky_area <- skyarea(nRings, nSectors)
# print(norm_sky_area)
# print(sum(norm_sky_area))

#' NORMALIZED UNIVERSAL OVERCAST (UOC) SKY IRRADIANCE DISTRIBUTION
#'
#' Note: Uses elevation angle (complement of zenith) for calculation
#'
#' @param n_elevation_rings no. of elevation rings
#' @param n_azimuth_sectors no. of azimuth sectors
#' @keywords internal
#' @noRd
uoc <- function(n_elevation_rings, n_azimuth_sectors) {
  # Create vectors to store ring and sector limits
  # Using elevation angle (90 degrees - zenith angle)
  elev_lim <- seq(0, rad_90(), rad_90() / n_elevation_rings)
  # Create array to store UOC calculation for each sky region
  sky_rad_array <- array(0, dim = c(n_elevation_rings, n_azimuth_sectors))
  # By ring
  for (i in 1:n_elevation_rings) {
    # compute mid-point elevation angle
    elev_mid <- (elev_lim[i] + elev_lim[i + 1]) / 2
    # compute ring area using elevation angle and sin()
    ring_area <- sin(elev_lim[i + 1]) - sin(elev_lim[i])
    # By sector
    for (j in 1:n_azimuth_sectors) {
      # compute normalized sky radiance per sky sector using sin(elevation)
      Id_uoc <- ring_area * sin(elev_mid) / n_azimuth_sectors
      sky_rad_array[i, j] <- Id_uoc
    }
  }
  # Relativize UOC so that all sky sectors sum to 1
  rel_Id_uoc <- sky_rad_array / sum(sky_rad_array)
  return(rel_Id_uoc)
}


# SOLAR GEOMETRY AND OTHER SUPPORTING FUNCTIONS

#' Convert from azimuth angle (north = 0, CW rotation) to math angle (east = 0, CCW rotation)
#'
#' @param azimuth_rad solar azimuth angle in radians
#' @keywords internal
#' @noRd
azi2math <- function(azimuth_rad) {
  ma <- rad_450() - azimuth_rad
  if (ma > two_pi()) {
    ma <- ma - two_pi()
  }
  return(ma)
}

#' DAY ANGLE in radians
#'
#' See, https://nsidc.org/data/user-resources/help-center/day-year-doy-calendar
#'
#' @param day_number day number (Jan. 1 = 1 to Dec. 31 = 365); Sept. 12 = 255; Aug. 1 = 213; Aug. 31 = 243
#' @keywords internal
#' @noRd
da <- function(day_number) {
  # Day angle in radians
  da <- two_pi() * (day_number - 1) / 365
  return(da)
}

#' ECCENTRICITY CORRECTION FACTOR
#'
#' @param day_angle_rad day angle in radians
#' @keywords internal
#' @noRd
ecf <- function(day_angle_rad) {
  # Eccentricity correction factor (Rc)
  Rc <- 1.000110 +
    0.034221 * cos(day_angle_rad) +
    0.001280 * sin(day_angle_rad) +
    0.000719 * cos(2 * day_angle_rad) +
    0.000077 * sin(2 * day_angle_rad)
  return(Rc)
}

#' SOLAR DECLINATION in radians
#'
#' @param day_angle_rad day angle in radians
#' @keywords internal
#' @noRd
soldec <- function(day_angle_rad) {
  # solar declination
  soldec <- 0.006918 -
    0.399912 * cos(day_angle_rad) +
    0.070257 * sin(day_angle_rad) -
    0.006758 * cos(2 * day_angle_rad) +
    0.000907 * sin(2 * day_angle_rad) -
    0.002697 * cos(3 * day_angle_rad) +
    0.00148 * sin(3 * day_angle_rad)
  return(soldec)
}

#' SOLAR TIME (LAT - local apparent time or TST - true solar time) in radians
#'
#' @param decimal_hours time in decimal hours (0-24 hr.)
#' @keywords internal
#' @noRd
soltime <- function(decimal_hours) {
  st <- 15 * decimal_hours * deg_to_rad()
  return(st)
}

#' HOUR ANGLE in radians
#'
#' @param solar_time_rad solar time in radians
#' @keywords internal
#' @noRd
hrangle <- function(solar_time_rad) {
  w <- pi - solar_time_rad
  return(w)
}

#' TIME ZONE AND STANDARD MERIDIAN in degrees
#'
#' @param long_deg longitude in degrees
#' @keywords internal
#' @noRd
timezone <- function(long_deg) {
  # Time zones away from prime meridian (negative west, positive east)
  y <- long_deg / 15
  # Extract decimal portion
  frac <- y - floor(y)
  # Place in correct time zone
  if (frac < 0.5) {
    time_zone <- floor(y)
  } else {
    time_zone <- floor(y) + 1
  }
  # Compute standard meridian of time zone
  std_merid <- time_zone * 15
  return(c(time_zone, std_merid))
}

#' EQUATION oF TIME (EoT) in minutes
#'
#' @param day_angle_rad day angle in radians
#' @param long_deg longitude in degrees
#' @keywords internal
#' @noRd
eot <- function(day_angle_rad, long_deg) {
  # Equation of time in minutes
  Etm <- 229.18 *
    (0.000075 +
      0.001868 * cos(day_angle_rad) -
      0.032077 * sin(day_angle_rad) -
      0.014615 * cos(2 * day_angle_rad) -
      0.04089 * sin(2 * day_angle_rad))
  # Time offset in minutes (earth rotates 1 degree every 4 minutes)
  time_zone <- timezone(long_deg = long_deg)[1]
  t_offset <- Etm + 4 * long_deg - 60 * time_zone
  return(c(Etm, t_offset))
}

#' LOCAL STANDARD TIME in decimal hours
#'
#' @param solar_time_hours solar time in decimal hours
#' @param time_offset_min time offset in minutes
#' @keywords internal
#' @noRd
lst <- function(solar_time_hours, time_offset_min) {
  lst <- solar_time_hours - time_offset_min / 60
  return(lst)
}

#' SUNRISE AND SUNSET HOUR ANGLE in radians (see, Iqbal for limits on the cosine of the hour angle)
#'
#' Note: if cosine of the hour angle is > 1, then the sun does not rise (24 hours darkness, no daily solar insolation), and
#' if the cosine of the hour angle is < - 1, then the sun does not set (24 hours of light).
#'
#' @param lat_rad latitude in radians
#' @param solar_declination_rad solar declination in radians
#' @keywords internal
#' @noRd
sshourangle <- function(lat_rad, solar_declination_rad) {
  # Cosine of sunrise hour angle
  cos_ws <- -tan(lat_rad) * tan(solar_declination_rad)
  # No sunrise (24 hours of dark)
  if (cos_ws > 1) {
    sunrise <- NA_real_
    sunset <- NA_real_
    # No sunset (24 hours of light)
  } else if (cos_ws < -1) {
    sunrise <- pi
    sunset <- -pi
    # Discrete angle for sunrise and sunset
  } else {
    sunrise <- acos(max(min(cos_ws, 1), -1))
    sunset <- -sunrise
  }
  # Return vector of hour angles
  # If the hour angle of sunrise and sunset are NA then goto next day number and record 0 as daily solar insolation
  return(c(cos_ws, sunrise, sunset))
}

#' SOLAR POSITION: ZENITH AND AZIMUTH
#'
#' @param solar_declination_rad solar declination in radians
#' @param lat_rad latitude in radians
#' @param hour_angle_rad hour angle in radians
#' @keywords internal
#' @noRd
solpos <- function(solar_declination_rad, lat_rad, hour_angle_rad) {
  # Solar zenith in radians
  cos_sz <- sin(solar_declination_rad) *
    sin(lat_rad) +
    cos(solar_declination_rad) * cos(lat_rad) * cos(hour_angle_rad)
  # Guard against floating point trickery
  sz <- acos(min(max(cos_sz, -1.0), 1.0))
  # Solar elevation in radians
  se <- rad_90() - sz
  # Solar azimuth in radians
  # Note: solar azimuth is undefined if sun is directly overhead (sz == 0)
  if (sz == 0) {
    sa <- NA_real_
    sa_rot <- NA_real_
    sa_rot_ccw <- NA_real_
    x_sun <- 0
    y_sun <- 0
  } else {
    # Cosine of the solar azimuth
    cos_sa <- (sin(se) * sin(lat_rad) - sin(solar_declination_rad)) /
      (cos(se) * cos(lat_rad))
    # Solar azimuth in radians
    sa <- acos(min(max(cos_sa, -1.0), 1.0))
    # Solar azimuth is dependent on sign of the hour angle
    # Hour angle is positive in morning, zero at noon, and negative in afternoon
    # Convert to compass direction (NORTH = 0, clockwise rotation)
    sa_rot <- ifelse(hour_angle_rad > 0, pi - sa, pi + sa)
    # Convert solar azimuth to math CCW rotation (EAST = 0) for sun path plotting
    sa_rot_ccw <- azi2math(azimuth_rad = sa_rot)
    # Convert to (x, y) Cartesian coordinates for plotting solar positions
    # Note: (x,y) coordinates are scaled to pi/2
    x_sun <- sz * cos(sa_rot_ccw)
    y_sun <- sz * sin(sa_rot_ccw)
  }
  # Return vector of output variables
  return(c(sz, se, sa, sa_rot, sa_rot_ccw, x_sun, y_sun))
}

#' INSTANTANEOUS EXTRATERRESTRIAL SOLAR IRRADIANCE AND INSTANTANEOUS TERRESTRIAL BEAM INTENSITY WEIGHTINGS
#'
#' See, Chapter 5, Iqbal, A cloudless-sky atmosphere and its optics
#'
#' @param solar_constant solar constant (1367 W/m2)
#' @param eccentricity_correction eccentricity correction factor (Eo)
#' @param solar_zenith_angle solar zenith angle (radians)
#' @param site_elevation site elevation in m.a.s.l
#' @param clearsky_transmission clear-sky transmission coefficient (default 0.65)
#' @keywords internal
#' @noRd
solrad <- function(
  solar_constant,
  eccentricity_correction,
  solar_zenith_angle,
  site_elevation,
  clearsky_transmission
) {
  # Only compute SR when sun is visible
  if (solar_zenith_angle >= 0 & solar_zenith_angle <= rad_90()) {
    # Instantaneous extraterrestrial irradiance (W/m2) on a horizontal surface at time t
    Io <- solar_constant * eccentricity_correction * cos(solar_zenith_angle)
    # Relative optical airmass at sea level (Kasten and Young, 1989)
    # Make sure the relative optical airmass is equal to 1 when the solar zenith angle = 0
    ram <- ifelse(
      solar_zenith_angle > 0,
      1 /
        (cos(solar_zenith_angle) +
          0.50572 * (96.07995 - solar_zenith_angle * rad_to_deg())^-1.6343),
      1
    )
    # Relative air density at station elevation in metres above mean sea level (List, 1964)
    p_po <- (1 - 2.2569 * 10^-5 * site_elevation)^5.2553
    # Airmass corrected for station air pressure
    ram_corr <- ram * p_po
    # Beam (direct) radiation intensity weightings under clear-skies at time t
    # Duffie and Beckman, 2013, Ch. 2: Estimation of clear-sky radiation (Eqn. 2.8.3)
    Rb <- eccentricity_correction *
      clearsky_transmission^ram_corr *
      cos(solar_zenith_angle)
  } else {
    Io <- 0
    Rb <- 0
  }
  # Return vector of outputs
  return(c(Io, Rb))
}

#' Compute 1-based bin index for an angular value
#'
#' Assigns angles to bins where bin 1 starts at 0 and bin n_bins ends at max_rad.
#' Values exactly at max_rad are placed in bin n_bins rather than overflowing.
#'
#' @param angle_rad angle value(s) in radians
#' @param max_rad maximum angle defining the range (e.g., pi/2 or 2*pi)
#' @param n_bins number of bins to divide the range into
#' @return integer bin index (1 to n_bins)
#' @keywords internal
#' @noRd
angular_bin_idx <- function(angle_rad, max_rad, n_bins) {
  pmin(floor(angle_rad / max_rad * n_bins) + 1L, n_bins)
}

#' Convert rotated azimuth to geographic azimuth
#'
#' Converts a rotated azimuth angle to geographic convention where North is 0,
#' East is pi/2, South is pi, and West is 3*pi/2. This handles the East/West
#' flip needed for skyward fisheye views.
#'
#' @param rot_azi rotated azimuth angle in radians (can be negative)
#' @return geographic azimuth in radians (0 to 2*pi)
#' @keywords internal
#' @noRd
convert_to_geographic_azimuth <- function(rot_azi) {
  ifelse(rot_azi <= 0, -rot_azi, two_pi() - rot_azi)
}

#' SKY REGION INDICES
#'
#' @param solar_elevation_rad solar elevation in radians
#' @param solar_azimuth_rad solar azimuth in radians
#' @param n_elevation_rings no. of elevation rings
#' @param n_azimuth_sectors no. of azimuth sectors
#' @keywords internal
#' @noRd
skyregidx <- function(
  solar_elevation_rad,
  solar_azimuth_rad,
  n_elevation_rings,
  n_azimuth_sectors
) {
  elev_bin_idx <- angular_bin_idx(
    solar_elevation_rad,
    rad_90(),
    n_elevation_rings
  )
  azi_bin_idx <- angular_bin_idx(solar_azimuth_rad, two_pi(), n_azimuth_sectors)
  return(c(elev_bin_idx, azi_bin_idx))
}


#' Degree to radian conversion factor
#' @return pi / 180
#' @keywords internal
#' @noRd
deg_to_rad <- function() {
  pi / 180
}

#' Radian to degree conversion factor
#' @return 180 / pi
#' @keywords internal
#' @noRd
rad_to_deg <- function() {
  180 / pi
}

#' 90 degrees in radians (pi/2)
#' @return pi / 2
#' @keywords internal
#' @noRd
rad_90 <- function() {
  pi / 2
}

#' 450 degrees in radians (2.5 * pi)
#' @return 2 * pi * 450 / 360
#' @keywords internal
#' @noRd
rad_450 <- function() {
  2 * pi * 450 / 360
}

#' 2*pi (full circle in radians)
#' @return 2 * pi
#' @keywords internal
#' @noRd
two_pi <- function() {
  2 * pi
}
