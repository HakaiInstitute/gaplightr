# DEFINE ALL REQUIRED CUSTOM R FUNCTIONS
# NORMALIZED AREA FOR EACH ZENITH BY AZIMUTH SKY-REGION SECTOR
# Required input parameters: nRing - no. of zenith rings; nSector - no. of azimuth sectors
# Note: Uses elevation angle (complement of zenith) for calculation
skyarea <- function(nRing, nSector) {
  # Define ring and sector limits for sky region segments
  # Using elevation angle (90 degrees - zenith angle)
  elev_lim <- seq(0, rad_90(), rad_90() / nRing)
  azi_lim <- seq(0, two_pi(), two_pi() / nSector)
  # Create array for sky-region area calculations
  sky_area <- array(0, dim = c(nRing, nSector))
  # Normalized sky area by ring
  for (i in 1:nRing) {
    # Ring area using elevation angle and sin()
    ring_area <- sin(elev_lim[i + 1]) - sin(elev_lim[i])
    # Sector area
    for (j in 1:nSector) {
      # compute sky area per sky sector
      seg_area <- ring_area / nSector
      sky_area[i, j] <- seg_area
    }
  }
  return(sky_area)
}
# TEST: Compute sky region areas (note: must sum to one)
# norm_sky_area <- skyarea(nRings, nSectors)
# print(norm_sky_area)
# print(sum(norm_sky_area))

# NORMALIZED UNIVERSAL OVERCAST (UOC) SKY IRRADIANCE DISTRIBUTION
# Required input parameters: nRing - no. of zenith rings; nSector - no. of azimuth sectors
# Note: Uses elevation angle (complement of zenith) for calculation
uoc <- function(nRing, nSector) {
  # Create vectors to store ring and sector limits
  # Using elevation angle (90 degrees - zenith angle)
  elev_lim <- seq(0, rad_90(), rad_90() / nRing)
  azi_lim <- seq(0, two_pi(), two_pi() / nSector)
  # Create array to store UOC calculation for each sky region
  sky_rad_array <- array(0, dim = c(nRing, nSector))
  # By ring
  for (i in 1:nRing) {
    # compute mid-point elevation angle
    elev_mid <- (elev_lim[i] + elev_lim[i + 1]) / 2
    # compute ring area using elevation angle and sin()
    ring_area <- sin(elev_lim[i + 1]) - sin(elev_lim[i])
    # By sector
    for (j in 1:nSector) {
      # compute normalized sky radiance per sky sector using sin(elevation)
      Id_uoc <- ring_area * sin(elev_mid) / nSector
      sky_rad_array[i, j] <- Id_uoc
    }
  }
  # Relativize UOC so that all sky sectors sum to 1
  rel_Id_uoc <- sky_rad_array / sum(sky_rad_array)
  return(rel_Id_uoc)
}


# SOLAR GEOMETRY AND OTHER SUPPORTING FUNCTIONS
# Convert from azimuth angle (north = 0, CW rotation) to math angle (east = 0, CCW rotation)
# Required input parameters: solar azimuth angle in radians
azi2math <- function(x) {
  ma <- rad_450() - x
  if (ma > two_pi()) {
    ma <- ma - two_pi()
  }
  return(ma)
}

# DAY ANGLE in radians
# Required input parameters: x - day number (Jan. 1 = 1 to Dec. 31 = 365); Sept. 12 = 255; Aug. 1 = 213; Aug. 31 = 243
# See, https://nsidc.org/data/user-resources/help-center/day-year-doy-calendar
da <- function(x) {
  # Day angle in radians
  da <- two_pi() * (x - 1) / 365
  return(da)
}

# ECCENTRICITY CORRECTION FACTOR
# Required input parameters: x = day angle (DA)
ecf <- function(x) {
  # Eccentricity correction factor (Rc)
  Rc <- 1.000110 +
    0.034221 * cos(x) +
    0.001280 * sin(x) +
    0.000719 * cos(2 * x) +
    0.000077 * sin(2 * x)
  return(Rc)
}

# SOLAR DECLINATION in radians
# Required input parameters: x = day angle (DA) in radians
soldec <- function(x) {
  # solar declination
  soldec <- 0.006918 -
    0.399912 * cos(x) +
    0.070257 * sin(x) -
    0.006758 * cos(2 * x) +
    0.000907 * sin(2 * x) -
    0.002697 * cos(3 * x) +
    0.00148 * sin(3 * x)
  return(soldec)
}

# SOLAR TIME (LAT - local apparent time or TST - true solar time) in radians
# Required input parameters: x = time in decimal hours (0-24 hr.)
soltime <- function(x) {
  st <- 15 * x * deg_to_rad()
  return(st)
}

# HOUR ANGLE in radians
# Required input parameters: x = solar time in radians
hrangle <- function(x) {
  w <- pi - x
  return(w)
}

# TIME ZONE AND STANDARD MERIDIAN in degrees
# Required input parameters: x1 = longtitude in degrees
timezone <- function(x) {
  # Time zones away from prime meridian (negative west, positive east)
  y <- x / 15
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

# EQUATION oF TIME (EoT) in minutes
# Required input parameters: x = day angle (DA) in radians
eot <- function(x, long_deg) {
  # Equation of time in minutes
  Etm <- 229.18 *
    (0.000075 +
      0.001868 * cos(x) -
      0.032077 * sin(x) -
      0.014615 * cos(2 * x) -
      0.04089 * sin(2 * x))
  # Time offset in minutes (earth rotates 1 degree every 4 minutes)
  time_zone <- timezone(long_deg)[1]
  t_offset <- Etm + 4 * long_deg - 60 * time_zone
  return(c(Etm, t_offset))
}

# LOCAL STANDARD TIME in decimal hours
# Required input parameters: x1 = solar time in decimal hours, x2 = Time offset in minutes
lst <- function(x1, x2) {
  lst <- x1 - x2 / 60
  return(lst)
}

# SUNRISE AND SUNSET HOUR ANGLE in radians (see, Iqbal for limits on the cosine of the hour angle)
# Note: if cosine of the hour angle is > 1, then the sun does not rise (24 hours darkness, no daily solar insolation), and
# if the cosine of the hour angle is < - 1, then the sun does not set (24 hours of light).
# Required input parameters: x1 = latitude (radians), x2 = solar declination (radians)
sshourangle <- function(x1, x2) {
  # Cosine of sunrise hour angle
  cos_ws <- -tan(x1) * tan(x2)
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

# SOLAR POSITION: ZENITH AND AZIMUTH
# Required input parameters: x1 - solar declination (soldec, radians), x2 - latitude (radians), x3 - hour angle (radians)
solpos <- function(x1, x2, x3) {
  # Solar zenith in radians
  cos_sz <- sin(x1) * sin(x2) + cos(x1) * cos(x2) * cos(x3)
  # Guard against floating point trickery
  sz <- acos(min(max(cos_sz, -1.0), 1.0))
  # Solar elevation in radians
  se <- rad_90() - sz
  # Solar azimuth in radians
  cos_sa <- (sin(se) * sin(x2) - sin(x1)) / (cos(se) * cos(x2))
  sa <- acos(min(max(cos_sa, -1.0), 1.0))
  # Solar azimuth is dependent on sign of the hour angle
  # Hour angle is positive in morning, zero at noon, and negative in afternoon
  # Convert to compass direction (NORTH = 0, clockwise rotation)
  sa_rot <- ifelse(x3 > 0, pi - sa, pi + sa)
  # Convert solar azimuth to math CCW rotation (EAST = 0) for sun path plotting
  sa_rot_ccw <- azi2math(sa_rot)
  # Convert to (x, y) Cartesian coordinates for plotting solar positions
  x_sun <- sz * cos(sa_rot_ccw)
  y_sun <- sz * sin(sa_rot_ccw)
  # Return vector of output variables
  return(c(sz, se, sa, sa_rot, sa_rot_ccw, x_sun, y_sun))
}

# INSTANTANEOUS EXTRATERRESTRIAL SOLAR IRRADIANCE AND INSTANTANEOUS TERRESTRIAL BEAM INTENSITY WEIGHTINGS
# Required input parameters: x1 - eccentricity correction factor (Eo), x2 - solar zenith angle (radians),
# x3 - site elevation in m.a.s.l, x4 - clear-sky transmission coefficient (0.65), x5 - solar constant (W/m2)
solrad <- function(x1, x2, x3, x4, x5) {
  # Only compute SR when sun is visible
  if (x2 >= 0 & x2 <= rad_90()) {
    # Solar constant (W/m2) - user defined
    sc <- x5
    # Instantaneous extraterrestrial irradiance (W/m2) at time t
    Io <- sc * x1 * cos(x2)
    # Relative optical airmass (Kasten and Young, 1989)
    am <- 1 / (cos(x2) + 0.50572 * (96.07995 - x2 * rad_to_deg())^-1.6343)
    # Relative air density at elevation z (List, 1964)
    pzpo <- (1 - 2.2569 * 10^-5 * x3)^5.2553
    # Airmass corrected for elevation
    CorAM <- pzpo * am
    # Relative beam (direct) intensity at time t
    Rb <- x1 * x4^CorAM * cos(x2)
  } else {
    Io <- 0
    Rb <- 0
  }
  # Return vector of outputs
  return(c(Io, Rb))
}

# SKY REGION INDICES
# Required input parameters: x1 - solar elevation in radians (se), x2 - solar azimuth in radians (sa.rot), x3 - no. of sky elevation bins (nRings),
# x4 - no. of sky sectors (nSectors)
skyregidx <- function(x1, x2, x3, x4) {
  # Elevation rings
  elev_bin_idx <- ifelse(
    x1 < rad_90(),
    floor(x1 / rad_90() * x3) + 1,
    floor(x1 / rad_90() * x3)
  )
  # Azimuth sectors
  azi_bin_idx <- ifelse(
    x2 < two_pi(),
    floor(x2 / two_pi() * x4) + 1,
    floor(x2 / two_pi() * x4)
  )
  return(c(elev_bin_idx, azi_bin_idx))
}


#' Degree to radian conversion factor
#' @return pi / 180
deg_to_rad <- function() {
  pi / 180
}

#' Radian to degree conversion factor
#' @return 180 / pi
rad_to_deg <- function() {
  180 / pi
}

#' 90 degrees in radians (pi/2)
#' @return pi / 2
rad_90 <- function() {
  pi / 2
}

#' 450 degrees in radians (2.5 * pi)
#' @return 2 * pi * 450 / 360
rad_450 <- function() {
  2 * pi * 450 / 360
}

#' 2*pi (full circle in radians)
#' @return 2 * pi
two_pi <- function() {
  2 * pi
}
