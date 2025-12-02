# library(imager, warn.conflicts = FALSE)
library(testthat)
library(withr)


# Load reference image for comparison tests
ref_path <- test_path(
  "testdata",
  "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
)

# Compute solar positions once for reuse
solar_data <- gla_compute_solar_positions(
  lat_deg = 50.1876,
  long_deg = -125.6827,
  elev = 238.44, # Elevation above mean sea level (m)
  clearsky_coef = 0.65, # Clear-sky transmission coefficient
  time_step_min = 2, # SOLAR TIME STEP for computing solar position and instantaneous SR estimates (minutes)
  day_start = 1, # Start day (August 1 = 213). See https://nsidc.org/data/user-resources/help-center/day-year-doy-calendar
  day_end = 365, # End day (August 31 = 243)
  day_res = 1, # Day time step (e.g., every day = 1, every other day = 2, every third day = 3, etc.)
  elev_res = 5, # Elevation bin width in degrees
  azi_res = 5 # Azimuth bin width in degrees
)

# Process the photo once for reuse
results_df <- gla_process_fisheye_photo_single(
  solar_data = solar_data,
  img_file = ref_path,
  lat_deg = 50.1876,
  long_deg = -125.6827,
  Kt = 0.45, # Mean cloudiness index (Kt = H/Ho) for the period of interest
  elev_res = 5, # Elevation bin width in degrees
  azi_res = 5, # Azimuth bin width in degrees
  rotation_deg = 0
)
