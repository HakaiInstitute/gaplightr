# library(imager, warn.conflicts = FALSE)
library(testthat)
library(withr)

# LOAD TOPO MASK
# dat <- read.csv(
#   test_path("testdata", "R2D2_horizon_ccw_rgrass.csv"),
#   colClasses = c("numeric", "numeric", "numeric", "numeric")
# )

# # LOAD AND PROCESS LiDAR POINTS
# csv_files <- list.files(
#   test_path("testdata", "R2D2_hemi"),
#   full.names = TRUE,
#   pattern = "\\.csv$"
# )
# df <- do.call(
#   rbind,
#   lapply(csv_files, function(f) {
#     read.csv(
#       f,
#       colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric")
#     )
#   })
# )

# gen_path <- gla_create_fisheye_photo_single(
#   processed_lidar = df,
#   x_msk = dat$x_msk,
#   y_msk = dat$y_msk,
#   site_id = "R2D2",
#   img_path = testthat::test_path(),
#   max_cex = 0.3, # Symbol size for plotting
#   min_cex = 0.05, # Symbol size for plotting
#   min_dist = 1,
#   max_dist = 220,
#   width = 2800, # Graphical and bitmap image parameters
#   pointsize = 10, # Graphical and bitmap image parameters
#   res = 600, # Graphical and bitmap image parameters
#   height = 2800, # Graphical and bitmap image parameters
#   units = "px"
# )

# defer(unlink(gen_path), teardown_env())

# Load reference image for comparison tests
ref_path <- test_path(
  "testdata",
  "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
)
# ref_img <- imager::load.image(ref_path)
# gen_img <- imager::load.image(gen_path)

# Convert to arrays for numerical comparison
# imager uses 0-1 scale (normalized pixel values)
# ref_array <- as.vector(ref_img)
# gen_array <- as.vector(gen_img)

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
