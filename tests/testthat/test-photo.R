test_that("gla_compute_solar_positions returns expected structure", {
  # Compute solar data locally for this test
  solar_data <- gla_compute_solar_positions(
    lat_deg = 50.1876,
    long_deg = -125.6827,
    elev = 238.44,
    clearsky_coef = 0.65,
    time_step_min = 2,
    day_start = 1,
    day_end = 365,
    day_res = 1,
    elev_res = 5,
    azi_res = 5,
    solar_constant = 1367
  )

  expect_snapshot({
    list(
      solar_mat_dim = dim(solar_data$solar_mat),
      solar_mat_colnames = colnames(solar_data$solar_mat),
      solar_mat_head = head(solar_data$solar_mat, 3),
      beam_array_dim = dim(solar_data$beam_array),
      day_mat_dim = dim(solar_data$day_mat),
      Total_rbi = solar_data$Total_rbi
    )
  })
})

test_that("gla_process_fisheye_photo_single output matches snapshot", {
  # Load reference image
  ref_path <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # Compute solar data locally
  solar_data <- gla_compute_solar_positions(
    lat_deg = 50.1876,
    long_deg = -125.6827,
    elev = 238.44,
    clearsky_coef = 0.65,
    time_step_min = 2,
    day_start = 1,
    day_end = 365,
    day_res = 1,
    elev_res = 5,
    azi_res = 5,
    solar_constant = 1367
  )

  # Process the photo
  results_df <- gla_process_fisheye_photo_single(
    solar_data = solar_data,
    img_file = ref_path,
    lat_deg = 50.1876,
    long_deg = -125.6827,
    Kt = 0.45,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0
  )

  expect_snapshot(results_df)
})

test_that("gla_create_fisheye_photos generates expected filename format", {
  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(
    crs = 3005,
    n_points = 1,
    output_path = stream_network_path
  )

  las_dir <- withr::local_tempdir()
  las_path <- file.path(las_dir, "minimal_plot_3005.las")
  create_test_las(crs = 3005, n_points = 100, output_path = las_path)

  output_dir_virtual_plots <- withr::local_tempdir()
  output_dir_fisheye <- withr::local_tempdir()

  # Load points
  stream_points <- gla_load_points(stream_network_path, dem_path)

  # Create virtual plots with larger radius to capture LAS points
  stream_points <- gla_create_virtual_plots(
    points = stream_points,
    folder = las_dir,
    output_dir = output_dir_virtual_plots,
    plot_radius = 50, # Match LAS point spread radius
    chunk_size = 0,
    resume = FALSE
  )

  # Verify we have valid LAS files
  expect_true("las_files" %in% names(stream_points))
  expect_true(!is.na(stream_points$las_files[1]))
  expect_true(file.exists(stream_points$las_files[1]))

  # Extract horizons
  stream_points <- gla_extract_horizons(
    points = stream_points,
    dem_path = dem_path,
    output_dir = withr::local_tempdir(),
    step = 30,
    max_search_distance = 1000,
    verbose = FALSE
  )

  # Create fisheye photo with specific parameters
  pointsize <- 10
  dpi <- 300
  img_res <- 2800
  max_cex <- 0.2

  stream_points <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = output_dir_fisheye,
    camera_height_m = 1.37,
    min_dist = 1,
    max_dist = 50,
    img_res = img_res,
    max_cex = max_cex,
    min_cex = 0.05,
    pointsize = pointsize,
    dpi = dpi,
    parallel = FALSE,
    resume = FALSE
  )

  # Verify file was created
  expect_true(file.exists(stream_points$fisheye_photo_path[1]))

  # horizon_mask must be preserved in the returned object - it is temporarily
  # dropped before parallel export to reduce worker payload, then restored.
  expect_true("horizon_mask" %in% names(stream_points))
  expect_false(is.null(stream_points$horizon_mask[[1]]))

  # Get the actual filename
  actual_filename <- basename(stream_points$fisheye_photo_path[1])

  # Build expected filename format
  ss <- ifelse(max_cex == 0.2, "0pt2", ifelse(max_cex == 0.3, "0pt3", "0pt4"))
  expected_filename <- sprintf(
    "%d_ps%s_cex%s_%sdpi_%spx_polar.bmp",
    stream_points$point_id[1],
    pointsize,
    ss,
    dpi,
    img_res
  )

  # Verify filename matches expected format
  expect_equal(actual_filename, expected_filename)
})


test_that("gla_process_fisheye_photos adds solar radiation columns", {
  # Reuse existing test photo from testdata
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # Create minimal test points
  test_points <- create_test_photo_points(fisheye_photo_path = test_photo)

  # Run function (non-parallel, short date range for speed)
  result <- gla_process_fisheye_photos(
    points = test_points,
    clearsky_coef = 0.65,
    time_step_min = 2,
    day_start = 1,
    day_end = 3, # Only 3 days for fast test
    day_res = 1,
    elev_res = 5,
    azi_res = 5,
    Kt = 0.45,
    parallel = FALSE
  )

  # Verify structure
  expect_s3_class(result, "sf")

  # Check for expected solar radiation columns
  expected_cols <- c(
    "fisheye_photo_path",
    "canopy_openness_pct",
    "mean_daily_extraterrestrial_irradiance_Wm2",
    "mean_daily_direct_irradiation_MJm2d",
    "mean_daily_diffuse_irradiation_MJm2d",
    "mean_daily_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_MJm2d",
    "transmitted_diffuse_irradiation_MJm2d",
    "transmitted_global_irradiation_MJm2d",
    "transmitted_direct_irradiation_pct",
    "transmitted_diffuse_irradiation_pct",
    "transmitted_global_irradiation_pct",
    "subcanopy_solar_radiation_MJm2d",
    "light_penetration_index"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }

  # Verify values are numeric
  expect_true(is.numeric(result$canopy_openness_pct))
  expect_true(is.numeric(result$transmitted_global_irradiation_MJm2d))
  expect_true(is.numeric(result$light_penetration_index))
})

test_that("gla_process_fisheye_photos snapshot", {
  skip_on_cran()

  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  test_points <- create_test_photo_points(fisheye_photo_path = test_photo)

  result <- gla_process_fisheye_photos(
    points = test_points,
    clearsky_coef = 0.65,
    time_step_min = 2,
    day_start = 1,
    day_end = 2,
    day_res = 1,
    elev_res = 5,
    azi_res = 5,
    Kt = 0.45,
    parallel = FALSE
  )

  # Snapshot structure
  expect_snapshot(as.data.frame(sf::st_drop_geometry(result)))
})

test_that("gla_extract_gap_fraction returns correct structure", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  result <- gla_extract_gap_fraction(
    img_file = test_photo,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0
  )

  # Check structure
  expect_type(result, "list")
  expect_named(
    result,
    c("gap_fraction", "total_pixels", "gap_pixels", "nRings", "nSectors")
  )

  # Check dimensions
  expect_equal(result$nRings, 18) # 90 / 5
  expect_equal(result$nSectors, 72) # 360 / 5

  # Check matrices have correct dimensions
  expect_equal(dim(result$gap_fraction), c(18, 72))
  expect_equal(dim(result$total_pixels), c(18, 72))
  expect_equal(dim(result$gap_pixels), c(18, 72))

  # Check values are in valid range
  expect_true(all(
    result$gap_fraction >= 0 & result$gap_fraction <= 1,
    na.rm = TRUE
  ))
  expect_true(all(result$total_pixels >= 0))
  expect_true(all(result$gap_pixels >= 0))
  expect_true(all(result$gap_pixels <= result$total_pixels))
})

test_that("gla_process_fisheye_photos with keep_gap_fraction_data = TRUE", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  test_points <- create_test_photo_points(fisheye_photo_path = test_photo)

  result <- gla_process_fisheye_photos(
    points = test_points,
    day_start = 1,
    day_end = 3,
    elev_res = 5,
    azi_res = 5,
    parallel = FALSE,
    keep_gap_fraction_data = TRUE
  )

  # Check for gap fraction columns
  expect_true("gap_fraction" %in% names(result))
  expect_true("total_pixels" %in% names(result))
  expect_true("gap_pixels" %in% names(result))

  # Check they are list-columns
  expect_true(is.list(result$gap_fraction))
  expect_true(is.list(result$total_pixels))
  expect_true(is.list(result$gap_pixels))

  # Check matrix dimensions
  expect_equal(dim(result$gap_fraction[[1]]), c(18, 72))
  expect_equal(dim(result$total_pixels[[1]]), c(18, 72))
  expect_equal(dim(result$gap_pixels[[1]]), c(18, 72))
})

test_that("gla_process_fisheye_photos with keep_gap_fraction_data = FALSE", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  test_points <- create_test_photo_points(fisheye_photo_path = test_photo)

  result <- gla_process_fisheye_photos(
    points = test_points,
    day_start = 1,
    day_end = 3,
    elev_res = 5,
    azi_res = 5,
    parallel = FALSE,
    keep_gap_fraction_data = FALSE
  )

  # Should NOT have gap fraction columns
  expect_false("gap_fraction" %in% names(result))
  expect_false("total_pixels" %in% names(result))
  expect_false("gap_pixels" %in% names(result))
})

test_that("gap fraction values are consistent", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # Extract gap fraction directly
  direct_result <- gla_extract_gap_fraction(
    img_file = test_photo,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0
  )

  # Extract via process function
  test_points <- create_test_photo_points(fisheye_photo_path = test_photo)

  process_result <- gla_process_fisheye_photos(
    points = test_points,
    day_start = 1,
    day_end = 3,
    elev_res = 5,
    azi_res = 5,
    parallel = FALSE,
    keep_gap_fraction_data = TRUE
  )

  # Gap fractions should be identical
  expect_equal(direct_result$gap_fraction, process_result$gap_fraction[[1]])
  expect_equal(direct_result$total_pixels, process_result$total_pixels[[1]])
  expect_equal(direct_result$gap_pixels, process_result$gap_pixels[[1]])
})

# Radial calibration tests ----

test_that("gla_lens_sigma_8mm returns correct structure", {
  sigma_cal <- gla_lens_sigma_8mm()

  expect_type(sigma_cal, "list")
  expect_named(sigma_cal, c("radius", "elevation"))

  # Check dimensions - 24 calibration points
  expect_equal(length(sigma_cal$radius), 24)
  expect_equal(length(sigma_cal$elevation), 24)

  # Check range
  expect_equal(sigma_cal$radius[1], 0)
  expect_equal(sigma_cal$radius[24], 1)

  # Check elevation is in radians and decreases from zenith to horizon
  expect_true(all(sigma_cal$elevation >= 0))
  expect_true(all(sigma_cal$elevation <= pi / 2))
  expect_true(sigma_cal$elevation[1] > sigma_cal$elevation[24])
})

test_that("gla_extract_gap_fraction works with custom radial calibration", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # Extract with custom calibration
  result_custom <- gla_extract_gap_fraction(
    img_file = test_photo,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0,
    radial_distortion = gla_lens_sigma_8mm()
  )

  # Extract with default polar
  result_polar <- gla_extract_gap_fraction(
    img_file = test_photo,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0
  )

  # Both should return valid structures
  expect_type(result_custom, "list")
  expect_type(result_polar, "list")

  # Both should have same dimensions
  expect_equal(dim(result_custom$gap_fraction), dim(result_polar$gap_fraction))

  # Results should be highly correlated but not identical
  cor_value <- cor(
    as.vector(result_custom$gap_fraction),
    as.vector(result_polar$gap_fraction),
    use = "complete.obs"
  )
  expect_gt(cor_value, 0.95) # Very similar but not exactly 1.0
})

test_that("gla_extract_gap_fraction with equidistant uses polar projection", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # These should be identical (explicit vs implicit default)
  result1 <- gla_extract_gap_fraction(
    img_file = test_photo,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0,
    radial_distortion = "equidistant"
  )

  result2 <- gla_extract_gap_fraction(
    img_file = test_photo,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0
  )

  expect_equal(result1$gap_fraction, result2$gap_fraction)
})

test_that("gla_process_fisheye_photos works with radial_distortion", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  test_points <- create_test_photo_points(fisheye_photo_path = test_photo)

  # Process with Sigma lens calibration
  result <- gla_process_fisheye_photos(
    points = test_points,
    day_start = 1,
    day_end = 3,
    elev_res = 5,
    azi_res = 5,
    parallel = FALSE,
    radial_distortion = gla_lens_sigma_8mm()
  )

  # Should succeed and return valid results
  expect_s3_class(result, "sf")
  expect_true("canopy_openness_pct" %in% names(result))
  expect_true(is.numeric(result$canopy_openness_pct))
  expect_true(
    result$canopy_openness_pct > 0 && result$canopy_openness_pct < 100
  )
})

# Polar day/night edge case tests ----

test_that("gla_compute_solar_positions handles polar night correctly", {
  # Arctic winter: 80°N latitude, December 21 (day 356)
  # At this latitude/date, cos_ws > 1 so sun never rises (24h darkness)
  solar_data <- gla_compute_solar_positions(
    lat_deg = 80, # Arctic latitude
    long_deg = -125.6827,
    elev = 238.44,
    clearsky_coef = 0.65,
    time_step_min = 60, # Hourly for simplicity
    day_start = 356, # Dec 21 (winter solstice)
    day_end = 356, # Just one day
    day_res = 1,
    elev_res = 5,
    azi_res = 5,
    solar_constant = 1367
  )

  # Expected behavior for polar night:
  # - day_mat should record: day number, day_length=0, Ho_Wm2=0, Ho_MJm2=0
  # - solar_mat should have NO rows (k never increments, no solar positions)

  # Check day_mat structure
  expect_equal(nrow(solar_data$day_mat), 1)
  expect_equal(ncol(solar_data$day_mat), 7)

  # Check day_mat values
  expect_equal(solar_data$day_mat[1, 1], 356) # Day number
  expect_equal(solar_data$day_mat[1, 2], 0) # Day length = 0
  expect_true(is.na(solar_data$day_mat[1, 3])) # Sunrise = NA
  expect_true(is.na(solar_data$day_mat[1, 4])) # Sunset = NA
  expect_equal(solar_data$day_mat[1, 5], 0) # numSolarPos = 0
  expect_equal(solar_data$day_mat[1, 6], 0) # Ho_Wm2 = 0
  expect_equal(solar_data$day_mat[1, 7], 0) # Ho_MJm2 = 0

  # Check solar_mat - should be empty matrix (no solar positions calculated)
  # Should maintain matrix structure with column names even when empty
  expect_true(is.matrix(solar_data$solar_mat))
  expect_equal(nrow(solar_data$solar_mat), 0)
  expect_equal(ncol(solar_data$solar_mat), 11)

  # Verify column names are preserved
  expected_colnames <- c(
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
  expect_equal(colnames(solar_data$solar_mat), expected_colnames)
})

test_that("gla_compute_solar_positions handles polar day (midnight sun) correctly", {
  # Arctic summer: 80°N latitude, June 21 (day 172)
  # At this latitude/date, cos_ws < -1 so sun never sets (24h daylight)
  solar_data <- gla_compute_solar_positions(
    lat_deg = 80, # Arctic latitude
    long_deg = -125.6827,
    elev = 238.44,
    clearsky_coef = 0.65,
    time_step_min = 60, # Hourly for simplicity
    day_start = 172, # June 21 (summer solstice)
    day_end = 172, # Just one day
    day_res = 1,
    elev_res = 5,
    azi_res = 5,
    solar_constant = 1367
  )

  # Expected behavior for polar day: code proceeds normally
  # - day_mat should have day_length = 24 hours
  # - Ho_Wm2 and Ho_MJm2 should be > 0 (we have sunlight!)
  # - solar_mat should have 24 rows (hourly time steps from 0hr to 23hr)

  expect_equal(solar_data$day_mat[1, 1], 172) # Day number
  expect_equal(solar_data$day_mat[1, 2], 24) # Day length = 24 hours
  expect_gt(solar_data$day_mat[1, 6], 0) # Ho_Wm2 > 0
  expect_gt(solar_data$day_mat[1, 7], 0) # Ho_MJm2 > 0

  # Should have exactly 24 samples (0hr through 23hr, excluding 24hr to avoid duplicate)
  expect_equal(nrow(solar_data$solar_mat), 24)

  # Verify no duplicate at 0hr/24hr wraparound
  # All SOLAR_TIME_HR values should be unique
  solar_times <- solar_data$solar_mat[, "SOLAR_TIME_HR"]
  expect_equal(length(unique(solar_times)), length(solar_times))

  # Solar times should range from 0 to <24
  expect_gte(min(solar_times), 0)
  expect_lt(max(solar_times), 24)
})

test_that("gla_compute_solar_positions handles mixed polar day, normal, and polar night", {
  # Arctic: 80°N latitude
  # Test three specific days that span all conditions:
  # - Day 172 (Jun 21): Polar day (midnight sun)
  # - Day 264 (Sep 21): Normal equinox conditions
  # - Day 356 (Dec 21): Polar night (no sun)
  solar_data <- gla_compute_solar_positions(
    lat_deg = 80, # Arctic latitude
    long_deg = -125.6827,
    elev = 238.44,
    clearsky_coef = 0.65,
    time_step_min = 720, # 12-hour steps for speed
    day_start = 172, # June 21 (summer solstice)
    day_end = 356, # Dec 21 (winter solstice)
    day_res = 92, # Sample 3 days
    elev_res = 5,
    azi_res = 5,
    solar_constant = 1367
  )

  # day_mat should have ALL 3 days recorded
  expect_equal(nrow(solar_data$day_mat), 3)

  # Check that we have the expected day numbers
  expect_equal(solar_data$day_mat[1, 1], 172) # Jun 21 (polar day)
  expect_equal(solar_data$day_mat[2, 1], 264) # Sep 21 (equinox)
  expect_equal(solar_data$day_mat[3, 1], 356) # Dec 21 (polar night)

  # Day 1 (Jun 21): polar day - should have 24h daylight
  expect_equal(solar_data$day_mat[1, 2], 24) # Day length = 24
  expect_gt(solar_data$day_mat[1, 6], 0) # Ho_Wm2 > 0

  # Day 2 (Sep 21): equinox - should have normal daylight (~12h)
  expect_gt(solar_data$day_mat[2, 2], 0) # Day length > 0
  expect_lt(solar_data$day_mat[2, 2], 24) # Day length < 24
  expect_gt(solar_data$day_mat[2, 6], 0) # Ho_Wm2 > 0

  # Day 3 (Dec 21): polar night - should have 0h daylight
  expect_equal(solar_data$day_mat[3, 2], 0) # Day length = 0
  expect_equal(solar_data$day_mat[3, 6], 0) # Ho_Wm2 = 0

  # solar_mat should have data (matrix structure even if some days contribute 0 rows)
  expect_true(is.matrix(solar_data$solar_mat))
  expect_equal(ncol(solar_data$solar_mat), 11)

  # solar_mat should have rows for days with sun
  expect_gt(nrow(solar_data$solar_mat), 0) # Should have some data

  # Day 172 (Jun 21) and Day 264 (Sep 21) should be in solar_mat
  expect_true(172 %in% solar_data$solar_mat[, "DAY_NUM"])
  expect_true(264 %in% solar_data$solar_mat[, "DAY_NUM"])

  # Day 356 (Dec 21) should NOT be in solar_mat (polar night)
  expect_false(356 %in% solar_data$solar_mat[, "DAY_NUM"])
})

# Radial distortion for synthetic photo creation ----

test_that("gla_create_fisheye_photos works with radial_distortion parameter", {
  # Create test fixtures on-demand
  dem_path <- withr::local_tempfile(fileext = ".tif")
  create_test_dem(crs = 3005, output_path = dem_path)

  stream_network_path <- withr::local_tempfile(fileext = ".gpkg")
  create_test_points(
    crs = 3005,
    n_points = 1,
    output_path = stream_network_path
  )

  las_dir <- withr::local_tempdir()
  las_path <- file.path(las_dir, "minimal_plot_3005.las")
  create_test_las(crs = 3005, n_points = 100, output_path = las_path)

  output_dir_virtual_plots <- withr::local_tempdir()
  output_dir_fisheye_equi <- withr::local_tempdir()
  output_dir_fisheye_sigma <- withr::local_tempdir()

  # Load points
  stream_points <- gla_load_points(stream_network_path, dem_path)

  # Create virtual plots
  stream_points <- gla_create_virtual_plots(
    points = stream_points,
    folder = las_dir,
    output_dir = output_dir_virtual_plots,
    plot_radius = 50,
    chunk_size = 0,
    resume = FALSE
  )

  # Extract horizons (always equidistant, distortion applied at photo creation)
  stream_points <- gla_extract_horizons(
    points = stream_points,
    dem_path = dem_path,
    output_dir = withr::local_tempdir(),
    step = 30,
    max_search_distance = 1000,
    parallel = FALSE,
    verbose = FALSE
  )

  # Create fisheye photo with equidistant projection
  result_equi <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = output_dir_fisheye_equi,
    img_res = 1000,
    parallel = FALSE,
    resume = FALSE,
    radial_distortion = "equidistant"
  )

  # Create fisheye photo with Sigma 8mm distortion
  result_sigma <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = output_dir_fisheye_sigma,
    img_res = 1000,
    parallel = FALSE,
    resume = FALSE,
    radial_distortion = gla_lens_sigma_8mm()
  )

  # Both should succeed and create files
  expect_true(file.exists(result_equi$fisheye_photo_path[1]))
  expect_true(file.exists(result_sigma$fisheye_photo_path[1]))

  # With minimal test data (100 points), the images may be very similar
  # The prepare_horizon_mask test verifies distortion is applied correctly
})

test_that("prepare_horizon_mask applies radial distortion correctly", {
  # Create test horizon data
  test_horizon <- data.frame(
    azimuth = seq(0, 355, by = 5),
    horizon_height = rep(10, 72) # 10 degrees elevation
  )

  # Process with equidistant projection
  result_equi <- prepare_horizon_mask(
    test_horizon,
    radial_distortion = "equidistant"
  )

  # Process with Sigma 8mm distortion
  result_sigma <- prepare_horizon_mask(
    test_horizon,
    radial_distortion = gla_lens_sigma_8mm()
  )

  # Both should have x_msk and y_msk columns
  expect_true("x_msk" %in% names(result_equi))
  expect_true("y_msk" %in% names(result_equi))
  expect_true("x_msk" %in% names(result_sigma))
  expect_true("y_msk" %in% names(result_sigma))

  # Coordinates should be different due to distortion
  expect_false(identical(result_equi$x_msk, result_sigma$x_msk))
  expect_false(identical(result_equi$y_msk, result_sigma$y_msk))

  # Sigma 8mm has barrel distortion: at low elevations (near horizon),
  # it produces larger radii than equidistant projection
  radius_equi <- sqrt(result_equi$x_msk^2 + result_equi$y_msk^2)[1]
  radius_sigma <- sqrt(result_sigma$x_msk^2 + result_sigma$y_msk^2)[1]
  expect_gt(radius_sigma, radius_equi)
})

test_that("validate_radial_distortion catches invalid input", {
  # Valid "equidistant" string
  expect_true(validate_radial_distortion("equidistant"))

  # Invalid string (not "equidistant")
  expect_error(
    validate_radial_distortion("not_equidistant"),
    "must be 'equidistant'"
  )

  # Not a string or list
  expect_error(
    validate_radial_distortion(123),
    "must be 'equidistant' or a calibration list"
  )

  # Missing components
  expect_error(
    validate_radial_distortion(list(radius = c(0, 1))),
    "must have 'radius' and 'elevation' components"
  )

  # Radius not normalized (too large)
  expect_error(
    validate_radial_distortion(list(
      radius = c(0, 5, 10),
      elevation = c(0, 0.5, 1)
    )),
    "must be normalized to 0-1 range"
  )

  # Radius not normalized (negative)
  expect_error(
    validate_radial_distortion(list(
      radius = c(-0.1, 0.5, 1),
      elevation = c(0, 0.5, 1)
    )),
    "must be normalized to 0-1 range"
  )

  # Different lengths
  expect_error(
    validate_radial_distortion(list(
      radius = c(0, 0.5, 1),
      elevation = c(0, 1)
    )),
    "must have same length"
  )

  # Valid calibration list
  expect_true(
    validate_radial_distortion(list(
      radius = c(0, 0.5, 1),
      elevation = c(0, 0.785, 1.57)
    ))
  )
})

test_that("apply_radial_distortion_mapping works bidirectionally", {
  # Simple linear calibration for testing
  # Note: approx() requires 'from' values to be in increasing order
  cal <- list(
    elevation = c(0, 0.785, 1.57), # 0°, 45°, 90° (increasing)
    radius = c(0.0, 0.6, 1.0) # Normalized 0-1 (increasing from center to edge)
  )

  # Forward: elevation -> radius
  result_fwd <- apply_radial_distortion_mapping(
    0.785, # 45° input
    from = cal$elevation,
    to = cal$radius
  )
  expect_equal(result_fwd, 0.6)

  # Reverse: radius -> elevation
  result_rev <- apply_radial_distortion_mapping(
    0.6, # Normalized radius
    from = cal$radius,
    to = cal$elevation
  )
  expect_equal(result_rev, 0.785)

  # Test interpolation between points (forward direction)
  result_interp <- apply_radial_distortion_mapping(
    0.3925, # 22.5° (halfway between 0 and 45)
    from = cal$elevation,
    to = cal$radius
  )
  expect_equal(result_interp, 0.3, tolerance = 0.01) # Should be ~0.3

  # Test rule=2 boundary handling (forward)
  result_below <- apply_radial_distortion_mapping(
    -0.1, # Below range
    from = cal$elevation,
    to = cal$radius
  )
  expect_equal(result_below, 0.0) # Should use first value

  result_above <- apply_radial_distortion_mapping(
    2.0, # Above range
    from = cal$elevation,
    to = cal$radius
  )
  expect_equal(result_above, 1.0) # Should use last value
})
