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
    azi_res = 5
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

test_that("gla_process_fisheye_photo output matches snapshot", {
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
    azi_res = 5
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
  create_test_points(crs = 3005, n_points = 1, output_path = stream_network_path)

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
    cam_ht = 1.37,
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

  # Get the actual filename
  actual_filename <- basename(stream_points$fisheye_photo_path[1])

  # Build expected filename format
  site_id <- paste0(
    stream_points$x_meters[1],
    "_",
    stream_points$y_meters[1]
  )
  ss <- ifelse(max_cex == 0.2, "0pt2", ifelse(max_cex == 0.3, "0pt3", "0pt4"))

  expected_filename <- paste0(
    site_id,
    "_ps",
    pointsize,
    "_cex",
    ss,
    "_",
    dpi,
    "dpi",
    "_",
    img_res,
    "px_polar.bmp"
  )

  # Verify filename matches expected format
  expect_equal(actual_filename, expected_filename)
})
