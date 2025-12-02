# gla_process_fisheye_photos tests ----

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


# Gap fraction tests ----

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

test_that("gla_extract_gap_fraction with NULL radial_distortion uses polar", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # These should be identical
  result1 <- gla_extract_gap_fraction(
    img_file = test_photo,
    elev_res = 5,
    azi_res = 5,
    rotation_deg = 0,
    radial_distortion = NULL
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
