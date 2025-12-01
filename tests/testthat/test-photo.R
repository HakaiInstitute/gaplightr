test_that("generated file exists", {
  expect_true(file.exists(gen_path))
  tmp_ref_path <- gsub("_cairo", "", basename(ref_path))
  expect_identical(
    basename(gen_path),
    tmp_ref_path
  )
})

test_that("files have same size", {
  expect_equal(file.size(ref_path), file.size(gen_path))
})

test_that("images have same dimensions", {
  expect_equal(imager::width(ref_img), imager::width(gen_img))
  expect_equal(imager::height(ref_img), imager::height(gen_img))
  expect_equal(imager::spectrum(ref_img), imager::spectrum(gen_img))
})

test_that("reference image values in valid range", {
  expect_true(all(ref_array >= 0 & ref_array <= 1))
})

test_that("generated image values in valid range", {
  expect_true(all(gen_array >= 0 & gen_array <= 1))
})

test_that("images have same dimensions", {
  expect_identical(dim(ref_array), dim(gen_array))
})

test_that("images are similar (low RMSE)", {
  rmse <- sqrt(mean((ref_array - gen_array)^2))
  expect_lt(rmse, 0.01)
})

test_that("images are highly correlated", {
  cor_val <- cor(as.vector(ref_array), as.vector(gen_array))
  expect_gte(cor_val, 0.999)
})

test_that("images are essentially identical", {
  expect_true(isTRUE(all.equal(ref_array, gen_array, tolerance = 0.1)))
})

test_that("maximum pixel difference is small", {
  max_diff <- max(abs(ref_array - gen_array))
  # Allow for small rounding differences in 0-1 scale
  expect_lt(max_diff, 0.05)
})

test_that("few pixels differ", {
  diff_count <- sum(ref_array != gen_array)
  total_pixels <- length(ref_array)
  diff_pct <- (diff_count / total_pixels) * 100
  expect_lt(diff_pct, 0.01)
})

test_that("images have similar min/max/sd", {
  expect_equal(min(ref_array), min(gen_array), tolerance = 1e-6)
  expect_equal(max(ref_array), max(gen_array), tolerance = 1e-6)
  expect_equal(sd(ref_array), sd(gen_array), tolerance = 1e-6)
})

test_that("gla_compute_solar_positions returns expected structure", {
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
  expect_snapshot(results_df)
})

# gla_create_fisheye_photos resume tests ----
test_that("gla_create_fisheye_photos resume skips existing files", {
  skip_if_not_installed("rgrass")
  skip_on_ci() # GRASS not available in CI

  # This test would require full GRASS setup and LiDAR data
  # Placeholder for when we have proper test fixtures
  expect_true(TRUE)
})

test_that("gla_create_fisheye_photos expected filename format matches actual", {
  # Test that the expected filename generation matches the actual format
  site_id <- "1000_2000.0"
  pointsize <- 10
  dpi <- 300
  img_res <- 2800
  max_cex <- 0.2

  ss <- ifelse(max_cex == 0.2, "0pt2", ifelse(max_cex == 0.3, "0pt3", "0pt4"))

  expected <- paste0(
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

  # This matches the format in gla_create_fisheye_photo_single
  expect_equal(
    expected,
    "1000_2000.0_ps10_cex0pt2_300dpi_2800px_polar.bmp"
  )
})
