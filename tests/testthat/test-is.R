test_that("is_img_binary returns TRUE for binary images", {
  # Create a binary image with only 0s and 1s
  binary_img <- imager::as.cimg(matrix(c(0, 1, 0, 1, 1, 0), nrow = 2, ncol = 3))

  expect_true(is_img_binary(binary_img))
})

test_that("is_img_binary returns FALSE for grayscale images", {
  # Create a grayscale image with values 0-255
  grayscale_img <- imager::as.cimg(matrix(
    c(0, 128, 255, 64, 192, 32),
    nrow = 2,
    ncol = 3
  ))

  expect_false(is_img_binary(grayscale_img))
})

test_that("is_img_binary returns FALSE for normalized grayscale (0-1 range)", {
  # Create a grayscale image normalized to 0-1
  normalized_img <- imager::as.cimg(matrix(
    c(0, 0.5, 1, 0.25, 0.75, 0.1),
    nrow = 2,
    ncol = 3
  ))

  expect_false(is_img_binary(normalized_img))
})

test_that("is_img_binary handles all 0s", {
  # Image with all black pixels
  all_zeros <- imager::as.cimg(matrix(0, nrow = 5, ncol = 5))

  expect_true(is_img_binary(all_zeros))
})

test_that("is_img_binary handles all 1s", {
  # Image with all white pixels
  all_ones <- imager::as.cimg(matrix(1, nrow = 5, ncol = 5))

  expect_true(is_img_binary(all_ones))
})

test_that("is_img_binary errors on non-cimg input", {
  expect_error(
    is_img_binary(matrix(c(0, 1), nrow = 2)),
    "img must be an imager cimg object"
  )
})

test_that("is_img_binary works with real thresholded test image", {
  test_photo <- test_path(
    "testdata",
    "CP38_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )

  # Load and threshold
  img <- imager::load.image(test_photo)
  if (imager::spectrum(img) > 1) {
    img <- imager::channel(img, 1)
  }
  binary_img <- imager::threshold(img) |>
    imager::as.cimg()

  # Should be binary
  expect_true(is_img_binary(binary_img))
})

test_that("compute_gap_fractions errors on non-binary image", {
  skip()
  # Create a temporary grayscale image file
  temp_img <- withr::local_tempfile(fileext = ".png")

  # Create and save a grayscale image
  grayscale_img <- imager::as.cimg(matrix(
    runif(100 * 100, 0, 255),
    nrow = 100,
    ncol = 100
  ))
  imager::save.image(grayscale_img, temp_img)

  # Should error when trying to process non-binary image
  expect_error(
    gla_extract_gap_fraction(temp_img, elev_res = 5, azi_res = 5),
    "Image must be binary"
  )
})
