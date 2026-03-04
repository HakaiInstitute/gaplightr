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

test_that("is_valid_las_file returns FALSE for NA, missing, and empty files", {
  expect_false(is_valid_las_file(NA_character_))
  expect_false(is_valid_las_file("nonexistent.las"))

  empty_las <- withr::local_tempfile(fileext = ".las")
  file.create(empty_las)
  expect_false(is_valid_las_file(empty_las))
})

test_that("is_valid_las_file returns TRUE for a non-empty file", {
  valid_las <- withr::local_tempfile(fileext = ".las")
  writeBin(as.raw(1:10), valid_las)
  expect_true(is_valid_las_file(valid_las))
})

test_that("is_valid_bmp returns FALSE for NA, missing, and files too small for magic number", {
  expect_false(is_valid_bmp(NA_character_))
  expect_false(is_valid_bmp("nonexistent.bmp"))

  one_byte <- withr::local_tempfile(fileext = ".bmp")
  writeBin(as.raw(0x42), one_byte)
  expect_false(is_valid_bmp(one_byte))
})

test_that("is_valid_bmp returns FALSE for wrong magic number", {
  not_bmp <- withr::local_tempfile(fileext = ".bmp")
  writeBin(as.raw(c(0x89, 0x50)), not_bmp) # PNG magic
  expect_false(is_valid_bmp(not_bmp))
})

test_that("is_valid_bmp returns TRUE for correct BMP magic number", {
  valid_bmp <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )
  expect_true(is_valid_bmp(valid_bmp))
})

test_that("is_valid_bmp works with a real BMP file", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
  )
  expect_true(is_valid_bmp(test_photo))
})

test_that("is_img_binary works with real thresholded test image", {
  test_photo <- test_path(
    "testdata",
    "R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp"
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
