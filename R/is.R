is_img_binary <- function(img) {
  if (!inherits(img, "cimg")) {
    stop("img must be an imager cimg object")
  }

  unique_vals <- unique(as.vector(img))
  all(unique_vals %in% c(0, 1))
}

# Validates a single LAS file path: must be non-NA, exist on disk, and non-empty.
is_valid_las_file <- function(path) {
  !is.na(path) && file.exists(path) && file.size(path) > 0
}

# Validates a single BMP file path by checking the two-byte magic number (0x42 0x4D).
# A corrupted or partially written BMP passes file.exists() but fails read.bitmap(),
# which crashes parallel workers with no recoverable error.
is_valid_bmp <- function(path) {
  if (is.na(path) || !file.exists(path) || file.size(path) < 2) {
    return(FALSE)
  }
  identical(readBin(path, raw(), n = 2L), as.raw(c(0x42, 0x4d)))
}
