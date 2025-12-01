is_img_binary <- function(img) {
  if (!inherits(img, "cimg")) {
    stop("img must be an imager cimg object")
  }

  unique_vals <- unique(as.vector(img))
  all(unique_vals %in% c(0, 1))
}
