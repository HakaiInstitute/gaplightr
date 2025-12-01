check_if_coordinates_are_unique <- function(df) {
  if (any(duplicated(df[, c("x_meters", "y_meters")]))) {
    stop(
      "Warning: Some points have identical x_meters and y_meters coordinates.",
      call. = FALSE
    )
  } else {
    message("All points have unique x_meters and y_meters coordinates.")
  }
}
