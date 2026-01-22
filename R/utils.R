# Utility functions for the 50-watersheds-analysis package

#' Validate CRS match between two spatial objects
#'
#' Strict CRS validation to prevent spatial errors. Checks that both objects
#' have defined CRS and that they match exactly.
#'
#' @param obj1_crs CRS of first object (from sf::st_crs())
#' @param obj2_crs CRS of second object (from sf::st_crs())
#' @param obj1_name Name of first object for error message
#' @param obj2_name Name of second object for error message
#'
#' @return NULL (invisibly) if validation passes, otherwise stops with error
validate_crs_match <- function(obj1_crs, obj2_crs, obj1_name, obj2_name) {
  # Check if either CRS is NA/undefined
  if (is.na(obj1_crs)) {
    stop(
      obj1_name,
      " has no CRS defined.\n",
      "Please assign a CRS to your ",
      tolower(obj1_name),
      " before using this function.",
      call. = FALSE
    )
  }

  if (is.na(obj2_crs)) {
    stop(
      obj2_name,
      " has no CRS defined.\n",
      "Please assign a CRS to your ",
      tolower(obj2_name),
      " before using this function.",
      call. = FALSE
    )
  }

  # Check if CRS match
  if (!obj1_crs == obj2_crs) {
    stop(
      "CRS mismatch between ",
      obj1_name,
      " and ",
      obj2_name,
      ".\n",
      "  ",
      obj1_name,
      " CRS: ",
      obj1_crs$input,
      "\n",
      "  ",
      obj2_name,
      " CRS: ",
      obj2_crs$input,
      "\n",
      "Please reproject your data so they match before calling this function.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

## Internal helper to parse LAS filenames in format: X_Y.las
## Returns data frame with x_meters, y_meters, and filename columns
parse_las_filenames <- function(las_files) {
  file_info <- data.frame(do.call(
    rbind,
    strsplit(tools::file_path_sans_ext(basename(las_files)), "_")
  ))
  colnames(file_info) <- c("x_meters", "y_meters")
  file_info$las_files <- las_files
  file_info$x_meters <- as.numeric(file_info$x_meters)
  file_info$y_meters <- as.numeric(file_info$y_meters)

  file_info
}

add_las_filename <- function(stream_points, las_files) {
  # Check if las_files column already exists
  if ("las_files" %in% names(stream_points)) {
    # Remove existing column to avoid duplication
    stream_points <- stream_points[,
      !names(stream_points) %in% c("las_files")
    ]
  }
  file_info <- parse_las_filenames(las_files)

  stream_points <- merge(
    stream_points,
    file_info,
    by = c("x_meters", "y_meters")
  )

  class(stream_points) <- c("sf", "tbl_df", "tbl", "data.frame")
  stream_points
}

write_points_gpkg <- function(points, output_dir, prefix = "stream_points") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(output_dir, paste0(prefix, "_", timestamp, ".gpkg"))

  tryCatch(
    {
      sf::write_sf(points, output_file)
      message("Results saved to ", output_file)
    },
    error = function(e) {
      message("Failed to write points: ", e$message)
    }
  )
}


move_geom_col_to_end <- function(sf_object) {
  geom_col <- attr(sf_object, "sf_column")
  other_cols <- setdiff(names(sf_object), geom_col)
  sf_object[, c(other_cols, geom_col)]
}

validate_required_columns <- function(
  data,
  required_cols,
  data_name = "points",
  hint = NULL
) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    msg <- paste0(
      data_name,
      " must contain columns: ",
      paste(missing_cols, collapse = ", ")
    )
    if (!is.null(hint)) {
      msg <- paste0(msg, ". ", hint)
    }
    stop(msg, call. = FALSE)
  }
  invisible(NULL)
}

validate_sf_object <- function(x, arg_name = "points") {
  if (!inherits(x, "sf")) {
    stop("'", arg_name, "' must be an sf object", call. = FALSE)
  }
  invisible(NULL)
}

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
