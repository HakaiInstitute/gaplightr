# Clean up generated test fixtures
# This runs after all tests complete

if (exists(".fixture_paths")) {
  # Clean up files
  for (path in .fixture_paths) {
    if (file.exists(path)) {
      unlink(path)
    }
  }

  # Clean up directories
  las_dir_3005 <- test_path("testdata", "3005")
  las_dir_26912 <- test_path("testdata", "26912")
  if (dir.exists(las_dir_3005)) {
    unlink(las_dir_3005, recursive = TRUE)
  }
  if (dir.exists(las_dir_26912)) {
    unlink(las_dir_26912, recursive = TRUE)
  }
}
