# Performance benchmark tests
# These tests are skipped in CI and require manual execution with real data
# To run: testthat::test_file("tests/testthat/test-performance-benchmark.R")

test_that("Memory usage benchmark for terra workflow (MANUAL TEST)", {
  skip_on_ci()
  skip_if_not(interactive(), "Benchmark test - run manually")

  # This test requires real data paths - update these paths for your system
  # Example paths (will be skipped if not found):
  dem_path <- Sys.getenv("TEST_DEM_PATH", "/path/to/real/dem.tif")
  stream_network_path <- Sys.getenv(
    "TEST_STREAM_NETWORK_PATH",
    "/path/to/real/stream_network.gpkg"
  )
  raw_lidar_dir <- Sys.getenv("TEST_LIDAR_DIR", "/path/to/real/lidar")

  skip_if_not(
    file.exists(dem_path),
    "Real DEM file not found - set TEST_DEM_PATH env var"
  )
  skip_if_not(
    file.exists(stream_network_path),
    "Stream network not found - set TEST_STREAM_NETWORK_PATH env var"
  )
  skip_if_not(
    dir.exists(raw_lidar_dir),
    "LiDAR directory not found - set TEST_LIDAR_DIR env var"
  )

  output_dir_virtual_plots <- withr::local_tempdir()
  output_dir_fisheye <- withr::local_tempdir()

  cat("\n=== Performance Benchmark: Terra Workflow ===\n")
  cat("Test with first 10 stream points\n\n")

  # Load stream network (limit to 10 points for benchmark)
  stream_points <- gla_load_points(
    stream_network_path,
    dem_path
  )
  stream_points <- stream_points[1:min(10, nrow(stream_points)), ]

  cat("Testing with", nrow(stream_points), "points\n\n")

  # Benchmark: Virtual plots creation
  cat("1. Creating virtual plots...\n")
  gc_before <- gc()
  time_start <- Sys.time()

  stream_points <- gla_create_virtual_plots(
    points = stream_points,
    folder = raw_lidar_dir,
    output_dir = output_dir_virtual_plots,
    plot_radius = 150,
    chunk_size = 0,
    resume = FALSE
  )

  time_elapsed <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))
  gc_after <- gc()

  cat("   Time:", round(time_elapsed, 2), "seconds\n")
  cat(
    "   Memory delta:",
    round(sum(gc_after[, "(Mb)"]) - sum(gc_before[, "(Mb)"]), 2),
    "MB\n"
  )
  cat("   Peak memory:", round(max(gc_after[, "(Mb)"]), 2), "MB\n\n")

  # Filter to points with valid LAS files
  las_files_exist <- !is.na(stream_points$las_files) &
    file.exists(stream_points$las_files)
  stream_points <- stream_points[las_files_exist, ]
  cat("   Points with valid LAS files:", nrow(stream_points), "\n\n")

  if (nrow(stream_points) == 0) {
    skip("No valid LAS files created")
  }

  # Benchmark: Fisheye photo creation (sequential)
  cat("2. Creating fisheye photos (sequential)...\n")
  gc_before <- gc()
  time_start <- Sys.time()

  stream_points <- gla_create_fisheye_photos(
    points = stream_points,
    dem_path = dem_path,
    output_dir = output_dir_fisheye,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 220,
    img_res = 2800,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 300,
    parallel = FALSE, # Sequential for baseline
    resume = FALSE,
    horizon_estimation_method = "terra"
  )

  time_elapsed <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))
  gc_after <- gc()

  cat("   Time:", round(time_elapsed, 2), "seconds\n")
  cat(
    "   Avg time per point:",
    round(time_elapsed / nrow(stream_points), 2),
    "seconds\n"
  )
  cat(
    "   Memory delta:",
    round(sum(gc_after[, "(Mb)"]) - sum(gc_before[, "(Mb)"]), 2),
    "MB\n"
  )
  cat("   Peak memory:", round(max(gc_after[, "(Mb)"]), 2), "MB\n\n")

  # Benchmark: Solar radiation processing
  cat("3. Processing solar radiation (3-day window)...\n")
  gc_before <- gc()
  time_start <- Sys.time()

  stream_points <- gla_process_fisheye_photos(
    points = stream_points,
    clearsky_coef = 0.65,
    time_step_min = 2,
    day_start = 213,
    day_end = 215, # 3 days for benchmark
    day_res = 1,
    elev_res = 5,
    azi_res = 5,
    Kt = 0.45,
    parallel = FALSE
  )

  time_elapsed <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))
  gc_after <- gc()

  cat("   Time:", round(time_elapsed, 2), "seconds\n")
  cat(
    "   Avg time per point:",
    round(time_elapsed / nrow(stream_points), 2),
    "seconds\n"
  )
  cat(
    "   Memory delta:",
    round(sum(gc_after[, "(Mb)"]) - sum(gc_before[, "(Mb)"]), 2),
    "MB\n"
  )
  cat("   Peak memory:", round(max(gc_after[, "(Mb)"]), 2), "MB\n\n")

  cat("=== Benchmark Complete ===\n\n")

  # Basic validation
  expect_true(all(
    c("canopy_openness_pct", "subcanopy_solar_radiation_MJm2d") %in%
      names(stream_points)
  ))
  expect_true(all(
    stream_points$canopy_openness_pct >= 0 &
      stream_points$canopy_openness_pct <= 100
  ))
})

test_that("Memory usage comparison: parallel vs sequential (MANUAL TEST)", {
  skip_on_ci()
  skip_if_not(interactive(), "Benchmark test - run manually")

  # This test compares memory usage between parallel and sequential execution
  # Requires real data
  dem_path <- Sys.getenv("TEST_DEM_PATH", "/path/to/real/dem.tif")
  stream_network_path <- Sys.getenv(
    "TEST_STREAM_NETWORK_PATH",
    "/path/to/real/stream_network.gpkg"
  )
  raw_lidar_dir <- Sys.getenv("TEST_LIDAR_DIR", "/path/to/real/lidar")

  skip_if_not(file.exists(dem_path), "Real DEM file not found")
  skip_if_not(file.exists(stream_network_path), "Stream network not found")
  skip_if_not(dir.exists(raw_lidar_dir), "LiDAR directory not found")

  # Load and prepare points
  stream_points <- gla_load_points(stream_network_path, dem_path)
  stream_points <- stream_points[1:min(5, nrow(stream_points)), ]

  output_dir_virtual_plots <- withr::local_tempdir()

  stream_points <- gla_create_virtual_plots(
    points = stream_points,
    folder = raw_lidar_dir,
    output_dir = output_dir_virtual_plots,
    plot_radius = 150,
    chunk_size = 0,
    resume = FALSE
  )

  las_files_exist <- !is.na(stream_points$las_files) &
    file.exists(stream_points$las_files)
  stream_points <- stream_points[las_files_exist, ]

  if (nrow(stream_points) == 0) {
    skip("No valid LAS files")
  }

  cat("\n=== Memory Comparison: Parallel vs Sequential ===\n")
  cat("Testing with", nrow(stream_points), "points\n\n")

  # Test 1: Sequential execution
  cat("Test 1: Sequential execution\n")
  output_dir_seq <- withr::local_tempdir()
  gc()
  gc_before_seq <- gc()
  mem_before_seq <- sum(gc_before_seq[, "(Mb)"])

  stream_points_seq <- gla_create_fisheye_photos(
    points = stream_points,
    dem_path = dem_path,
    output_dir = output_dir_seq,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 220,
    img_res = 2800,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 300,
    parallel = FALSE,
    resume = FALSE,
    horizon_estimation_method = "terra"
  )

  gc_after_seq <- gc()
  mem_after_seq <- sum(gc_after_seq[, "(Mb)"])
  mem_peak_seq <- max(gc_after_seq[, "(Mb)"])

  cat("   Memory delta:", round(mem_after_seq - mem_before_seq, 2), "MB\n")
  cat("   Peak memory:", round(mem_peak_seq, 2), "MB\n\n")

  # Test 2: Parallel execution with 3 workers
  cat("Test 2: Parallel execution (3 workers)\n")
  output_dir_par <- withr::local_tempdir()
  gc()
  gc_before_par <- gc()
  mem_before_par <- sum(gc_before_par[, "(Mb)"])

  future::plan(future::multisession, workers = 3)

  stream_points_par <- gla_create_fisheye_photos(
    points = stream_points,
    dem_path = dem_path,
    output_dir = output_dir_par,
    cam_ht = 1.37,
    min_dist = 1,
    max_dist = 220,
    img_res = 2800,
    max_cex = 0.2,
    min_cex = 0.05,
    pointsize = 10,
    dpi = 300,
    parallel = TRUE,
    resume = FALSE,
    horizon_estimation_method = "terra"
  )

  future::plan(future::sequential)

  gc_after_par <- gc()
  mem_after_par <- sum(gc_after_par[, "(Mb)"])
  mem_peak_par <- max(gc_after_par[, "(Mb)"])

  cat("   Memory delta:", round(mem_after_par - mem_before_par, 2), "MB\n")
  cat("   Peak memory:", round(mem_peak_par, 2), "MB\n\n")

  cat(
    "Memory overhead from parallelization:",
    round(mem_peak_par - mem_peak_seq, 2),
    "MB\n"
  )
  cat("Note: Worker process memory is not shown (separate processes)\n\n")

  cat("=== Comparison Complete ===\n\n")

  # Validate both produced results
  expect_true(all(file.exists(stream_points_seq$fisheye_photo_path)))
  expect_true(all(file.exists(stream_points_par$fisheye_photo_path)))
})
