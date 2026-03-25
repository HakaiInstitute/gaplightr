# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Workflow Requirements
- Review and update CLAUDE.md regularly as the codebase evolves
- Always ask clarifying questions in planning mode

## Project Overview

**gaplightr** analyzes forest canopy gap light transmission using LiDAR point cloud data and hemispherical photography. The package provides tools for processing LiDAR data, computing horizon angles, and calculating gap light metrics.

**Key functionality:**
- Load and preprocess point locations from spatial files (`gla_load_points()`)
- Transform LiDAR data to hemispherical coordinates (`gla_transform_lidar`)
- Create virtual LiDAR plots at point locations (`gla_create_virtual_plots()`)
- Create synthetic fisheye photographs from LiDAR:
  - Single point: `gla_create_fisheye_photo_single()`
  - Batch processing: `gla_create_fisheye_photos()` (with parallel support)
- Compute horizon angles from DEMs:
  - Single point: `gla_extract_horizon_terra()`
  - Batch processing: `gla_extract_horizons()` (with parallel support and caching)
- Process real fisheye photos for gap fraction analysis (`gla_process_fisheye_photos()`)

## Package Structure

Standard R package layout:
- `R/` - Package functions for gap light analysis
  - `lidar.R` - LiDAR processing, transformation, and virtual plot creation
  - `photo.R` - Fisheye photo creation and processing (single + batch)
  - `horizon.R` - Horizon angle computation using terra
  - `points.R` - Point loading and preprocessing
  - `gla-utils.R` - Gap light analysis utilities (sky area, irradiance)
  - `utils.R` - Helper functions
  - `is.R` - Validation functions
  - `gaplightr-package.R` - Package-level documentation and imports
- `man/` - Documentation (auto-generated via roxygen2)
- `tests/testthat/` - Test suite with lazy fixture generation
  - `testdata/` - Static test data (reference photos, processed results)
  - `setup.R` - Loads libraries and provides fixture factory functions for on-demand test data generation
- `DESCRIPTION` - Package metadata and dependencies
- `NAMESPACE` - Package namespace (auto-generated via roxygen2)

## Development Commands

General advice:
- When running R from the console, always run it with `--quiet --vanilla`
- Always run `air format .` after generating code (if available)
- After making roxygen changes, run `devtools::document()` then `devtools::check()`

### Documentation
```r
# Generate documentation from roxygen2 comments
devtools::document()
```

### Building and Checking
```r
# Load package for interactive development
devtools::load_all()

# Run R CMD check
devtools::check()

# Install package locally
devtools::install()
```

### Testing
```r
# Run all tests (when tests/ exists)
devtools::test()

# Run tests with coverage
covr::package_coverage()
```

**IMPORTANT**: Never use `skip_if()` or similar to make a failing test pass. If a test doesn't work:
1. Fix the underlying issue (e.g., fixture geometry, parameter values)
2. Ensure fixtures properly align (e.g., LAS point spread radius must match virtual plot radius)
3. Add explicit assertions to verify preconditions are met
4. A skipping test means you're not testing anything - this is unacceptable

Example of what NOT to do:
```r
# BAD - hides the real problem
skip_if(nrow(stream_points) == 0, "No valid LAS files created")
```

Example of what TO do:
```r
# GOOD - fixes the root cause
stream_points <- gla_create_virtual_plots(
  plot_radius = 50, # Match LAS point spread radius
  ...
)
# Verify it worked
expect_true(file.exists(stream_points$las_files[1]))
```

## Test Architecture

### Fixture Management
Tests use **lazy fixture generation** for on-demand test data creation:

- **`setup.R`**: Runs once before all tests, providing:
  - Library loads (testthat, withr)
  - Fixture factory functions available to all tests:
    - `create_test_dem(crs, nrows, ncols, output_path)` - Generate minimal DEM
    - `create_test_las(crs, n_points, output_path)` - Generate minimal LAS point cloud
    - `create_test_points(crs, n_points, output_path)` - Generate minimal point locations
    - `create_test_photo_points(fisheye_photo_path, ...)` - Create sf point dataframe for photo processing tests
  - All functions use explicit package prefixes (`terra::`, `lidR::`, `sf::`)
  - No camelCase in variable names

- **Test files**: Call fixture functions with `withr::local_tempfile()` for automatic cleanup
  ```r
  # Example: Create spatial fixtures
  test_that("example test", {
    dem_path <- withr::local_tempfile(fileext = ".tif")
    create_test_dem(crs = 3005, output_path = dem_path)
    # Test code here - file auto-deleted after test
  })

  # Example: Create sf point dataframe for photo tests
  test_that("photo processing test", {
    test_photo <- test_path("testdata", "some_photo.bmp")
    test_points <- create_test_photo_points(fisheye_photo_path = test_photo)
    # All default coordinates, can override any parameter
  })
  ```

### Benefits of This Approach
- **Faster**: Only create fixtures needed for tests that actually run
- **Isolated**: Each test gets fresh fixtures via `withr` temp files
- **No cleanup needed**: `withr` handles automatic deletion
- **Easier debugging**: Run single tests without global setup overhead
- **Single file**: All test infrastructure consolidated in `setup.R`

## Documentation Standards

- Use roxygen2 for function documentation with markdown enabled (`Roxygen: list(markdown = TRUE)`)
- All exported functions must have `@export` tag
- Include `@param`, `@return`, `@examples`, and `@details` sections
- Parameter names in documentation MUST match function signature exactly
- Use `\code{\link{function_name}}` for internal package function references
- Use `\code{package::function()}` or markdown `[package::function()]` for external references
- Run `devtools::document()` after modifying roxygen comments
- Verify with `devtools::check()` to catch documentation mismatches

## Adding Dependencies

### In DESCRIPTION:
- **Imports**: Packages that code directly uses (e.g., `future.apply`, `terra`, `sf`)
- **Suggests**: Packages used in examples, tests, or optional features (e.g., `future`, `testthat`)
- **Base packages**: No need to declare (utils, stats, graphics, grDevices)

### In R code:
- Base R functions: Use directly, no imports needed
- Non-base packages: Use `package::function()` OR add `@importFrom package function` in roxygen
- Package-level imports: Add to `R/gaplightr-package.R` with `@importFrom` tags

### For base packages requiring imports:
Add to `R/gaplightr-package.R`:
```r
#' @importFrom grDevices bmp dev.off
#' @importFrom graphics par points polygon
#' @importFrom stats approx
#' @importFrom utils read.csv write.csv
```

## Parallel Processing

- Package uses `future.apply::future_lapply()` for parallel operations
- Users configure parallelization with `future::plan()` before calling functions
- Examples show `future::plan(future::multisession, workers = N)`
- `future` is in Suggests (users install it, not a hard dependency)
- Batch functions support `parallel = TRUE/FALSE` parameter