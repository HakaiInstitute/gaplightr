# Extract horizon masks for multiple points

Extracts terrain horizon elevation angles from a DEM for multiple
observation points and converts them to horizon masks suitable for
fisheye photo creation. Supports caching to disk for efficient resume of
interrupted workflows.

## Usage

``` r
gla_extract_horizons(
  points,
  dem_path,
  output_dir,
  step = 5,
  max_search_distance = NULL,
  distance_step = NULL,
  camera_height_m = 1.37,
  parallel = TRUE,
  resume = TRUE,
  verbose = FALSE
)
```

## Arguments

- points:

  sf object containing observation points with point geometry. Must have
  x_meters and y_meters columns. Coordinates will be extracted from the
  geometry and transformed to WGS84 if necessary.

- dem_path:

  Character path to the DEM raster file (GeoTIFF)

- output_dir:

  Character path to directory for caching horizon CSV files. Each
  point's horizon data is saved as x_y_horizon.csv. Required parameter.

- step:

  Numeric azimuth step size in degrees for horizon calculation (default:
  5)

- max_search_distance:

  Maximum search distance in meters for horizon detection (default:
  NULL, uses full DEM extent)

- distance_step:

  Distance step size in meters for sampling along line of sight
  (default: NULL, uses raster resolution)

- camera_height_m:

  Camera height above ground in meters (default: 1.37). The observer
  elevation is calculated as ground elevation (from DEM) plus this
  height.

- parallel:

  Logical indicating whether to process points in parallel (default:
  TRUE). When TRUE, uses the future backend configured with
  [`future::plan()`](https://future.futureverse.org/reference/plan.html).
  When FALSE, processes sequentially.

- resume:

  Logical indicating whether to skip points that already have cached
  horizon files (default: TRUE). When FALSE, recomputes all horizons.

- verbose:

  Logical indicating whether to print progress messages (default: FALSE)

## Value

The input sf object with an added horizon_mask list-column. Each element
is a list containing:

- x_msk:

  Numeric vector of x-coordinates for horizon mask polygon

- y_msk:

  Numeric vector of y-coordinates for horizon mask polygon

## Details

This function:

1.  For sequential (parallel=FALSE): Loads DEM once, processes all
    points

2.  For parallel (parallel=TRUE): Each worker loads DEM independently
    from file

3.  Extracts horizon angles using `gla_extract_horizon_terra`

4.  Converts to polar projection mask using `prepare_horizon_mask`

5.  Cleans up DEM from memory

Memory usage: Each worker loads its own copy of the DEM from disk. For N
points with a DEM of size M GB and W workers:

- Sequential (parallel=FALSE): Peak memory ~M GB, Time ~40-60 sec/point

- Parallel: Peak memory ~W×M GB, Time ~(40-60 sec/point)/W

Set up parallel processing before calling this function:
`future::plan(future::multisession, workers = 3)`

## See also

[`gla_extract_horizon_terra`](https://hakaiinstitute.github.io/gaplightr/reference/gla_extract_horizon_terra.md)
for single-point horizon extraction,
[`gla_create_fisheye_photos`](https://hakaiinstitute.github.io/gaplightr/reference/gla_create_fisheye_photos.md)
for using extracted horizons

## Examples

``` r
if (FALSE) { # \dontrun{
  # Load stream points
  stream_points <- gla_load_stream_network(
    stream_network_path,
    dem_path,
    HydroID = NULL
  )

  # Extract horizons with parallel processing and caching
  future::plan(future::multisession, workers = 3)
  stream_points <- gla_extract_horizons(
    points = stream_points,
    dem_path = dem_path,
    output_dir = "output/horizons",
    step = 5,
    max_search_distance = NULL,
    parallel = TRUE,
    resume = TRUE
  )

  # Horizon masks are now stored in stream_points$horizon_mask
  # Use in fisheye photo creation
  stream_points <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = "output/fisheye_photos"
  )
} # }
```
