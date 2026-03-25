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

  Camera height above ground in meters. The observer elevation is
  calculated as ground elevation (from DEM) plus this height.

- parallel:

  Logical. If TRUE (default), use parallel processing via
  `future.apply`. Set up parallel plan with
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  before calling this function

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

When `parallel = TRUE`, each worker loads the DEM independently from
disk. Set up a parallel plan before calling this function:
`future::plan(future::multisession, workers = 3)`

## See also

[`gla_create_fisheye_photos()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_create_fisheye_photos.md)
for using extracted horizons

## Examples

``` r
if (FALSE) { # \dontrun{
  points <- gla_load_points("points.gpkg", "dem.tif")
  points <- gla_extract_horizons(
    points = points,
    dem_path = "dem.tif",
    output_dir = "output/horizons"
  )
} # }
```
