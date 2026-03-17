# Extract horizon angles from DEM using terra

Calculates the horizon elevation angle for each azimuth direction from a
given observation point using a digital elevation model (DEM).

## Usage

``` r
gla_extract_horizon_terra(
  dem_rast,
  x_meters,
  y_meters,
  step = 5,
  max_search_distance = NULL,
  distance_step = NULL,
  dem_max = NULL,
  camera_height_m,
  verbose = FALSE
)
```

## Arguments

- dem_rast:

  SpatRaster object from terra package containing the DEM

- x_meters:

  Numeric X coordinate in meters (in the DEM's coordinate system)

- y_meters:

  Numeric Y coordinate in meters (in the DEM's coordinate system)

- step:

  Numeric value specifying the azimuth step size in degrees (default: 5)

- max_search_distance:

  Maximum search distance in meters (default: NULL, uses full extent)

- distance_step:

  Distance step size for sampling along line of sight in meters
  (default: NULL, uses raster resolution)

- dem_max:

  Maximum elevation in the DEM for early termination optimization
  (default: NULL, will be computed). When processing multiple points
  with the same DEM, pass this value to avoid recomputing (expensive on
  large DEMs).

- camera_height_m:

  Camera height above ground in meters. The observer elevation is
  calculated as ground elevation (from DEM) plus this height.

- verbose:

  Logical indicating whether to print progress messages (default: FALSE)

## Value

Data frame with two columns:

- azimuth: azimuth angles in degrees (0-360, East=0, counterclockwise)

- horizon_height: horizon elevation angles in degrees

## Details

The algorithm:

1.  Tracks the tangent of the current maximum horizon angle, tan(theta)

2.  Compares each point against the current horizon line

3.  Updates horizon when: z_point \> z_origin + curvature + distance \*
    tan(theta)

4.  Applies Earth curvature correction: 0.5 \* distance^2 / 6371000

5.  Terminates when reaching max elevation or max distance

The observer elevation is computed as: ground_elev (from DEM) +
camera_height_m.

## Examples

``` r
if (FALSE) { # \dontrun{
  dem <- terra::rast("path/to/dem.tif")
  horizon_data <- gla_extract_horizon_terra(
    dem_rast = dem,
    x_meters = 500000,
    y_meters = 5500000,
    step = 5,
    camera_height_m = 1.37
  )
} # }
```
