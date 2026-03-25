# Load and validate observation points against a DEM

Loads point locations from a spatial file or sf object, validates them
against a DEM, and enriches each point with elevation, projected
coordinates, and WGS84 lat/lon. The result is the starting point for all
downstream gaplightr workflows.

## Usage

``` r
gla_load_points(x, dem, drop_na_dem = FALSE, ...)
```

## Arguments

- x:

  Either a file path to any file that
  [`sf::read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)
  can read, or an sf object containing point geometries

- dem:

  Either a file path to a DEM raster file, or a `SpatRaster` object

- drop_na_dem:

  Logical. If `FALSE` (default), points falling on NoData cells in the
  DEM cause an error. If `TRUE`, those points are dropped with a warning
  and processing continues with the remaining points.

- ...:

  Additional arguments passed to
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html)
  when `x` is a file path

## Details

This function performs strict validation:

- Geometry type must be POINT

- CRS must be defined and projected (not geographic lat/lon)

- Point and DEM CRS must match exactly

- All points must fall within DEM spatial extent

- Points on NoData cells error by default; set `drop_na_dem = TRUE` to
  drop them with a warning instead

### Point IDs

Every point is assigned a `point_id`, a positive integer used to name
all downstream output files (LAS clips, horizon CSVs, fisheye photos).
If `x` does not contain a `point_id` column, sequential IDs are assigned
automatically (1, 2, 3, ...). To use your own IDs (for example to
preserve cached outputs across re-runs, or to match an existing site
numbering scheme), include a `point_id` column containing unique
positive integers before calling this function.

## Examples

``` r
# \donttest{
  points_path <- system.file("extdata", "points.geojson", package = "gaplightr")
  dem_path <- system.file("extdata", "dem.tif", package = "gaplightr")
  points <- gla_load_points(points_path, dem_path)
#> Assigning sequential point_id (1 to 3).
# }
```
