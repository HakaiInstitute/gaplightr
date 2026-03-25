# Create virtual plots from LiDAR data

Clips circular plots from a LiDAR catalog at specified point locations.
Processes points in batches to avoid memory exhaustion with large
datasets.

## Usage

``` r
gla_create_virtual_plots(
  points,
  folder,
  output_dir,
  plot_radius,
  filter = "-keep_class 1 2 9",
  chunk_size = 0,
  batch_size = 1000,
  resume = TRUE,
  check_las_catalog = FALSE,
  ...
)
```

## Arguments

- points:

  sf object with point locations

- folder:

  Directory containing LAS/LAZ files

- output_dir:

  Directory to save clipped plot files

- plot_radius:

  Radius of circular plots in meters

- filter:

  LAS filter string (default: "-keep_class 1 2 9")

- chunk_size:

  Chunk size for LAScatalog processing (default: 0)

- batch_size:

  Number of plots to process per batch (default: 1000). Reduce if
  encountering memory errors with large datasets.

- resume:

  Skip points with existing output files (default: TRUE)

- check_las_catalog:

  Run las_check on catalog (default: FALSE)

- ...:

  Additional arguments passed to readLAScatalog

## Value

sf object with added las_files column containing paths to clipped plots

## Details

Processes plots in batches to prevent memory exhaustion. With large
datasets (e.g., 5000+ plots), processing all at once can fail. The
batch_size parameter controls how many plots are clipped simultaneously.
Default of 1000 works well for most systems. Reduce to 500 or lower if
still encountering memory issues.

## Examples

``` r
# \donttest{
  points_path <- system.file("extdata", "points.geojson", package = "gaplightr")
  dem_path <- system.file("extdata", "dem.tif", package = "gaplightr")
  las_folder <- system.file("extdata", "lidar", package = "gaplightr")

  points <- gla_load_points(points_path, dem_path)
#> Assigning sequential point_id (1 to 3).
  points <- gla_create_virtual_plots(
    points = points,
    folder = las_folder,
    output_dir = tempdir(),
    plot_radius = 50
  )
#> Clipping 3 circular plots with radius 50m
#> Processing batch 1/1 (3 plots)
#> Created 3 new plot files
# }
```
