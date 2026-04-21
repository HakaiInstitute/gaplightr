# Create fisheye photos for multiple points

Batch process LiDAR data to create synthetic hemispherical (fisheye)
photographs for multiple spatial points. Supports parallel processing
and can resume from previous runs.

## Usage

``` r
gla_create_fisheye_photos(
  points,
  output_dir,
  camera_height_m = 1.37,
  min_dist = 1,
  img_res = 2800,
  max_cex = 1.5,
  min_cex = 0.04,
  pointsize = 20,
  dpi = 1200,
  parallel = TRUE,
  resume = TRUE,
  radial_distortion = "equidistant"
)
```

## Arguments

- points:

  An sf object containing spatial points with required columns:
  `las_files`, `lat`, `lon`, `elevation`, and `horizon_mask`. Use
  [`gla_extract_horizons()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_extract_horizons.md)
  to add the `horizon_mask` column.

- output_dir:

  Directory path where fisheye photo BMP files will be saved

- camera_height_m:

  Camera height above ground in meters. Default is 1.37m

- min_dist:

  Minimum distance from camera to include LiDAR points (meters). Points
  closer than this distance are excluded. Must be greater than 0 - a
  value of 0 would allow rho = 0, causing division by zero in point size
  scaling. Because points with rho \< min_dist are filtered out in
  [`gla_transform_lidar()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_transform_lidar.md),
  symbol size (cex) can only exceed max_cex when min_dist is set to a
  value less than 1 (or if that filtering behavior changes), leading to
  very large dots near the camera. Choose min_dist in concert with
  max_cex. Default is 1m.

- img_res:

  Image resolution in pixels (width and height). Default is 2800.

- max_cex:

  Maximum symbol size for plotting points (CEX value). Controls the size
  of points at rho = 1 (1 metre from camera) under the inverse-distance
  formula. Default is 1.5.

- min_cex:

  Minimum symbol size for plotting points (CEX value). The asymptotic
  lower bound approached as distance increases. Default is 0.04.

- pointsize:

  Point size parameter for bitmap graphics device. Default is 20.

- dpi:

  Resolution in dots per inch for output image. Default is 1200.

- parallel:

  Logical. If TRUE (default), use parallel processing via
  `future.apply`. Set up parallel plan with
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  before calling this function

- resume:

  Logical. If TRUE (default), skip points that already have fisheye
  photos in the output directory

- radial_distortion:

  Lens projection method. Use "equidistant" (default) for standard
  equidistant polar projection, or provide custom lens calibration data
  (see
  [`gla_lens_sigma_8mm()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_lens_sigma_8mm.md)
  for format).

## Value

The input `points` sf object with an added column `fisheye_photo_path`
containing the file paths to the generated fisheye photos

## Details

This function processes multiple points in batch, transforming LiDAR
data into synthetic hemispherical photographs. For each point, it:

1.  Reads and transforms the LiDAR point cloud

2.  Projects points onto a hemispherical image plane

3.  Creates a bitmap image with distance-weighted point sizes

4.  Saves the result as a BMP file

**Graphical parameter calibration:** The default values for `img_res`,
`pointsize`, `min_cex`, `max_cex`, and `dpi` were derived from
calibration against real hemispherical photographs collected in a
coastal temperate rainforest. Because optimal values depend on forest
structure, canopy density, and LiDAR point density, users working in
other forest types or with different LiDAR acquisitions should expect to
calibrate these parameters for their study region. Calibration can be
done formally (e.g., optimizing against co-located real photos) or
informally through visual trial and error.

Parallel processing can significantly speed up processing for many
points. Set up a parallel plan before calling this function:
`future::plan(future::multisession, workers = 4)`

The resume feature allows you to interrupt and restart processing
without re-creating existing photos.

## See also

[`gla_extract_horizons()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_extract_horizons.md)
for extracting horizon masks

## Examples

``` r
if (FALSE) { # \dontrun{
  # Assuming you have points with horizon masks already extracted
  future::plan(future::multisession, workers = 4)

  points_with_photos <- gla_create_fisheye_photos(
    points = stream_points,
    output_dir = "output/fisheye_photos",
    camera_height_m = 1.37,
    parallel = TRUE,
    resume = TRUE
  )
} # }
```
