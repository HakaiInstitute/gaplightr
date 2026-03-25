# Create fisheye image (single point)

Create fisheye image (single point)

## Usage

``` r
gla_create_fisheye_photo_single(
  processed_lidar,
  x_msk,
  y_msk,
  site_id,
  img_path,
  max_cex,
  min_cex,
  min_dist,
  width,
  pointsize,
  res,
  radial_distortion = "equidistant",
  ...
)
```

## Arguments

- radial_distortion:

  Lens projection method. Use "equidistant" (default) for standard
  equidistant polar projection, or provide custom lens calibration data
  (see
  [`gla_lens_sigma_8mm()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_lens_sigma_8mm.md)
  for format).
