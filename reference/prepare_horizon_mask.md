# Prepare horizon mask for fisheye photo creation

Converts horizon elevation angles to polar projected coordinates
suitable for masking fisheye photographs.

## Usage

``` r
prepare_horizon_mask(
  horizon_data,
  radial_distortion = "equidistant",
  verbose = FALSE
)
```

## Arguments

- horizon_data:

  Data frame with horizon data. Must contain at least two columns:
  azimuth (degrees, 0-360) and elevation angle (degrees)

- radial_distortion:

  Lens projection method. Use "equidistant" (default) for standard
  equidistant polar projection, or provide custom lens calibration data
  (see
  [`gla_lens_sigma_8mm()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_lens_sigma_8mm.md)
  for format).

- verbose:

  Logical indicating whether to print the processed data (default:
  FALSE)

## Value

Data frame with original columns plus:

- x_msk:

  X-coordinates in polar projection

- y_msk:

  Y-coordinates in polar projection

## Details

This function:

1.  Converts horizon elevation angles to zenith angles

2.  Converts to radians

3.  Projects to Cartesian coordinates using polar projection

4.  Flips x-axis so East is on left (camera convention)
