# Extract gap fraction from fisheye image

Processes a fisheye image and computes gap fraction (proportion of sky
visible) in elevation and azimuth bins.

## Usage

``` r
gla_extract_gap_fraction(
  img_file,
  elev_res = 5,
  azi_res = 5,
  rotation_deg = 0,
  radial_distortion = "equidistant",
  threshold = 0
)
```

## Arguments

- img_file:

  Path to fisheye image file

- elev_res:

  Elevation resolution in degrees (default 5)

- azi_res:

  Azimuth resolution in degrees (default 5)

- rotation_deg:

  Rotation angle in degrees to align image with true north (default 0)

- radial_distortion:

  Lens projection method. Use "equidistant" (default) for standard
  equidistant polar projection, or provide custom lens calibration data
  (see
  [`gla_lens_sigma_8mm`](https://hakaiinstitute.github.io/gaplightr/reference/gla_lens_sigma_8mm.md)
  for format).

- threshold:

  Threshold value for converting image to binary. Can be:

  - Numeric value (0-1): pixels below threshold become 0, above
    become 1. Default is 0 (matches original behavior).

  - "auto": automatic threshold detection using Otsu's method

  - "XX\\

## Value

A list with gap fraction matrix and metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Default behavior (threshold = 0, any non-zero pixel becomes white)
gla_extract_gap_fraction("photo.jpg")

# Automatic threshold detection
gla_extract_gap_fraction("photo.jpg", threshold = "auto")

# Custom numeric threshold
gla_extract_gap_fraction("photo.jpg", threshold = 0.5)

# Use lens calibration
sigma_cal <- gla_lens_sigma_8mm()
gla_extract_gap_fraction("photo.jpg", radial_distortion = sigma_cal)
} # }
```
