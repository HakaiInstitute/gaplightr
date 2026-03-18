# Validate radial distortion calibration data

Checks that radial_distortion is either "equidistant" (default) or a
valid calibration list with required components and normalized radius
values.

## Usage

``` r
validate_radial_distortion(radial_distortion)
```

## Arguments

- radial_distortion:

  Lens projection method. Use "equidistant" (default) for standard
  equidistant polar projection, or provide custom lens calibration data
  (see
  [`gla_lens_sigma_8mm`](https://hakaiinstitute.github.io/gaplightr/reference/gla_lens_sigma_8mm.md)
  for format).

## Value

TRUE invisibly if valid, otherwise stops with error
