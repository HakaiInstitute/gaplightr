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

  Either "equidistant" string or calibration list with radius and
  elevation components

## Value

TRUE invisibly if valid, otherwise stops with error
