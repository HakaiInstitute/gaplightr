# Apply radial distortion mapping in either direction

Performs linear interpolation for lens distortion calibration. Used
bidirectionally: forward (elevation → radius) for synthetic photo
creation, reverse (radius → elevation) for photo analysis.

## Usage

``` r
apply_radial_distortion_mapping(input_values, from, to)
```

## Arguments

- input_values:

  Vector of input values to map

- from:

  Vector of input reference values (from calibration)

- to:

  Vector of output reference values (from calibration)

## Value

Vector of mapped values
