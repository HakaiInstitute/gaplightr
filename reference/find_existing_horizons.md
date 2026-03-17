# Find which points have existing horizon CSV files

Find which points have existing horizon CSV files

## Usage

``` r
find_existing_horizons(points, output_dir)
```

## Arguments

- points:

  sf object with points (must have point_id column)

- output_dir:

  Directory containing horizon CSV files

## Value

Logical vector indicating which points have cached horizons
