# Validate CRS match between two spatial objects

Strict CRS validation to prevent spatial errors. Checks that both
objects have defined CRS and that they match exactly.

## Usage

``` r
validate_crs_match(obj1_crs, obj2_crs, obj1_name, obj2_name)
```

## Arguments

- obj1_crs:

  CRS of first object (from sf::st_crs())

- obj2_crs:

  CRS of second object (from sf::st_crs())

- obj1_name:

  Name of first object for error message

- obj2_name:

  Name of second object for error message

## Value

NULL (invisibly) if validation passes, otherwise stops with error
