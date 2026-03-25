# Process fisheye photos for solar radiation analysis

Batch processes fisheye photos for multiple points, computing gap
fractions and transmitted solar radiation. This is the main workflow
function for analyzing hemispherical fisheye photos.

## Usage

``` r
gla_process_fisheye_photos(
  points,
  clearsky_coef = 0.65,
  time_step_min = 2,
  day_start = 1,
  day_end = 365,
  day_res = 1,
  elev_res = 5,
  azi_res = 5,
  Kt = 0.45,
  rotation_deg = 0,
  parallel = TRUE,
  keep_gap_fraction_data = FALSE,
  radial_distortion = "equidistant",
  threshold = 0,
  solar_constant = 1367
)
```

## Arguments

- points:

  An sf object with point locations. Must contain columns:
  `fisheye_photo_path`, `lat`, `lon`, and `elevation`.

- clearsky_coef:

  Clear-sky transmission coefficient (default 0.65). Proportion of
  extraterrestrial radiation reaching the surface under clear skies.

- time_step_min:

  Time step in minutes for computing solar positions (default 2).

- day_start:

  Start day of year (default 1). Use day-of-year format (1-365).

- day_end:

  End day of year (default 365).

- day_res:

  Day resolution - compute every N days (default 1 for daily).

- elev_res:

  Elevation resolution in degrees (default 5)

- azi_res:

  Azimuth resolution in degrees (default 5)

- Kt:

  Mean cloudiness index (Kt = H/Ho) for the period of interest (default
  0.45). Ratio of measured to extraterrestrial radiation.

- rotation_deg:

  Rotation angle in degrees to align image with true north (default 0)

- parallel:

  Logical. If TRUE (default), use parallel processing via
  `future.apply`. Set up parallel plan with
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  before calling this function

- keep_gap_fraction_data:

  Include gap fraction matrix in output (default FALSE). If TRUE, adds
  `gap_fraction_data` column to output.

- radial_distortion:

  Lens projection method. Use "equidistant" (default) for standard
  equidistant polar projection, or provide custom lens calibration data
  (see
  [`gla_lens_sigma_8mm()`](https://hakaiinstitute.github.io/gaplightr/reference/gla_lens_sigma_8mm.md)
  for format).

- threshold:

  Threshold value for converting image to binary. Can be:

  - Numeric value (0-1): pixels below threshold become 0, above
    become 1. Default is 0 (matches original behavior).

  - "auto": automatic threshold detection using Otsu's method

  - "XX%": percentile-based threshold (e.g., "95%")

- solar_constant:

  Solar constant in W/m² (default 1367). The total solar electromagnetic
  radiation per unit area at the top of Earth's atmosphere.

## Value

An sf object with computed solar radiation metrics:

- canopy_openness_pct:

  Canopy openness percentage

- mean_daily_extraterrestrial_irradiance_Wm2:

  Mean daily extraterrestrial irradiance (W/m²)

- mean_daily_direct_irradiation_MJm2d:

  Mean daily direct irradiation (MJ/m²/day)

- mean_daily_diffuse_irradiation_MJm2d:

  Mean daily diffuse irradiation (MJ/m²/day)

- mean_daily_global_irradiation_MJm2d:

  Mean daily global irradiation (MJ/m²/day)

- transmitted_direct_irradiation_MJm2d:

  Transmitted direct irradiation (MJ/m²/day)

- transmitted_diffuse_irradiation_MJm2d:

  Transmitted diffuse irradiation (MJ/m²/day)

- transmitted_global_irradiation_MJm2d:

  Transmitted global irradiation (MJ/m²/day)

- transmitted_direct_irradiation_pct:

  Transmitted direct irradiation percentage

- transmitted_diffuse_irradiation_pct:

  Transmitted diffuse irradiation percentage

- transmitted_global_irradiation_pct:

  Transmitted global irradiation percentage

## Examples

``` r
if (FALSE) { # \dontrun{
# Process fisheye photos for August (days 213-243)
results <- gla_process_fisheye_photos(
  points = stream_points,
  day_start = 213,
  day_end = 243,
  Kt = 0.45
)

# Use automatic thresholding for real photos
results <- gla_process_fisheye_photos(
  points = stream_points,
  threshold = "auto"
)

# Keep gap fraction data for further analysis
results <- gla_process_fisheye_photos(
  points = stream_points,
  keep_gap_fraction_data = TRUE
)
} # }
```
