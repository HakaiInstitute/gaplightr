# Get Sigma 8mm f/3.5 EX DG lens radial calibration

Returns the radial distortion calibration data for a Sigma 8mm f/3.5 EX
DG circular fisheye lens. This calibration maps normalized image radius
to elevation angle.

## Usage

``` r
gla_lens_sigma_8mm()
```

## Value

A list with two components:

- radius:

  Normalized radial distance from image center (0 to 1)

- elevation:

  Elevation angle in radians (0 at horizon, pi/2 at zenith)

A list with components `radius` (normalized image radius), `elevation`
(elevation angle in radians), and `name` (lens identifier for this
calibration; currently `"sigma8mm"`).
