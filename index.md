# gaplightr

## Overview

**gaplightr** analyzes forest canopy gap light transmission using LiDAR
point cloud data and hemispherical photography. The package provides
tools for:

- Processing LiDAR data to create synthetic hemispherical (fisheye)
  photographs
- Computing horizon angles from digital elevation models and LiDAR data
- Calculating gap light metrics and radiation indices
- Batch processing multiple sites

This package was originally developed by Gord Frazer for watershed-scale
forest light analysis.

## Installation

You can install the development version of gaplightr from the Hakai
Institute r-universe server:

``` r

install.packages("gaplightr", repos = "https://hakaiinstitute.r-universe.dev")
```

## Previous work

The gaplightr package is an R implementation of the [Gap Light
Analyzer](https://www.caryinstitute.org/science/our-scientists/dr-charles-d-canham/gap-light-analyzer-gla)
(GLA) software.

    Frazer, G.W., Canham, C.D., Lertzman, K.P., 1999. Gap Light Analyzer (GLA),
    Version 2.0: Imaging software to extract canopy structure and gap light
    transmission indices from true-colour fisheye photographs. Simon Fraser
    University, Burnaby, BC, and the Cary Institute of Ecosystem Studies,
    Millbrook, NY.

    Frazer, G.W., Canham, C.D., Lertzman, K.P., 1999. GAP LIGHT ANALYZER,
    VERSION 2.0. Bulletin of the Ecological Society of American, Technological
    Tools, July 2000: 191-197.

## License

MIT License - see
[LICENSE.md](https://hakaiinstitute.github.io/gaplightr/LICENSE.md) for
details

## Acknowledgements

The gaplightr package was initially released in the context of a
three-year study of forestry effects on stream ecosystems: the
N*a*nwa*k*olas 50 Watersheds (NC50) Project. The underlying concepts and
tools reflect many years of development by Dr. Gordon Frazer prior to
the NC50 project and significant in-kind contributions the Hakai
Institute (Tula Foundation) beyond the scope of the NC50 project and
BCSRIF funding. The N*a*nwa*k*olas 50 Watersheds Project was an
innovative Indigenous-led science partnership to monitor and develop
tools to address the threats posed by climate change and forest
management on salmon habitat in the territories of the N*a*nwa*k*olas
member First Nations. The N*a*nwa*k*olas 50 Watersheds Project was led
by the N*a*nwa*k*olas Council and five of its member Nations (We Wai
Kai, Wei Wai Kum, Tlowitsis, Mamalilikulla and K’ómoks First Nations) in
close partnership with the Hakai Institute. Funding for the
N*a*nwa*k*olas 50 Watersheds Project was provided by Fisheries and
Oceans Canada and the Province of British Columbia through the BC Salmon
Restoration and Innovation Fund. N*a*nwa*k*olas Council, the
N*a*nwa*k*olas member First Nations, and the Hakai Institute (Tula
Foundation) contributed significant in-kind contributions to the
project.
