# gaplightr

<!-- badges: start -->
[![R-CMD-check](https://github.com/HakaiInstitute/gaplightr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HakaiInstitute/gaplightr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

**gaplightr** analyzes forest canopy gap light transmission using LiDAR point cloud data and hemispherical photography. The package provides tools for:

- Processing LiDAR data to create synthetic hemispherical (fisheye) photographs
- Computing horizon angles from digital elevation models and LiDAR data
- Calculating gap light metrics and radiation indices
- Batch processing multiple sites

This package was originally developed by Gord Frazer for watershed-scale forest light analysis.

## Installation

You can install the development version of gaplightr from GitHub:

```r
# install.packages("pak")
pak::pak("HakaiInstitute/gaplightr")
```

## License

MIT License - see [LICENSE.md](LICENSE.md) for details

## Acknowledgements

The gaplightr package was initially released in the context of a three-year study of forestry effects on stream ecosystems: the Nanwakolas 50 Watersheds (NC50) Project. The underlying concepts and tools reflect many years of development by Dr. Gordon Frazer prior to the NC50 project and significant in-kind contributions the Hakai Institute (Tula Foundation) beyond the scope of the NC50 project and BCSRIF funding. The Nanwakolas 50 Watersheds Project was an innovative Indigenous-led science partnership to monitor and develop tools to address the threats posed by climate change and forest management on salmon populations and their habitat in the territories of the Nanwakolas member First Nations. The Nanwakolas 50 Watersheds Project was led by the Nanwakolas Council and five of its member Nations (We Wai Kai, Wei Wai Kum, Tlowitsis, Mamalilikulla and K’ómoks First Nations) in close partnership with the Hakai Institute. Funding for the Nanwakolas 50 Watersheds Project was provided by Fisheries and Oceans Canada and the Province of British Columbia through the BC Salmon Restoration and Innovation Fund. Nanwakolas Council, the Nanwakolas member First Nations, and the Hakai Institute (Tula Foundation) contributed significant in-kind contributions to the project.
