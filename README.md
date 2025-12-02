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
