# Script to generate demo data for the gaplightr introduction vignette.
# Committed for reproducibility. Run with:
#   Rscript --no-save --no-restore-data data-raw/create_demo_data.R

library(terra)
library(sf)
library(lidR)

dir.create("inst/demo/lidar", recursive = TRUE, showWarnings = FALSE)

# ---- DEM -----------------------------------------------------------------
# 200x200 cells at 2m resolution, EPSG:3005 (BC Albers).
# Extent: (1000000, 1000400, 500000, 500400). Flat at 100m with several peaks.
nrows <- 200
ncols <- 200
res_m <- 2
x_min <- 1000000
y_min <- 500000

# Each peak defined as (col_center, row_center, height_m, spread).
# Spread controls width: larger = broader peak.
peaks <- list(
  list(cx = 100, cy = 100, height = 8, spread = 3000), # broad central rise
  list(cx = 35, cy = 45, height = 12, spread = 600), # sharp NW peak
  list(cx = 165, cy = 55, height = 9, spread = 800), # NE peak
  list(cx = 50, cy = 160, height = 10, spread = 700) # SW peak
)

elev_matrix <- matrix(100, nrow = nrows, ncol = ncols)
for (pk in peaks) {
  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      dist2 <- (i - pk$cy)^2 + (j - pk$cx)^2
      elev_matrix[i, j] <- elev_matrix[i, j] +
        pk$height * exp(-dist2 / pk$spread)
    }
  }
}

dem <- terra::rast(
  elev_matrix,
  extent = terra::ext(
    x_min,
    x_min + ncols * res_m,
    y_min,
    y_min + nrows * res_m
  ),
  crs = "EPSG:3005"
)
names(dem) <- "elevation"
terra::writeRaster(dem, "inst/demo/dem.tif", overwrite = TRUE)
message("Created inst/demo/dem.tif")

# ---- Sample points -------------------------------------------------------
# Three points inside the DEM, spaced ~140m apart, each at least 50m from the edge.
points <- sf::st_as_sf(
  data.frame(
    x = c(1000170.23, 1000200.83, 1000300.21),
    y = c(500090.47, 500200.14, 500300.95)
  ),
  coords = c("x", "y"),
  crs = 3005
)
sf::st_write(
  points,
  "inst/demo/points.geojson",
  delete_dsn = TRUE,
  quiet = TRUE
)
message("Created inst/demo/points.geojson")

# ---- LiDAR point cloud ---------------------------------------------------
# Ground returns (class 2) sit at terrain elevation and are filtered below
# camera height during fisheye projection. Vegetation returns (class 1) model
# individual conifer trees: each tree is a cone whose crown radius tapers from
# a base radius at ~60% of tree height to zero at the apex. This vertical
# structure spreads returns across multiple zenith angles in the fisheye
# projection, producing realistic canopy texture rather than flat blobs.
#
# gaplightr only reads X, Y, Z, Classification (opt_select = "xyzc"), so
# Intensity/ReturnNumber/NumberOfReturns are set to constants to minimise
# file size.
set.seed(42)

lidar_cx <- x_min + ncols * res_m / 2 # 1000200
lidar_cy <- y_min + nrows * res_m / 2 # 500200
lidar_radius <- 180

# Tree stand clusters. At least one cluster overlaps each sampling point's
# 50m virtual plot so every plot contains vegetation returns.
tree_clusters <- data.frame(
  cx = c(
    1000165,
    1000185,
    1000195,
    1000225,
    1000295,
    1000315,
    1000155,
    1000260
  ),
  cy = c(500095, 500075, 500205, 500185, 500295, 500270, 500270, 500145),
  r = c(18, 22, 20, 15, 18, 25, 28, 20)
)

# Helper: sample n points uniformly within a circle.
sample_circle <- function(n, cx, cy, r) {
  x_try <- runif(n * 4, cx - r, cx + r)
  y_try <- runif(n * 4, cy - r, cy + r)
  keep <- sqrt((x_try - cx)^2 + (y_try - cy)^2) <= r
  list(x = x_try[keep][seq_len(n)], y = y_try[keep][seq_len(n)])
}

# Generate LiDAR returns for a single conifer tree. The crown is modelled as a
# cone: radius tapers linearly from crown_base_r at the base to 0 at the apex.
# Points are distributed uniformly within this volume, producing the full range
# of Z values (and hence zenith angles) seen in real canopy returns.
generate_tree_points <- function(
  cx,
  cy,
  terrain_elev,
  n_pts,
  total_height,
  trunk_ratio,
  crown_base_r
) {
  crown_base_z <- terrain_elev + total_height * trunk_ratio
  crown_top_z <- terrain_elev + total_height
  z_frac <- runif(n_pts)
  z <- crown_base_z + z_frac * (crown_top_z - crown_base_z)
  r_at_z <- crown_base_r * (1 - z_frac) # taper to zero at apex
  theta <- runif(n_pts, 0, 2 * pi)
  r <- r_at_z * sqrt(runif(n_pts)) # uniform within disc
  # Small Gaussian jitter breaks the geometrically perfect cone outline.
  jitter_sd <- crown_base_r * 0.15
  data.frame(
    X = cx + r * cos(theta) + rnorm(n_pts, 0, jitter_sd),
    Y = cy + r * sin(theta) + rnorm(n_pts, 0, jitter_sd),
    Z = z
  )
}

# Generate all trees within one stand cluster. Tree density ~300 stems/ha.
generate_stand_points <- function(
  cluster_cx,
  cluster_cy,
  cluster_r,
  dem,
  n_pts_per_tree = 400
) {
  n_trees <- max(5L, as.integer(pi * cluster_r^2 * 0.03))
  tree_pos <- sample_circle(n_trees, cluster_cx, cluster_cy, cluster_r)
  elev <- terra::extract(dem, cbind(tree_pos$x, tree_pos$y))[, 1]
  pts_list <- lapply(seq_len(n_trees), function(k) {
    generate_tree_points(
      cx = tree_pos$x[k],
      cy = tree_pos$y[k],
      terrain_elev = elev[k],
      n_pts = n_pts_per_tree,
      total_height = runif(1, 20, 35),
      trunk_ratio = runif(1, 0.50, 0.65),
      crown_base_r = runif(1, 1.5, 3.5)
    )
  })
  do.call(rbind, pts_list)
}

# Ground points: sparse across the full LiDAR circle.
n_ground <- 4500
grd <- sample_circle(n_ground, lidar_cx, lidar_cy, lidar_radius)
grd_elev <- terra::extract(dem, cbind(grd$x, grd$y))[, 1]

# Vegetation: individual trees within each stand cluster.
veg_list <- lapply(seq_len(nrow(tree_clusters)), function(k) {
  generate_stand_points(
    tree_clusters$cx[k],
    tree_clusters$cy[k],
    tree_clusters$r[k],
    dem
  )
})
veg_df <- do.call(rbind, veg_list)

x_coords <- c(grd$x, veg_df$X)
y_coords <- c(grd$y, veg_df$Y)
z_coords <- c(grd_elev + runif(n_ground, 0, 0.3), veg_df$Z)
n_total <- length(x_coords)
class_coords <- c(rep(2L, n_ground), rep(1L, nrow(veg_df)))

las_data <- data.frame(
  X = x_coords,
  Y = y_coords,
  Z = z_coords,
  Classification = class_coords,
  Intensity = 0L,
  ReturnNumber = 1L,
  NumberOfReturns = 1L
)

las <- suppressMessages(lidR::LAS(las_data))
lidR::projection(las) <- 3005
lidR::writeLAS(las, "inst/demo/lidar/points.laz")

laz_size <- file.size("inst/demo/lidar/points.laz")
message(sprintf(
  "Created inst/demo/lidar/points.laz (%d points: %d ground, %d vegetation; %.0fK)",
  n_total,
  n_ground,
  nrow(veg_df),
  laz_size / 1024
))
message("Demo data generation complete.")
