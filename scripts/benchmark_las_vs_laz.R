library(lidR)
library(sf)
library(bench)

catalog_path <- "/Users/sam.albers/_dev/gh_repos/50-watersheds-analysis/data/TRIB08/lidar"
ctg <- readLAScatalog(catalog_path)

# Clip 100m x 100m from catalog center
bbox <- st_bbox(ctg)
center_x <- (bbox["xmin"] + bbox["xmax"]) / 2
center_y <- (bbox["ymin"] + bbox["ymax"]) / 2
las <- clip_rectangle(
  ctg,
  center_x - 50,
  center_y - 50,
  center_x + 50,
  center_y + 50
)

las_out <- tempfile(fileext = ".las")
laz_out <- tempfile(fileext = ".laz")

results <- mark(
  LAS = {
    writeLAS(las, las_out)
    readLAS(las_out)
  },
  LAZ = {
    writeLAS(las, laz_out)
    readLAS(laz_out)
  },
  iterations = 3,
  check = FALSE
)

cat("\n=== LAS vs LAZ Benchmark ===\n")
cat("Points:", format(nrow(las@data), big.mark = ","), "\n\n")
print(results[, c("expression", "median", "mem_alloc")])
cat(
  "\nFile sizes - LAS:",
  round(file.size(las_out) / 1e6, 2),
  "MB | LAZ:",
  round(file.size(laz_out) / 1e6, 2),
  "MB\n"
)

unlink(c(las_out, laz_out))
