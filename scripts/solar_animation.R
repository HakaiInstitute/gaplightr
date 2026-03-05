# =============================================================================
# Solar Animation: 2D Side-View of Radiation Through Forest Canopy
# =============================================================================
#
# Creates animated GIF showing sun moving through sky over a day with light
# rays penetrating canopy gaps, using real LiDAR data for canopy structure.
#
# Dependencies: gaplightr, ggplot2, gganimate, gifski, transformr
#
# Usage: Source this script or run interactively after setting user parameters.
#
# =============================================================================

# --- USER PARAMETERS ---------------------------------------------------------
# Fill in these values before running

# Required: LAS file path (coordinates extracted from filename, e.g., "1022193_576342.4.las")
las_path <- "/Users/sam.albers/_dev/gh_repos/50-watersheds-analysis/output/TRIB08/virtual_plots/1022193_576342.4.las"

# Required: DEM raster path (for ground elevation extraction)
dem_path <- "/Users/sam.albers/_dev/gh_repos/50-watersheds-analysis/data/TRIB08/dem_2m/Salmon_River_DEM_Flattened_2m.tif"

# Required: CRS EPSG code for the LAS/DEM coordinate system
crs_epsg <- 3005 # BC Albers

# Animation parameters
day_of_year <- 172 # Day of year (1-365), e.g., 172 = June 21 solstice
target_azimuth <- 180 # Slice azimuth (0=N, 90=E, 180=S, 270=W)
output_gif <- "solar_animation.gif"

# Optional parameters
slice_tolerance_deg <- 10 # Azimuth tolerance for slice extraction (degrees)
time_steps <- 48 # Number of frames across daylight hours
animation_fps <- 10 # Frames per second in output GIF
animation_width <- 800 # GIF width in pixels
animation_height <- 500 # GIF height in pixels

# =============================================================================
# SECTION 1: Setup
# =============================================================================

devtools::load_all()

library(ggplot2)
library(gganimate)

# Check for gifski renderer
if (!requireNamespace("gifski", quietly = TRUE)) {
  stop(
    "Package 'gifski' required for GIF rendering. ",
    "Install with: install.packages('gifski')"
  )
}

# Check for transformr (needed for some animations)
if (!requireNamespace("transformr", quietly = TRUE)) {
  message(
    "Package 'transformr' not installed. ",
    "Some animation features may not work. ",
    "Install with: install.packages('transformr')"
  )
}

# =============================================================================
# SECTION 2: Extract Coordinates from LAS Filename and DEM
# =============================================================================

message("Extracting coordinates from LAS filename...")

# Parse coordinates from filename (format: X_Y.las)
file_info <- gaplightr:::parse_las_filenames(las_path)
plot_x <- file_info$x_meters
plot_y <- file_info$y_meters
message(sprintf("  Plot coordinates: X=%.1f, Y=%.1f", plot_x, plot_y))

# Create sf point for DEM extraction and coordinate transformation
plot_point <- sf::st_as_sf(
  data.frame(x = plot_x, y = plot_y),
  coords = c("x", "y"),
  crs = crs_epsg
)

# Extract elevation from DEM
message("Extracting elevation from DEM...")
dem_rast <- terra::rast(dem_path)
ground_elev <- terra::extract(dem_rast, plot_point)[[2]]
message(sprintf("  Ground elevation: %.1f m", ground_elev))

# Transform to WGS84 for lat/lon (needed for solar calculations)
plot_wgs84 <- sf::st_transform(plot_point, crs = 4326)
coords_wgs84 <- sf::st_coordinates(plot_wgs84)
lat_deg <- coords_wgs84[1, "Y"]
long_deg <- coords_wgs84[1, "X"]
message(sprintf("  Lat/Lon: %.4f, %.4f", lat_deg, long_deg))

# =============================================================================
# SECTION 3: Extract Azimuth Slice from LiDAR
# =============================================================================

message("Extracting azimuth slice from LiDAR...")

# Transform LiDAR to hemispherical coordinates
# Returns: rho (distance), theta (azimuth, math convention), phi (zenith),
#          x and y (2D polar projection)
lidar_hemi <- gaplightr:::gla_transform_lidar(
  las_input = las_path,
  x_meters = plot_x,
  y_meters = plot_y,
  elev_m = ground_elev
)

# Convert target azimuth from geographic (N=0, CW) to math convention (E=0, CCW)
target_azi_rad <- target_azimuth * pi / 180
target_azi_math <- gaplightr:::azi2math(target_azi_rad)
slice_tolerance_rad <- slice_tolerance_deg * pi / 180

# Compute angular difference accounting for wrap-around at 2*pi
angular_diff <- function(a, b) {
  diff <- abs(a - b)
  pmin(diff, 2 * pi - diff)
}

# Filter points within azimuth tolerance of target slice
in_slice <- angular_diff(lidar_hemi$theta, target_azi_math) <
  slice_tolerance_rad

# Also include points from opposite azimuth (180 deg away) to get full cross-section
opposite_azi_math <- (target_azi_math + pi) %% (2 * pi)
in_opposite <- angular_diff(lidar_hemi$theta, opposite_azi_math) <
  slice_tolerance_rad

# Combine both halves
slice_points <- lidar_hemi[in_slice | in_opposite, ]

# Project to 2D side view coordinates:
# horiz_dist: horizontal distance from plot center (positive toward target azimuth)
# height: vertical height above camera

# For points in target direction: positive horizontal distance
# For points in opposite direction: negative horizontal distance
slice_points$horiz_dist <- slice_points$rho * sin(slice_points$phi)

# Sign based on which side of center
is_target_side <- angular_diff(slice_points$theta, target_azi_math) <
  slice_tolerance_rad
slice_points$horiz_dist <- ifelse(
  is_target_side,
  slice_points$horiz_dist,
  -slice_points$horiz_dist
)

# Height is always positive (points above camera)
slice_points$height <- slice_points$rho * cos(slice_points$phi)

message(sprintf("  Extracted %d points in slice", nrow(slice_points)))

# =============================================================================
# SECTION 4: Compute Sun Track
# =============================================================================

message("Computing sun track for day ", day_of_year, "...")

# Convert latitude to radians
lat_rad <- lat_deg * pi / 180

# Compute day angle and solar declination
day_angle <- gaplightr:::da(day_of_year)
solar_dec <- gaplightr:::soldec(day_angle)

# Compute sunrise/sunset hour angles
ss_angles <- gaplightr:::sshourangle(lat_rad, solar_dec)
cos_ws <- ss_angles[1]
sunrise_ha <- ss_angles[2]
sunset_ha <- ss_angles[3]

# Check for polar day/night conditions
if (is.na(sunrise_ha)) {
  stop(
    "No sunrise on day ",
    day_of_year,
    " at latitude ",
    lat_deg,
    " (polar night conditions)"
  )
}

# Convert hour angles to solar time (decimal hours)
# Hour angle: positive in morning, zero at noon, negative in afternoon
sunrise_hours <- 12 - sunrise_ha * 180 / (pi * 15)
sunset_hours <- 12 - sunset_ha * 180 / (pi * 15)

message(sprintf(
  "  Sunrise: %.2f hours, Sunset: %.2f hours",
  sunrise_hours,
  sunset_hours
))

# Generate time sequence across daylight hours
solar_times <- seq(
  sunrise_hours + 0.1,
  sunset_hours - 0.1,
  length.out = time_steps
)

# Compute sun position at each time step
sun_track <- data.frame(
  time_hours = solar_times,
  solar_time_rad = NA_real_,
  hour_angle = NA_real_,
  zenith = NA_real_,
  elevation = NA_real_,
  azimuth = NA_real_,
  azimuth_math = NA_real_
)

for (i in seq_along(solar_times)) {
  st_rad <- gaplightr:::soltime(solar_times[i])
  ha_rad <- gaplightr:::hrangle(st_rad)

  # Get sun position: zenith, elevation, azimuth angles
  sol <- gaplightr:::solpos(solar_dec, lat_rad, ha_rad)

  sun_track$solar_time_rad[i] <- st_rad
  sun_track$hour_angle[i] <- ha_rad
  sun_track$zenith[i] <- sol[1]
  sun_track$elevation[i] <- sol[2]
  sun_track$azimuth[i] <- sol[4] # Geographic azimuth (N=0, CW)
  sun_track$azimuth_math[i] <- sol[5] # Math convention (E=0, CCW)
}

# Add frame index for animation
sun_track$frame <- seq_len(nrow(sun_track))

# Convert time to readable format
sun_track$time_label <- sprintf(
  "%02d:%02d",
  floor(sun_track$time_hours),
  round((sun_track$time_hours %% 1) * 60)
)

# Project sun position into 2D slice view
# Sun is rendered at large distance from plot center
sun_display_distance <- max(abs(slice_points$horiz_dist), na.rm = TRUE) * 1.3

# Calculate angular difference between sun azimuth and slice azimuth
sun_azi_diff <- angular_diff(sun_track$azimuth_math, target_azi_math)
sun_opposite_diff <- angular_diff(sun_track$azimuth_math, opposite_azi_math)

# Sun x position: based on azimuth relative to slice
# When sun is toward target azimuth: positive x
# When sun is toward opposite azimuth: negative x
# When sun is perpendicular to slice: at x=0 but "behind" the view plane
sun_track$sun_x <- sun_display_distance * cos(sun_azi_diff)
sun_track$sun_x <- ifelse(
  sun_opposite_diff < sun_azi_diff,
  -sun_track$sun_x,
  sun_track$sun_x
)

# Sun y position: based on elevation angle
sun_track$sun_y <- sun_display_distance * tan(sun_track$elevation)

# Flag frames where sun is roughly in plane of slice (visible)
sun_track$in_slice_plane <- pmin(sun_azi_diff, sun_opposite_diff) < (pi / 4)

message(sprintf("  Computed %d sun positions", nrow(sun_track)))

# =============================================================================
# SECTION 5: Build Animation Data
# =============================================================================

message("Building animation frames...")

# Determine plot bounds
x_range <- range(slice_points$horiz_dist, na.rm = TRUE)
y_range <- c(0, max(slice_points$height, sun_track$sun_y, na.rm = TRUE) * 1.1)

# Expand x range slightly
x_pad <- diff(x_range) * 0.15
x_range <- c(x_range[1] - x_pad, x_range[2] + x_pad)

# Create ray intersection function
# Determines if a ray from sun to ground point is blocked by canopy
ray_blocked <- function(sun_x, sun_y, ground_x, canopy_x, canopy_y, tol = 0.5) {
  # For each canopy point, check if it lies on the ray path

  # Ray direction vector
  dx <- ground_x - sun_x
  dy <- -sun_y # Ground is at y=0

  # Parametric position of canopy point along ray
  # t=0 at sun, t=1 at ground
  t_vals <- (canopy_y - sun_y) / dy

  # x position of ray at canopy height
  ray_x_at_height <- sun_x + t_vals * dx

  # Check if canopy point is close to ray
  blocked <- any(
    t_vals > 0 & t_vals < 1 & abs(canopy_x - ray_x_at_height) < tol,
    na.rm = TRUE
  )

  return(blocked)
}

# Generate light rays and radiation accumulation for each frame
n_ground_points <- 30 # Number of points on ground to check for light
ground_x_seq <- seq(
  x_range[1] * 0.9,
  x_range[2] * 0.9,
  length.out = n_ground_points
)

# Storage for ray data and radiation accumulation
ray_data_list <- vector("list", nrow(sun_track))
radiation_accum <- numeric(nrow(sun_track))

for (frame in seq_len(nrow(sun_track))) {
  sun_x <- sun_track$sun_x[frame]
  sun_y <- sun_track$sun_y[frame]

  # Only draw rays if sun is above horizon and roughly in slice plane
  if (sun_y > 0 && sun_track$in_slice_plane[frame]) {
    rays <- data.frame(
      frame = frame,
      ground_x = ground_x_seq,
      sun_x = sun_x,
      sun_y = sun_y,
      blocked = FALSE
    )

    # Check each ray for canopy blockage
    for (r in seq_len(nrow(rays))) {
      rays$blocked[r] <- ray_blocked(
        sun_x,
        sun_y,
        rays$ground_x[r],
        slice_points$horiz_dist,
        slice_points$height,
        tol = diff(x_range) / n_ground_points * 0.8
      )
    }

    ray_data_list[[frame]] <- rays

    # Accumulate radiation (proportion of unblocked rays, weighted by elevation)
    gap_fraction <- 1 - mean(rays$blocked)
    radiation_intensity <- cos(sun_track$zenith[frame]) # Higher when sun is higher

    if (frame == 1) {
      radiation_accum[frame] <- gap_fraction * radiation_intensity
    } else {
      radiation_accum[frame] <- radiation_accum[frame - 1] +
        gap_fraction * radiation_intensity
    }
  } else {
    # Empty data frame for frames where sun is not visible in slice
    ray_data_list[[frame]] <- data.frame(
      frame = integer(0),
      ground_x = numeric(0),
      sun_x = numeric(0),
      sun_y = numeric(0),
      blocked = logical(0)
    )
    radiation_accum[frame] <- ifelse(frame == 1, 0, radiation_accum[frame - 1])
  }
}

# Combine ray data
ray_data <- do.call(rbind, ray_data_list)

# Normalize radiation accumulation to 0-1 range
sun_track$radiation <- radiation_accum / max(radiation_accum, na.rm = TRUE)

# Replicate canopy points for each frame
canopy_anim <- do.call(
  rbind,
  lapply(seq_len(nrow(sun_track)), function(f) {
    df <- slice_points[, c("horiz_dist", "height")]
    df$frame <- f
    df
  })
)

message(sprintf(
  "  Generated %d ray segments across %d frames",
  nrow(ray_data),
  nrow(sun_track)
))

# =============================================================================
# SECTION 6: Create Animation Plot
# =============================================================================

message("Creating animation...")

# Build the base plot
p <- ggplot() +
  # Ground line
  geom_hline(yintercept = 0, color = "saddlebrown", linewidth = 1.5) +

  # Sun arc (full day path) - faint background (static, no frame column)
  geom_path(
    data = sun_track[sun_track$in_slice_plane, c("sun_x", "sun_y")],
    aes(x = sun_x, y = sun_y),
    color = "gray70",
    linetype = "dashed",
    linewidth = 0.5
  ) +

  # Light rays (unblocked = yellow, blocked = transparent)
  geom_segment(
    data = ray_data,
    aes(x = sun_x, y = sun_y, xend = ground_x, yend = 0, alpha = !blocked),
    color = "gold",
    linewidth = 0.3
  ) +
  scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 0), guide = "none") +

  # Canopy points
  geom_point(
    data = canopy_anim,
    aes(x = horiz_dist, y = height),
    color = "darkgreen",
    size = 0.8,
    alpha = 0.7
  ) +

  # Sun circle
  geom_point(
    data = sun_track,
    aes(x = sun_x, y = sun_y),
    color = "orange",
    fill = "yellow",
    shape = 21,
    size = 8,
    stroke = 2
  ) +

  # Time label
  geom_label(
    data = sun_track,
    aes(x = x_range[2], y = y_range[2] * 0.95, label = time_label),
    hjust = 1,
    vjust = 1,
    size = 5,
    label.size = 0
  ) +

  # Radiation meter (bar on right side)
  geom_rect(
    data = sun_track,
    aes(
      xmin = x_range[2] * 1.02,
      xmax = x_range[2] * 1.08,
      ymin = 0,
      ymax = radiation * y_range[2] * 0.8
    ),
    fill = "gold",
    color = "darkorange"
  ) +

  # Radiation meter outline
  annotate(
    "rect",
    xmin = x_range[2] * 1.02,
    xmax = x_range[2] * 1.08,
    ymin = 0,
    ymax = y_range[2] * 0.8,
    fill = NA,
    color = "gray40",
    linewidth = 0.5
  ) +

  # Labels
  labs(
    x = "Horizontal Distance from Plot Center (m)",
    y = "Height Above Ground (m)",
    title = sprintf(
      "Solar Radiation Through Canopy - Day %d, Azimuth %d deg",
      day_of_year,
      target_azimuth
    )
  ) +

  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10) # Extra right margin for radiation bar
  ) +

  coord_fixed(
    xlim = c(x_range[1], x_range[2] * 1.15),
    ylim = y_range,
    expand = FALSE
  ) +

  # Animation by frame
  transition_manual(frame) +
  ease_aes("linear")

# =============================================================================
# SECTION 7: Render GIF
# =============================================================================

message("Rendering GIF to: ", output_gif)

anim <- animate(
  p,
  nframes = nrow(sun_track),
  fps = animation_fps,
  width = animation_width,
  height = animation_height,
  renderer = gifski_renderer()
)

anim_save(output_gif, animation = anim)

message("Done! Animation saved to: ", output_gif)
message(sprintf(
  "  Frames: %d, FPS: %d, Duration: %.1f seconds",
  nrow(sun_track),
  animation_fps,
  nrow(sun_track) / animation_fps
))
