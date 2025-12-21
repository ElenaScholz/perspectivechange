rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
renv::activate()
source("utils.R", chdir = TRUE)

load_libs(c(
  "tidyverse", "sf", "geodata", "terra",
  "classInt", "rayshader", "ggplot2"
))

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
area <- "germany" # "bochum" # "ruhrgebiet"
area <- tolower(area)
stopifnot(area %in% c("germany", "ruhrgebiet", "bochum"))

# OPTIMIZATION: Preview vs Final mode
PREVIEW_MODE <- TRUE  # Set FALSE for final high-quality render
cat("\n=== FOREST MAP (MATCHES POPULATION) ===\n")
cat("MODE:", ifelse(PREVIEW_MODE, "PREVIEW", "FINAL"), "\n\n")

# LOAD CAMERA SETTINGS FROM POPULATION SCRIPT
settings_file <- sprintf("data/cache/%s_camera_settings.rds", area)

if (!file.exists(settings_file)) {
  cat("⚠ WARNING: No saved camera settings found!\n")
  cat("Run the population script first to establish camera angles.\n")
  cat("Continuing with default settings...\n\n")
  USE_SAVED_SETTINGS <- FALSE
} else {
  saved_settings <- readRDS(settings_file)
  cat("✓ Loading camera settings from population script\n\n")
  USE_SAVED_SETTINGS <- TRUE
}

# ------------------------------------------------------------
# DOWNLOAD CANOPY HEIGHT DATA
# ------------------------------------------------------------

urls <- c(
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N54E006_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N54E009_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N54E012_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N51E003_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N51E006_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N51E009_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N51E012_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N51E015_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N48E003_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N48E006_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N48E009_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N48E012_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N45E006_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N45E006_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N45E012_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N45E009_Map.tif"
)

dest_files <- sub(".*files=", "", urls)
data_dir <- "data/canopy"
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

force_download <- FALSE
full_dest_files <- file.path(data_dir, dest_files)

for (i in seq_along(urls)) {
  dest_path <- full_dest_files[i]
  if (!file.exists(dest_path) || isTRUE(force_download)) {
    message("Downloading: ", dest_files[i])
    download.file(urls[i], destfile = dest_path, mode = "wb", quiet = FALSE)
  } else {
    message("Already present, skipping: ", basename(dest_path))
  }
}

raster_files <- full_dest_files[file.exists(full_dest_files)]

# ------------------------------------------------------------
# GET ADMIN BOUNDARIES (AOI) - Using local RDS file
# ------------------------------------------------------------
cat("Loading admin boundaries from RDS file...\n")

# Load the local GADM file and convert SpatVector to sf
country_borders <- readRDS("data/gadm/gadm41_DEU_3_pk.rds") |>
  sf::st_as_sf() |>  # Convert terra SpatVector to sf
  st_make_valid()

cat("Loaded", nrow(country_borders), "administrative regions\n")

nrw <- country_borders |>
  filter(NAME_1 == "Nordrhein-Westfalen")

ruhrgebiet <- nrw |>
  filter(NAME_2 %in% c(
    "Bochum", "Dortmund", "Essen", "Duisburg", "Gelsenkirchen",
    "Oberhausen", "Herne", "Mülheim an der Ruhr", "Bottrop"
  )) |>
  st_union()

bochum <- nrw |>
  filter(NAME_2 == "Bochum") |>
  st_union()

aoi <- switch(
  area,
  "germany"    = st_union(country_borders),
  "ruhrgebiet" = ruhrgebiet,
  "bochum"     = bochum
)

aoi_name <- area

# ------------------------------------------------------------
# LOAD AND PROCESS CANOPY HEIGHT
# ------------------------------------------------------------
cat("Loading and processing rasters...\n")

forest_height_list <- lapply(raster_files, terra::rast)

forest_height_rasters <- lapply(
  forest_height_list,
  function(x) {
    terra::crop(
      x,
      terra::vect(aoi),
      snap = "in",
      mask = TRUE
    )
  }
)

if (length(forest_height_rasters) > 1) {
  forest_height_mosaic <- do.call(terra::mosaic, forest_height_rasters)
} else {
  forest_height_mosaic <- forest_height_rasters[[1]]
}

# OPTIMIZATION: Adaptive aggregation based on mode and area
base_agg_factor <- switch(
  area,
  "germany"    = 20,
  "ruhrgebiet" = 10,
  "bochum"     = 5
)

# Increase aggregation in preview mode for faster processing
agg_factor <- if (PREVIEW_MODE) {
  base_agg_factor * 2  # 2x more aggregation in preview
} else {
  base_agg_factor
}

cat(sprintf("Aggregation factor: %d (base: %d)\n", agg_factor, base_agg_factor))

forest_height <- forest_height_mosaic |>
  terra::aggregate(fact = agg_factor)

# ------------------------------------------------------------
# CONVERT TO DATAFRAME
# ------------------------------------------------------------
cat("Converting to dataframe...\n")

forest_height_df <- forest_height %>%
  as.data.frame(xy = TRUE)
names(forest_height_df)[3] <- "height"

# Remove zeros and NAs
forest_height_df <- forest_height_df %>%
  filter(!is.na(height), height > 0)

cat("Height range:", range(forest_height_df$height, na.rm = TRUE), "\n")
cat("Data points:", nrow(forest_height_df), "\n")

# ------------------------------------------------------------
# BREAKS AND COLORS
# ------------------------------------------------------------
breaks <- classInt::classIntervals(
  forest_height_df$height,
  n = 5,
  style = "quantile"
)$brks

cols <- c(
  "white", "#ffd3af", "#fbe06e",
  "#6daa55", "#205544"
)

texture <- colorRampPalette(cols, bias = 2)(6)

# ------------------------------------------------------------
# CREATE GGPLOT
# ------------------------------------------------------------
cat("Creating ggplot...\n")

p <- ggplot(forest_height_df) +
  geom_tile(
    aes(x = x, y = y, fill = height)
  ) +
  geom_sf(
    data = aoi,
    fill = NA,
    color = "black",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    name = "height (m)",
    colors = texture,
    breaks = round(breaks, 0)
  ) +
  coord_sf(crs = 4326) +
  guides(
    fill = guide_colorbar(
      direction = "vertical",
      barheight = unit(50, "mm"),
      barwidth = unit(5, "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11, color = "grey10"),
    legend.text = element_text(size = 10, color = "grey10"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(fill = NA, color = "white"),
    plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")
  )

print(p)

# ------------------------------------------------------------
# RENDER 3D SCENE - ADJUSTED FOR AOI SIZE
# ------------------------------------------------------------
h <- nrow(forest_height)
w <- ncol(forest_height)

cat("\nRaster dimensions: w =", w, ", h =", h, "\n")

# Version without legend for 3D
p_no_legend <- p + theme(legend.position = "none")

# USE SAVED SETTINGS FROM POPULATION SCRIPT IF AVAILABLE
if (USE_SAVED_SETTINGS) {
  # Override with saved camera settings
  camera_from_pop <- saved_settings$camera
  
  rayshader_params <- list(
    scale = switch(area,
                   "bochum" = 60,
                   "ruhrgebiet" = 80,
                   "germany" = 150
    ),
    zoom = camera_from_pop$zoom,
    phi = camera_from_pop$phi,
    theta = camera_from_pop$theta,
    width_div = switch(area,
                       "bochum" = 500,
                       "ruhrgebiet" = 400,
                       "germany" = 300
    ),
    height_div = switch(area,
                        "bochum" = 500,
                        "ruhrgebiet" = 400,
                        "germany" = 300
    )
  )
  
  cat("Using camera settings from population map:\n")
  cat(sprintf("  phi: %.1f, theta: %.1f, zoom: %.2f\n", 
              rayshader_params$phi, rayshader_params$theta, rayshader_params$zoom))
  
} else {
  # Default settings if no saved settings
  rayshader_params <- switch(
    area,
    "bochum" = list(
      scale = 60, zoom = 0.7, phi = 45, theta = 0,
      width_div = 500, height_div = 500
    ),
    "ruhrgebiet" = list(
      scale = 80, zoom = 0.6, phi = 40, theta = 0,
      width_div = 400, height_div = 400
    ),
    "germany" = list(
      scale = 150, zoom = 0.4, phi = 35, theta = -15,
      width_div = 300, height_div = 300
    )
  )
}

cat("Building 3D scene...\n")

# Render 3D
rayshader::plot_gg(
  ggobj = p_no_legend,
  width = w / rayshader_params$width_div,
  height = h / rayshader_params$height_div,
  scale = rayshader_params$scale,
  solid = FALSE,
  soliddepth = 0,
  shadow = TRUE,
  shadow_intensity = 0.95,
  offset_edges = FALSE,
  sunangle = 315,
  zoom = rayshader_params$zoom,
  phi = rayshader_params$phi,
  theta = rayshader_params$theta,
  multicore = TRUE
)

# No need to save settings - population script is the master

# ------------------------------------------------------------
# RENDER SNAPSHOTS - ADAPTIVE DIMENSIONS
# ------------------------------------------------------------
if (!dir.exists("maps")) dir.create("maps")

# OPTIMIZATION: Adjust dimensions based on mode
if (PREVIEW_MODE) {
  # Preview: Lower resolution for fast iteration
  dpi <- 150  # Half the DPI
  width_cm <- 40.3
  height_cm <- 46.32
  suffix <- "preview"
  cat("\n=== PREVIEW MODE: Rendering at 150 DPI ===\n")
} else {
  # Final: Full resolution for poster
  dpi <- 300
  width_cm <- 40.3
  height_cm <- 46.32
  suffix <- "final"
  cat("\n=== FINAL MODE: Rendering at 300 DPI ===\n")
}

# Convert cm to pixels
width_px <- round((width_cm / 2.54) * dpi)
height_px <- round((height_cm / 2.54) * dpi)

cat(sprintf("Render dimensions: %d x %d px (%.1f x %.1f cm @ %d dpi)\n",
            width_px, height_px, width_cm, height_cm, dpi))

# Quick snapshot (always fast)
quick_file <- sprintf("maps/%s-canopy-height-quick-%s.png", aoi_name, suffix)
cat("\nRendering quick snapshot...\n")

rayshader::render_snapshot(
  filename = quick_file,
  width = width_px,
  height = height_px,
  software_render = FALSE
)

cat(sprintf("✓ Quick snapshot saved: %s\n", quick_file))

# High quality render with optimized settings
hq_file <- sprintf("maps/%s-canopy-height-hq-%s.png", aoi_name, suffix)

# USE SAVED LIGHTING FROM POPULATION SCRIPT IF AVAILABLE
if (USE_SAVED_SETTINGS) {
  lighting_params <- list(
    lightdirection = saved_settings$lighting$lightdirection,
    lightintensity = saved_settings$lighting$lightintensity,
    lightaltitude = saved_settings$lighting$lightaltitude
  )
  cat("Using lighting from population map\n")
} else {
  # Default lighting params
  lighting_params <- switch(
    area,
    "bochum" = list(
      lightdirection = c(315, 310, 315, 310),
      lightintensity = c(1000, 1500, 150, 100),
      lightaltitude = c(15, 15, 80, 80)
    ),
    "ruhrgebiet" = list(
      lightdirection = c(315, 310, 315, 310),
      lightintensity = c(1200, 1800, 200, 150),
      lightaltitude = c(20, 20, 80, 80)
    ),
    "germany" = list(
      lightdirection = c(315, 310, 280, 320),
      lightintensity = c(1500, 2000, 300, 200),
      lightaltitude = c(25, 25, 75, 75)
    )
  )
}

# OPTIMIZATION: Adaptive render quality
if (PREVIEW_MODE) {
  cat("\nRendering high-quality preview (optimized settings)...\n")
  samples <- 200
  min_variance <- 1e-4
} else {
  cat("\nRendering final high-quality image (this may take time)...\n")
  samples <- 500
  min_variance <- 1e-6
}

cat(sprintf("  Samples: %d\n", samples))
cat(sprintf("  Min variance: %.0e\n", min_variance))

rayshader::render_highquality(
  filename = hq_file,
  preview = FALSE,  # Don't show preview window
  interactive = FALSE,
  parallel = TRUE,  # Enable parallel processing
  light = TRUE,
  lightdirection = lighting_params$lightdirection,
  lightintensity = lighting_params$lightintensity,
  lightaltitude = lighting_params$lightaltitude,
  ground_material = rayrender::microfacet(roughness = 0.6),
  # OPTIMIZATION: Quality settings
  samples = samples,
  min_variance = min_variance,
  sample_method = "sobol",
  # Dimensions
  width = width_px,
  height = height_px
)

cat(sprintf("✓ High-quality render saved: %s\n", hq_file))

# ------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------
cat("\n=== RENDERING COMPLETE ===\n")
cat(sprintf("Mode: %s\n", ifelse(PREVIEW_MODE, "PREVIEW", "FINAL")))
cat(sprintf("Area: %s\n", aoi_name))
cat(sprintf("Aggregation factor: %d\n", agg_factor))
cat(sprintf("Resolution: %d x %d px (%.1f x %.1f cm @ %d dpi)\n",
            width_px, height_px, width_cm, height_cm, dpi))
cat("\nFiles created:\n")
cat(sprintf("  - %s\n", quick_file))
cat(sprintf("  - %s\n", hq_file))

if (PREVIEW_MODE) {
  cat("\n💡 TIP: Set PREVIEW_MODE = FALSE for final 300 DPI render\n")
}