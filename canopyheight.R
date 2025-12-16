rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
renv::activate()
source("utils.R", chdir = TRUE)

# Kernlibraries
load_libs(c(
  "tidyverse", "sf", "geodata", "terra",
  "classInt", "rayshader", "ggplot2"
))

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
area <- "germany" #"bochum" #"ruhrgebiet 
area <- tolower(area)
stopifnot(area %in% c("germany", "ruhrgebiet", "bochum"))

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
    message("Already present, skipping download: ", dest_path)
  }
}

raster_files <- full_dest_files[file.exists(full_dest_files)]
# 
# # ------------------------------------------------------------
# # GET ADMIN BOUNDARIES
# # ------------------------------------------------------------
# country_borders <- get_country_borders("DEU", 3)
# country_borders <- st_make_valid(country_borders)
# 
# nrw <- country_borders %>%
#   filter(NAME_1 == "Nordrhein-Westfalen")
# 
# bochum <- nrw %>% 
#   filter(NAME_2 == "Bochum") %>%
#   st_union()
# 

# ------------------------------------------------------------
# GET ADMIN BOUNDARIES (AOI)
# ------------------------------------------------------------
country_borders <- get_country_borders("DEU", 3) |> 
  st_make_valid()

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
  "germany"     = st_union(country_borders),
  "ruhrgebiet"  = ruhrgebiet,
  "bochum"      = bochum
)

aoi_name <- area





# ------------------------------------------------------------
# LOAD AND PROCESS CANOPY HEIGHT
# ------------------------------------------------------------
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

agg_factor <- switch(
  area,
  "germany"    = 20,
  "ruhrgebiet" = 10,
  "bochum"     = 5
)

forest_height <- forest_height_mosaic |>
  terra::aggregate(fact = agg_factor)

# ------------------------------------------------------------
# CONVERT TO DATAFRAME
# ------------------------------------------------------------
forest_height_df <- forest_height %>%
  as.data.frame(xy = TRUE)
names(forest_height_df)[3] <- "height"

# Remove zeros and NAs
forest_height_df <- forest_height_df %>%
  filter(!is.na(height), height > 0)

cat("Height range:", range(forest_height_df$height, na.rm = TRUE), "\n")

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

cat("Raster dimensions: w =", w, ", h =", h, "\n")

# Version without legend for 3D
p_no_legend <- p + theme(legend.position = "none")

# Adjust parameters based on area
rayshader_params <- switch(
  area,
  "bochum" = list(
    scale = 60,
    zoom = 0.7,
    phi = 45,
    theta = 0,
    width_div = 500,
    height_div = 500
  ),
  "ruhrgebiet" = list(
    scale = 80,
    zoom = 0.6,
    phi = 40,
    theta = 0,
    width_div = 400,
    height_div = 400
  ),
  "germany" = list(
    scale = 150,      # Much higher scale for larger area
    zoom = 0.4,       # Zoom out to see whole country
    phi = 35,         # Flatter viewing angle
    theta = -15,      # Slight rotation for better perspective
    width_div = 300,  # Smaller divisions for better performance
    height_div = 300
  )
)

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
# ------------------------------------------------------------
# RENDER SNAPSHOTS - A2 POSTER DIMENSIONS
# ------------------------------------------------------------
if(!dir.exists("maps")) dir.create("maps")
quick_file <- sprintf("maps/%s-canopy-height-quick.png", aoi_name)
hq_file    <- sprintf("maps/%s-canopy-height-hq.png", aoi_name)

# A2 poster dimensions
dpi <- 300
width_cm <- 40.3
height_cm <- 46.32

# Convert cm to pixels
width_px <- round((width_cm / 2.54) * dpi)
height_px <- round((height_cm / 2.54) * dpi)

cat("\nRender dimensions:\n")
cat(sprintf("  Width:  %d px (%.1f cm @ %d dpi)\n", width_px, width_cm, dpi))
cat(sprintf("  Height: %d px (%.1f cm @ %d dpi)\n", height_px, height_cm, dpi))

# Quick snapshot
rayshader::render_snapshot(
  filename = quick_file,
  width = width_px,
  height = height_px
)

# High quality render - adjusted lighting for different areas
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
    lightdirection = c(315, 310, 280, 320),  # More varied lighting
    lightintensity = c(1500, 2000, 300, 200),  # Brighter for larger area
    lightaltitude = c(25, 25, 75, 75)  # Higher sun angle
  )
)

rayshader::render_highquality(
  filename = hq_file,
  preview = TRUE,
  interactive = FALSE,
  light = TRUE,
  lightdirection = lighting_params$lightdirection,
  lightintensity = lighting_params$lightintensity,
  lightaltitude = lighting_params$lightaltitude,
  ground_material = rayrender::microfacet(roughness = 0.6),
  width = width_px,
  height = height_px
)

cat("\n=== RENDERING COMPLETE ===\n")
cat("Files created:\n")
cat(sprintf("  - %s (%d x %d px)\n", quick_file, width_px, height_px))
cat(sprintf("  - %s (%d x %d px)\n", hq_file, width_px, height_px))