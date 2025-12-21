#######################################################
# Population Map - SAVES camera settings for forest map
# Run this FIRST to establish the visual style
#######################################################
rm(list = ls())

libs <- c(
  "tidyverse", "R.utils",
  "httr", "sf", "stars",
  "rayshader"
)

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
area <- "germany"  # "bochum" # "ruhrgebiet"
area <- tolower(area)
PREVIEW_MODE <- FALSE  # Set FALSE for final render

cat("\n=== POPULATION MAP (MASTER) ===\n")
cat("This will save camera settings for the forest map\n\n")

# ------------------------------------------------------------
# DOWNLOAD & UNZIP DATA
# ------------------------------------------------------------
url <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_DE_20220630.gpkg.gz"
file_name <- "data/germany-population.gpkg.gz"

if (!dir.exists("data")) dir.create("data", recursive = TRUE)

if (!file.exists(file_name)) {
  res <- httr::GET(url, write_disk(file_name), progress())
  R.utils::gunzip(file_name, remove = FALSE)
}

load_file_name <- gsub(".gz", "", file_name)
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

pop_sf <- sf::st_read(load_file_name) |>
  sf::st_transform(crs = crsLAEA)

# ------------------------------------------------------------
# LOAD AOI
# ------------------------------------------------------------
# country_borders <- readRDS("data/gadm/gadm41_DEU_3_pk.rds") |>
#   sf::st_as_sf() |>
#   st_make_valid()

country_borders <- readRDS("data/gadm/gadm41_DEU_3_pk.rds")

# Check if it's packed and unwrap if needed
if (inherits(country_borders, "PackedSpatVector")) {
  country_borders <- terra::unwrap(country_borders)
}

# Now convert to sf
country_borders <- country_borders |>
  sf::st_as_sf() |>
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
  "germany"    = st_union(country_borders),
  "ruhrgebiet" = ruhrgebiet,
  "bochum"     = bochum
)

# Crop to AOI
pop_sf <- st_intersection(pop_sf, aoi)

# ------------------------------------------------------------
# SHP TO RASTER
# ------------------------------------------------------------
bb <- sf::st_bbox(pop_sf)

get_raster_size <- function() {
  height <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
  )
  width <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
  )
  if (height > width) {
    height_ratio <- 1
    width_ratio <- width / height
  } else {
    width_ratio <- 1
    height_ratio <- height / width
  }
  return(list(width_ratio, height_ratio))
}

width_ratio <- get_raster_size()[[1]]
height_ratio <- get_raster_size()[[2]]

if (PREVIEW_MODE) {
  size <- 1500
} else {
  size <- 3000
}

width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

pop_rast <- stars::st_rasterize(
  pop_sf |> dplyr::select(population, geom),
  nx = width, ny = height
)

pop_mat <- pop_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

cols <- rev(c(
  "#a24444", "#8e7cc3",
   "#cd7a23", "#7da0c1"
  
  #"#0b1354", "#283680",
  #"#6853a9", "#c863b3"
))
texture <- grDevices::colorRampPalette(cols)(256)

# ------------------------------------------------------------
# DEFINE CAMERA & LIGHTING SETTINGS (MASTER SETTINGS)
# ------------------------------------------------------------

# Camera settings per area
camera_settings <- switch(
  area,
  "bochum" = list(
    phi = 65,
    theta = -30,
    zoom = 0.7,
    zscale = 15
  ),
  "ruhrgebiet" = list(
    phi = 60,
    theta = -20,
    zoom = 0.65,
    zscale = 15
  ),
  "germany" = list(
    phi = 75,
    theta = 0,
    zoom = 0.7,
    zscale = 15
  )
)

# Lighting settings per area
lighting_settings <- switch(
  area,
  "bochum" = list(
    lightdirection = 225,
    lightaltitude = 60,
    lightintensity = 400
  ),
  "ruhrgebiet" = list(
    lightdirection = 225,
    lightaltitude = 60,
    lightintensity = 450
  ),
  "germany" = list(
    lightdirection = 225,
    lightaltitude = 60,
    lightintensity = 400
  )
)

# ------------------------------------------------------------
# CREATE 3D OBJECT
# ------------------------------------------------------------
pop_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat,
    solid = FALSE,
    soliddepth = 0,
    zscale = camera_settings$zscale,
    shadowdepth = 0,
    shadow_darkness = 0.95,
    windowsize = c(800, 800),
    phi = camera_settings$phi,
    zoom = camera_settings$zoom,
    theta = camera_settings$theta,
    background = "white"
)

# Apply camera settings
rayshader::render_camera(
  phi = camera_settings$phi,
  zoom = camera_settings$zoom,
  theta = camera_settings$theta
)

# ------------------------------------------------------------
# SAVE SETTINGS FOR FOREST MAP TO USE
# ------------------------------------------------------------
if (!dir.exists("data/cache")) dir.create("data/cache", recursive = TRUE)
settings_file <- sprintf("data/cache/%s_camera_settings.rds", area)

saveRDS(list(
  camera = camera_settings,
  lighting = lighting_settings,
  area = area
), settings_file)

cat(sprintf("\n✓ Camera settings saved to: %s\n", settings_file))
cat("  Forest map will use these exact settings!\n\n")

# ------------------------------------------------------------
# RENDER
# ------------------------------------------------------------
if (!dir.exists("maps")) dir.create("maps")

if (PREVIEW_MODE) {
  dpi <- 150
  suffix <- "preview"
} else {
  dpi <- 300
  suffix <- "final"
}

width_cm <- 40.3
height_cm <- 46.32
width_px <- round((width_cm / 2.54) * dpi)
height_px <- round((height_cm / 2.54) * dpi)

# Quick snapshot
quick_file <- sprintf("maps/%s-population-quick-%s.png", area, suffix)
rayshader::render_snapshot(
  filename = quick_file,
  width = width_px,
  height = height_px,
  software_render = FALSE
)
cat(sprintf("✓ Quick: %s\n", quick_file))

# High quality
hq_file <- sprintf("maps/%s-population-hq-%s.png", area, suffix)

if (PREVIEW_MODE) {
  samples <- 200
  min_variance <- 1e-4
} else {
  samples <- 500
  min_variance <- 1e-6
}

rayshader::render_highquality(
  filename = hq_file,
  preview = FALSE,
  interactive = FALSE,
  parallel = TRUE,
  light = TRUE,
  lightdirection = lighting_settings$lightdirection,
  lightaltitude = lighting_settings$lightaltitude,
  lightintensity = lighting_settings$lightintensity,
  samples = samples,
  min_variance = min_variance,
  sample_method = "sobol",
  width = width_px,
  height = height_px,
  background = "white"
)

cat(sprintf("✓ HQ: %s\n", hq_file))

cat("\n=== POPULATION MAP COMPLETE ===\n")
cat("Now run the forest script to create matching map!\n")

 # Optimized spike map rendering
# # Milos Popovic's original code with performance improvements
# rm(list = ls())
# libs <- c(
#   "tidyverse", "R.utils",
#   "httr", "sf", "stars",
#   "rayshader"
# )
# 
# installed_libs <- libs %in% rownames(installed.packages())
# if (any(installed_libs == F)) {
#   install.packages(libs[!installed_libs])
# }
# 
# invisible(lapply(libs, library, character.only = T))
# 
# ### 1. DOWNLOAD & UNZIP DATA
# url <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_DE_20220630.gpkg.gz"
# file_name <- "germany-population.gpkg.gz"
# 
# get_population_data <- function() {
#   res <- httr::GET(
#     url,
#     write_disk(file_name),
#     progress()
#   )
#   R.utils::gunzip(file_name, remove = F)
# }
# 
# # Only download if not already present
# if (!file.exists(gsub(".gz", "", file_name))) {
#   get_population_data()
# }
# 
# ### 2. LOAD DATA
# load_file_name <- gsub(".gz", "", file_name)
# crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
# 
# get_population_data <- function() {
#   pop_df <- sf::st_read(load_file_name) |>
#     sf::st_transform(crs = crsLAEA)
# }
# 
# pop_sf <- get_population_data()
# 
# ### 3. SHP TO RASTER
# bb <- sf::st_bbox(pop_sf)
# 
# get_raster_size <- function() {
#   height <- sf::st_distance(
#     sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
#     sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
#   )
#   width <- sf::st_distance(
#     sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
#     sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
#   )
#   if (height > width) {
#     height_ratio <- 1
#     width_ratio <- width / height
#   } else {
#     width_ratio <- 1
#     height_ratio <- height / width
#   }
#   return(list(width_ratio, height_ratio))
# }
# 
# width_ratio <- get_raster_size()[[1]]
# height_ratio <- get_raster_size()[[2]]
# 
# # OPTIMIZATION: Choose resolution based on mode
# PREVIEW_MODE <- TRUE  # Set to FALSE for final high-quality render
# 
# if (PREVIEW_MODE) {
#   size <- 1000  # Fast preview - renders in ~5-10 minutes
# } else {
#   size <- 4000  # High quality for poster - may take 30-60 minutes
# }
# 
# width <- round((size * width_ratio), 0)
# height <- round((size * height_ratio), 0)
# 
# cat("Rendering at", width, "x", height, "pixels\n")
# 
# get_population_raster <- function() {
#   pop_rast <- stars::st_rasterize(
#     pop_sf |>
#       dplyr::select(population, geom),
#     nx = width, ny = height
#   )
#   return(pop_rast)
# }
# 
# pop_rast <- get_population_raster()
# pop_mat <- pop_rast |>
#   as("Raster") |>
#   rayshader::raster_to_matrix()
# 
# cols <- rev(c(
#   "#a24444", "#8e7cc3",
#   "#cd7a23", "#7da0c1"
# ))
# texture <- grDevices::colorRampPalette(cols)(256)
# 
# # Create the 3D object
# pop_mat |>
#   rayshader::height_shade(texture = texture) |>
#   rayshader::plot_3d(
#     heightmap = pop_mat,
#     solid = FALSE,
#     soliddepth = 0,
#     zscale = 15,
#     shadowdepth = 0,
#     shadow_darkness = .95,
#     windowsize = c(800, 800),
#     phi = 65,
#     zoom = .65,
#     theta = -30,
#     background = "white"
#   )
# 
# rayshader::render_camera(phi = 75, zoom = .7, theta = 0)
# 
# # OPTIMIZATION: Choose rendering method
# if (PREVIEW_MODE) {
#   # FAST METHOD: Snapshot rendering (~seconds)
#   cat("Using fast snapshot rendering...\n")
#   rayshader::render_snapshot(
#     filename = "maps/germany_population_preview.png",
#     width = width,
#     height = height,
#     software_render = FALSE
#   )
# } else {
#   # HIGH QUALITY METHOD: Raytracing with optimizations
#   cat("Using high-quality raytracing (this will take time)...\n")
#   rayshader::render_highquality(
#     filename = "maps/germany_population_2022_final.png",
#     preview = FALSE,  # Disable preview window
#     parallel = TRUE,  # Use multiple cores
#     light = TRUE,
#     lightdirection = 225,
#     lightaltitude = 60,
#     lightintensity = 400,
#     interactive = FALSE,
#     samples = 400,  # Reduced from default ~1000
#     min_variance = 1e-5,  # Stop early if converged
#     sample_method = "sobol",  # Faster sampling method
#     width = width,
#     height = height
#   )
# }
# 
# cat("Rendering complete!\n")
# 
# # For 46.3x40.3cm poster at 300 DPI:
# # Final size should be approximately 5500x4800 pixels
# # Set size <- 5000 for final render