rm(list = ls())

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
renv::activate()
source("utils.R", chdir = TRUE)

# Kernlibraries
load_libs(c(
  "sf", "geodata", "terra", "elevatr",
  "png", "rayshader", "magick",
  "osmdata", "ggplot2", "dplyr", "purrr"
))

# Overpass-Server setzen
set_overpass_url("https://overpass.kumi.systems/api/interpreter")

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
area <- "bochum"   # "bochum" oder "ruhrgebiet"

# ------------------------------------------------------------
# GET ADMIN BOUNDARIES (GADM LEVEL 3)
# ------------------------------------------------------------
country_borders <- get_country_borders("DEU", 3)
country_borders <- st_make_valid(country_borders)

nrw <- country_borders %>%
  filter(NAME_1 == "Nordrhein-Westfalen")

# Ruhrgebiet
ruhrgebiet <- nrw %>%
  filter(
    NAME_2 %in% c(
      "Bochum", "Bottrop", "Dortmund", "Duisburg", "Essen", "Gelsenkirchen",
      "Hagen", "Hamm", "Herne", "Mülheim an der Ruhr", "Oberhausen",
      "Recklinghausen", "Unna", "Wesel", "Ennepe-Ruhr-Kreis"
    )
  )

# Bochum extrahieren
bochum <- nrw %>% filter(NAME_2 == "Bochum")

# sf → terra
bochum_sf <- st_make_valid(bochum)
bochum_terra <- terra::vect(bochum_sf)

# ------------------------------------------------------------
# LOAD RASTER (CLC+)
# ------------------------------------------------------------
clc <- terra::rast(
  "data/clc/CLMS_CLCPLUS_RAS_S2023_R10m_E41N31_03035_V01_R00.tif"
)

# ------------------------------------------------------------
# REPROJECT BOCHUM → CRS DES RASTERS
# ------------------------------------------------------------
bochum_terra <- terra::project(bochum_terra, terra::crs(clc))

# ------------------------------------------------------------
# CROP + MASK
# ------------------------------------------------------------
country_clc <- terra::crop(
  clc,
  bochum_terra,
  mask = TRUE,
  snap = "in"
)

# ------------------------------------------------------------
# RECLASSIFY AND SIMPLIFY CATEGORIES
# ------------------------------------------------------------
rcl_matrix <- matrix(c(
  1,  1,   # Sealed -> Urban/Built-up
  2,  2,   # Needle-leaved trees -> Coniferous forest
  3,  3,   # Broadleaved deciduous -> Deciduous forest
  4,  3,   # Broadleaved evergreen -> Deciduous forest (merge)
  5,  4,   # Low-growing woody -> Shrubland
  6,  5,   # Permanent herbaceous -> Grassland/Meadow
  7,  6,   # Periodically herbaceous -> Agricultural land
  8,  4,   # Lichens/mosses -> Shrubland (merge)
  9,  7,   # Non/sparsely vegetated -> Bare ground
  10, 8,   # Water -> Water
  11, 7,   # Snow/ice -> Bare ground (merge)
  253, 9,  # Coastal buffer -> No data
  254, 9,  # Outside area -> No data
  255, 9   # No data -> No data
), ncol = 2, byrow = TRUE)

# Reclassify
country_forest_simplified <- terra::classify(
  country_clc,
  rcl_matrix,
  right = FALSE
)

# ------------------------------------------------------------
# REPROJECT TO LAMBERT
# ------------------------------------------------------------
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_forest_simplified <- country_forest_simplified %>%
  terra::project(crs_lambert)

# ------------------------------------------------------------
# DEFINE COLORS
# ------------------------------------------------------------
cols <- c(
  "#4a4a4a",  # 1: Urban/Sealed - dark gray
  "#0d5c3f",  # 2: Coniferous forest - deep green
  "#52a447",  # 3: Deciduous forest - bright green
  "#8fbc8f",  # 4: Shrubland - light olive green
  "#c8d96f",  # 5: Grassland - yellow-green
  "#e8c547",  # 6: Agricultural - golden yellow
  "#d4a574",  # 7: Bare ground - tan/brown
  "#1e90ff"   # 8: Water - bright blue
)

cat_names <- c(
  "Urban/Built-up",
  "Coniferous Forest", 
  "Deciduous Forest",
  "Shrubland",
  "Grassland/Meadow",
  "Agricultural Land",
  "Bare Ground",
  "Water"
)

# ------------------------------------------------------------
# CREATE RGB RASTER
# ------------------------------------------------------------
from <- 1:8
to <- t(col2rgb(cols))

lut <- data.frame(
  ID = from,
  R  = to[,1],
  G  = to[,2],
  B  = to[,3]
)

print("Color LUT:")
print(lut)

forest_terra <- na.omit(country_forest_simplified)

# Initialize RGB layers
r_layer <- forest_terra
g_layer <- forest_terra
b_layer <- forest_terra

# Set all to 0 first
r_layer[] <- 0
g_layer[] <- 0
b_layer[] <- 0

# Apply colors for each class
for (i in 1:nrow(lut)) {
  mask <- forest_terra == lut$ID[i]
  r_layer[mask] <- lut$R[i]
  g_layer[mask] <- lut$G[i]
  b_layer[mask] <- lut$B[i]
  
  n_pixels <- sum(terra::values(mask), na.rm = TRUE)
  cat(sprintf("Class %d (%s): %d pixels assigned RGB(%d,%d,%d)\n", 
              lut$ID[i], 
              cat_names[i],
              n_pixels,
              lut$R[i], lut$G[i], lut$B[i]))
}

# Combine into RGB raster
forest_type_rgb <- c(r_layer, g_layer, b_layer)
names(forest_type_rgb) <- c("R","G","B")

img_file <- "data/clc/forest_type_image.png"
terra::writeRaster(
  forest_type_rgb, 
  img_file, 
  overwrite = TRUE,
  datatype = "INT1U",
  NAflag = 255
)

# Read back as PNG
img <- png::readPNG(img_file)

# Print color legend
cat("\n=== COLOR LEGEND ===\n")
for(i in 1:length(cat_names)) {
  cat(sprintf("%d: %s - %s\n", i, cat_names[i], cols[i]))
}

# ------------------------------------------------------------
# GET ELEVATION DATA
# ------------------------------------------------------------
elev <- elevatr::get_elev_raster(
  location = bochum_sf,
  z = 12,
  clip = "locations"
)

elev_lambert <- elev %>%
  terra::rast() %>%
  terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(elev_lambert)

cat("Elevation range:", range(elmat, na.rm = TRUE), "\n")

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

# ------------------------------------------------------------
# OVERLAY FUNCTION
# ------------------------------------------------------------
overlay_image <- function(base_texture, overlay_img, alpha = 0.8) {
  overlay_resized <- array(0, dim = dim(base_texture))
  base_dims <- dim(base_texture)
  img_dims <- dim(overlay_img)
  
  scale_h <- img_dims[1] / base_dims[1]
  scale_w <- img_dims[2] / base_dims[2]
  
  for(i in 1:base_dims[1]) {
    for(j in 1:base_dims[2]) {
      img_i <- min(ceiling(i * scale_h), img_dims[1])
      img_j <- min(ceiling(j * scale_w), img_dims[2])
      for(k in 1:3) {
        overlay_resized[i, j, k] <- overlay_img[img_i, img_j, k]
      }
    }
  }
  
  blended <- base_texture * (1 - alpha) + overlay_resized * alpha
  return(blended)
}

# ------------------------------------------------------------
# CREATE TEXTURE WITH SHADOWS
# ------------------------------------------------------------
base_map <- elmat %>% 
  rayshader::height_shade(
    texture = colorRampPalette("white")(512)
  ) %>% 
  rayshader::add_shadow(
    rayshader::lamb_shade(
      elmat,
      zscale = 45,
      sunaltitude = 45,
      sunangle = 315
    ), 
    max_darken = 0.4
  ) %>% 
  rayshader::add_shadow(
    rayshader::texture_shade(
      elmat,
      detail = 0.8,
      brightness = 90,
      contrast = 80
    ), 
    max_darken = 0.2
  )

# ------------------------------------------------------------
# OVERLAY FOREST TYPE IMAGE
# ------------------------------------------------------------
final_texture <- overlay_image(base_map, img, alpha = 0.8)

# ------------------------------------------------------------
# RENDER 3D PLOT
# ------------------------------------------------------------
final_texture %>% 
  rayshader::plot_3d(
    elmat, 
    zscale = 20,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 0.5,
    background = "white",
    windowsize = c(800, 800),
    zoom = 0.6,
    phi = 50,
    theta = 0
  )

# ------------------------------------------------------------
# RENDER HIGH QUALITY IMAGE
# ------------------------------------------------------------
# Create maps directory if it doesn't exist
if(!dir.exists("maps")) dir.create("maps")

rayshader::render_highquality(
  filename = "maps/bochum-clc-3d.png",
  preview = TRUE,
  light = FALSE,
  interactive = FALSE,
  parallel = TRUE,
  width = w * 2,  # Higher resolution
  height = h * 2
)

# ------------------------------------------------------------
# CREATE LEGEND
# ------------------------------------------------------------
png("maps/bochum_legend.png", width = 800, height = 600, bg = "transparent")
par(family = "sans")
plot(
  NULL, xaxt = "n",
  yaxt = "n", bty = "n",
  ylab = "", xlab = "",
  xlim = 0:1, ylim = 0:1,
  xaxs = "i", yaxs = "i"
)
legend(
  "center",
  legend = cat_names,
  pch = 16,
  pt.cex = 3,
  cex = 1.5,
  bty = "n",
  col = cols,
  title = "Land Cover - Bochum",
  title.cex = 1.8
)
dev.off()

# ------------------------------------------------------------
# COMBINE MAP AND LEGEND
# ------------------------------------------------------------
bochum_img <- magick::image_read("maps/bochum-clc-3d.png")
my_legend <- magick::image_read("maps/bochum_legend.png")

my_legend_scaled <- magick::image_scale(
  my_legend, 
  "x1000"  # Scale to fit nicely
)

final_map <- magick::image_composite(
  bochum_img,
  my_legend_scaled,
  gravity = "northeast",  # Position in top-right
  offset = "+50+50"       # 50px padding from edges
)

magick::image_write(
  final_map,
  "maps/bochum-final-map.png"
)

cat("\n=== RENDERING COMPLETE ===\n")
cat("Files created:\n")
cat("  - maps/bochum-clc-3d.png (high quality 3D render)\n")
cat("  - maps/bochum_legend.png (legend)\n")
cat("  - maps/bochum-final-map.png (final map with legend)\n")
