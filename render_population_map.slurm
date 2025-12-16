#######################################################
# Fast local rayshader render (Windows)
#######################################################

# ---- PERFORMANCE -----------------------------------

options(rayrender.cores = max(1, parallel::detectCores() - 1))

# ---- TARGET PRINT SIZE ------------------------------

dpi_render <- 300   # render DPI (fast)
dpi_print  <- 600   # final DPI (after upscale)

width_cm  <- 40.32
height_cm <- 46.3

width_px_render  <- round((width_cm  / 2.54) * dpi_render)
height_px_render <- round((height_cm / 2.54) * dpi_render)

# ---- LIBRARIES -------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(stars)
  library(rayshader)
  library(tidyverse)
})

# ---- DATA ------------------------------------------

input_gpkg <- "germany-population.gpkg"
output_png <- "maps/germany_population_2022_300dpi.png"

crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

pop_sf <- st_read(input_gpkg, quiet = TRUE) |>
  st_transform(crsLAEA)

# ---- RASTERIZE -------------------------------------

bb <- st_bbox(pop_sf)

height_m <- st_distance(
  st_point(c(bb["xmin"], bb["ymin"])),
  st_point(c(bb["xmin"], bb["ymax"]))
)

width_m <- st_distance(
  st_point(c(bb["xmin"], bb["ymin"])),
  st_point(c(bb["xmax"], bb["ymin"]))
)

if (height_m > width_m) {
  height_ratio <- 1
  width_ratio  <- width_m / height_m
} else {
  width_ratio  <- 1
  height_ratio <- height_m / width_m
}

nx <- round(width_px_render  * width_ratio)
ny <- round(height_px_render * height_ratio)

pop_rast <- st_rasterize(
  pop_sf |> select(population, geom),
  nx = nx,
  ny = ny
)

pop_mat <- pop_rast |>
  as("Raster") |>
  raster_to_matrix()

# ---- CACHE (optional but recommended) --------------

saveRDS(pop_mat, "cache/pop_mat.rds")

# ---- COLORS ----------------------------------------

cols <- rev(c(
  "#0b1354",
  "#283680",
  "#6853a9",
  "#c863b3"
))

texture <- colorRampPalette(cols)(256)

# ---- RENDER ----------------------------------------

clear3d()

scene <- pop_mat |>
  height_shade(texture) |>
  add_shadow(ray_shade(pop_mat, zscale = 15), 0.5) |>
  add_shadow(ambient_shade(pop_mat, zscale = 15), 0)

render_highquality(
  filename = output_png,
  width  = width_px_render,
  height = height_px_render,
  samples = 48,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  clamp_value = 10
)

cat("Render completed:", output_png, "\n")
