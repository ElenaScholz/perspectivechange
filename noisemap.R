# https://github.com/milos-agathon/traffic-noise-maps/blob/main/R/main.r
rm(list = ls())
source("utils.R")
# Install pacman & remotes in renv
renv::install("pacman")
renv::install("remotes")
load_libs("remotes", "terra", "osmdata", "dplyr")

# -------------------------
# Select area
# -------------------------
area <- "bochum"  # "bochum" | "ruhrgebiet"

stopifnot(area %in% c("germany", "ruhrgebiet", "bochum"))

# -------------------------
# Load base geometry
# -------------------------
# ger <- readRDS("data/gadm/gadm41_DEU_3_pk.rds") %>%
#   sf::st_as_sf()
ger <- readRDS("data/gadm/gadm41_DEU_3_pk.rds")

# Check if it's packed and unwrap if needed
if (inherits(ger, "PackedSpatVector")) {
  ger <- terra::unwrap(ger)
}


# -------------------------
# Define AOI geometry
# -------------------------
if (area == "germany") {
  aoi <- ger
}
if (area == "bochum") {
  aoi <- ger[
    ger$NAME_1 == "Nordrhein-Westfalen" &
      ger$NAME_2 == "Bochum",
  ]
}


if (area == "ruhrgebiet") {
  aoi <- ger[
    ger$NAME_1 == "Nordrhein-Westfalen" &
      ger$NAME_2 %in% c(
        "Bochum", "Bottrop", "Dortmund", "Duisburg", "Essen",
        "Gelsenkirchen", "Hagen", "Hamm", "Herne",
        "Mülheim an der Ruhr", "Oberhausen",
        "Recklinghausen", "Unna", "Wesel",
        "Ennepe-Ruhr-Kreis"
      ),
  ]
}




# -------------------------
# Bounding box
# -------------------------
bbox <- sf::st_bbox(aoi)
# 
# 
noise_url <- "https://noise.discomap.eea.europa.eu/arcgis/rest/services/noiseStoryMap/NoiseContours_road_lden/ImageServer"

noise_data <- arcgislayers::arc_open(noise_url)

colors <- hcl.colors(
  n = 5,
  palette ="YlOrBr", #"Batlow",
  rev = TRUE
)
message("Processing AOI: ", area)

area_sf <- aoi
bbox <- sf::st_bbox(area_sf)

# tiles <- list(as.numeric(bbox)) 
tiles <- if (area == "bochum") {
  list(as.numeric(bbox))
} else {
  split_bbox(bbox)
}


#------------------------
# Noise raster
# ------------------------
res_m <- 50  # meters per pixel (same for Bochum and Ruhrgebiet)

width  <- 4000
height <- 4000

noise_raster <- arcgislayers::arc_raster(
    x = noise_data,
    xmin = bbox[["xmin"]],
    xmax = bbox[["xmax"]],
    ymin = bbox[["ymin"]],
    ymax = bbox[["ymax"]],
    crs = sf::st_crs(area_sf),
    width = width,
    height = height
)

library("terra")
library("dplyr")
noise_raster_clean <- terra::ifel(
    noise_raster %in% c(0, 15),
    NA,
    noise_raster
  )
library("osmdata")
  # ------------------------
  # OSM – major roads
  # ------------------------
highways <- get_osm_cached(
    paste0("data/highways_", area, ".rds"),
    function() {
      combine_osm(
        lapply(tiles, function(t)
          safe_osm(
            t,
            "highway",
            c("motorway", "trunk", "primary", "secondary")
          )
        )
      )
    }
  )
library(sf)
sel <- sf::st_as_sf(area_sf)

highways_clip  <- clip_to_sel(highways$osm_lines, sel)

# ------------------------
# Plot noise raster
# ------------------------
library(ggplot2)

# First, reproject aoi to match the raster's CRS
aoi_reproj <- terra::project(aoi, terra::crs(noise_raster_clean))

# Mask raster to actual Bochum boundary (not bounding box)
noise_raster_masked <- terra::mask(noise_raster_clean, aoi_reproj)

# Filter to only keep values 1-5
noise_raster_filtered <- terra::ifel(
  noise_raster_masked < 1 | noise_raster_masked > 5,
  NA,
  noise_raster_masked
)

# Convert to factor properly
noise_raster_factor <- terra::as.factor(noise_raster_filtered)

# Set up the factor levels manually for 1-5
rat <- data.frame(
  ID = 1:5,
  category = as.character(1:5)
)
levels(noise_raster_factor) <- rat

# Convert aoi to sf for ggplot
aoi_sf <- sf::st_as_sf(aoi_reproj)

p <- ggplot() +
  # Black background for Bochum area
  geom_sf(data = aoi_sf, fill = "black", color = NA) +
  # Noise raster on top
  tidyterra::geom_spatraster(
    data = noise_raster_factor
  ) +
  scale_fill_manual(
    name = "",
    values = colors[1:5],  # Only use first 5 colors
    na.value = "transparent",
    drop = FALSE,  # Keep all factor levels in legend
    na.translate = FALSE  # Don't show NA in legend
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 20),  # Legend text size = 20
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "white", color = NA)  # White plot background
  )

p

ggsave(
  filename = paste0("maps/noise_", area, ".png"),
  plot = p,
  width = 40.3,
  height = 46,
  units = "cm",
  bg = "white"  # White background for the entire image
)


















# ------------------------
# Plot noise raster
# ------------------------
library(ggplot2)

# First, reproject aoi to match the raster's CRS
aoi_reproj <- terra::project(aoi, terra::crs(noise_raster_clean))

# Mask raster to actual Bochum boundary (not bounding box)
noise_raster_masked <- terra::mask(noise_raster_clean, aoi_reproj)

# Filter to only keep values 1-5
noise_raster_filtered <- terra::ifel(
  noise_raster_masked < 1 | noise_raster_masked > 5,
  NA,
  noise_raster_masked
)

# Convert to factor properly
noise_raster_factor <- terra::as.factor(noise_raster_filtered)

# Set up the factor levels manually for 1-5
rat <- data.frame(
  ID = 1:5,
  category = as.character(1:5)
)
levels(noise_raster_factor) <- rat

p <- ggplot() +
  tidyterra::geom_spatraster(
    data = noise_raster_factor
  ) +
  scale_fill_manual(
    name = "",
    values = colors[1:5],  # Only use first 5 colors
    na.value = "transparent",
    drop = FALSE,  # Keep all factor levels in legend
    na.translate = FALSE
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 20),  # Legend text size = 20
    legend.title = element_text(size = 20)
  )

p

ggsave(
  filename = paste0("maps/noise_", area, ".png"),
  plot = p,
  width = 40.3,
  height = 46,
  units = "cm",
  bg = "white"  # White background instead of black
)




# ------------------------# ------------------------
# Plot noise raster
# ------------------------
library(ggplot2)
p <- ggplot() +
    tidyterra::geom_spatraster(
      data = as.factor(noise_raster_clean)
    ) +
    scale_fill_manual(
      name = "",
      values = colors,
      na.value = "black"
    ) +
    theme_void()
p

ggsave(
    filename = paste0("maps/noise_", area, ".png"),
    plot = p,
    width = 40.3,
    height = 46,
    units = "cm"
  )
  
  # ------------------------
  # Noise extraction on roads
  # ------------------------
highways_buffer <- sf::st_buffer(
    highways$osm_lines,
    dist = units::set_units(50, "m")
  )

 
  
major_noise_extract <- exactextractr::exact_extract(
    x = noise_raster_clean,
    y = highways_buffer,
    fun = "mode"
  )
  
major_roads_noise_sf <- cbind(
    highways$osm_lines,
    major_noise_extract
  )
  
  # ------------------------
  # Basemap + roads
  # ------------------------
streets <- maptiles::get_tiles(
    bbox,
    provider = "CartoDB.Positron",
    zoom = 12,
    crop = TRUE,
    project = FALSE
  )
  
map <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = streets) +
    geom_sf(
      data = subset(
        major_roads_noise_sf,
        !is.na(major_noise_extract)
      ),
      aes(color = as.factor(major_noise_extract)),
      size = 0.8
    ) +
    scale_color_manual(
      name = "",
      values = colors,
      na.value = "white"
    ) +
    guides(
      color = guide_legend(
        override.aes = list(size = 5)
      )
    ) +
    theme_void()
map  

ggsave(
    filename = paste0("maps/noise_roads_", area, ".png"),
    plot = map,
    width = 46,
    height = 43,
    units = "cm"
  )

