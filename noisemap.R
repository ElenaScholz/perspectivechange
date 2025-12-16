# https://github.com/milos-agathon/traffic-noise-maps/blob/main/R/main.r
rm(list = ls())
source("utils.R")
# Install pacman & remotes in renv
renv::install("pacman")
renv::install("remotes")

area <- "germany" #"bochum" #"ruhrgebiet 
area <- tolower(area)
stopifnot(area %in% c("germany", "ruhrgebiet", "bochum"))


# Run your integrated loading workflow
load_libs(
  libs = c(
    "remotes", "dplyr", "geodata", "arcgis",
    "tidyverse", "sf", "terra", "maptiles",
    "tidyterra", "exactextractr", "osmdata"
  ),
  github_libs = list(
    # GitHub packages: name = repo
    arcgis = "r-arcgis/arcgis"
  )
)

ger <- readRDS("data/gadm/gadm41_DEU_3_pk.rds")
ruhrgebiet <- ger %>%  
  dplyr::filter(
    NAME_1 == "Nordrhein-Westfalen"
  ) %>% 
  dplyr::filter(
    NAME_2 %in% c(
      "Bochum"
      #, "Bottrop", "Dortmund", "Duisburg", "Essen", "Gelsenkirchen", "Hagen", "Hamm", 
      #"Herne", "Mülheim an der Ruhr", "Oberhausen", "Recklinghausen", "Unna", "Wesel",  "Ennepe-Ruhr-Kreis" 
    )
  ) %>% 
  sf::st_as_sf()

bbox <- sf::st_bbox(area)

# Get Noise Image

noise_url <- "https://noise.discomap.eea.europa.eu/arcgis/rest/services/noiseStoryMap/NoiseContours_road_lden/ImageServer"

noise_data <- arcgislayers::arc_open(
  noise_url
)

noise_raster <- arcgislayers::arc_raster(
  x = noise_data,
  xmin = bbox[["xmin"]],
  xmax = bbox[["xmax"]],
  ymin = bbox[["ymin"]],
  ymax = bbox[["ymax"]],
  crs = sf::st_crs(ruhrgebiet),
  width = 4000,
  height = 4000
)

unique(terra::values(noise_raster))
# 15 = missing 
noise_raster_clean <- terra::ifel(
  noise_raster == 0 | noise_raster == 15,
  NA,
  noise_raster
)

split_bbox <- function(bbox) {
  xm <- (bbox["xmin"] + bbox["xmax"]) / 2
  ym <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  list(
    c(bbox["xmin"], bbox["ymin"], xm,          ym),
    c(xm,          bbox["ymin"], bbox["xmax"], ym),
    c(bbox["xmin"], ym,           xm,          bbox["ymax"]),
    c(xm,          ym,           bbox["xmax"], bbox["ymax"])
  )
}

if (area == "bochum") {
  tiles <- list(bbox)  # kein Split
} else {
  tiles <- split_bbox(bbox)  # Ruhrgebiet → split
}


# load the major roads of a region 
bbox_vec <- as.numeric(bbox)  # opq braucht numeric vektor

highways <- get_osm_cached(
  paste0("data/highways_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(
          t,
          "highway",
          c(
            "motorway", "trunk", "primary", "secondary"#,
            #"tertiary", "motorway_link", "trunk_link"
          )
        )
      )
    )
  }
)

#Streets
streets <- get_osm_cached(
  paste0("data/streets_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(t, "highway",
                 c("residential", "living_street"
                   #,
                   # "service", "unclassified", "pedestrian"
                 ))
      )
    )
  }
)



colors <- hcl.colors(
   n = 5,
   palette = "Plasma",
   rev = T
)

p <- ggplot()+
  tidyterra::geom_spatraster(
    data = as.factor(noise_raster_clean)
  )+
  scale_fill_manual(
    name="",
    values = colors,
    na.value = "black"
  )+
  theme_void()

ggsave(filename = paste0("noise", area, ".png"),
       plot = p,
       width = 7,
       height = 7,
       units = "in"
)

highways_buffer <- sf::st_buffer(
  highways$osm_lines,
  dist = units::set_units(
    50, m))

major_noise_extract <- exactextractr::exact_extract(
  x = noise_raster_clean,
  y = highways_buffer,
  fun = "mode"
)

major_roads_noise_sf <- cbind(
  highways$osm_lines ,
  major_noise_extract
)

names(major_roads_noise_sf)

streets <- maptiles::get_tiles(
  bbox,
  provider = "CartoDB.Positron",
  zoom = 12,
  crop = TRUE,
  project = FALSE
)

map <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = streets
  ) +
  geom_sf(
    data = subset(
      major_roads_noise_sf,
      !is.na(major_noise_extract)
    ),
    aes(
      color = as.factor(
        major_noise_extract
      )
    ),
    size = .25
  ) +
  scale_color_manual(
    name = "",
    values = colors,
    na.value = "white"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = 3
      )
    )
  ) +
  theme_void()
