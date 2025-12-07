# 
# #https://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/
rm(list = ls())
# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
area <- "bochum"   # "bochum" oder "ruhrgebiet"

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
source("utils.R")
load_libs(c("osmdata","sf","ggplot2", "geodata", "terra", "dplyr", "purrr"))

# Stabiler Overpass-Server
set_overpass_url("https://overpass.kumi.systems/api/interpreter")


# ------------------------------------------------------------
# GET ADMIN BOUNDARIES
# ------------------------------------------------------------
country_borders <- get_country_borders("DEU", 3)

nrw <- country_borders %>%
  filter(NAME_1 == "Nordrhein-Westfalen")

ruhrgebiet <- nrw %>%
  filter(
    NAME_2 %in% c(
      "Bochum", "Bottrop", "Dortmund", "Duisburg", "Essen", "Gelsenkirchen",
      "Hagen", "Hamm", "Herne", "Mülheim an der Ruhr", "Oberhausen",
      "Recklinghausen", "Unna", "Wesel", "Ennepe-Ruhr-Kreis"
    )
  )

bochum <- nrw %>% filter(NAME_2 == "Bochum")


# ------------------------------------------------------------
# BOUNDING BOX SELECTION
# ------------------------------------------------------------
if (area == "bochum") {
  sel <- bochum
} else if (area == "ruhrgebiet") {
  sel <- ruhrgebiet
} else {
  stop("area must be 'bochum' or 'ruhrgebiet'")
}

bbox <- st_bbox(sel)
bbox <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])


# ------------------------------------------------------------
# OPTIONAL: BBOX TILING FOR LARGE AREAS
# ------------------------------------------------------------
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


# ------------------------------------------------------------
# FUNCTION: SAFE OSM DOWNLOAD
# ------------------------------------------------------------
# ------------------------------------------------------------
# HIGHWAYS BEISPIEL (GENAU DEIN CODE)
# ------------------------------------------------------------
highways <- get_osm_cached(
  paste0("highways_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(
          t,
          "highway",
          c(
            "motorway", "trunk", "primary", "secondary",
            "tertiary", "motorway_link", "trunk_link"
          )
        )
      )
    )
  }
)

#Streets
streets <- get_osm_cached(
  paste0("streets_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(t, "highway",
                 c("residential", "living_street"
                   ,
                   "service", "unclassified", "pedestrian"
                   ))
      )
    )
  }
)

# Waterways
water <- get_osm_cached(
  paste0("water_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(t, "waterway",
                 c("river", "stream", "canal"))
      )
    )
  }
)

# Water bodies (lakes)
water_bodies <- get_osm_cached(
  paste0("waterbodies_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(t, "water",
                 c("river", "lake", "reservoir", "pond"))
      )
    )
  }
)

# Water areas (natural=water)
water_areas <- get_osm_cached(
  paste0("waterareas_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(t, "natural", "water")
      )
    )
  }
)


# ------------------------------------------------------------
# PLOTTING
# ------------------------------------------------------------
color_roads <- rgb(0.42, 0.449, 0.488)
color_water <- rgb(0.2, 0.4, 0.6, 0.5)

bochum_p <- ggplot() +
  geom_sf(data = sel, fill = rgb(0.203,0.234,0.277), alpha = 0.5, lwd = 0) +
  geom_sf(data = water_areas$osm_polygons, fill = color_water,
          col = color_water, alpha = 0.6, lwd = 0) +
  geom_sf(data = water_bodies$osm_polygons, fill = color_water,
          col = color_water, alpha = 0.6, lwd = 0) +
  geom_sf(data = water$osm_lines, col = color_water, size = 0.8, alpha = 0.7) +
  geom_sf(data = streets$osm_lines, col = color_roads, size = 0.4, alpha = 0.65) +
  geom_sf(data = highways$osm_lines, col = color_roads, size = 0.6, alpha = 0.8) +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(
    fill = adjustcolor(rgb(0.92,0.679,0.105), alpha.f = 0.5),
    color = NA
  ))

bochum_p
library(Cairo)

Cairo::CairoPNG("ruhrgebiet_map.png", width = 2000, height = 1600, bg = "transparent")
print(bochum_p)  # print plot to device
dev.off()        # close device
