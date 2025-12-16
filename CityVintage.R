# # 
# # #https://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/
# rm(list = ls())
# # ------------------------------------------------------------
# # CONFIG
# # ------------------------------------------------------------
# area <- "bochum"   # "bochum" oder "ruhrgebiet"
# 
# # ------------------------------------------------------------
# # Libraries
# # ------------------------------------------------------------
# source("utils.R")
# load_libs(c("osmdata","sf","ggplot2", "geodata", "terra", "dplyr", "purrr"))
# 
# # Stabiler Overpass-Server
# set_overpass_url("https://overpass.kumi.systems/api/interpreter")
# 
# 
# # ------------------------------------------------------------
# # GET ADMIN BOUNDARIES
# # ------------------------------------------------------------
# country_borders <- get_country_borders("DEU", 3)
# 
# nrw <- country_borders %>%
#   filter(NAME_1 == "Nordrhein-Westfalen")
# 
# ruhrgebiet <- nrw %>%
#   filter(
#     NAME_2 %in% c(
#       "Bochum", "Bottrop", "Dortmund", "Duisburg", "Essen", "Gelsenkirchen",
#       "Hagen", "Hamm", "Herne", "Mülheim an der Ruhr", "Oberhausen",
#       "Recklinghausen", "Unna", "Wesel", "Ennepe-Ruhr-Kreis"
#     )
#   )
# 
# bochum <- nrw %>% filter(NAME_2 == "Bochum")
# 
# 
# # ------------------------------------------------------------
# # BOUNDING BOX SELECTION
# # ------------------------------------------------------------
# if (area == "bochum") {
#   sel <- bochum
# } else if (area == "ruhrgebiet") {
#   sel <- ruhrgebiet
# } else {
#   stop("area must be 'bochum' or 'ruhrgebiet'")
# }
# 
# bbox <- st_bbox(sel)
# bbox <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
# 
# 
# # ------------------------------------------------------------
# # OPTIONAL: BBOX TILING FOR LARGE AREAS
# # ------------------------------------------------------------
# split_bbox <- function(bbox) {
#   xm <- (bbox["xmin"] + bbox["xmax"]) / 2
#   ym <- (bbox["ymin"] + bbox["ymax"]) / 2
#   
#   list(
#       c(bbox["xmin"], bbox["ymin"], xm,          ym),
#       c(xm,          bbox["ymin"], bbox["xmax"], ym),
#       c(bbox["xmin"], ym,           xm,          bbox["ymax"]),
#       c(xm,          ym,           bbox["xmax"], bbox["ymax"])
#     )
# }
# 
# if (area == "bochum") {
#   tiles <- list(bbox)  # kein Split
# } else {
#   tiles <- split_bbox(bbox)  # Ruhrgebiet → split
# }
# 
# 
# # ------------------------------------------------------------
# # FUNCTION: SAFE OSM DOWNLOAD
# # ------------------------------------------------------------
# # ------------------------------------------------------------
# # HIGHWAYS BEISPIEL (GENAU DEIN CODE)
# # ------------------------------------------------------------
# highways <- get_osm_cached(
#   paste0("data/highways_", area, ".rds"),
#   function() {
#     combine_osm(
#       lapply(tiles, function(t)
#         safe_osm(
#           t,
#           "highway",
#           c(
#             "motorway", "trunk", "primary", "secondary",
#             "tertiary", "motorway_link", "trunk_link"
#           )
#         )
#       )
#     )
#   }
# )
# 
# #Streets
# streets <- get_osm_cached(
#   paste0("data/streets_", area, ".rds"),
#   function() {
#     combine_osm(
#       lapply(tiles, function(t)
#         safe_osm(t, "highway",
#                  c("residential", "living_street"
#                    ,
#                    "service", "unclassified", "pedestrian"
#                    ))
#       )
#     )
#   }
# )
# 
# 
# 
# # Waterways
# water <- get_osm_cached(
#   paste0("data/water_", area, ".rds"),
#   function() {
#     combine_osm(
#       lapply(tiles, function(t)
#         safe_osm(t, "waterway",
#                  c("river", "stream", "canal"))
#       )
#     )
#   }
# )
# 
# # Water bodies (lakes)
# water_bodies <- get_osm_cached(
#   paste0("data/waterbodies_", area, ".rds"),
#   function() {
#     combine_osm(
#       lapply(tiles, function(t)
#         safe_osm(t, "water",
#                  c("river", "lake", "reservoir", "pond"))
#       )
#     )
#   }
# )
# 
# # Water areas (natural=water)
# water_areas <- get_osm_cached(
#   paste0("data/waterareas_", area, ".rds"),
#   function() {
#     combine_osm(
#       lapply(tiles, function(t)
#         safe_osm(t, "natural", "water")
#       )
#     )
#   }
# )
# 
# 
# ################
# # Black and White Plot
# ################
# color_roads <- rgb(0.42, 0.449, 0.488)
# color_water <- rgb(0.2, 0.4, 0.6, 0.5)
# 
# 
# 
# 
# bochum_p <- ggplot() +
#   geom_sf(data = sel, fill = "lightgrey", alpha = 0.4, lwd = 0) +
#   geom_sf(data = water_areas$osm_polygons, fill = color_water,
#           col = color_water, alpha = 0.6, lwd = 0) +
#   geom_sf(data = water_bodies$osm_polygons, fill = color_water,
#           col = color_water, alpha = 0.6, lwd = 0) +
#   geom_sf(data = water$osm_lines, col = color_water, size = 0.8, alpha = 0.7) +
#   geom_sf(data = streets$osm_lines, col = color_roads, size = 0.4, alpha = 0.65) +
#   geom_sf(data = highways$osm_lines, col = color_roads, size = 0.6, alpha = 0.8) +
#   coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE) +
#   theme_void() 
# 
# bochum_p
# 
# 
# # Berechne Pixelgröße aus DPI und Inch
# dpi <- 600
# width_inch <- 40.3
# height_inch <- 46.32
# 
# ggsave(
#   filename = paste0("maps/",area, "a2_poster.png"),
#   plot = bochum_p,
#   width = width_inch,
#   height = height_inch,
#   dpi = dpi,
#   units = "cm",
#   type = "cairo" # bessere Kantenglättung für Linien
# )


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
highways <- get_osm_cached(
  paste0("data/highways_", area, ".rds"),
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
  paste0("data/streets_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(t, "highway",
                 c("residential", "living_street",
                   "service", "unclassified", "pedestrian"
                 ))
      )
    )
  }
)

# Waterways
water <- get_osm_cached(
  paste0("data/water_", area, ".rds"),
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
  paste0("data/waterbodies_", area, ".rds"),
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
  paste0("data/waterareas_", area, ".rds"),
  function() {
    combine_osm(
      lapply(tiles, function(t)
        safe_osm(t, "natural", "water")
      )
    )
  }
)


# ------------------------------------------------------------
# FUNCTION: CREATE MAP WITH CUSTOMIZABLE COLORS
# ------------------------------------------------------------
create_city_map <- function(
    bg_color = "lightgrey",
    bg_alpha = 0.4,
    water_color = "#336699",
    water_alpha = 0.6,
    road_color = "#6B7280",
    road_alpha_streets = 0.65,
    road_alpha_highways = 0.8,
    water_line_size = 0.8,
    street_size = 0.4,
    highway_size = 0.6
) {
  
  p <- ggplot() +
    geom_sf(data = sel, fill = bg_color, alpha = bg_alpha, lwd = 0) +
    geom_sf(data = water_areas$osm_polygons, fill = water_color,
            col = water_color, alpha = water_alpha, lwd = 0) +
    geom_sf(data = water_bodies$osm_polygons, fill = water_color,
            col = water_color, alpha = water_alpha, lwd = 0) +
    geom_sf(data = water$osm_lines, col = water_color, 
            size = water_line_size, alpha = water_alpha + 0.1) +
    geom_sf(data = streets$osm_lines, col = road_color, 
            size = street_size, alpha = road_alpha_streets) +
    geom_sf(data = highways$osm_lines, col = road_color, 
            size = highway_size, alpha = road_alpha_highways) +
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), 
             expand = FALSE) +
    theme_void()
  
  return(p)
}


# ------------------------------------------------------------
# EXAMPLE USAGE - DIFFERENT COLOR SCHEMES
# ------------------------------------------------------------

# 1. Original (Black & White style)
bochum_bw <- create_city_map(
  bg_color = "lightgrey",
  water_color = rgb(0.2, 0.4, 0.6),
  road_color = rgb(0.42, 0.449, 0.488)
)

# 2. Warm Curcuma/Yellow theme
bochum_warm <- create_city_map(
  bg_color = "#F4A700",          # Sunny yellow
  water_color = "#0B4F6C",       # Dark blue-green
  road_color = "#8B3A3A",        # Warm dark red
  bg_alpha = 0.8,
  water_alpha = 0.9,
  road_alpha_streets = 0.7,
  road_alpha_highways = 0.9
)

# 3. Dark theme
bochum_dark <- create_city_map(
  bg_color = "#1a1a1a",
  water_color = "#4A95B8",
  road_color = "#CCCCCC",
  bg_alpha = 1.0,
  water_alpha = 0.7,
  road_alpha_streets = 0.6,
  road_alpha_highways = 0.8
)

# 4. Vintage sepia
bochum_vintage <- create_city_map(
  bg_color = "#FFF8DC",
  water_color = "#5C7A99",
  road_color = "#8B7355",
  bg_alpha = 1.0,
  water_alpha = 0.5,
  road_alpha_streets = 0.5,
  road_alpha_highways = 0.7
)

# 5. Minimal blue
bochum_minimal <- create_city_map(
  bg_color = "#F5F5F5",
  water_color = "#2E86AB",
  road_color = "#555555",
  bg_alpha = 1.0,
  water_alpha = 0.6
)


# ------------------------------------------------------------
# SAVE YOUR PREFERRED VERSION
# ------------------------------------------------------------
# Choose which plot to save
bochum_p <- bochum_bw  # Change this to your preferred style

# Berechne Pixelgröße aus DPI und Inch
dpi <- 600
width_inch <- 40.3
height_inch <- 46.32

ggsave(
  filename = paste0("maps/", area, "_bw.png"),
  plot = bochum_p,
  width = width_inch,
  height = height_inch,
  dpi = dpi,
  units = "cm",
  type = "cairo"
)
