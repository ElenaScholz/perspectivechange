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

color_roads <- rgb(0.42, 0.449, 0.488)
color_water <- rgb(0.2, 0.4, 0.6, 0.5)

bochum_p <- ggplot() +
  geom_sf(data = sel, fill = "lightgrey", alpha = 0.4, lwd = 0) +
  geom_sf(data = water_areas$osm_polygons, fill = color_water,
          col = color_water, alpha = 0.6, lwd = 0) +
  geom_sf(data = water_bodies$osm_polygons, fill = color_water,
          col = color_water, alpha = 0.6, lwd = 0) +
  geom_sf(data = water$osm_lines, col = color_water, size = 0.8, alpha = 0.7) +
  geom_sf(data = streets$osm_lines, col = color_roads, size = 0.4, alpha = 0.65) +
  geom_sf(data = highways$osm_lines, col = color_roads, size = 0.6, alpha = 0.8) +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE) +
  theme_void() 

bochum_p


# Berechne Pixelgröße aus DPI und Inch
dpi <- 300
width_inch <- 16.5
height_inch <- 23.4

ggsave(
  filename = paste0("maps/",area, "a2_poster.png"),
  plot = bochum_p,
  width = width_inch,
  height = height_inch,
  dpi = dpi,
  units = "in",
  type = "cairo" # bessere Kantenglättung für Linien
)


# Pastel color palette with depth
color_bg <- "#F5F3F0"           # Soft cream background
color_area <- "#E8E4E1"         # Light grey for city area
color_water <- "#B8D4E8"        # Soft pastel blue for water
color_water_dark <- "#9BBFD4"   # Slightly darker blue for depth

# Road colors - different shades for hierarchy
color_highway <- "#D4A5A5"      # Muted rose for highways
color_primary <- "#C5B8D4"      # Soft lavender for primary roads
color_secondary <- "#B8D4C5"    # Mint green for secondary
color_tertiary <- "#D4C5B8"     # Warm beige for tertiary
color_residential <- "#E8DDD4"  # Very light peach for residential

# Separate highways by type for color coding
highways_motor <- highways$osm_lines %>% 
  filter(highway %in% c("motorway", "motorway_link"))

highways_trunk <- highways$osm_lines %>% 
  filter(highway %in% c("trunk", "trunk_link"))

highways_primary <- highways$osm_lines %>% 
  filter(highway %in% c("primary", "secondary"))

highways_tertiary <- highways$osm_lines %>% 
  filter(highway == "tertiary")

bochum_p <- ggplot() +
  # Background
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.background = element_rect(fill = color_bg, color = NA)
  ) +
  
  # City boundary with subtle shadow effect
  geom_sf(data = sel, fill = color_area, alpha = 0.3, lwd = 0) +
  
  # Water features - layered for depth
  geom_sf(data = water_bodies$osm_polygons, 
          fill = color_water_dark, col = NA, alpha = 0.7, lwd = 0) +
  geom_sf(data = water_areas$osm_polygons, 
          fill = color_water, col = NA, alpha = 0.8, lwd = 0) +
  geom_sf(data = water$osm_lines, 
          col = color_water_dark, size = 0.6, alpha = 0.6) +
  
  # Roads - from smallest to largest for proper layering
  geom_sf(data = streets$osm_lines, 
          col = color_residential, size = 0.3, alpha = 0.5) +
  geom_sf(data = highways_tertiary, 
          col = color_tertiary, size = 0.5, alpha = 0.7) +
  geom_sf(data = highways_primary, 
          col = color_secondary, size = 0.6, alpha = 0.75) +
  geom_sf(data = highways_trunk, 
          col = color_primary, size = 0.7, alpha = 0.8) +
  geom_sf(data = highways_motor, 
          col = color_highway, size = 0.9, alpha = 0.85) +
  
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)

bochum_p
ggsave(
  filename = paste0("maps/",area, "_pastel_a2_poster.png"),
  plot = bochum_p,
  width = width_inch,
  height = height_inch,
  dpi = dpi,
  units = "in",
  type = "cairo" # bessere Kantenglättung für Linien
)
