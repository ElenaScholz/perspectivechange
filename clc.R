rm(list = ls())

renv::activate()
source("utils.R", chdir = TRUE)

libs <- c(
  "sf", "geodata", "terra", "elevatr", "png", "rayshader", "magick"
)

load_libs(libs)
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


country_clc <- terra::crop(
  clc,
  terra::vect(country_sf),
  mask = TRUE
)

clc <- terra::rast(
  "data/clc/CLMS_CLCPLUS_RAS_S2023_R10m_E41N31_03035_V01_R00.tif"
)

vals <- terra::values(
  clc, 
  dataframe = T
)

#'
#'                          Class_name
#' 1              Periodically herbaceous
#' 27                Permanent herbaceous
#' 29                              Sealed
#' 80   Woody broadleaved deciduous trees
#' 378         Non and sparsely vegetated
#' 657          Woody needle leaved trees
#' 711           Low-growing woody plants
#' 2419                             Water
#' 

# 3. CROP FOREST TYPE RASTER
#---------------------------

crs_lambert <-
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"
bochum_terra <- terra::vect(bochum)
bochum_terra <- terra::project(bochum_terra, terra::crs(clc))

country_forest_type <- clc |>
  terra::crop(bochum_terra, snap = "in") %>% 
  terra::mask(bochum_terra) %>% 
  terra::project(crs_lambert)


