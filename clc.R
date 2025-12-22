rm(list = ls())
# 1. PACKAGES

libs <- c(
  "terra",
  "giscoR",
  "sf",
  "tidyverse",
  "ggtern",
  "elevatr",
  "png",
  "rayshader",
  "magick"
)


source("utils.R")


load_libs(libs)
invisible(
  lapply(
    libs, library, character.only = T
  )
)

# 2. COUNTRY BORDERS
# #https://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/
rm(list = ls())
# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
area <- "ruhrgebiet"   # "bochum" oder "ruhrgebiet"

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------

# ------------------------------------------------------------
# GET ADMIN BOUNDARIES
# ------------------------------------------------------------
country_borders <- readRDS("data/gadm/gadm41_DEU_3_pk.rds")

# Check if it's packed and unwrap if needed
if (inherits(country_borders, "PackedSpatVector")) {
  country_borders <- terra::unwrap(country_borders)
}

# Now convert to sf
country_borders <- country_borders |>
  sf::st_as_sf() |>
  st_make_valid()


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


plot(sf::st_geometry(country_sf))

country_sf <- ruhrgebiet
plot(sf::st_geometry(country_sf))

png("maps/ruhrgebiet-borders.png")
plot(sf::st_geometry(country_sf))
dev.off()


# 4 LOAD TILES

raster_files <- list.files(
  path = "C:/Users/elena/Documents/RProjects/perspectivechange/data/clc/",
  pattern = "20241231.tif$",
  full.names = T
)

crs <- "EPSG:4326"

for(raster in raster_files){
  rasters <- terra::rast(raster)
  
  country <- country_sf |>
    sf::st_transform(
      crs = terra::crs(
        rasters
      )
    )
  
  land_cover <- terra::crop(
    rasters,
    terra::vect(
      country
    ),
    snap = "in",
    mask = T
  ) |>
    terra::aggregate(
      fact = 5,
      fun = "modal"
    ) |>
    terra::project(crs)
  
  terra::writeRaster(
    land_cover,
    paste0(
      raster,
      "_ger",
      ".tif"
    )
  )
}

 # 5 LOAD VIRTUAL LAYER

r_list <- list.files(
  path = "C:/Users/elena/Documents/RProjects/perspectivechange/data/clc/",
  pattern = "_ger",
  full.names = T
)

land_cover_vrt <- terra::vrt(
  r_list,
  "ger_land_cover_vrt.vrt",
  overwrite = T
)

# 6 FETCH ORIGINAL COLORS

ras <- terra::rast(
  raster_files[[1]]
)

raster_color_table <- do.call(
  data.frame,
  terra::coltab(ras)
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)

# 7 ASSIGN COLORS TO RASTER

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_bosnia <- terra::subst(
  land_cover_vrt,
  from = from,
  to = to,
  names = cols
)

terra::plotRGB(land_cover_bosnia)

# 8 DIGITAL ELEVATION MODEL


ruhr_wgs84 <- sf::st_transform(country_sf, 4326)

elev <- elevatr::get_elev_raster(
  locations = ruhr_wgs84,
  z = 12,
  clip = "locations"
)


crs_lambert <-
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

land_cover_bosnia_resampled <- terra::resample(
  x = land_cover_bosnia,
  y = terra::rast(elev),
  method = "near"
) |>
  terra::project(crs_lambert)

terra::plotRGB(land_cover_bosnia_resampled)

img_file <- "land_cover_bosnia.png"

terra::writeRaster(
  land_cover_bosnia_resampled,
  img_file,
  overwrite = T,
  NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE
#----------------

elev_lambert <- elev |>
  terra::rast() |>
  terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
  elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      cols[9]
    )(256)
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = 1
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 12,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 5, h / 5
    ),
    zoom = .5,
    phi = 85,
    theta = 0
  )

rayshader::render_camera(
  zoom = .58
)

# 10. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

filename <- "3d_land_cover_bosnia-dark.png"

rayshader::render_highquality(
  filename = filename,
  preview = T,
  light = F,
  environment_light = hdri_file,
  intensity_env = 1,
  rotate_env = 90,
  interactive = F,
  parallel = T,
  width = w * 1.5,
  height = h * 1.5
)

# 11. PUT EVERYTHING TOGETHER

legend <- c(
  "#419bdf", "#397d49", "#7a87c6", 
  "#e49635", "#c4281b", "#a59b8f", 
  "#a8ebff", "#616161", "#e3e2c3"
)

legend_name <- "land_cover_legend.png"
png(legend_name)
par(family = "mono")

plot(
  NULL,
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  ylab = "",
  xlab = "",
  xlim = 0:1,
  ylim = 0:1,
  xaxs = "i",
  yaxs = "i"
)
legend(
  "center",
  legend = c(
    "Water",
    "Trees",
    "Crops",
    "Built area",
    "Rangeland"
  ),
  pch = 15,
  cex = 2,
  pt.cex = 1,
  bty = "n",
  col = c(cols[c(1:2, 4:5, 9)]),
  fill = c(cols[c(1:2, 4:5, 9)]),
  border = "grey20"
)
dev.off()

# filename <- "land-cover-bih-3d-b.png"

lc_img <- magick::image_read(
  filename
)

my_legend <- magick::image_read(
  legend_name
)

my_legend_scaled <- magick::image_scale(
  magick::image_background(
    my_legend, "none"
  ), 2500
)

p <- magick::image_composite(
  magick::image_scale(
    lc_img, "x7000" 
  ),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+100+0"
)

magick::image_write(
  p, "3d_bosnia_land_cover_final.png"
)