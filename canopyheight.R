# Mapping 3D Forest map
# https://www.youtube.com/watch?v=4ScYWPMzy6E
# 0. Preparations
source("utils.R")
 load_libs(c(
  "tidyverse",
  "sf",
  "geodata",
  "terra",
  "classInt",
  "rayshader"
))


# 1. Download data
urls <- c(
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N51E006_Map.tif"
)

# Provide a proper filename
dest_files <- c("ETH_GlobalCanopyHeight_10m_2020_N51E006_Map.tif")

for (i in seq_along(urls)) {
  if 
  download.file(urls[i], destfile = dest_files[i], mode = "wb")
}


raster_files <- list.files(
  path = getwd(),
  pattern ="ETH",
  full.names = T
  )

# 2. Download Boundaries



country_borders <- get_country_borders("DEU", 3) 

nrw <- country_borders %>% 
  dplyr::filter(
    NAME_1 == "Nordrhein-Westfalen"
  )

ruhrgebiet <- nrw %>% 
  dplyr::filter(
    NAME_2 %in% c(
      "Bochum"
      , "Bottrop", "Dortmund", "Duisburg", "Essen", "Gelsenkirchen", "Hagen", "Hamm", 
      "Herne", "Mülheim an der Ruhr", "Oberhausen", "Recklinghausen", "Unna", "Wesel",  "Ennepe-Ruhr-Kreis" 
    )
  ) %>% 
  sf::st_union()

# 3. Load Forest Height
# - load in the files
# - crop the files
# - mosaic 

forest_height_list <- lapply(
  raster_files,
  terra::rast
  )

forest_height_rasters <-  lapply(
  forest_height_list,
  function(x){
    terra::crop(
      x,
      terra::vect(ruhrgebiet),
      snap = "in",
      mask = T
    )
  }
)

if (length(forest_height_rasters) > 1) {
  forest_height_mosaic <- do.call(
    terra::mosaic,
    forest_height_rasters
  )
} else {
  forest_height_mosaic <- forest_height_rasters[[1]]
}

# Decrease the resolution by aggregation

forest_height_ruhrgebiet <- forest_height_mosaic %>% 
  terra::aggregate(
    fact = 5 # might be to low
  )


# 4. Raster to Df

forest_height_ruhrgebiet_df <- forest_height_ruhrgebiet %>% 
  as.data.frame(xy = T)
names(forest_height_ruhrgebiet_df)[3] <- "height"

# 5. Breaks

breaks <- classInt::classIntervals(
  forest_height_ruhrgebiet_df$height,
  n = 5,
  style = "quantile"#"fisher"
)$brks

# 6. COLORS
#----------

cols <-
  c(
    "white", "#ffd3af", "#fbe06e",
    "#6daa55", "#205544"
  )

texture <- colorRampPalette(
  cols,
  bias = 2 # inceasing will increase number of greens
)(6)

# 7. GGPLOT2

# Legend is wrong (high at 0 and low at 45)
#-----------
p <- ggplot(
  forest_height_ruhrgebiet_df
)+
  geom_raster(
    aes(
      x = x,
      y = y,
      fill = height
    )
  )+ 
  geom_sf(data = ruhrgebiet, 
          fill = NA,
          color = "black",
          linewidth = 0.5,
          inherit.aes = F)
  scale_fill_gradientn(
    name = "height (m)",
    colors = texture,
    breaks = round(breaks, 0)
  ) + 
  coord_sf( crs = 4326) + 
  guides(
    fill = guide_colorbar(  # CHANGED FROM guide_legend
      direction = "vertical",
      barheight = unit(50, "mm"),  # CHANGED FROM keyheight
      barwidth = unit(5, "mm"),    # CHANGED FROM keywidth
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  # guides(
  #   fill = guide_legend(
  #     direction = "vertical",
  #     keyheight = unit (5, "mm"),
  #     keywidth = unit(5, "mm"),
  #     title.position = "top",
  #     label.position = "right",
  #     title.hjust = .5,
  #     label.hjust = .5,
  #     ncol = 1,
  #     byrow = F
  #    )
   ) +
  theme_minimal()+
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(
      size = 11,
      color = "grey10"
    ),
    legend.text = element_text(
      size = 10,
      color = "grey10"
    ),
    panel.grid.major = element_line(
      color = "white"
    ),
    
    panel.grid.minor = element_line(
      color = "white"
    ),
    plot.background = element_rect(
      fill = "white", color = NA
    ),
    
    legend.background = element_rect(
      fill = "white", color = NA
    ),
    panel.border =  element_rect(
      fill = NA, color = "white"
    ),
    plot.margin = unit(
      c(
        t=0, r=0, b=0,l=0
      ), "lines"
    )
  )

# 8. render scene

h <- nrow(forest_height_ruhrgebiet)
w <- ncol(forest_height_ruhrgebiet)

# 
# rayshader::plot_gg(
#   ggobj = p,
#   width = w/1000, # in inches
#   height = h/1000,
#   scale = 150, # height of spikes
#   solid = F,
#   soliddepth = 0,
#   shadow = T,
#   shadow_intensity = 0.95,  # from 0 (strong) to 1 (weak)
#   offset_edges = F,
#   sunangle = 315, # degrees 0-360
#  # window.size = c(800,800),
#   zoom = 0.8,  # zoom out = 1, zoom in = 0
#   phi = 35,
#   theta = -15,#-180-180
#   multicore = T
# )

p_no_legend <- p + theme(legend.position = "none")


rayshader::plot_gg(
  ggobj = p,
  width = w/500,
  height = h/500,
  scale = 40,
  solid = F,
  soliddepth = 0,
  shadow = T,
  shadow_intensity = 0.95,
  offset_edges = F,
  sunangle = 315,
  zoom = 0.95,  # Adjusted zoom for top view
  phi = 65,     # Nearly top-down (85-89 works well)
  theta = 0,    # Facing north
  multicore = T
)
#9. Render object
# After plot_gg(), just take a snapshot
rayshader::render_snapshot(
  filename = "ruhrgebiet-forest-height-2020_quick.png",
  width = 4000,
  height = 4000
)


rayshader::render_highquality(
  filename = "ruhrgebiet-forest-height-2020_scale40.png",
  preview = T,
  interactive = F,
  light = T,
  lightdirection = c(
    315, 310, 315, 310
  ) , # 0-360,
  lightintensity = c(
    1000, 1500, 150, 100
  ),
  lightaltitude = c(
    15, 15, 80, 80
  ),
  ground_material = 
    rayrender::microfacet(
      roughness = .6
    ),
  width = 2000,
  height = 2000
)

