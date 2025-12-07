# libraries
pacman::p_load(
  osmdata, sf, dplyr, rayrender
)

# AOI (Copenhagen)

lon <- 14.41158111539747#7.171619228819382
lat <- 50.08700634162275#51.44727320000573

ctr_wgs <- sf::st_sfc(
  sf::st_point(
    c(lon, lat)
  ), crs = 4326
)

utm_epsg <- function(lon, lat) {
  stopifnot(
    is.finite(lon), is.finite(lat)
  )
  if (lat >= 84) {
    return(32661)
  } # UPS North
  if (lat <= -80) {
    return(32761)
  } # UPS South
  zone <- floor((lon + 180) / 6) + 1
  if (lat >= 0) 32600 + zone else 32700 + zone
}

crs_m <- utm_epsg(lon, lat)
ctr_m <- sf::st_transform(ctr_wgs, crs_m)
radius <- 1000
aoi_m <- sf::st_buffer(ctr_m, radius)
aoi_wgs <- sf::st_transform(aoi_m, 4326)
bb <- sf::st_bbox(aoi_wgs)

# OSM pulls (helpers)
get_polys <- function(q) {
  x <- osmdata::osmdata_sf(q)$osm_polygons
  if (is.null(x)) {
    sf::st_sf(
      geometry = sf::st_sfc(crs = 4326)
    )
  } else {
    sf::st_make_valid(x)
  }
}

get_lines <- function(q) {
  x <- osmdata::osmdata_sf(q)$osm_lines
  if (is.null(x)) {
    sf::st_sf(
      geometry = sf::st_sfc(crs = 4326)
    )
  } else {
    sf::st_make_valid(x)
  }
}

bld <- get_polys(osmdata::opq(bb) |>
                   osmdata::add_osm_feature("building"))

landuse <- get_polys(osmdata::opq(bb) |>
                       osmdata::add_osm_feature("landuse"))

# keep green landuse types
green_cover <- subset(
  landuse, landuse %in% c(
    "grass", "recreation_ground",
    "forest", "greenery"
  )
)

parks <- dplyr::bind_rows(
  green_cover,
  get_polys(osmdata::opq(bb) |>
              osmdata::add_osm_feature(
                "leisure", "park"
              )),
  get_polys(osmdata::opq(bb) |>
              osmdata::add_osm_feature(
                "natural", c("wood", "scrub")
              ))
)

water <- dplyr::bind_rows(
  get_polys(osmdata::opq(bb) |>
              osmdata::add_osm_feature(
                "natural", "water"
              )),
  get_polys(osmdata::opq(bb) |>
              osmdata::add_osm_feature(
                "natural", "water"
              ) |>
              osmdata::add_osm_feature("water", "river")) 
)

roads <- get_lines(osmdata::opq(bb) |>
                     osmdata::add_osm_feature(
                       "highway"
                     ))

rails <- get_lines(osmdata::opq(bb) |>
                     osmdata::add_osm_feature(
                       "railway"
                     ))

# Clip to circle & project to meters

clip_m <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(
      sf::st_sf(
        geometry = sf::st_sfc(
          crs = crs_m
        )
      )
    )
  }
  x <- suppressWarnings(
    sf::st_intersection(x, aoi_wgs)
  )
  if (nrow(x) == 0) {
    return(
      sf::st_sf(
        geometry = sf::st_sfc(
          crs = crs_m
        )
      )
    )
  }
  sf::st_transform(x, crs_m)
}

bld <- clip_m(bld)
parks <- clip_m(parks)
landuse <- clip_m(landuse)
water <- clip_m(water)
roads <- clip_m(roads)
rails <- clip_m(rails)

# Heights & line buffers
if (nrow(bld) > 0) {
  h_raw <- suppressWarnings(
    as.numeric(gsub(",", ".", bld$height))
  )
  levraw <- suppressWarnings(
    as.numeric(gsub(",", ".", bld$`building:levels`))
  )
  bld$h <- ifelse(!is.na(h_raw), h_raw,
                  ifelse(!is.na(levraw), pmax(levraw, 1) * 3.2, 12)
  )
  bld$h <- pmin(bld$h, 80)
}

roads_buf <- if (nrow(roads) > 0) {
  sf::st_buffer(roads, 3)
} else {
  sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
}

rails_buf <- if (nrow(rails) > 0) {
  sf::st_buffer(rails, 2)
} else {
  sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
}

# thin "crown" highlight down the middle of roads
roads_crown <- if (nrow(roads) > 0) {
  sf::st_buffer(roads, 1.3)
} else {
  sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
}

# Recenter near origin
center_xy <- sf::st_coordinates(ctr_m)[1, 1:2]
recenter <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(x)
  }
  sf::st_geometry(x) <- sf::st_geometry(x) - center_xy
  x 
}

bld <- recenter(bld)
landuse <- recenter(landuse)
parks <- recenter(parks)
water <- recenter(water)
roads_buf <- recenter(roads_buf)
rails_buf <- recenter(rails_buf)
roads_crown <- recenter(roads_crown)

# Palette/Materials

col_bld_low <- "#c88a1e"
col_bld_mid <- "#b7720e"
col_bld_high <-"#98590a"
col_landuse <- "#b89c3a"
col_park <- "#66A61E"
col_road <- "#7e8792"
col_road_hi <- "#e6ebf1"
col_water <- "#3E8fe0"
col_base_hi <- "#FBFCFE"
col_base_lo <- "#D8DDE3"

mat_landuse <- rayrender::diffuse(col_landuse)
mat_park <- rayrender::diffuse(col_park)
mat_road <- rayrender::diffuse(col_road)
mat_road_hi <- rayrender::diffuse(col_road_hi)
mat_water <- rayrender::metal(
  color = col_water, fuzz = 0.5
)

# Build scene objects

objs <- list()

# landuse/parks/water/roads
if (nrow(landuse) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        landuse,
        top = 0.5,
        bottom = 0.02,
        material = mat_landuse
      )
    )
  )
}

if (nrow(parks) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        parks,
        top = 0.5,
        bottom = 0.02,
        material = mat_park
      )
    )
  )
}

if (nrow(water) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        water,
        top = 0.5,
        bottom = -1,
        material = mat_water
      )
    )
  )
}

rr <- dplyr::bind_rows(
  roads_buf, rails_buf
)

if (nrow(rr) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        rr,
        top = 1.0,
        bottom = 0.02,
        material = mat_road
      )
    )
  )
}

if (nrow(roads_crown) > 0) {
  objs <- append(
    objs, list(
      rayrender::extruded_polygon(
        roads_crown,
        top = 1.12,
        bottom = 1.02,
        material = mat_road_hi
      )
    )
  )
}

# Building color by height bins
bin_buildings <- function(bld, use_quantiles = FALSE) {
  bld <- bld[!is.na(bld$h) & is.finite(bld$h) & bld$h > 0, ]
  if (nrow(bld) == 0) {
    return(bld)
  }
  if (use_quantiles) {
    qs <- stats::quantile(
      bld$h, c(1 / 3, 2 / 3),
      na.rm = TRUE
    )
    brks <- c(-Inf, qs, Inf)
  } else {
    brks <- c(-Inf, 12, 24, Inf)
  }
  bld$bin <- cut(
    bld$h, breaks = brks, labels = c(
      "low", "mid", "high"
    ), include.lowest = TRUE, right = TRUE
  )
  bld
}

bld <- bin_buildings(bld, use_quantiles = FALSE)
pal <- c(
  low = col_bld_low, mid = col_bld_mid, 
  high = col_bld_high
)
mat_map <- setNames(lapply(pal, rayrender::diffuse), 
                    names(pal))

add_buildings <- function(scene, b) {
  if (is.null(b) || nrow(b) == 0) {
    return(scene)
  }
  for(lev in levels(b$bin)) {
    sel <- b[b$bin == lev, ]
    if (nrow(sel)) {
      scene <- rayrender::add_object(
        scene,
        rayrender::extruded_polygon(
          sel, data_column_top = "h", #data_column_top
          scale_data = 1,
          material = mat_map[[lev]]
        )
      )
    }
  }
  scene
}

# build scene and add buildings + outline
scene <- objs[[1]]
if (length(objs) > 1) {
  for (i in 2:length(objs))
  scene <- rayrender::add_object(
    scene, objs[[i]]
  )}
scene <- add_buildings(scene, bld)

# Camera: look from SW, slightly elevated
lookfrom <- c(-2000, 2000, -2000)
lookat <- c(0, 10, 50)

# Lighting (soft shadows)
scene <- rayrender::add_object(
  scene,
  rayrender::sphere(
    x = -1200, y = 2500,
    z = -1200, radius = 400,
    material = rayrender::light(
      intensity = 15
    )
  )
)

# Render
rayrender::render_scene(
  scene = scene,
  lookfrom = lookfrom,
  lookat = lookat,
  fov = 35,
  width = 1800, height = 1800,
  samples = 1e2,
  sample_method = "sobol",
  aperture = 0,
  denoise = TRUE,
  ambient_light = TRUE,
  clamp_value = 1,
  min_variance = 1e-15,
  backgroundlow = "#FFFFFF",
  parallel = TRUE,
  interactive = FALSE,
  filename = "prague.png"
)
