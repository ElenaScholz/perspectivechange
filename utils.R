# ----------------------------
# Load & Install Packages (renv + GitHub)
# ----------------------------

load_libs <- function(libs, github_libs = list()) {
  
  # Ensure renv project is active
  if (is.null(renv::paths$library())) {
    renv::activate()
  }
  
  
  # Check what is installed
  installed <- libs %in% rownames(installed.packages())
  missing <- libs[!installed]
  
  # Install missing CRAN packages via renv
  if (length(missing) > 0) {
    message("Installing missing CRAN packages: ",
            paste(missing, collapse = ", "))
    renv::install(missing)
  }
  
  # Install GitHub packages if needed
  if (length(github_libs) > 0) {
    for (pkg in names(github_libs)) {
      if (!pkg %in% rownames(installed.packages())) {
        repo <- github_libs[[pkg]]
        message("Installing GitHub package ", pkg, " from ", repo)
        renv::install(repo)
      }
    }
  }
  
  # Load all packages quietly
  invisible(lapply(libs, function(pkg) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, quietly = TRUE)
    )
  }))
}

get_country_borders <- function(country_ISO, level) {
  # Ensure needed packages are loaded
  load_libs(c("geodata", "sf"))
  
  country_borders <- geodata::gadm(
    country = country_ISO,
    level = level,
    path  = getwd()
  )
  
  # If not already sf, convert
  if (!inherits(country_borders, "sf")) {
    country_borders <- sf::st_as_sf(country_borders)
  }
  
  return(country_borders)
}

safe_osm <- function(bbox, key, value) {
  opq(bbox = bbox) %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf()
}

# Combine results from multiple tiles
combine_osm <- function(list_of_osm) {
  Reduce(osmdata::c.osmdata_sf, list_of_osm)
}


# ------------------------------------------------------------
# DOWNLOAD OR LOAD CACHED DATA
# ------------------------------------------------------------
# ------------------------------------------------------------
# MERGE FUNCTION (statt c.osmdata_sf)
# ------------------------------------------------------------


get_osm_cached <- function(filename, query_fun) {
  if (file.exists(filename)) {
    message("Loading from cache: ", filename)
    readRDS(filename)
  } else {
    message("Downloading: ", filename)
    data <- query_fun()
    saveRDS(data, filename)
    data
  }
}
merge_osm <- function(osm_list) {
  
  # wenn nur ein Tile, direkt zurückgeben
  if (length(osm_list) == 1) return(osm_list[[1]])
  
  merged <- osm_list[[1]]
  
  # helper für einzelne sf-Objekte
  merge_sf <- function(a, b) {
    if (!is.null(a) && !is.null(b)) {
      return(dplyr::bind_rows(a, b))
    }
    if (!is.null(a)) return(a)
    if (!is.null(b)) return(b)
    return(NULL)
  }
  
  # Tiles nacheinander hinzufügen
  for (i in 2:length(osm_list)) {
    current <- osm_list[[i]]
    
    merged$osm_points        <- merge_sf(merged$osm_points,        current$osm_points)
    merged$osm_lines         <- merge_sf(merged$osm_lines,         current$osm_lines)
    merged$osm_polygons      <- merge_sf(merged$osm_polygons,      current$osm_polygons)
    merged$osm_multilines    <- merge_sf(merged$osm_multilines,    current$osm_multilines)
    merged$osm_multipolygons <- merge_sf(merged$osm_multipolygons, current$osm_multipolygons)
  }
  
  return(merged)
}

# Wrapper wie vorher c.osmdata_sf()
combine_osm <- function(list_of_osm) {
  merge_osm(list_of_osm)
}


# ------------------------------------------------------------
# CACHING FUNKTION
# ------------------------------------------------------------
get_osm_cached <- function(filename, query_fun) {
  if (file.exists(filename)) {
    message("Loading from cache: ", filename)
    readRDS(filename)
  } else {
    message("Downloading: ", filename)
    data <- query_fun()
    saveRDS(data, filename)
    data
  }
}


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

clip_to_sel <- function(x, sel) {
  if (is.null(x) || nrow(x) == 0) return(x)
  st_intersection(st_make_valid(x), st_make_valid(sel))
}

