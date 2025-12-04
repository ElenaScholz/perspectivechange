load_libs <- function(libs) {
  # Identify which packages are not installed
  installed_libs <- libs %in% rownames(installed.packages())
  
  # Install missing ones
  if (any(!installed_libs)) {
    install.packages(libs[!installed_libs])
  }
  
  # Load all packages quietly
  invisible(lapply(libs, library, character.only = TRUE))
}

get_country_borders <- function(country_ISO, level){
  main_path <- getwd()
  country_borders <- geodata::gadm(
    country = country_ISO,
    level = level,
    path = main_path
  ) |>
    sf::st_as_sf()
  
  return (country_borders)
}