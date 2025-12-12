rm(list = ls())
renv::activate()

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
area <- "bochum"

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
source("utils.R")
load_libs(c("osmdata","sf","ggplot2", "geodata", "terra", "dplyr", "purrr"))

set_overpass_url("https://overpass.kumi.systems/api/interpreter")

# ------------------------------------------------------------
# GET ADMIN BOUNDARIES
# ------------------------------------------------------------
country_borders <- get_country_borders("DEU", 3)
nrw <- country_borders %>%
  filter(NAME_1 == "Nordrhein-Westfalen")
bochum <- nrw %>% filter(NAME_2 == "Bochum")

# Paths to seasonal images
files <- list(
  spring = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-03-01_2024-05-31_Bochum_Spring_Sen2NDBI.tif",
  summer = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-06-01_2024-08-31_Bochum_Summer_Sen2NDBI.tif",
  autumn = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-09-01_2024-11-30_Bochum_Autumn_ModLST.tif",
  winter = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-12-01_2024-02-28_Bochum_Winter_Sen2NDBI.tif"
)

# ------------------------------------------------------------
# 1. Lade alle Daten und schneide auf Bochum zu
# ------------------------------------------------------------
cat("Lade und schneide Daten zu...\n")

# Transformiere Bochum-Grenze ins CRS des ersten Rasters
r_example <- rast(files[[1]])
bochum_transformed <- st_transform(bochum, crs(r_example))

# Lade und schneide alle Raster zu
rasters <- list()
for(season in names(files)) {
  cat("  -", season, "\n")
  r <- rast(files[[season]])
  r_cropped <- crop(r, bochum_transformed)
  r_masked <- mask(r_cropped, bochum_transformed)
  rasters[[season]] <- r_masked
}

# ------------------------------------------------------------
# 2. Berechne globale Min/Max für jede Bandkombination
# ------------------------------------------------------------
cat("\nBerechne globale Min/Max-Werte...\n")

# Extrahiere alle Indizes
all_ndvi <- lapply(rasters, function(r) r[[11]])
all_ndwi <- lapply(rasters, function(r) r[[12]])
all_ndbi <- lapply(rasters, function(r) r[[13]])

# Berechne globale Minima und Maxima
ndvi_min <- min(sapply(all_ndvi, function(x) global(x, "min", na.rm = TRUE)$min))
ndvi_max <- max(sapply(all_ndvi, function(x) global(x, "max", na.rm = TRUE)$max))

ndwi_min <- min(sapply(all_ndwi, function(x) global(x, "min", na.rm = TRUE)$min))
ndwi_max <- max(sapply(all_ndwi, function(x) global(x, "max", na.rm = TRUE)$max))

ndbi_min <- min(sapply(all_ndbi, function(x) global(x, "min", na.rm = TRUE)$min))
ndbi_max <- max(sapply(all_ndbi, function(x) global(x, "max", na.rm = TRUE)$max))

cat(sprintf("  NDVI: %.3f bis %.3f\n", ndvi_min, ndvi_max))
cat(sprintf("  NDWI: %.3f bis %.3f\n", ndwi_min, ndwi_max))
cat(sprintf("  NDBI: %.3f bis %.3f\n", ndbi_min, ndbi_max))

# ------------------------------------------------------------
# 3. Normalisierungsfunktion mit Perzentilen (robuster!)
# ------------------------------------------------------------
normalize_index_percentile <- function(x, p_min = 0.02, p_max = 0.98) {
  # Berechne Perzentile (ignoriert Extremwerte)
  vals <- values(x, na.rm = TRUE)
  vals <- vals[!is.na(vals) & is.finite(vals)]
  
  q_min <- quantile(vals, p_min, na.rm = TRUE)
  q_max <- quantile(vals, p_max, na.rm = TRUE)
  
  # Normalisiere auf 0-255 für bessere Visualisierung
  normalized <- (x - q_min) / (q_max - q_min) * 255
  normalized[normalized < 0] <- 0
  normalized[normalized > 255] <- 255
  
  return(normalized)
}

# ------------------------------------------------------------
# 4. Erstelle RGB-Plots mit Perzentil-basierter Normalisierung
# ------------------------------------------------------------
plot_indices_rgb <- function(raster_obj, season_name) {
  # Extrahiere Bänder
  ndvi <- raster_obj[[11]]
  ndwi <- raster_obj[[12]]
  ndbi <- raster_obj[[13]]
  
  # Debug: Prüfe Wertebereich
  cat(sprintf("\n%s - Original ranges:\n", season_name))
  cat(sprintf("  NDVI: %.3f to %.3f\n", 
              global(ndvi, "min", na.rm = TRUE)$min,
              global(ndvi, "max", na.rm = TRUE)$max))
  cat(sprintf("  NDWI: %.3f to %.3f\n",
              global(ndwi, "min", na.rm = TRUE)$min,
              global(ndwi, "max", na.rm = TRUE)$max))
  cat(sprintf("  NDBI: %.3f to %.3f\n",
              global(ndbi, "min", na.rm = TRUE)$min,
              global(ndbi, "max", na.rm = TRUE)$max))
  
  # Normalisiere mit Perzentilen (robuster gegen Ausreißer)
  ndbi_norm <- normalize_index_percentile(ndbi)
  ndvi_norm <- normalize_index_percentile(ndvi)
  ndwi_norm <- normalize_index_percentile(ndwi)
  
  # Erstelle RGB-Stack: R=NDBI, G=NDVI, B=NDWI
  rgb_stack <- c(ndbi_norm, ndvi_norm, ndwi_norm)
  names(rgb_stack) <- c("NDBI", "NDVI", "NDWI")
  
  # Plot ohne zusätzlichen Stretch (Werte sind schon 0-255)
  plotRGB(rgb_stack, 
          r = 1, g = 2, b = 3, 
          scale = 255,
          stretch = NULL,
          main = paste(season_name, "| R=NDBI G=NDVI B=NDWI"),
          axes = TRUE)
  
  # Füge Bochum-Grenze hinzu
  plot(st_geometry(bochum_transformed), add = TRUE, border = "yellow", lwd = 2)
}

# ------------------------------------------------------------
# 5. Erstelle Plots für jede Jahreszeit
# ------------------------------------------------------------
cat("\nErstelle Plots...\n")

for(season in names(rasters)) {
  cat(sprintf("\nPlotte %s...\n", season))
  r <- rasters[[season]]
  
  # Extrahiere und normalisiere Indizes
  ndvi <- r[[11]]
  ndwi <- r[[12]]
  ndbi <- r[[13]]
  
  ndvi_norm <- normalize_index_percentile(ndvi)
  ndwi_norm <- normalize_index_percentile(ndwi)
  ndbi_norm <- normalize_index_percentile(ndbi)
  
  # Erstelle RGB-Stack
  rgb_stack <- c(ndbi_norm, ndvi_norm, ndwi_norm)
  
  # 2x2 Layout
  par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
  
  # 1. NDVI - Grüne Farbskala
  plot(ndvi_norm, 
       col = colorRampPalette(c("white", "lightgreen", "darkgreen"))(100),
       main = paste(tools::toTitleCase(season), "- NDVI (Vegetation)"),
       axes = FALSE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "black", lwd = 1.5)
  
  # 2. NDWI - Blaue Farbskala
  plot(ndwi_norm,
       col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
       main = paste(tools::toTitleCase(season), "- NDWI (Wasser)"),
       axes = FALSE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "black", lwd = 1.5)
  
  # 3. NDBI - Rote Farbskala
  plot(ndbi_norm,
       col = colorRampPalette(c("white", "orange", "red", "darkred"))(100),
       main = paste(tools::toTitleCase(season), "- NDBI (Bebauung)"),
       axes = FALSE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "black", lwd = 1.5)
  
  # 4. RGB-Kombination
  plotRGB(rgb_stack, 
          r = 1, g = 2, b = 3, 
          scale = 255,
          stretch = NULL,
          main = paste(tools::toTitleCase(season), "- RGB (R=NDBI, G=NDVI, B=NDWI)"),
          axes = FALSE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
  
  # Warte auf Benutzereingabe für nächste Jahreszeit (optional)
  if(season != names(rasters)[length(rasters)]) {
    cat("Drücke Enter für nächste Jahreszeit...\n")
    readline()
  }
}

# ------------------------------------------------------------
# Optional: Speichere einzelne Plots als PNG
# ------------------------------------------------------------
cat("\nSpeichere Plots als PNG-Dateien...\n")

for(season in names(rasters)) {
  r <- rasters[[season]]
  
  # Normalisiere
  ndvi_norm <- normalize_index_percentile(r[[11]])
  ndwi_norm <- normalize_index_percentile(r[[12]])
  ndbi_norm <- normalize_index_percentile(r[[13]])
  rgb_stack <- c(ndbi_norm, ndvi_norm, ndwi_norm)
  
  # Speichere als PNG (hohe Auflösung)
  png(paste0("maps/", season, "_Sen24panel_bochum.png"), 
      width = 3000, height = 3000, res = 300)
  
  par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
  
  # NDVI - Grün
  plot(ndvi_norm, 
       col = colorRampPalette(c("white", "lightgreen", "darkgreen"))(100),
       main = paste(tools::toTitleCase(season), "- NDVI"),
       axes = FALSE, legend = TRUE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
  
  # NDWI - Blau
  plot(ndwi_norm,
       col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
       main = paste(tools::toTitleCase(season), "- NDWI"),
       axes = FALSE, legend = TRUE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
  
  # NDBI - Rot
  plot(ndbi_norm,
       col = colorRampPalette(c("white", "orange", "red", "darkred"))(100),
       main = paste(tools::toTitleCase(season), "- NDBI"),
       axes = FALSE, legend = TRUE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
  
  # RGB
  plotRGB(rgb_stack, r = 1, g = 2, b = 3, scale = 255, stretch = NULL,
          main = paste(tools::toTitleCase(season), "- RGB Composite"),
          axes = FALSE)
  plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
  
  dev.off()
  cat("  Gespeichert:", paste0(season, "_4panel_bochum.png\n"))
}
