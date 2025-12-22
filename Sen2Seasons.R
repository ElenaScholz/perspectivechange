# rm(list = ls())
# renv::activate()
# 
# # ------------------------------------------------------------
# # CONFIG
# # ------------------------------------------------------------
# area <- "bochum"
# use_ndmi <- FALSE  # TRUE = berechne NDMI neu, FALSE = nutze vorhandenen NDWI
# 
# # ------------------------------------------------------------
# # Libraries
# # ------------------------------------------------------------
# source("utils.R")
# load_libs(c("osmdata","sf","ggplot2", "geodata", "terra", "dplyr", "purrr"))
# 
# set_overpass_url("https://overpass.kumi.systems/api/interpreter")
# 
# # ------------------------------------------------------------
# # GET ADMIN BOUNDARIES
# # ------------------------------------------------------------
# country_borders <- get_country_borders("DEU", 3)
# nrw <- country_borders %>%
#   filter(NAME_1 == "Nordrhein-Westfalen")
# bochum <- nrw %>% filter(NAME_2 == "Bochum")
# 
# # Paths to seasonal images
# files <- list(
#   spring = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-03-01_2024-05-31_Bochum_Spring_Sen2NDBI.tif",
#   summer = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-06-01_2024-08-31_Bochum_Summer_Sen2NDBI.tif",
#   autumn = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-09-01_2024-11-30_Bochum_Autumn_ModLST.tif",
#   winter = "data/Sen2Seasons/Sentinel_2_All_bands_and_indices_median_2015-12-01_2024-02-28_Bochum_Winter_Sen2NDBI.tif"
# )
# 
# # ------------------------------------------------------------
# # 1. Lade alle Daten und schneide auf Bochum zu
# # ------------------------------------------------------------
# cat("Lade und schneide Daten zu...\n")
# 
# # Transformiere Bochum-Grenze ins CRS des ersten Rasters
# r_example <- rast(files[[1]])
# bochum_transformed <- st_transform(bochum, crs(r_example))
# 
# # Lade und schneide alle Raster zu
# rasters <- list()
# for(season in names(files)) {
#   cat("  -", season, "\n")
#   r <- rast(files[[season]])
#   r_cropped <- crop(r, bochum_transformed)
#   r_masked <- mask(r_cropped, bochum_transformed)
#   rasters[[season]] <- r_masked
# }
# 
# # ------------------------------------------------------------
# # 2. Berechne NDMI oder nutze vorhandenen NDWI
# # ------------------------------------------------------------
# if(use_ndmi) {
#   cat("\nBerechne NDMI (NIR - SWIR) / (NIR + SWIR)...\n")
#   cat("NDMI = Feuchtigkeit in Vegetation/Blättern\n")
#   
#   # NDMI = (NIR - SWIR) / (NIR + SWIR)
#   # Sentinel-2: NIR = Band 8, SWIR = Band 11
#   for(season in names(rasters)) {
#     r <- rasters[[season]]
#     
#     nir <- r[[8]]   # Band 8 = NIR (B8)
#     swir <- r[[11]] # Band 11 = SWIR1 (B11)
#     
#     # Berechne NDMI
#     ndmi <- (nir - swir) / (nir + swir)
#     
#     # Füge NDMI als neues Band hinzu
#     rasters[[season]] <- c(r, ndmi)
#     names(rasters[[season]])[nlyr(rasters[[season]])] <- "NDMI"
#     
#     cat(sprintf("  %s: NDMI berechnet (Band %d)\n", season, nlyr(rasters[[season]])))
#   }
#   
#   water_index_name <- "NDMI"
#   water_index_band <- function(r) nlyr(r)  # Letztes Band
#   
# } else {
#   cat("\nNutze vorhandenen NDWI (GREEN - NIR) / (GREEN + NIR)\n")
#   cat("NDWI = Wasserkörper (Seen, Flüsse)\n")
#   
#   water_index_name <- "NDWI"
#   water_index_band <- function(r) 12  # Band 12 = vorberechneter NDWI
# }
# 
# # ------------------------------------------------------------
# # 3. Berechne globale Min/Max für alle Indizes
# # ------------------------------------------------------------
# cat("\nBerechne globale Min/Max-Werte...\n")
# 
# # Extrahiere alle Indizes
# all_ndvi <- lapply(rasters, function(r) r[[11]])  # Band 11 = NDVI
# all_water <- lapply(rasters, function(r) r[[water_index_band(r)]])  # NDWI oder NDMI
# all_ndbi <- lapply(rasters, function(r) r[[13]])  # Band 13 = NDBI
# 
# # Berechne globale Minima und Maxima
# ndvi_min <- min(sapply(all_ndvi, function(x) global(x, "min", na.rm = TRUE)$min))
# ndvi_max <- max(sapply(all_ndvi, function(x) global(x, "max", na.rm = TRUE)$max))
# 
# water_min <- min(sapply(all_water, function(x) global(x, "min", na.rm = TRUE)$min))
# water_max <- max(sapply(all_water, function(x) global(x, "max", na.rm = TRUE)$max))
# 
# ndbi_min <- min(sapply(all_ndbi, function(x) global(x, "min", na.rm = TRUE)$min))
# ndbi_max <- max(sapply(all_ndbi, function(x) global(x, "max", na.rm = TRUE)$max))
# 
# cat(sprintf("  NDVI: %.3f bis %.3f\n", ndvi_min, ndvi_max))
# cat(sprintf("  %s: %.3f bis %.3f\n", water_index_name, water_min, water_max))
# cat(sprintf("  NDBI: %.3f bis %.3f\n", ndbi_min, ndbi_max))
# 
# # ------------------------------------------------------------
# # 4. Berechne GLOBALE Perzentile über alle Jahreszeiten
# # ------------------------------------------------------------
# cat("\nBerechne globale Perzentile für Normalisierung...\n")
# 
# # Sammle alle Werte für jeden Index
# all_ndvi_vals <- unlist(lapply(all_ndvi, function(x) values(x, na.rm = TRUE)))
# all_water_vals <- unlist(lapply(all_water, function(x) values(x, na.rm = TRUE)))
# all_ndbi_vals <- unlist(lapply(all_ndbi, function(x) values(x, na.rm = TRUE)))
# 
# # Entferne NA und Inf Werte
# all_ndvi_vals <- all_ndvi_vals[!is.na(all_ndvi_vals) & is.finite(all_ndvi_vals)]
# all_water_vals <- all_water_vals[!is.na(all_water_vals) & is.finite(all_water_vals)]
# all_ndbi_vals <- all_ndbi_vals[!is.na(all_ndbi_vals) & is.finite(all_ndbi_vals)]
# 
# # Berechne globale Perzentile
# ndvi_q02 <- quantile(all_ndvi_vals, 0.02)
# ndvi_q98 <- quantile(all_ndvi_vals, 0.98)
# 
# water_q02 <- quantile(all_water_vals, 0.02)
# water_q98 <- quantile(all_water_vals, 0.98)
# 
# ndbi_q02 <- quantile(all_ndbi_vals, 0.02)
# ndbi_q98 <- quantile(all_ndbi_vals, 0.98)
# 
# cat(sprintf("  NDVI Perzentile (2%%, 98%%): %.3f, %.3f\n", ndvi_q02, ndvi_q98))
# cat(sprintf("  %s Perzentile (2%%, 98%%): %.3f, %.3f\n", water_index_name, water_q02, water_q98))
# cat(sprintf("  NDBI Perzentile (2%%, 98%%): %.3f, %.3f\n", ndbi_q02, ndbi_q98))
# 
# # Normalisierungsfunktion mit GLOBALEN Werten
# normalize_with_global <- function(x, q_min, q_max) {
#   normalized <- (x - q_min) / (q_max - q_min) * 255
#   normalized[normalized < 0] <- 0
#   normalized[normalized > 255] <- 255
#   return(normalized)
# }
# 
# # ------------------------------------------------------------
# # 5. Erstelle Plots für jede Jahreszeit
# # ------------------------------------------------------------
# cat("\nErstelle Plots...\n")
# 
# for(season in names(rasters)) {
#   cat(sprintf("\nPlotte %s...\n", season))
#   r <- rasters[[season]]
#   
#   # Extrahiere Indizes
#   ndvi <- r[[11]]
#   water <- r[[water_index_band(r)]]
#   ndbi <- r[[13]]
#   
#   # Debug: Prüfe Wertebereich
#   cat(sprintf("  Original ranges:\n"))
#   cat(sprintf("    NDVI: %.3f to %.3f\n", 
#               global(ndvi, "min", na.rm = TRUE)$min,
#               global(ndvi, "max", na.rm = TRUE)$max))
#   cat(sprintf("    %s: %.3f to %.3f\n", water_index_name,
#               global(water, "min", na.rm = TRUE)$min,
#               global(water, "max", na.rm = TRUE)$max))
#   cat(sprintf("    NDBI: %.3f to %.3f\n",
#               global(ndbi, "min", na.rm = TRUE)$min,
#               global(ndbi, "max", na.rm = TRUE)$max))
#   
#   # Normalisiere Indizes mit GLOBALEN Perzentilen
#   ndvi_norm <- normalize_with_global(ndvi, ndvi_q02, ndvi_q98)
#   water_norm <- normalize_with_global(water, water_q02, water_q98)
#   ndbi_norm <- normalize_with_global(ndbi, ndbi_q02, ndbi_q98)
#   
#   # Erstelle RGB-Stack: R=NDBI, G=NDVI, B=NDWI/NDMI
#   rgb_stack <- c(ndbi_norm, ndvi_norm, water_norm)
#   
#   # 2x2 Layout
#   par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
#   
#   # 1. NDVI - Grüne Farbskala
#   plot(ndvi_norm, 
#        col = colorRampPalette(c("white", "lightgreen", "darkgreen"))(100),
#        main = paste(tools::toTitleCase(season), "- NDVI (Vegetation)"),
#        axes = FALSE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "black", lwd = 1.5)
#   
#   # 2. NDWI/NDMI - Blaue Farbskala
#   water_label <- ifelse(use_ndmi, "NDMI (Vegetationsfeuchtigkeit)", "NDWI (Wasserkörper)")
#   plot(water_norm,
#        col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
#        main = paste(tools::toTitleCase(season), "-", water_label),
#        axes = FALSE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "black", lwd = 1.5)
#   
#   # 3. NDBI - Rote Farbskala
#   plot(ndbi_norm,
#        col = colorRampPalette(c("white", "orange", "red", "darkred"))(100),
#        main = paste(tools::toTitleCase(season), "- NDBI (Bebauung)"),
#        axes = FALSE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "black", lwd = 1.5)
#   
#   # 4. RGB-Kombination
#   rgb_label <- paste0("RGB (R=NDBI, G=NDVI, B=", water_index_name, ")")
#   plotRGB(rgb_stack, 
#           r = 1, g = 2, b = 3, 
#           scale = 255,
#           stretch = NULL,
#           main = paste(tools::toTitleCase(season), "-", rgb_label),
#           axes = FALSE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
#   
#   # Warte auf Benutzereingabe für nächste Jahreszeit (optional)
#   if(season != names(rasters)[length(rasters)]) {
#     cat("Drücke Enter für nächste Jahreszeit...\n")
#     readline()
#   }
# }
# 
# 
# # ------------------------------------------------------------
# # 6. Speichere einzelne Plots als PNG
# # ------------------------------------------------------------
# cat("\nSpeichere Plots als PNG-Dateien...\n")
# 
# for(season in names(rasters)) {
#   r <- rasters[[season]]
#   
#   # Normalisiere mit GLOBALEN Perzentilen
#   ndvi_norm <- normalize_with_global(r[[11]], ndvi_q02, ndvi_q98)
#   water_norm <- normalize_with_global(r[[water_index_band(r)]], water_q02, water_q98)
#   ndbi_norm <- normalize_with_global(r[[13]], ndbi_q02, ndbi_q98)
#   rgb_stack <- c(ndbi_norm, ndvi_norm, water_norm)
#   
#   # Speichere als PNG (hohe Auflösung)
#   outfile <- paste0("maps/", season, "_Sen24panel_bochum_", water_index_name, ".png")
#   png(outfile, width = 3000, height = 3000, res = 300)
#   
#   par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
#   
#   # NDVI - Grün
#   plot(ndvi_norm, 
#        col = colorRampPalette(c("white", "lightgreen", "darkgreen"))(100),
#        main = paste(tools::toTitleCase(season), "- NDVI (Vegetation)"),
#        axes = FALSE, legend = TRUE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
#   
#   # NDWI/NDMI - Blau
#   water_label <- ifelse(use_ndmi, "NDMI (Vegetationsfeuchtigkeit)", "NDWI (Wasserkörper)")
#   plot(water_norm,
#        col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
#        main = paste(tools::toTitleCase(season), "-", water_label),
#        axes = FALSE, legend = TRUE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
#   
#   # NDBI - Rot
#   plot(ndbi_norm,
#        col = colorRampPalette(c("white", "orange", "red", "darkred"))(100),
#        main = paste(tools::toTitleCase(season), "- NDBI (Bebauung)"),
#        axes = FALSE, legend = TRUE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
#   
#   # RGB
#   rgb_label <- paste0("RGB (R=NDBI, G=NDVI, B=", water_index_name, ")")
#   plotRGB(rgb_stack, r = 1, g = 2, b = 3, scale = 255, stretch = NULL,
#           main = paste(tools::toTitleCase(season), "-", rgb_label),
#           axes = FALSE)
#   plot(st_geometry(bochum_transformed), add = TRUE, border = "white", lwd = 2)
#   
#   dev.off()
#   cat("  Gespeichert:", outfile, "\n")
# }
# 
# cat("\n=======================================================")
# cat("\nIndex-Erklärung:")
# cat("\n")
# cat("\nNDWI (GREEN - NIR) / (GREEN + NIR):")
# cat("\n  - Zeigt Wasserkörper: Seen, Flüsse, Teiche")
# cat("\n  - Hohe Werte (blau) = offenes Wasser")
# cat("\n  - Niedrige Werte = Land/Bebauung")
# cat("\n")
# cat("\nNDMI (NIR - SWIR) / (NIR + SWIR):")
# cat("\n  - Zeigt Feuchtigkeit in Vegetation/Blättern")
# cat("\n  - Hohe Werte (blau) = feuchte Vegetation")
# cat("\n  - Niedrige Werte = trockene Vegetation/Bebauung")
# cat("\n")
# cat("\nAKTUELL VERWENDET: ", water_index_name)
# cat("\n")
# cat("\nUm zwischen NDWI und NDMI zu wechseln:")
# cat("\n  Setze 'use_ndmi <- TRUE' oder 'use_ndmi <- FALSE' oben im Script")
# cat("\n=======================================================\n")



rm(list = ls())
renv::activate()

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
source("utils.R")
load_libs(c("osmdata","sf","ggplot2", "geodata", "terra", "dplyr", "purrr"))

set_overpass_url("https://overpass.kumi.systems/api/interpreter")




r_list <- list.files(
  path = "C:/Users/elena/Documents/RProjects/perspectivechange/data/tulips/",
  full.names = T
)

spring <- rast(r_list[[1]])
summer <- rast(r_list[[2]])
autumn <- rast(r_list[[3]])
# ------------------------------------------------------------
# 3. Extrahiere Indizes für alle Jahreszeiten
# ------------------------------------------------------------
cat("\nExtrahiere Indizes...\n")

# NDVI (Band 11)
ndvi_spring <- spring[[11]]
ndvi_summer <- summer[[11]]
ndvi_autumn <- autumn[[11]]



library(terra)

ndvi_stack <- c(ndvi_spring, ndvi_summer, ndvi_autumn)
names(ndvi_stack) <- c("spring", "summer", "autumn")

# global min/max across all three rasters
vmin <- global(ndvi_stack, "min", na.rm = TRUE)[1]
vmax <- global(ndvi_stack, "max", na.rm = TRUE)[1]

vmin
vmax


ndvi_rgb <- (ndvi_scaled + 1) / 2

rgb_ndvi <- c(
  ndvi_stack[["spring"]],
  ndvi_stack[["summer"]],
  ndvi_stack[["autumn"]]
)

png(
  filename = "ndvi_seasonal_rgb.png",
  width  = 40,
  height = 46,
  units  = "cm",
  res    = 300
)

plotRGB(
  rgb_ndvi,
  r = 1, g = 2, b = 3,
  stretch = "lin",
  axes = FALSE,
  main = "Seasonal NDVI RGB (R=Summer, G=Spring, B=Autumn)"
)

dev.off()





png(
  filename = "ndvi_seasonal_rgb_streched.png",
  width  = 40,
  height = 46,
  units  = "cm",
  res    = 300
)
plotRGB(
  rgb_ndvi,
  r = 1, g = 2, b = 3,
  zlim = c(-1,1),
  stretch = "lin",
  main = "Seasonal NDVI RGB (R=Summer, G=Spring, B=Autumn)",
  axes = FALSE
)

dev.off()
