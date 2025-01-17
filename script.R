#DATA PREPARATION
# Install necessary packages if they are not already installed
if (!require("rstudioapi")) install.packages("rstudioapi")
if (!require("tiff")) install.packages("tiff")
if (!require("terra")) install.packages("terra")
if (!require("sf")) install.packages("sf")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")

# Load packages
library(tiff)
library(terra)
library(sf)
library(rnaturalearthdata)

# Set the working directory to the directory where this script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("data")

# Print the current working directory
getwd()

# For data reconstructions in climate you can contact the first author of this paper: https://gmd.copernicus.org/articles/12/5137/2019/gmd-12-5137-2019-relations.html
# Relative path to the .tif file
EastAfrica <- file.path("..", "data", "EastAfrica_2_5My.tif")

# Path to the .tif file with the Köppen-Geiger reclassification of our study area (East Africa)
#EastAfrica <- "C:/Users/Usuario/OneDrive - Universidade de Vigo/PhD/ETIOPIA/DATA/EastAfrica_2_5My.tif"

# Read the .tif file
image_EastAfrica <- rast(EastAfrica)

# Load shapefiles with ocean and countries layers (both outline and filled) that we downloaded from NaturalEarth
ocean <- read_sf("ne_110m_ocean.shp")
countries <- read_sf("ne_110m_admin_0_countries.shp")
countries_filler <- read_sf("ne_110m_land.shp")

#####################################
#RESULT 1: CLASSIFICATION OF CLIMATE ZONES AT THE TIME OF MAXIMUM AND MINIMUM TEMPERATE

# Load shapefiles of rivers and lakes layers that we downloaded from NaturalEarth
rivers <- read_sf("ne_10m_rivers_lake_centerlines.shp")
lakes <- read_sf("ne_10m_lakes.shp")

# Define the coordinates of Melka Kunture (in decimal degrees)
lat_Melka <- 8 + 41/60 + 0/3600  
lon_Melka <- 37 + 38/60 + 0/3600  

par(mfrow=c(1, 2))
# Plot the reclassification of the study area at a warm period (minimum temperate pixels)
#pdf("2ClimateZones.pdf", width = 7, height = 4)
plot(image_EastAfrica[[2400]], main="2.399 Ma", col=c("#FF82AB", "#CDBE70", "#008B00"))
plot(rivers, add=T, col="#8EE5EE")
plot(lakes, add=T, col="#8EE5EE")
plot(ocean[2], add=T, col="#8EE5EE")
plot(st_geometry(countries), border="black", add=TRUE)
# Place a red point at the Melka Kunture's location
points(lon_Melka, lat_Melka, col="red", pch=19, cex=1, lwd=1)

# Plot the reclassification of the study area at a glacial period (maximum temperate pixels)
plot(image_EastAfrica[[1843]], main="1.842 Ma", col=c("#FF82AB", "#CDBE70", "#008B00"))
plot(rivers, add=T, col="#8EE5EE")
plot(lakes, add=T, col="#8EE5EE")
plot(ocean[2], add=T, col="#8EE5EE")
plot(st_geometry(countries), border="black", add=TRUE)
# Place a red point at the Melka Kunture's location
points(lon_Melka, lat_Melka, col="red", pch=19, cex=1, lwd=1)

#dev.off()

#####################################
#RESULT 2: MAP OF THE NUMBER OF CHANGES OF PIXELS
# Install necessary packages if they are not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("viridis")) install.packages("viridis")
if (!require("fields")) install.packages("fields")

# Load packages
library(ggplot2)
library(viridis)
library(fields)

# Define the relative path to the .nc file
Nchanges <- "../data/Nchanges_EastAfrica_2_5My.nc"

# Path to the .nc file with data on the age of pixels in the study area (East Africa)
#Nchanges <- "C:/Users/Usuario/OneDrive - Universidade de Vigo/PhD/ETIOPIA/DATA/Nchanges_EastAfrica_2_5My.nc"

# Read the .nc file
image_nchanges <- rast(Nchanges)

par(mfrow=c(1, 1))
# Define interval limits to represent the results with appropriate colours
breaks <- c(seq(0, 500, length.out = 101), seq(500.01, 1000, length.out = 2))
# Define colors for each interval
colors_viridis <- rev(viridis(100))

par(mfrow = c(1, 1))

# Define the color palette using viridis
colors_viridis <- rev(viridis(100))

# Plot the number of the pixels
plot(image_nchanges, main = "Number of changes", col = colors_viridis, legend = FALSE)
plot(ocean, add = TRUE, col = "#E0EEEE")
plot(st_geometry(countries), border = "black", add = TRUE)
# Place a red point at the Melka Kunture's location
points(lon_Melka, lat_Melka, col = "red", pch = 19, cex = 1, lwd = 1)

# Manually calculate the min and max for zlim
zlim_vals <- range(image_nchanges[], na.rm = TRUE)

# Add a gradient colour bar for the viridis palette
image.plot(zlim = zlim_vals, legend.only = TRUE, col = colors_viridis, smallplot = c(0.85, 0.9, 0.2, 0.8))
#####################################
# RESULT 3: CORRELATION BETWEEN CLIMATE ZONES

# Get the size of each pixel in "10^8ha"
size_pixel <- as.vector(cellSize(image_EastAfrica, unit = "ha") / 10000000)

# View the list of pixel sizes in "10^8ha"
print(size_pixel)

# Observe the pixel sizes represented
#plot(size_pixel)

# Calculate the dimensions of the first matrix in the list (returns the number of rows and columns of the matrix)
dim(image_EastAfrica[[1]])

# Create an empty matrix (res) with 2501 rows and 6400 columns filled with NA (to be filled in with the loop)
res <- matrix(NA, 2501, 6400)

# Convert the "res" matrix into a vector
for (i in 1:2501) {
  res[i, ] <- as.vector(image_EastAfrica[[i]]) 
}

# Save data so that the project ("Etiopia_Scripts") doesn't take up too much space (an example that saves "res" as excel (.csv))
#write.table(res, "area.csv", sep=",", row.names = F)

# Create an empty table (res_size) with 3 columns named "tropical", "desert" and "temperate"
res_size <- data.frame(tropical=0, desert=0, temperate=0)

# Calculate and store the results of the sum of "size_pixel" values for each type of pixel ("tropical", "desert" and "temperate")
for (i in 1:2501) {
  tropical <- sum(size_pixel[res[i,]==1], na.rm=T)
  desert <- sum(size_pixel[res[i,]==2], na.rm=T)
  temperate <- sum(size_pixel[res[i,]==3], na.rm=T)
  res_size[i,] <- c(tropical, desert, temperate) 
}

# AITCHISON: AVOIDING SPURIOUS CORRELATIONS
# Install necessary packages if they are not already installed
if (!require("compositions")) install.packages("compositions")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")

# Load packages
library(compositions)
library(ggplot2)
library(gridExtra)

# Define the function to calculate the centered log-ratio (CLR)
clr_transform <- function(data) {
  return(clr(data))
}

# Create the matrix with the data of the three climate zones
climate_data <- data.frame(
  tropical = res_size$tropical,
  temperate = res_size$temperate,
  desert = res_size$desert
)

# Apply CLR transformation to the data
clr_data <- as.data.frame(clr_transform(climate_data))

# Reverse the temporal order for plotting purposes
clr_data <- clr_data[2501:1, ]

# Plot temporal evolution after CLR transformation
par(mfrow = c(1, 1))
par(mar = c(5, 5, 4, 4))

# First plot: original temporal evolution in CLR-TRANSFORMATION
plot(2501:1, clr_data$tropical, type = "l", col = "#FF82AB", axes = FALSE,
     ylab = "CLR Transformed Area", xlab = "Million Years Before Present", frame.plot = FALSE,
     xlim = c(2500, 500), ylim = range(clr_data), main = "Temporal Evolution of Climate Zones (CLR)")
axis(1, at = seq(2500, 500, by = -100), labels = seq(2.5, 0.5, by = -0.1))
axis(2)
lines(2501:1, clr_data$temperate, type = "l", col = "#008B00")
lines(2501:1, clr_data$desert, type = "l", col = "#CDBE70")
legend("topright", legend = c("Tropical", "Temperate", "Desert"), col = c("#FF82AB", "#008B00", "#CDBE70"),
       lty = 1, bg = "white", y.intersp = 0.9, cex = 0.8)

# Calculate correlation and R2 of the comparison between temperate and tropical after CLR transformation
correlation_temperate_tropical <- cor(clr_data$tropical, clr_data$temperate)
model_temperate_tropical <- lm(temperate ~ tropical , data = clr_data)
r2_temperate_tropical <- summary(model_temperate_tropical)$r.squared
eq_temperate_tropical <- paste("y = ", round(coef(model_temperate_tropical)[1], 2), " + ", round(coef(model_temperate_tropical)[2], 2), "x", sep="")

# Calculate correlation and R2 of the comparison between temperate and desert after CLR transformation
correlation_temperate_desert <- cor(clr_data$desert, clr_data$temperate)
model_temperate_desert <- lm(temperate  ~ desert , data = clr_data)
r2_temperate_desert <- summary(model_temperate_desert)$r.squared
eq_temperate_desert <- paste("y = ", round(coef(model_temperate_desert)[1], 2), " + ", round(coef(model_temperate_desert)[2], 2), "x", sep="")

# Calculate correlation and R2 of the comparison between tropical and desert after CLR transformation
correlation_tropical_desert <- cor(clr_data$tropical, clr_data$desert)
model_tropical_desert <- lm( desert ~ tropical , data = clr_data)
r2_tropical_desert <- summary(model_tropical_desert)$r.squared
eq_tropical_desert <- paste("y = ", round(coef(model_tropical_desert)[1], 2), " + ", round(coef(model_tropical_desert)[2], 2), "x", sep="")

# Create a summary table with correlation and R2 values after CLR transformation
summary_table_clr <- data.frame(
  Comparison = c("Temperate vs Tropical (CLR)", "Temperate vs Desert (CLR)", "Tropical vs Desert (CLR)"),
  Correlation = c(correlation_temperate_tropical, correlation_temperate_desert, correlation_tropical_desert),
  R2 = c(r2_temperate_tropical, r2_temperate_desert, r2_tropical_desert),
  Equation = c(eq_temperate_tropical, eq_temperate_desert, eq_tropical_desert)
)
print(summary_table_clr)

par(mfrow=c(1,3))

# Plot the 3 comparisons with their respective regression lines after CLR transformation

# Tropical vs Temperate
plot1 <- ggplot(clr_data, aes(x=tropical, y=temperate)) +
  geom_point(size=1, shape=21, color="grey75", alpha=0.5, show.legend = FALSE) +
  geom_smooth(method=lm, color="grey20") +
  theme_minimal() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(x="Tropical", y="Temperate") + 
  xlim(-0.5, 1) + 
  ylim(-1.5, 1)

# Tropical vs Desert
plot2 <- ggplot(clr_data, aes(x=tropical, y=desert)) +
  geom_point(size=1, shape=19, color="grey75", alpha=0.5, show.legend = FALSE) +
  geom_smooth(method=lm, color="grey20") +
  theme_minimal() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(x="Tropical", y="Desert") + 
  xlim(-0.5, 1) + 
  ylim(-1.5, 1)

# Desert vs Temperate
plot3 <- ggplot(clr_data, aes(x=desert, y=temperate)) +
  geom_point(size=1, shape=19, color="grey75", alpha=0.5, show.legend = FALSE) +
  geom_smooth(method=lm, color="grey20") +
  theme_minimal() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(x="Desert", y="Temperate") + 
  xlim(-0.5, 1) + 
  ylim(-1.5, 1)

grid.arrange(plot1, plot2, plot3, ncol = 3)


#####################################
# RESULT 4: TEMPORAL EVOLUTION OF THE PIXEL AREA OF EACH CLIMATE ZONE AFTER CLR TRANSFORMATION

par(mfrow=c(1, 1))
# Plot the temporal evolution of pixel area for the 3 climate zones overlapped after CLR transformation
plot(2501:1, clr_data$tropical, type="l", col= "#FF82AB", axes=FALSE, ylab="CLR Transformed Area", xlab="Million Years Before Present", frame.plot = FALSE, xlim=c(2500, 500), ylim=range(clr_data))
axis(1, at = seq(2500, 500, by = -100), labels = seq(2.5, 0.5, by = -0.1))
axis(2)
lines(2501:1, clr_data$temperate, type="l", col="#008B00")
lines(2501:1, clr_data$desert, type="l", col="#CDBE70")
legend("topright", legend=c("Tropical", "Temperate", "Desert"), col=c("#FF82AB", "#008B00", "#CDBE70"), lty=1, bg="white", y.intersp=0.9, cex=0.8)

# REPRESENT THE SMOOTH TREND LINE
# Define a function to calculate moving average
moving_average <- function(x, n = 5){
  filter(x, rep(1/n, n), sides = 2)
}

# Smooth the time series using a 50-point moving average
tropical_smooth <- moving_average(clr_data$tropical, n = 50)
temperate_smooth <- moving_average(clr_data$temperate, n = 50)
desert_smooth <- moving_average(clr_data$desert, n = 50)

# Set up two plots, one above the other
par(mfrow = c(2, 1)) 

# First plot: original temporal evolution after CLR transformation
plot(2501:1, clr_data$tropical, type = "l", col = "#FF82AB", axes = FALSE,
     ylab = "CLR Transformed Area", xlab = "Million Years Before Present", frame.plot = FALSE,
     xlim = c(2500, 500), ylim = range(clr_data), main = "Temporal Evolution of the Pixel Area of Each Climate Zone (CLR)")
axis(1, at = seq(2500, 500, by = -100), labels = seq(2.5, 0.5, by = -0.1))
axis(2)
lines(2501:1, clr_data$temperate, type = "l", col = "#008B00")
lines(2501:1, clr_data$desert, type = "l", col = "#CDBE70")
legend("topright", legend = c("Tropical", "Temperate", "Desert"), col = c("#FF82AB", "#008B00", "#CDBE70"),
       lty = 1, bg = "white", y.intersp = 0.9, cex = 0.8)

# Second plot: smoothed temporal evolution after CLR transformation
plot(2501:1, tropical_smooth, type = "l", col = "#FF82AB", axes = FALSE,
     ylab = "CLR Transformed Area", xlab = "Million Years Before Present", frame.plot = FALSE,
     xlim = c(2500, 500), ylim = range(clr_data), main = "Smooth Time Evolution of the Pixel Area of Each Climate Zone (CLR)")
axis(1, at = seq(2500, 500, by = -100), labels = seq(2.5, 0.5, by = -0.1))
axis(2)
lines(2501:1, temperate_smooth, type = "l", col = "#008B00")
lines(2501:1, desert_smooth, type = "l", col = "#CDBE70")
legend("topright", legend = c("Tropical", "Temperate", "Desert"), col = c("#FF82AB", "#008B00", "#CDBE70"),
       lty = 1, bg = "white", y.intersp = 0.9, cex = 0.8)
