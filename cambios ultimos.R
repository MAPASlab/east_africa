# Create a raster for the warm period (2.399 Ma) with only pixel values 1 (tropical)
image_EastAfrica_warm <- classify(image_EastAfrica[[2400]], rbind(c(-Inf, 0.9, NA), c(1, 1, 1), c(1.1, Inf, NA)))

# Create a raster for the cold period (1.842 Ma) with only pixel values 3 (temperate)
image_EastAfrica_cold <- classify(image_EastAfrica[[1843]], rbind(c(-Inf, 2.9, NA), c(3, 3, 3), c(3.1, Inf, NA)))


par(mfrow=c(1, 2))
# Plot at a warm period only tropical (minimum temperate pixels)
plot(image_EastAfrica_warm, main="2.399 Ma (tropical maximum)", col=c("#FF82AB"))
plot(rivers, add=T, col="#8EE5EE")
plot(lakes, add=T, col="#8EE5EE")
plot(ocean[2], add=T, col="#8EE5EE")
plot(st_geometry(countries), border="black", add=TRUE)
points(lon_Melka, lat_Melka, col="red", pch=19, cex=1, lwd=1)

# Plot at a cold period only temperate (maximum temperate pixels)
plot(image_EastAfrica_cold, main="1.842 Ma (temperate maximum)", col=c("#008B00"))
plot(rivers, add=T, col="#8EE5EE")
plot(lakes, add=T, col="#8EE5EE")
plot(ocean[2], add=T, col="#8EE5EE")
plot(st_geometry(countries), border="black", add=TRUE)
points(lon_Melka, lat_Melka, col="red", pch=19, cex=1, lwd=1)

