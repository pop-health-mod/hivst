
# Load libraries
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)

# Load the world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define northern African countries to exclude
northern_africa <- c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia", "W. Sahara")

# Filter Sub-Saharan Africa by excluding northern African countries
sub_saharan_africa_map <- world[world$region_un == "Africa" & !world$name %in% northern_africa, ]

# Adjust country names to match shapefile and highlight countries in Sub-Saharan Africa
highlight_countries <- c("Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
                         "Côte d'Ivoire", 
                         "Dem. Rep. Congo", "eSwatini", 
                         "Ghana", "Guinea", "Guinea-Bissau", "Kenya", 
                         "Lesotho", "Liberia", "Madagascar", "Malawi", 
                         "Mali", "Mozambique", "Namibia", 
                         "Rwanda", "Senegal", 
                         "Sierra Leone", "South Africa", "Tanzania", 
                         "Uganda", "Zambia", "Zimbabwe")

# Filter highlighted countries from the Sub-Saharan Africa map
highlighted_map <- sub_saharan_africa_map[sub_saharan_africa_map$name %in% highlight_countries, ]

# Set tmap to plot mode
tmap_mode("plot")

# Adjust the plot with zoom into Sub-Saharan Africa
tm_shape(sub_saharan_africa_map) + 
  tm_borders() + 
  tm_shape(highlighted_map) + 
  tm_fill(col = "#FFA07A") +  # Use light orange shade
  tm_borders(lwd = 1.5, col = "darkgrey") +
  tm_layout(
    title = "Countries with available HIVST information", 
    title.position = c("center", "bottom"),  
    title.size = 1.5,  
    inner.margins = c(0.02, 0.1, 0.05, 0.1),  # Adjust margins to zoom in and center the map
    frame = FALSE
  )



