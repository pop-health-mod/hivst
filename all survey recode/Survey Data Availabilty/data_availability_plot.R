
library(readxl)
library(dplyr)
library(ggplot2)
library(Cairo)
data_avail <- read_excel("Survey_Data_Availability_Plot (cnt without pgm data).xlsx")

data_avail$Year <- as.integer(sub("-.*", "", data_avail$Year))

country_to_region <- c(
  "Kenya" = "Eastern", "Madagascar" = "Eastern", 
  "Malawi" = "Eastern", "Mozambique" = "Eastern", "Rwanda" = "Eastern", 
  "Tanzania" = "Eastern", "Uganda" = "Eastern", "Zambia" = "Eastern", 
  "Zimbabwe" = "Eastern", "Burundi" = "Central", 
  "Cameroon" = "Central", "Democratic Republic of the Congo" = "Central", 
  "Botswana" = "Southern", "Eswatini" = "Southern", "Lesotho" = "Southern", 
  "Namibia" = "Southern", "South Africa" = "Southern", "Benin" = "Western", 
  "Burkina Faso" = "Western", "Cote d'Ivoire" = "Western", "Ghana" = "Western", 
  "Guinea" = "Western", "Guinea-Bissau" = "Western", "Liberia" = "Western", 
  "Mali" = "Western", "Nigeria" = "Western", "Senegal" = "Western", 
  "Sierra Leone" = "Western"
)

data_avail$Region  <- country_to_region[data_avail$Country]
data_avail$Country <- factor(
  data_avail$Country,
  levels = names(country_to_region)[order(country_to_region)]
)



p <- ggplot(data_avail, aes(x = Year, y = Country,
                       colour = Survey,
                       shape  = Survey)) +
  geom_point(size = 3, alpha = 0.7) +
  
  scale_colour_manual(values = c(
    DHS  = "#8E24AA",
    MICS = "#43A047",
    PHIA = "#D32F2F",
    KAIS = "#FFB300",
    BAIS = "#1E88E5"
  )) +
  
  scale_shape_manual(values = c(
    DHS  = 16,   # ●
    MICS = 17,   # ▲
    PHIA = 18,   # ◆
    BAIS = 15,   # ■
    KAIS = 19    # ● (smaller bullet)
  )) +
  
  scale_x_continuous(breaks = 2012:2022) +
  facet_grid(Region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(
    x      = "Year",
    y      = "Country",
    colour = "Survey Type",
    shape  = "Survey Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey85", colour = NA),
    panel.grid       = element_line(color = "grey95")
  )



ggsave("survey_data_availability.png", p,
       width  = 10, height = 7, units = "in", dpi = 300)


