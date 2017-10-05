# data(quakes)
# 
# # Show first 20 rows from the `quakes` dataset
# leaflet(data = quakes[1:20,]) %>% addTiles() %>%
# addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
# 
library(leaflet)

Data_final.data <- read.csv("/home/amcdowald/wrksite/DS610/R_app/data/Anthony_csv.csv")
theft.data <-
  read.csv(
    "./data/Theftvehicles_with_time.csv",
    sep = "\t",
    quote = "\"",
    header = TRUE
  )

m <- leaflet(data = theft.data[1:20,]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng= ~Long, lat= ~Lat, popup="The birthplace of R")
m  # Print the map