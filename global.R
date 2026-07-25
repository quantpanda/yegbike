# Global file
library(readr)
library(dplyr)
# load data from OpenEdmonton via API

# api_endpoint <- "https://data.edmonton.ca/resource/vd4b-a4iv.csv?$query=SELECT%20*%20WHERE%20(%60route_coming_soon%60%20%3D%20false)"
# response <- read.csv(api_endpoint)
response <- read_csv("Bike_Routes_20251013.csv")
bike_routes <- as.data.frame(response) %>% 
  sf::st_as_sf( wkt = "geometry_line", crs = 4326)


# Dataframe of Bike Sensor Counts

sensor_counts <- read_csv("Daily_Pedestrian_and_Bike_Counts_20251102.csv") 
sensor_counts <- sensor_counts %>%
  mutate(day = as.Date(as.POSIXct(log_timstamp, format = "%Y %b %d %I:%M:%S %p")),
         year = as.numeric(format(as.Date(day), "%Y")),
         month = as.numeric(format(as.Date(day), "%m")),
         `Total Cyclist Count` = case_when(
           `Total Cyclist Count`>= 2500 ~ NA,
           TRUE ~ `Total Cyclist Count`
         ),
         `Total Pedestrian Count` = case_when(
           `Total Pedestrian Count`>= 15000 ~ NA,
           TRUE ~ `Total Pedestrian Count`
         )
         )
  
  
cycling_counts_by_location <- sensor_counts %>%
  group_by(`Counter Location Description`) %>%
  summarise(Total_Cyclist_Count = sum(`Total Cyclist Count`, na.rm = TRUE)) %>%
  arrange(desc(Total_Cyclist_Count))

cycling_counts_by_day <- sensor_counts %>%
  group_by(day) %>%
  summarise(Total_Cyclist_Count = sum(`Total Cyclist Count`, na.rm = TRUE)) %>%
  arrange(day)


plot(cycling_counts_by_day$day, cycling_counts_by_day$Total_Cyclist_Count, type = "l",
     main = "Total Cyclist Counts Over Time",
     xlab = "Date",
     ylab = "Total Cyclist Count")

#PEDESTRIAN COUNTS
pedestrian_counts_by_location <- sensor_counts %>%
  group_by(`Counter Location Description`) %>%
  summarise(Total_Pedestrian_Count = sum(`Total Pedestrian Count`, na.rm
 = TRUE)) %>%
  arrange(desc(Total_Pedestrian_Count))

pedestrian_counts_by_day <- sensor_counts %>%
  group_by(day) %>%
  summarise(Total_Pedestrian_Count = sum(`Total Pedestrian Count`, na.rm
 = TRUE)) %>%
  arrange(day)


plot(pedestrian_counts_by_day$day, pedestrian_counts_by_day$Total_Pedestrian_Count, type = "l",
     main = "Total Pedestrian Counts Over Time",
     xlab = "Date",
     ylab = "Total Pedestrian Count")

# bike counts by year
sensor_counts %>%
  group_by(year) %>%
  summarise(Total_Cyclist_Count = sum(`Total Cyclist Count`, na.rm = TRUE))

# total daily reports from sensors by year
table(sensor_counts$year)

# cumulative bike counts
sensor_counts %>%
  group_by(day) %>%
  summarise(Total_Cyclist_Count = sum(`Total Cyclist Count`, na.rm
 = TRUE)) %>%
  arrange(day) %>%
  mutate(Cumulative_Cyclist_Count = cumsum(Total_Cyclist_Count)) %>% 
  tail()

length(table(sensor_counts$`Counter Location Description`))

# bot plot of `Total Cycling Count` by `Counter Location Description`
library(ggplot2)
ggplot(sensor_counts, aes(x = `Counter Location Description`, y = `Total Cyclist Count`)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Total Cycling Counts by Counter Location",
    x = "Counter Location Description",
    y = "Total Cycling Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sensor_counts, aes(x = `Counter Location Description`, y = `Total Cyclist Count`)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Total Cycling Counts by Counter Location",
    x = "Counter Location Description",
    y = "Total Cycling Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plot the number of cyclist for each day
ggplot(sensor_counts, aes(x = log_timstamp, y = `Total Cyclist Count`)) + geom_line(color = "blue") +
  theme_minimal() +
  labs(
    title = "Total Cycling Counts Over Time",
    x = "Date",
    y = "Total Cycling Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sensor_counts, aes(x = log_timstamp, y = `Total Pedestrian Count`)) + geom_line(color = "blue") +
  theme_minimal() +
  labs(
    title = "Total Pedestrian Counts Over Time",
    x = "Date",
    y = "Total Pedestrian Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# library(httr)
# library(jsonlite)
# library(dplyr)
# library(sf)
# library(purrr)
# 
# # 1. Download data
# api_endpoint2 <- "https://data.edmonton.ca/resource/vd4b-a4iv.json?$limit=20000"
# response <- httr::GET(api_endpoint2) %>%
#   httr::content("text") %>%
#   jsonlite::fromJSON(flatten = TRUE)
# 
# # 2. Convert to tibble
# bike_routes <- as_tibble(response) %>%
#   filter(!sapply(geometry_line.coordinates, is.null)) %>%
#   mutate(
#     geometry = map(geometry_line.coordinates, function(coord_entry) {
#       if (is.atomic(coord_entry)) {
#         coords <- as.numeric(coord_entry)
#         mat <- matrix(coords, ncol = 2, byrow = TRUE)
#         st_multilinestring(list(mat))
#       } else if (is.list(coord_entry)) {
#         flat <- unlist(coord_entry)
#         mat <- matrix(as.numeric(flat), ncol = 2, byrow = TRUE)
#         st_multilinestring(list(mat))
#       } else {
#         st_multilinestring()
#       }
#     })
#   )
# 
# # Then convert to sf
# bike_routes <- sf::st_sf(
#   bike_routes,
#   geometry = sf::st_sfc(bike_routes$geometry, crs = 4326)
# )
# plot(bike_routes$geometry)


