# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)  # For better number formatting

# Load your vehicle sales data
vehicle_sales <- read.csv("/Users/kristenpechin/Desktop/MVA_Vehicle_Sales_Counts_by_Month_for_Calendar_Year_2002_through_December_2023.csv")

# Load your vaccination data
vaccination_data <- read.csv("/Users/kristenpechin/Downloads/us_state_vaccinations.csv")

# Filter vehicle sales data to include only the years 2021 and 2022
vehicle_sales_filtered <- vehicle_sales %>%
  filter(Year >= 2021 & Year <= 2022)

# Standardize the Month format in vehicle sales data to abbreviated month names
vehicle_sales_filtered <- vehicle_sales_filtered %>%
  mutate(Month = toupper(Month))  # Ensure the Month column is in uppercase abbreviated format

# Summarize the vehicle sales data by month across all years
monthly_vehicle_sales <- vehicle_sales_filtered %>%
  group_by(Year, Month) %>%
  summarise(
    Total_New_Vehicle_Sales = sum(New, na.rm = TRUE),
    Total_Used_Vehicle_Sales = sum(Used, na.rm = TRUE),
    .groups = "drop"  # Ungroup after summarization
  )

# Correctly extract Year and Month from the vaccination data and filter for 2021 to 2022
vaccination_data_filtered <- vaccination_data %>%
  mutate(Date = as.Date(date)) %>%
  filter(Date >= as.Date("2021-01-01") & Date <= as.Date("2022-12-31")) %>%
  mutate(Year = year(Date),
         Month = toupper(month(Date, label = TRUE, abbr = TRUE))) %>%  # Ensure the Month column is in uppercase abbreviated format
  group_by(Year, Month) %>%
  summarise(
    Total_People_Vaccinated = sum(people_vaccinated, na.rm = TRUE),
    .groups = "drop"  # Ungroup after summarization
  )

# Merge the vehicle sales data with the vaccination data by year and month
merged_monthly_data <- merge(monthly_vehicle_sales, vaccination_data_filtered, by = c("Year", "Month"))

# Convert data to long format for easier plotting
merged_monthly_data_long <- merged_monthly_data %>%
  pivot_longer(cols = starts_with("Total_"),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = recode(Metric, 
                         "Total_New_Vehicle_Sales" = "New Sales",
                         "Total_Used_Vehicle_Sales" = "Used Sales",
                         "Total_People_Vaccinated" = "Vaccinated"))

# Plotting the seasonal trends by month with improved readability and separate y-scales
ggplot(merged_monthly_data_long, aes(x = Month, y = Value, color = Metric, group = interaction(Year, Metric))) +
  geom_line(size = 1) +  # Adjusted line size
  geom_point(size = 3) +  # Adjusted point size for better visibility
  facet_grid(Metric ~ Year, scales = "free_y") +  # Separate y-scales for each metric
  labs(
    title = "Seasonal Trends in Sales and Vaccinations",
    subtitle = "Comparing trends by month from 2021 to 2022",
    x = "Month",
    y = "Total Count",
    color = "Metric",
    caption = "Data Source: Your Data Source Name"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("New Sales" = "#1f78b4", "Used Sales" = "orange", "Vaccinated" = "#33a02c")) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Center-aligned and bold title
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)  # Adjusted margins for better spacing
  ) +
  scale_y_continuous(labels = scales::comma)  # Use commas to format large numbers on y-axis
