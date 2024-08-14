# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # Needed for pivot_longer

# Load your vehicle sales data
vehicle_sales <- read.csv("/Users/kristenpechin/Desktop/MVA_Vehicle_Sales_Counts_by_Month_for_Calendar_Year_2002_through_December_2023.csv")

# Load your vaccination data
vaccination_data <- read.csv("/Users/kristenpechin/Downloads/us_state_vaccinations.csv")

# Filter the vehicle sales data to include only the years 2018 to 2022
vehicle_sales_filtered <- vehicle_sales %>%
  filter(Year >= 2018 & Year <= 2022)

# Summarize the total new and used vehicle sales for each year
yearly_vehicle_sales <- vehicle_sales_filtered %>%
  group_by(Year) %>%
  summarise(
    Total_New_Vehicle_Sales_1000 = sum(New, na.rm = TRUE) / 1e3,  # Convert to thousands
    Total_Used_Vehicle_Sales_1000 = sum(Used, na.rm = TRUE) / 1e3  # Convert to thousands
  )

# Summarize the vaccination data by year
vaccination_data <- vaccination_data %>%
  mutate(Date = as.Date(date)) %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    Total_People_Vaccinated_1000 = sum(people_vaccinated, na.rm = TRUE) / 1e3  # Convert to thousands
  )

# Merge the vehicle sales data with the vaccination data by year
merged_data <- merge(yearly_vehicle_sales, vaccination_data, by = "Year")

# Normalize the data for comparison
merged_data_normalized <- merged_data %>%
  mutate(across(c(Total_New_Vehicle_Sales_1000, Total_Used_Vehicle_Sales_1000, Total_People_Vaccinated_1000), 
                ~ .x / max(.x), .names = "Normalized_{col}"))

# Convert data to long format for easier plotting
merged_data_long <- merged_data_normalized %>%
  pivot_longer(cols = starts_with("Normalized_"),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = recode(Metric, 
                         "Normalized_Total_New_Vehicle_Sales_1000" = "New Vehicle Sales",
                         "Normalized_Total_Used_Vehicle_Sales_1000" = "Used Vehicle Sales",
                         "Normalized_Total_People_Vaccinated_1000" = "People Vaccinated"))

# Plotting a line chart with enhanced differentiation
ggplot(merged_data_long, aes(x = as.factor(Year), y = Value, color = Metric, group = Metric)) +
  geom_line(aes(linetype = Metric), size = 1.5) +  # Adjusted line size and type
  geom_point(aes(shape = Metric), size = 4, stroke = 1.5, color = "white") +  # Adjusted point size and outline
  geom_text(aes(label = round(Value, 2)), vjust = -1, size = 3, show.legend = FALSE) +  # Adjusted text position
  labs(
    title = "Trends in Vehicle Sales and Vaccinations",
    subtitle = "Normalized values show relative changes over time",
    x = "Year",
    y = "Normalized Value",
    color = "Metric",
    linetype = "Metric",
    shape = "Metric",
    caption = "Data Source: Your Data Source Name"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("New Vehicle Sales" = "#1f78b4", "Used Vehicle Sales" = "orange", "People Vaccinated" = "#33a02c")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +  # Different line types
  scale_shape_manual(values = c(16, 17, 15)) +  # Different shapes for points
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Center-aligned and bold title
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)  # Adjusted margins for better spacing
  )

