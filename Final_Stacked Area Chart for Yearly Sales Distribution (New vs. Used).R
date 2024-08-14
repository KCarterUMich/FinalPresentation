# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load your data from the specified file path
vehicle_sales <- read.csv("/Users/kristenpechin/Desktop/MVA_Vehicle_Sales_Counts_by_Month_for_Calendar_Year_2002_through_December_2023.csv")

# Filter the data to include only the years 2018 to 2022
vehicle_sales_filtered <- vehicle_sales %>%
  filter(Year >= 2018 & Year <= 2022)

# Convert Year and Month to a proper date format for plotting
vehicle_sales_filtered <- vehicle_sales_filtered %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep="-"), format="%Y-%b-%d"))

# Plot the stacked area chart for Monthly Sales Distribution with reversed stacking order
ggplot(vehicle_sales_filtered, aes(x=Date)) +
  geom_area(aes(y=Used, fill="Used Vehicle Sales"), position="stack") +
  geom_area(aes(y=New, fill="New Vehicle Sales"), position="stack") +
  labs(title="Yearly Sales Distribution of New and Used Vehicles (2018-2022)",
       x="Date",
       y="Total Sales (in USD)",
       fill="Legend") +
  scale_fill_manual(values=c("New Vehicle Sales"="blue", "Used Vehicle Sales"="red")) +
  theme_minimal()
