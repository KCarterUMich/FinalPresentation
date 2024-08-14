# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggrepel)

# Load your data from the specified file path
vehicle_sales <- read.csv("/Users/kristenpechin/Desktop/MVA_Vehicle_Sales_Counts_by_Month_for_Calendar_Year_2002_through_December_2023.csv")

# Convert sales columns to numeric (remove any non-numeric characters like $ or ,)
vehicle_sales <- vehicle_sales %>%
  mutate(
    Total.Sales.New = as.numeric(gsub("[^0-9.]", "", Total.Sales.New)),
    Total.Sales.Used = as.numeric(gsub("[^0-9.]", "", Total.Sales.Used))
  )

# Filter the data to include only the years 2018 to 2022
vehicle_sales_filtered <- vehicle_sales %>%
  filter(Year >= 2018 & Year <= 2022)

# Calculate the total vehicles sold and total sales value for each month, scale both to hundreds of thousands
vehicle_sales_summary <- vehicle_sales_filtered %>%
  group_by(Year, Month) %>%
  summarise(
    Total_Vehicles_Sold_Hundreds_Thousands = sum(New + Used, na.rm = TRUE) / 1e5,  # Convert to hundreds of thousands
    Total_Sales_Value_Hundreds_Thousands = sum(Total.Sales.New + Total.Sales.Used, na.rm = TRUE) / 1e5,  # Convert to hundreds of thousands
    .groups = "drop"
  ) %>%
  arrange(Total_Vehicles_Sold_Hundreds_Thousands)

# Identify the 3 lowest and 3 highest months by sales count
lowest_sales_months <- vehicle_sales_summary %>%
  top_n(-3, Total_Vehicles_Sold_Hundreds_Thousands)

highest_sales_months <- vehicle_sales_summary %>%
  top_n(3, Total_Vehicles_Sold_Hundreds_Thousands)

# Create a scatter plot with highlighted points and better label positioning
ggplot(vehicle_sales_summary, aes(x=Total_Vehicles_Sold_Hundreds_Thousands, y=Total_Sales_Value_Hundreds_Thousands)) +
  geom_point(color="gray", size=3) +  # Default points
  geom_point(data=lowest_sales_months, aes(x=Total_Vehicles_Sold_Hundreds_Thousands, y=Total_Sales_Value_Hundreds_Thousands), color="red", size=4) +
  geom_point(data=highest_sales_months, aes(x=Total_Vehicles_Sold_Hundreds_Thousands, y=Total_Sales_Value_Hundreds_Thousands), color="darkgreen", size=4) +
  geom_text_repel(data=lowest_sales_months, aes(x=Total_Vehicles_Sold_Hundreds_Thousands, y=Total_Sales_Value_Hundreds_Thousands, label=paste(Month, Year)), 
                  color="red", size=4, max.overlaps=10) +
  geom_text_repel(data=highest_sales_months, aes(x=Total_Vehicles_Sold_Hundreds_Thousands, y=Total_Sales_Value_Hundreds_Thousands, label=paste(Month, Year)), 
                  color="darkgreen", size=4, max.overlaps=10) +
  labs(title="Correlation Between Lowest and Highest Vehicle Sales with Total Revenue (2018-2022)",
       x="Total Vehicle Sales Count (in Hundreds of Thousands)",
       y="Total Revenue (in Hundreds of Thousands USD)") +
  theme_minimal()

    
    
    