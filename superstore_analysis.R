# Load libraries 
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stringr)
library(skimr)
library(dplyr)

# Import the dataset
superstore_data <- read_csv("sample_superstore.csv", show_col_types = FALSE)
superstore_data

# Check: number of rows and columns. column names. data types
nrow(superstore_data)
ncol(superstore_data)
colnames(superstore_data)

# Check for data types 
skim(superstore_data)
sapply(superstore_data, class)

# Look for: missing values. wrong data types
anyNA(superstore_data)
colSums(is.na(superstore_data))

# Convert: Order Date and Ship Date into proper date format.
superstore_data <- superstore_data %>%
  mutate(
    order_date = as.Date(order_date, format = "%m/%d/%Y"),
    ship_date = as.Date(ship_date, format = "%m/%d/%Y")
  )
superstore_data

# Create a new column: Shipping Duration = difference between Ship Date and Order Date
superstore_data <- superstore_data %>%
  mutate(
    shipping_duration = ship_date - order_date
  )
superstore_data

# Check for: duplicate rows, inconsistent text (if any)
sum(duplicated(superstore_data))

# Check for inconsistent text if any
unique(superstore_data$category)
unique(superstore_data$sub_category)
unique(superstore_data$ship_mode)
unique(superstore_data$segment)
unique(superstore_data$region)
unique(superstore_data$market)
unique(superstore_data$country)
unique(superstore_data$state)

# Create a column: Profit Category. Rules (you decide slightly, but here is a guide): 
# Loss → profit < 0. Low Profit. High Profit. CLUE: use conditional logic
profit_superstore_data <- superstore_data %>%
  mutate(
    profit_category = case_when(
      profit < 0 ~ "Loss",
      profit < 10 ~ "Low Profit",
      profit < 40 ~ "Medium Profit",
      TRUE ~ "High Profit"
    ),
    sales_category = case_when(
      sales < 85 ~ "Low",
      sales < 250 ~ "Medium",
      TRUE ~ "High"
    )
  )
profit_superstore_data

# Which Region generates the highest sales
region_superstore <- profit_superstore_data %>%
  group_by(region) %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales))
region_superstore

# Which Category makes the most profit?
category_superstore <- profit_superstore_data %>%
  group_by(category) %>%
  summarise(total_profit = sum(profit)) %>%
  arrange(desc(total_profit))
category_superstore

# Which Sub-Category has the highest sales?
sub_cate_superstore <- profit_superstore_data %>%
  group_by(sub_category) %>%
  summarise(total_sales_cat = sum(sales)) %>%
  arrange(desc(total_sales_cat))
sub_cate_superstore

# Which Customer Segment spends the most?
segment_superstore <- profit_superstore_data %>%
  group_by(segment) %>%
  summarise(total_sales_seg = sum(sales)) %>%
  arrange(desc(total_sales_seg))
segment_superstore

# Which products are losing money? CLUE: filter profit < 0
product_superstore <- profit_superstore_data %>%
  filter(profit < 0) %>%
  group_by(product_name) %>%
  summarise(total_loss = sum(profit)) %>%
  arrange(total_loss)
product_superstore

# Top 5 customers by total sales. CLUE: ranking function
top_5_customers <- profit_superstore_data %>%
  group_by(customer_name) %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  slice_head(n = 5)
top_5_customers

# Which state generates the most profit?
states_profits <- profit_superstore_data %>%
  group_by(state) %>%
  summarise(total_profit = sum(profit)) %>%
  arrange(desc(total_profit))
states_profits

# Create a table: Rows → Category. Columns → Region. Values → Total Sales
pivot_table <- profit_superstore_data %>%
  group_by(category, region) %>%
  summarise(total_sales = sum(sales), .groups = "drop") %>%
  pivot_wider(
    names_from = region,
    values_from = total_sales
  )
pivot_table

# Convert it back to long format
long_superstrore_pivot <- pivot_table %>%
  pivot_longer(
    cols = -category,
    names_to = "region",
    values_to = "total_sales"
  ) %>%
  arrange(desc(total_sales))
long_superstrore_pivot

# Create a plot of Sales by Region
profit_superstore_data %>%
  group_by(region) %>%
  summarise(total_sales = sum(sales)) %>%
  ggplot(aes(x = reorder(region, total_sales), y = total_sales, fill = region)) +
  geom_col() +
  ggtitle("Sales by Region") +
  coord_flip()

# Create a plot of profit by category
profit_superstore_data %>%
  group_by(category) %>%
  summarise(total_profit = sum(profit)) %>%
  ggplot(aes(x = reorder(category, total_profit), y = total_profit, fill = category)) +
  geom_col() +
  ggtitle("Profit by Category")

# Create a plot of top 10 products by sales
profit_superstore_data %>%#
  group_by(product_name) %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(product_name, total_sales), y = total_sales, fill = product_name)) +
  geom_col() +
  ggtitle("Top 10 Products by Sales") +
  coord_flip()
