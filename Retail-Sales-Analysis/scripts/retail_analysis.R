# =========================
# RETAIL SALES ANALYSIS PROJECT 
# =========================

# Load libraries
library(tidyverse)
library(janitor)

# -------------------------
# 1. LOAD DATA
# -------------------------
sales <- read.csv("data/superstore.csv")
sales <- clean_names(sales)

# -------------------------
# 2. EXPLORE DATA
# -------------------------
head(sales)
dim(sales)
names(sales)
str(sales)

# -------------------------
# 3. BASIC KPI ANALYSIS
# -------------------------

total_revenue <- sum(sales$sales)
total_profit <- sum(sales$profit)
average_sale <- mean(sales$sales)
largest_sale <- max(sales$sales)
smallest_sale <- min(sales$sales)

total_revenue
total_profit
average_sale
largest_sale
smallest_sale

# -------------------------
# 4. PROFITABILITY METRICS (ADVANCED)
# -------------------------

# Profit Margin
sales <- sales %>%
  mutate(profit_margin = profit / sales)

overall_profit_margin <- mean(sales$profit_margin, na.rm = TRUE)
overall_profit_margin

# -------------------------
# 5. CUSTOMER ANALYSIS
# -------------------------

top_customers <- sales %>%
  group_by(customer_name) %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  head(10)

top_customers

# -------------------------
# 6. CATEGORY ANALYSIS
# -------------------------

sales_by_category <- sales %>%
  group_by(category) %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit),
    avg_profit_margin = mean(profit_margin, na.rm = TRUE)
  )

sales_by_category

# Category profitability ranking
sales_by_category %>%
  arrange(desc(total_profit))

# -------------------------
# 7. LOSS ANALYSIS (VERY IMPORTANT)
# -------------------------

loss_making_sales <- sales %>%
  filter(profit < 0)

total_loss <- sum(loss_making_sales$profit)
total_loss

loss_by_product <- loss_making_sales %>%
  group_by(product_name) %>%
  summarise(total_loss = sum(profit)) %>%
  arrange(total_loss) %>%
  head(10)

loss_by_product

# -------------------------
# 8. PRODUCT ANALYSIS
# -------------------------

top_products <- sales %>%
  group_by(product_name) %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  head(10)

top_products

top_profit_products <- sales %>%
  group_by(product_name) %>%
  summarise(total_profit = sum(profit)) %>%
  arrange(desc(total_profit)) %>%
  head(10)

top_profit_products

# -------------------------
# 9. REGION ANALYSIS
# -------------------------

region_analysis <- sales %>%
  group_by(region) %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit),
    avg_profit_margin = mean(profit_margin, na.rm = TRUE)
  )

region_analysis

# -------------------------
# 10. TIME SERIES ANALYSIS
# -------------------------

sales$order_date <- as.Date(sales$order_date, format = "%m/%d/%Y")

sales_trend <- sales %>%
  mutate(month = format(order_date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit)
  )

sales_trend

# Month-over-month growth (ADVANCED)
sales_trend <- sales_trend %>%
  arrange(month) %>%
  mutate(
    sales_growth = (total_sales - lag(total_sales)) / lag(total_sales)
  )

sales_trend

# -------------------------
# 11. ADVANCED BUSINESS INSIGHT (TOP 20% RULE)
# -------------------------

product_contribution <- sales %>%
  group_by(product_name) %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  mutate(cumulative_share = cumsum(total_sales) / sum(total_sales))

product_contribution

# -------------------------
# 12. VISUALIZATION
# -------------------------

# Category Sales Chart
ggplot(sales_by_category, aes(category, total_sales)) +
  geom_col()

# Sales Trend Chart
ggplot(sales_trend, aes(month, total_sales, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90))

# -------------------------
# END OF PROJECT
# -------------------------

