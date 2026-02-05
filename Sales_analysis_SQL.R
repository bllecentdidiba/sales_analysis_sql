library(readr)
library(ggplot2)
library(tidyverse)
library(caret)
library(sqldf)

#SQL in R
SalesTransactions <- read_csv("Sales_Transactions.csv")
head(SalesTransactions)
glimpse(SalesTransactions)


#Checking structure
cat("Rows:", nrow(SalesTransactions), "\n")
cat("Columns:", ncol(SalesTransactions), "\n")
cat("Column names:", names(SalesTransactions), "\n")

#Checking missing values
cat("\nData Quality Check:\n")
cat("Rows with NA in Price:", sum(is.na(SalesTransactions$Price)), "\n")
cat("Rows with NA in Quantity:", sum(is.na(SalesTransactions$Quantity)), "\n")
cat("Rows with NA in Date:", sum(is.na(SalesTransactions$Date)), "\n")
#No NA values so no need to remove rows with NA values

cat("\n=== BASIC DESCRIPTIVE STATISTICS ===\n")

#Revenue Overview
cat("\n1. REVENUE OVERVIEW:\n")
q1 <- sqldf("
  SELECT 
    COUNT(DISTINCT TransactionNo) as transaction_count,
    COUNT(DISTINCT CustomerNo) as customer_count,
    COUNT(DISTINCT ProductNo) as product_count,
    SUM(Quantity) as total_units,
    ROUND(SUM(Price*Quantity), 2) as total_revenue,
    ROUND(AVG(Price), 2) as avg_price,
    ROUND(AVG(Quantity), 2) as avg_quantity
  FROM SalesTransactions
")
print(q1)

#Time based Analysis
cat("\n2. TEMPORAL ANALYSIS:\n")
q2 <- sqldf("
  SELECT 
    SUBSTR(Date, LENGTH(Date)-3, 4) as year,
    CASE 
      WHEN INSTR(Date, '/') = 2 THEN '0' || SUBSTR(Date, 1, 1)
      ELSE SUBSTR(Date, 1, 2)
    END as month,
    COUNT(DISTINCT TransactionNo) as transactions,
    ROUND(SUM(Price * Quantity), 2) as monthly_revenue,
    ROUND(AVG(Price * Quantity), 2) as avg_transaction_value
  FROM SalesTransactions
  GROUP BY SUBSTR(Date, LENGTH(Date)-3, 4),
           CASE 
             WHEN INSTR(Date, '/') = 2 THEN '0' || SUBSTR(Date, 1, 1)
             ELSE SUBSTR(Date, 1, 2)
           END
  ORDER BY year, month
")
print(q2)


#Geographic Distribution
cat("\n3. GEOGRAPHICAL DISTRIBUTION:\n")
q3 <- sqldf("
  SELECT 
    Country,
    COUNT(DISTINCT CustomerNo) as customers,
    COUNT(DISTINCT TransactionNo) as transactions,
    ROUND(SUM(Price * Quantity), 2) as revenue,
    SUM(Quantity) as units_sold
  FROM SalesTransactions
  GROUP BY Country
  ORDER BY revenue DESC
  LIMIT 10
")
print(q3)


cat("\n=== CUSTOMER ANALYSIS ===\n")

#Customer Segmentation by Spending
cat("\n4.1 CUSTOMER SPENDING SEGMENTS:\n")
q4 <- sqldf("
  WITH CustomerSpending AS (
    SELECT 
      CustomerNo,
      SUM(Price * Quantity) as total_spent,
      COUNT(DISTINCT TransactionNo) as transaction_count
    FROM SalesTransactions
    GROUP BY CustomerNo
  )
  
  SELECT 
    CASE 
      WHEN total_spent > 10000 THEN 'VIP (>10k)'
      WHEN total_spent > 5000 THEN 'Premium (5k-10k)'
      WHEN total_spent > 1000 THEN 'Regular(1k-5k)'
      WHEN total_spent > 100 THEN 'Occasional(100-1k)'
      
      ELSE 'Infrequent (<100)'
    END as customer_segment,
    COUNT(*) as customer_count,
    ROUND(SUM(total_spent), 2) as segment_revenue,
    ROUND(AVG(total_spent), 2) as avg_spend_per_customer,
    ROUND(100 * SUM(total_spent) / (SELECT SUM(Price * Quantity) FROM SalesTransactions), 2) as revenue_percentage
 
  FROM CustomerSpending
  GROUP BY customer_segment
  ORDER BY segment_revenue DESC
")
print(q4)

#Top 20 Customers
cat("\n4.2 TOP 20 CUSTOMERS:\n")
q5 <- sqldf("
  SELECT 
    CustomerNo,
    COUNT(DISTINCT TransactionNo) as transaction_count,
    SUM(Quantity) as total_items,
    ROUND(SUM(Price * Quantity), 2) as total_spent,
    ROUND(AVG(Price * Quantity), 2) as avg_transaction_value,
    strftime('%d/%m/%Y', MIN(Date)) as first_purchase,
    strftime('%d/%m/%Y', MAX(Date)) as last_purchase
  FROM SalesTransactions
  GROUP BY CustomerNo
  ORDER BY total_spent DESC
  LIMIT 20
")
print(q5)

cat("\n=== PRODUCT ANALYSIS ===\n")

#Top 20 Products by Revenue
cat("\n5.1 TOP 20 PRODUCTS BY REVENUE:\n")
q6 <- sqldf("
  SELECT 
    ProductNo,
    MAX(ProductName) as product_name,
    COUNT(DISTINCT TransactionNo) as transaction_count,
    SUM(Quantity) as total_units,
    ROUND(SUM(Price * Quantity), 2) as total_revenue,
    ROUND(AVG(Price), 2) as avg_price,
    ROUND(AVG(Quantity), 2) as avg_quantity_per_transaction
  FROM SalesTransactions
  GROUP BY ProductNo
  ORDER BY total_revenue DESC
  LIMIT 20
")
print(q6)

#Product Price Distribution
cat("\n5.2 PRODUCT PRICE DISTRIBUTION:\n")
q7 <- sqldf("
  SELECT 
    CASE 
      WHEN Price < 1 THEN 'Under R1'
      WHEN Price < 5 THEN 'R1-R5'
      WHEN Price < 10 THEN 'R5-R10'
      WHEN Price < 20 THEN 'R10-R20'
      WHEN Price < 50 THEN 'R20-R50'
      WHEN Price < 100 THEN 'R50-R100'
      ELSE 'Over R100'
    END as price_range,
    COUNT(DISTINCT ProductNo) as product_count,
    COUNT(*) as transaction_count,
    SUM(Quantity) as total_units,
    ROUND(SUM(Price * Quantity), 2) as total_revenue
  FROM SalesTransactions
  GROUP BY price_range
  ORDER BY total_revenue DESC
")
print(q7)

#Daily Transaction Patterns
cat("\n6.2 DAILY TRANSACTION VOLUME:\n")
q8 <- sqldf("
  SELECT 
    Date,
    COUNT(DISTINCT TransactionNo) as daily_transactions,
    COUNT(*) as daily_items,
    ROUND(SUM(Price * Quantity), 2) as daily_revenue
  FROM SalesTransactions
  GROUP BY Date
  ORDER BY daily_revenue DESC
  LIMIT 10
")
print(q8)

cat("\n=== TIME SERIES ANALYSIS ===\n")

#Monthly Performance
cat("\n7.1 MONTHLY PERFORMANCE SUMMARY:\n")
q9 <- sqldf("
  SELECT 
    SUBSTR(Date, LENGTH(Date)-3, 4) || '-' || 
    CASE 
      WHEN INSTR(Date, '/') = 2 THEN '0' || SUBSTR(Date, 1, 1)
      ELSE SUBSTR(Date, 1, 2)
    END as year_month,
    COUNT(DISTINCT TransactionNo) as transactions,
    COUNT(DISTINCT CustomerNo) as unique_customers,
    COUNT(DISTINCT ProductNo) as unique_products,
    SUM(Quantity) as total_units,
    ROUND(SUM(Price * Quantity), 2) as monthly_revenue,
    ROUND(AVG(Price * Quantity), 2) as avg_transaction_value
  FROM SalesTransactions
  GROUP BY SUBSTR(Date, LENGTH(Date)-3, 4), 
           CASE 
             WHEN INSTR(Date, '/') = 2 THEN '0' || SUBSTR(Date, 1, 1)
             ELSE SUBSTR(Date, 1, 2)
           END
  ORDER BY year_month
")
print(q9)

#Year on Year Comparison
cat("\n7.2 YEAR OVER YEAR COMPARISON:\n")
q10 <- sqldf("
  SELECT 
    SUBSTR(Date, LENGTH(Date)-3, 4) as year,
    COUNT(DISTINCT TransactionNo) as annual_transactions,
    COUNT(DISTINCT CustomerNo) as annual_customers,
    ROUND(SUM(Price * Quantity), 2) as annual_revenue,
    ROUND(AVG(Price), 2) as avg_price,
    ROUND(AVG(Quantity), 2) as avg_quantity
  FROM SalesTransactions
  GROUP BY SUBSTR(Date, LENGTH(Date)-3, 4)
  ORDER BY year
")
print(q10)

#Data Visualisation in R
#Bar plot for CUSTOMER SEGMENTATION
p1 <- ggplot(q4, aes(x = customer_segment,
               y = revenue_percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Customer Revenue Distribution",
       subtitle = "Top 1.3% customers generate 80.9% of revenue",
       x = "Customer Segment",
       y = "Percentage of Total Revenue") +
  theme_minimal()
print(p1)

# GEOGRAPHIC HEAT MAP
p2 <- ggplot(head(q3, 8), aes(x = reorder(Country, revenue),
                        y = revenue)) +
  geom_col(fill = "green") +
  coord_flip() +
  labs(title = "Revenue by Country",
       subtitle = "UK dominates with 88.3% market share",
       x = "Country",
       y = "Revenue (R)") +
  scale_y_continuous(labels = scales::comma)
print(p2)

#MONTHLY REVENUE TREND
q9$year_month <- as.Date(paste0(q9$year_month, "-01"))
p3 <- ggplot(q9, aes(x = year_month, y = monthly_revenue)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(title = "Monthly Revenue Trend",
       x = "Month", y = "Revenue (R)") +
  scale_y_continuous(labels = scales::comma)
print(p3)

#PRICE DISTRIBUTION

p4 <- ggplot(q7, aes(x = price_range, y = total_revenue)) +
  geom_bar(stat = "identity", fill = "yellow") +
  labs(title = "Revenue by Price Range",
       subtitle = "71% of transactions in R10-R20 range",
       x = "Price Range", y = "Total Revenue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p4)

# Saving the visualizations as PNG files
ggsave("customer_segmentation.png", p1, width = 10, height = 6, dpi = 300)
ggsave("country_revenue.png", p2, width = 10, height = 6, dpi = 300)

ggsave("monthly_trend.png", p3, width = 12, height = 6, dpi = 300)
ggsave("price_distribution.png", p4, width = 10, height = 6, dpi = 300)

# Save ALL output to file
sink("sales_analysis_SQL_output.txt", append = FALSE, split = TRUE)

cat("=== DESCRIPTIVE STATISTICS ===\n\n")
cat("1. REVENUE OVERVIEW:\n")
print(q1)

cat("\n2. TEMPORAL ANALYSIS:\n")
print(q2)

cat("\n3. GEOGRAPHICAL DISTRIBUTION:\n")
print(q3)

cat("\n=== CUSTOMER ANALYSIS ===\n\n")
cat("4.1 CUSTOMER SPENDING SEGMENTS:\n")
print(q4)

cat("\n4.2 TOP 20 CUSTOMERS:\n")
print(q5)

cat("\n=== PRODUCT ANALYSIS ===\n\n")
cat("5.1 TOP 20 PRODUCTS BY REVENUE:\n")
print(q6)

cat("\n5.2 PRODUCT PRICE DISTRIBUTION:\n")
print(q7)

cat("\n6.2 DAILY TRANSACTION VOLUME:\n")
print(q8)

cat("\n=== TIME SERIES ANALYSIS ===\n\n")
cat("7.1 MONTHLY PERFORMANCE SUMMARY:\n")
print(q9)

cat("\n7.2 YEAR OVER YEAR COMPARISON:\n")
print(q10)

sink()


