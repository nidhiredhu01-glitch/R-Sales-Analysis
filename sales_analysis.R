# Experiment Title: Sales Data Analysis and Visualization in R

# Objective:
# To import, clean, analyze, and visualize sales data using R

library(dplyr)
library(tidyr)
library(ggplot2)

# Import Data
sales_data <- read.csv("sales_data.csv")

# View structure
str(sales_data)
summary(sales_data)

# Data Cleaning
sales_data <- sales_data %>% drop_na()
sales_data$Date <- as.Date(sales_data$Date)

# Data Manipulation
category_sales <- sales_data %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales))

print(category_sales)

# Descriptive Statistics
mean_sales <- mean(sales_data$Sales)
median_sales <- median(sales_data$Sales)
sd_sales <- sd(sales_data$Sales)

print(mean_sales)
print(median_sales)
print(sd_sales)

# Base R Visualization
hist(sales_data$Sales, main="Sales Distribution", col="blue")
plot(sales_data$Sales, sales_data$Profit,
     main="Sales vs Profit",
     xlab="Sales", ylab="Profit")

# ggplot2 Visualization
ggplot(category_sales, aes(x=Category, y=Total_Sales, fill=Category)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  ggtitle("Total Sales by Category")

# Correlation & Regression
correlation <- cor(sales_data$Sales, sales_data$Profit)
print(correlation)

model <- lm(Profit ~ Sales, data=sales_data)
summary(model)

ggplot(sales_data, aes(x=Sales, y=Profit)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Regression Analysis")

# Outlier Detection
boxplot(sales_data$Sales, main="Outlier Detection")

# Scaling
sales_data$Scaled_Sales <- scale(sales_data$Sales)

head(sales_data)
