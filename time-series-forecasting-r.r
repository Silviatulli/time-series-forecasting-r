#Installing relevant packages
install.packages("ggplot2")
install.packages("forecast")

# Generate example CSV data for product_ts
data <- data.frame(
  Date = seq(as.Date("2020-01-01"), by = "month", length.out = 30),
  ProductCode = rep("ABC123", 30),
  OrderQuantity = sample(1:100, 30, replace = TRUE)
)

# Extend the data to have at least 24 data points
extended_data <- rbind(data, data, data[1:6, ])

# Write the data to a CSV file named "inventory_data.csv"
write.csv(extended_data, file = "inventory_data.csv", row.names = FALSE)


# Assuming your data is in a CSV file named "inventory_data.csv"
# Load the data into a data frame
inventory_data <- read.csv("inventory_data.csv", header = TRUE)

# Perform data cleaning and filtering, removing unnecessary columns
# and handling missing values
clean_data <- inventory_data[, c("Date", "ProductCode", "OrderQuantity")]
clean_data <- na.omit(clean_data)  # Remove rows with missing values

# Convert the "Date" column to the appropriate date format
clean_data$Date <- as.Date(clean_data$Date)

# Plot the time series for a specific product code
product_code <- "ABC123"
product_data <- clean_data[clean_data$ProductCode == product_code, ]
plot(product_data$Date, product_data$OrderQuantity, type = "l",
     xlab = "Date", ylab = "Order Quantity", main = paste("Product Code:", product_code))

# Perform seasonal decomposition of the time series
library(ggplot2)
library(forecast)

product_ts <- ts(product_data$OrderQuantity, frequency = 12)  # Assuming monthly data with 12 periods per year
decomposed_ts <- decompose(product_ts)

autoplot(decomposed_ts)

# Assuming the data shows both trend and seasonality
# Fit a Seasonal ARIMA model
model <- auto.arima(product_ts)
summary(model)

# Split the data into training and testing sets
train_size <- floor(0.8 * length(product_ts))
train_data <- window(product_ts, end = train_size)


