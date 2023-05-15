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
test_data <- window(product_ts, start = train_size + 1)

# Train the ARIMA model using the training data
trained_model <- Arima(train_data, model = model)

# Forecast future inventory levels
forecast_data <- forecast(trained_model, h = length(test_data))

# Evaluate the model's performance
accuracy(forecast_data, test_data)

# Assuming you want to forecast for the next 12 periods
future_forecast <- forecast(trained_model, h = 12)

# Visualize the forecasted inventory levels
plot(future_forecast, xlab = "Date", ylab = "Order Quantity",
     main = paste("Forecast for Product Code:", product_code))

# Use the forecasted values for inventory optimization and planning

