# Load necessary libraries
library(ggplot2)
library(readr)

# Read the CSV file
df <- read_csv("C:/Users/minus/OneDrive/Desktop/retailsSJ/Rossmann Stores Data.csv")

# Print the DataFrame to verify its contents
print(head(df))

# Assuming 'Store' and 'Sales' are columns in your DataFrame
X <- df$Store
y <- df$Sales

# Fit a linear regression model
model <- lm(Sales ~ Store, data = df)

# Make predictions
df$Predicted <- predict(model, df)

# Plot the data and the regression line
ggplot(df, aes(x = Store, y = Sales)) +
  geom_point(color = 'blue', size = 3, alpha = 0.6, aes(label = "Data Points")) +
  geom_smooth(method = 'lm', color = 'red', se = FALSE, aes(label = "Linear Regression")) +
  labs(x = 'Store', y = 'Sales', title = 'Store vs Sales Linear Regression') +
  theme_minimal()

# Print the summary of the model
print(summary(model))

# Print the linear regression equation
cat(sprintf("Linear Regression Equation: Sales = %.2f * Store + %.2f\n",
            coef(model)[2], coef(model)[1]))
