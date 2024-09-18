# Load necessary libraries
library(ggplot2)
library(readr)

# Read the CSV file
df <- read_csv("C:/Users/minus/OneDrive/Desktop/retailsSJ/store.csv")

# Print the DataFrame to verify its contents
print(df)

# Example DataFrame (remove this section if you are using your actual data)
# df <- data.frame(
#   Store = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#   CompetitionOpenSinceMonth = c(1, 3, 5, NA, 7, 9, 10, 11, NA, 12)
# )

# Check if the column 'CompetitionOpenSinceMonth' exists
if (!"CompetitionOpenSinceMonth" %in% names(df)) {
  stop("'CompetitionOpenSinceMonth' column not found in DataFrame")
}

# Handle NA values by dropping rows where 'CompetitionOpenSinceMonth' is NA
df <- na.omit(df)

# Alternatively, you can fill NA values with a specific value (e.g., mean)
# df$CompetitionOpenSinceMonth[is.na(df$CompetitionOpenSinceMonth)] <- mean(df$CompetitionOpenSinceMonth, na.rm = TRUE)

# Create and fit the linear regression model
model <- lm(CompetitionOpenSinceMonth ~ Store, data = df)

# Print the summary of the model
print(summary(model))

# Make predictions
df$Predicted <- predict(model, df)

# Plot the data and the regression line
ggplot(df, aes(x = Store, y = CompetitionOpenSinceMonth)) +
  geom_point(color = 'blue', size = 3, alpha = 0.6) +
  geom_smooth(method = 'lm', color = 'red') +
  labs(x = 'Store', y = 'CompetitionOpenSinceMonth', title = 'Linear Regression: Store vs CompetitionOpenSinceMonth') +
  theme_minimal()

# Print the linear regression equation
cat(sprintf("Linear Regression Equation: CompetitionOpenSinceMonth = %.2f * Store + %.2f\n",
            coef(model)[2], coef(model)[1]))
