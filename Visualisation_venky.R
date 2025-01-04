# Required libraries for data analysis and visualization
library(tidyverse)   # Data manipulation and visualization
library(ggplot2)     # Plotting and visualizations

# Read the dataset
data <- read_csv("SummerOlympicsjoined.csv", show_col_types = FALSE)

# Modify column names to follow R naming conventions
data <- data %>% rename_with(~ make.names(.))

# Handle missing data by dropping rows with NA values
data <- data %>% drop_na()

# Display first few rows of the cleaned data
head(data)

# Display the structure of the data
str(data)

# Summarize the data to understand its statistics
summary(data)

# List of column names in the dataset
colnames(data)

# Visualize the number of games using a histogram with a bell curve
ggplot(data, aes(x = no_games)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", color = "white") +
  geom_density(color = "red", size = 1.2) +
  stat_function(fun = dnorm, args = list(mean = mean(data$no_games), 
                                         sd = sd(data$no_games)), color = "darkblue", size = 1) +
  labs(title = "Distribution of Number of Games", 
       x = "Number of Games", 
       y = "Density") +
  theme_light()

# Visualize the total summer values using a histogram with a bell curve
ggplot(data, aes(x = total_summer)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "green", color = "black") +
  geom_density(color = "blue", size = 1.2) +
  stat_function(fun = dnorm, args = list(mean = mean(data$total_summer), 
                                         sd = sd(data$total_summer)), color = "darkgreen", size = 1) +
  labs(title = "Distribution of Total Summer", 
       x = "Total Summer", 
       y = "Density") +
  theme_light()

# Scatter plot of total summer vs number of games with a linear model fit
ggplot(data, aes(x = total_summer, y = no_games)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Total Summer vs. Number of Games", 
       x = "Total Summer", 
       y = "Number of Games") +
  theme_light()

# Perform t-test to check if mean of total summer differs significantly from hypothetical value
print(TTestSummer <- t.test(data$total_summer, mu = 10))