# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define theme
theme_book <- function() {
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))
}

# Coverage example code

