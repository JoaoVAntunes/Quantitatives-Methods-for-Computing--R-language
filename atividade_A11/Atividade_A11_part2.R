#The number of houses per block in my sample
squares <- c(2, 2, 3, 10, 13, 14, 15, 15, 16, 16, 18, 18, 20, 21, 22, 22, 23, 24, 25, 25,
             26, 27, 29, 29, 30, 32, 36, 42, 44, 45, 45, 46, 48, 52, 58, 59, 61, 61, 61, 
             65, 66, 66, 68, 75, 78, 80, 89, 90, 92, 97)

# Load necessary library for plotting
library(ggplot2)

# Create a data frame from the data
sq_df <- data.frame(values = squares)

# Define the bin breaks for the histogram (intervals of 20)
breaks <- seq(0, 100, by = 20)

# Create the histogram with specified breaks
ggplot(sq_df, aes(x = values)) +
  # Draw the histogram with the given bin breaks, fill color, and white borders
  geom_histogram(breaks = breaks, fill = "steelblue", color = "white", closed = "left") +
  # Set the x-axis ticks to match the defined breaks
  scale_x_continuous(breaks = breaks) +
  # Add labels for the title and axes
  labs(title = "                                 Histograma", 
       x = "Número de Casas por Quarteirão", 
       y = "Frequência") +
  # Add the count labels on top of each bar
  geom_text(stat = 'bin', aes(label = ..count..), 
            breaks = breaks, vjust = -0.5, size = 4)


# To calculate the MAD (Mean Absolute Deviation)

# Calculate the median of the data
mediana <- median(squares)

# Calculate the Mean Absolute Deviation (MAD)
# This is the average of the absolute deviations from the median
mad <- mean(abs(squares - mediana))

mad
