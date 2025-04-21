# Load necessary libraries
library(dplyr)
library(modeest)

# Custom function to calculate mode with handling for multiple modes (ties)
moda_custom <- function(x) {
  m <- mfv(x)                         # mfv = most frequent value (mode)
  paste(m, collapse = ", ")          # In case of multiple modes, join them with commas
}

# Function to calculate descriptive statistics for a numeric variable
calc_stats <- function(var) {
  media <- mean(var)                              
  mediana <- median(var)                            
  moda <- moda_custom(var)                           
  dam <- mean(abs(var - media))                     
  variancia <- var(var)                           
  dp <- sd(var)                                   
  percentual_alem_1dp <- mean(abs(var - media) > dp) * 100  # Percentage of values beyond 1 std deviation
  
  # Determine skewness (asymmetry) of the data based on mean, median, and mode
  assimetria <- if (media > mediana && mediana > as.numeric(strsplit(moda, ",")[[1]][1])) {
    "Right-skewed"
  } else if (media < mediana && mediana > as.numeric(strsplit(moda, ",")[[1]][1])) {
    "Left-skewed"
  } else {
    "Symmetric"
  }
  
  # Justification for skewness classification
  justificativa <- paste("Mean =", round(media, 2),
                         ", Median =", round(mediana, 2),
                         ", Mode =", moda)
  
  # Return a data frame with all the calculated statistics
  return(data.frame(
    Variable = deparse(substitute(var)),
    Mean = round(media, 2),
    Median = round(mediana, 2),
    Mode = moda,
    Skewness = assimetria,
    Skewness_Justification = justificativa,
    Mean_Absolute_Deviation = round(dam, 2),
    Variance = round(variancia, 2),
    Standard_Deviation = round(dp, 2),
    "Percent_>1SD" = paste0(round(percentual_alem_1dp, 2), "%")
  ))
}

# Apply the function to each numeric variable in the iris dataset
res1 <- calc_stats(iris$Sepal.Length)
res2 <- calc_stats(iris$Sepal.Width)
res3 <- calc_stats(iris$Petal.Length)
res4 <- calc_stats(iris$Petal.Width)

# For the categorical variable Species, only the mode is meaningful
moda_species <- moda_custom(iris$Species)

# Create a summary row for the categorical variable
res5 <- data.frame(
  Variable = "Species",
  Mean = NA,
  Median = NA,
  Mode = moda_species,
  Skewness = "Not applicable",
  Skewness_Justification = "Categorical",
  Mean_Absolute_Deviation = NA,
  Variance = NA,
  Standard_Deviation = NA,
  "Percent_>1SD" = NA
)

# Combine all results into one final data frame
resultado_final <- bind_rows(res1, res2, res3, res4, res5)
