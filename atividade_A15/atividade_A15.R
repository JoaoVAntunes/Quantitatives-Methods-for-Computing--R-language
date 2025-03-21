# Defining the letter arrays
first_options <- c("q", "w", "x", "z")
second_options <- c("a", "i", "u")
third_options <- c("c", "f", "p")
forth_options <- c("e", "o")

# Generating all possible word combinations
words <- c()
for (i in first_options) {
  for (j in second_options) {
    for (k in third_options) {
      for (l in forth_options) {
        word <- paste0(i, j, k, l)
        words <- c(words, word)
      }
    }
  }
}

# Creating the initial data frame with zero counts for each draw column
words_DF <- data.frame(
  Words = words,
  `72 draws` = 0,
  `216 draws` = 0,
  `720 draws` = 0,
  `2160 draws` = 0,
  `7200 draws` = 0,
  `72000 draws` = 0,
  check.names = FALSE  # Allows column names with spaces
)

# Function to draw random words and update their counts in the specified column
draw_words <- function(df, draws, column) {
  sampled_words <- sample(df$Words, draws, replace = TRUE)  # Random sampling with replacement
  counts <- table(sampled_words)  # Count occurrences of each word
  
  for (word in names(counts)) {
    # Increment the count for each drawn word in the specified column
    df[df$Words == word, column] <- df[df$Words == word, column] + counts[[word]]
  }
  return(df)
}

# Performing the draws and updating the data frame accordingly
words_DF <- draw_words(words_DF, 72, "72 draws")
words_DF <- draw_words(words_DF, 216, "216 draws")
words_DF <- draw_words(words_DF, 720, "720 draws")
words_DF <- draw_words(words_DF, 2160, "2160 draws")
words_DF <- draw_words(words_DF, 7200, "7200 draws")
words_DF <- draw_words(words_DF, 72000, "72000 draws")

# Calculating the expected percentage based on 72000 draws
words_DF$`Expected (%)` <- (words_DF$`72000 draws` / 72000) * 100

# Displaying the first few rows of the updated data frame
print(head(words_DF))
