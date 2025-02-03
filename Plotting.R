
library(ggplot2)
library(dplyr)



# Gender data
gender_data <- c(
  "M", "M", "M", "F", "M", "F", "M", "F", "F", "F", "M", "M", "M", "M", "M", "F", 
  "M", "F", "M", "F", "F", "M", "F", "M", "F", "M", "M", "M", "M", "F", "F", "M", 
  "F", "M", "M", "F"
)


gender_counts <- table(gender_data)
percentages <- round((gender_counts / sum(gender_counts)) * 100, 1)

bar_positions <- barplot(
  gender_counts,
  main = "Gender Distribution",
  xlab = "Gender",
  ylab = "Count",
  col = c("skyblue", "pink"),
  names.arg = c("Male", "Female"),
  ylim = c(0, max(gender_counts) + 3)
)

text(
  bar_positions,
  gender_counts + 1, 
  labels = paste0(percentages, "%"),
  cex = 1.2, # Adjust font size
  col = "black"
)



# Race data
race_data <- c(
  "Multi", "B", "W", "B", "W", "W", "W", "W", "Unspecified", "Unspecified", 
  "W", "Multi", "B", "Multi", "W", "Multi", "B", "B", "W", "B", 
  "Asian", "Unspecified", "W", "B", "W", "B", "B", "W", "B", "B", "B", "B", "B", 
  "Multi", "W", "W"
)




race_counts <- table(race_data)
percentages <- round((race_counts / sum(race_counts)) * 100, 1)




bar_positions <- barplot(
  race_counts,
  main = "Race Distribution",
  xlab = "Race",
  ylab = "Count",
  col = rainbow(length(race_counts)),
  ylim = c(0, max(race_counts) + 5), # Add extra space on top
  names.arg = NA # Suppress default axis labels
)


text(
  bar_positions,
  race_counts + 1, # Adjust to place text above the bars
  labels = paste0(percentages, "%"),
  cex = 1, # Adjust font size
  col = "black"
)

# Add slanted x-axis labels
text(
  x = bar_positions,
  y = -0.5, # Adjust position slightly below the axis
  labels = names(race_counts),
  srt = 45, # Slant the text
  adj = 1, # Align the text to the end
  xpd = TRUE, # Allow text to be drawn outside the plot area
  cex = 0.8 # Adjust font size
)



# Age data
age_data <- c("7Y", "5M", "7M", "5M", "8Y", "1M", "16Y", "1M", "5M", "12D", 
              "4M", "5M", "17Y", "11M", "4M", "1M", "15Y", "4D", "10Y", "13Y", 
              "14Y", "8M", "5Y", "11Y", "7Y", "16M", "16Y", "38w", "6Y", "38w", 
              "20Y", "4W", "11Y", "12D", "4Y", "2Y")


convert_to_years <- function(age) {
  if (grepl("Y$", age)) {
    return(as.numeric(gsub("Y", "", age)))
  } else if (grepl("M$", age)) {
    return(as.numeric(gsub("M", "", age)) / 12)
  } else if (grepl("D$", age)) {
    return(as.numeric(gsub("D", "", age)) / 365)
  } else if (grepl("W$", age)) {
    return(as.numeric(gsub("W", "", age)) / 52)
  } else {
    return(NA)
  }
}


age_in_years <- sapply(age_data, convert_to_years)

# Create a histogram of the age distribution
library(ggplot2)

ggplot(data = data.frame(Age = age_in_years), aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age (Years)", y = "Frequency") +
  theme_minimal(base_size = 14)





