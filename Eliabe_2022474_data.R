# a) Identify which variables are categorical, discrete and continuous in the chosen data set and show
# using some visualization or plot. Explore whether there are missing values for any of the variables

# Set the path
getwd()
setwd("C:/Users/HP/Documents/R-studio")
# reading data
crimes<-read.csv(file="C:/Users/HP/Desktop/Eliabe_2022474CA1_dataEXP/crimes.csv",stringsAsFactors=TRUE)

head(crimes)

# Exploring categorical, discrete and continuous 
# Creating a bar graph for Year, which is a categorical variable
hist(crimes$YEAR, 
     col = "blue", 
     border = "black", 
     main = "Histogram of Crime Years",
     xlab = "YEAR",
     ylab = "Counts")

# Showing my TIME variable which is my continuous one
table(crimes$TIME)
summary(crimes$TIME)

# Using a Scatterplot to show the discrete variable "RAPE" in years
plot(crimes$YEAR, crimes$RAPE, 
     main = "Scatterplot of RAPE Counts over Time",
     xlab = "YEARS",
     ylab = "RAPE Counts",
     col = "red", 
     pch = 16)
box(which = "plot",
    lty = "solid",
    col="black")



# check if there are missing values in the variables: YEAR, TIME and, RAPE
total_NA_YEAR <- sum(is.na(crimes$YEAR ))
print(paste("Total of NA in YEAR column: ", total_NA_YEAR))

total_NA_TIME<- sum(is.na(crimes$TIME ))
print(paste("Total of NA in TIME column: ", total_NA_TIME))

total_NA_RAPE <- sum(is.na(crimes$RAPE ))
print(paste("Total of NA in RAPE column: ", total_NA_RAPE))


#-----------------------------------//----------------------------------------


# b) Calculate the statistical parameters (mean, median, minimum, maximum, and standard deviation)
# for each of the numerical variables.


# As I have some NA in my numeric columns, I need to covert those to 0
variables_to_replace <- c("MURDER", "YEAR", "CULPABLE_HOMICIDE", 
                          "RAPE", "CUSTODIAL_RAPE","OTHER_RAPE","KIDNAPPING_ACY_ABDUCTION", 
                          "KIDNAPPING_WOMEN_GIRLS","ROBBERY","BURGLARY",
                          "THEFT","AUTO_THEFT","RIOTS", "CHEATING")


# Loop through each variable and replace NA with 0
for (variable in variables_to_replace) {
  # Check if there are any NA values in the column
  if (any(is.na(crimes[[variable]]))) {
    crimes[[variable]][is.na(crimes[[variable]])] <- 0
  } else {
    cat(paste("No NA values found in column '", variable, "'. Skipping.\n"))
  }
}


# Select from my column 5 to 18
# Apply mean, median, minimum, maximum, and standard deviation to selected columns
summary_crimes <- apply(crimes[, 5:18], 2, function(x) 
  c(mean = mean(x), median = median(x), min = min(x), max = max(x), sd = sd(x)))

# Convert the result to a data frame for better readability
summary_crimes_c <- as.data.frame(summary_crimes)

# Print the result
print(summary_crimes_c)




#------------------------------------//---------------------------------------

# c) Apply Min-Max Normalization, Z-score Standardization and Robust scalar on the numerical data
# variables.


# Min-Max Scaling/Normalization
# Check data types of the specified columns
column_types <- sapply(crimes[, 5:18], class)

# Identify columns with non-numeric data
non_numeric_columns <- names(which(column_types != "numeric"))

# Convert non-numeric columns to numeric
crimes[, non_numeric_columns] <- lapply(crimes[, non_numeric_columns], as.numeric)

# Apply Min-Max Normalization
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
crime_minmax <- crimes
crime_minmax[, 5:18] <- apply(crimes[, 5:18], 2, normalizeMinMax)
head (crime_minmax)


# Z-score Standardization
normalizeStandardized <- function(x) {
  return ((x - mean(x)) / sd(x))
}
crimes_standardized <- crimes
crimes_standardized[, 5:18] <- apply(crimes[, 5:18], 2, normalizeStandardized)
print(crimes_standardized)

# Robust scalar

crimes_robust <- crimes
crimes_robust[, 5:18] <- scale(crimes[, 5:18], center = TRUE, scale = TRUE)
print(crimes_robust)



#---------------------------------------------//--------------------------------



# f) Apply dummy encoding to categorical variables (at least one variable used from the data set) and
# discuss the benefits of dummy encoding to understand the categorical data.
install.packages("fastDummies")

# Load the package
library(fastDummies)

# Create dummy variables
crimes <- dummy_cols(crimes$STATE.UT)

# Display the result
head(crimes, 10)


# --------------------------------------------//--------------------------------


# g) Apply PCA with your chosen number of components. Write up a short profile of the first few
# components extracted based on your understanding.



# Identify numeric columns
numeric_columns <- sapply(crimes, is.numeric)

# Handle missing values for numeric columns
crimes[, numeric_columns] <- lapply(crimes[, numeric_columns], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Replace infinite values with a large number (adjust as needed)
crimes[is.infinite(as.matrix(crimes))] <- 1e6

# Exclude unnecessary columns
columns_to_exclude <- c('index', 'STATE/UT', 'DISTRICT', 'YEAR')
crime_data_subset <- crimes[, !(names(crimes) %in% columns_to_exclude)]

# Identify numeric columns in the subset
numeric_columns_subset <- sapply(crime_data_subset, is.numeric)

# Scale only the numeric columns
scaled_data <- scale(crime_data_subset[, numeric_columns_subset])

# Perform PCA using prcomp
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Extract the scores and loading
pc_scores <- pca_result$x[, 1:3]
pc_loadings <- pca_result$rotation[, 11:13]

# Print the loadings
print(pc_scores)
print(pc_loadings)
summary(pca_result)



#------------------------------------------//-----------------------------------


# h) What is the purpose of dimensionality reduction? Explore the situations where you can gain the
# benefit of dimensionality reduction for data analysis.

install.packages("ggplot2")
library(ggplot2)

cumulative_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
ggplot() +
  geom_line(aes(x = 1:length(cumulative_variance), y = cumulative_variance), color = "blue") +
  labs(title = "PCA reduction graph",
       x = "Principal Components",
       y = "Proportion of Variance") 






