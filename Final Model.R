### Final Submission 

library(randomForest)
library(dplyr)

# Load the data
analysisData <- read.csv('analysisData.csv')
scoringData <- read.csv('scoringData.csv')  

head(analysisData)
dim(analysisData)
#skim(analysisData)
table(analysisData$make_name)

# Checking for missing values
missing_indicators <- c("", "NA", "null", "Null", "NULL", "-", "--")

# Replace non-standard missing values with NA
replace_missing <- function(column) {
  # Replace each indicator with NA
  column[column %in% missing_indicators] <- NA
  return(column)
}

# Apply the function to each column in the data frame
analysisData <- data.frame(lapply(analysisData, replace_missing))

# After conversion, check for NA values in each column
missing_data_table = sapply(analysisData, function(x) sum(is.na(x)))

# Calculate the proportion of NA values in each column
na_proportions <- sapply(data, function(x) sum(is.na(x)) / 50000)

variables_exceed_30_percent <- names(na_proportions[na_proportions > 0.3])


# Handling missing data by imputing with the median and modes
for (col in names(analysisData)) {
  if (is.numeric(analysisData[[col]])) {
    analysisData[[col]][is.na(analysisData[[col]])] <- median(analysisData[[col]], na.rm = TRUE)
  } else {
    analysisData[[col]][is.na(analysisData[[col]])] <- as.character(mode(analysisData[[col]][!is.na(analysisData[[col]])]))
  }
}

# Do the same preprocess to the scoring Data
for (col in names(scoringData)) {
  if (is.numeric(scoringData[[col]])) {
    scoringData[[col]][is.na(scoringData[[col]])] <- median(analysisData[[col]], na.rm = TRUE)  # use analysis data to find median
  } else {
    scoringData[[col]][is.na(scoringData[[col]])] <- as.character(mode(analysisData[[col]][!is.na(analysisData[[col]])]))
  }
}

# Train the Random Forest model
set.seed(123)  
model <- randomForest(price ~ make_name + model_name+
                          fuel_tank_volume_gallons +
                          fuel_type + highway_fuel_economy +
                          city_fuel_economy+
                          wheel_system_display +  
                          length_inches+width_inches +height_inches+
                          engine_type +  
                          maximum_seating + 
                          mileage, data = analysisData, ntree = 500, mtry =3)


# Predict on scoring data
predictions <- predict(model, newdata = scoringData)

# Create submission file
submissionFile <- data.frame(id = scoringData$id, price = predictions)
write.csv(submissionFile, 'model.csv', row.names = FALSE)