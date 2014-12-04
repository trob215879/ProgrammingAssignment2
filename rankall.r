rankall <- function(outcome, num = "best") {

# Initilizes variables to search for the outcomes we need and find the columns in the chart we are given.
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')  # defines the outcomes we are looking for
  indices <- c(11, 17, 23)  # these are the column numbers for 'heart attack', 'heart failure' & 'pneumonia'.
 
# Tests to see if user inputed a valid outcome to searh for, stops the program if it's invalid.
  if (!outcome %in% outcomes) stop("invalid outcome")   

# Loads all the data in outcome-of-care-measures.csv into one variable named)
  data <- read.csv("outcome-of-care-measures.csv")
 
# To make the chart easier to manage by r, we will extract only the columns needed 
  i <- indices[match(outcome, outcomes)]   # assign column number of the outcome we are looking for to i. 
  hospitals <- data[, c(2, 7, i)]  # subset the columns in data to hospitals

# Format values in hospital variable. 
  hospitals[, 3] <- as.numeric(as.character(hospitals[, 3]))  # change string values into integer values 
  hospitals <- na.omit(hospitals)  # take out all the rows with missing data
  names(hospitals) <- c("hospital", "state", "rate") # Add column headers to hospital variable

# Tests to see if the ranking we are looking for is valid. "best" and "worst" are valid rankings.
# We can also specify a numeric ranking.  The ranking is valid so long as it's not above the number of hospitals.
  if (num == "best") {
    num <- 1               
  } else if (num == "worst") {
  } else {
    num <- as.numeric(num)
    if (is.na(num)) {
      stop("invalid num")
    } else if (num > nrow(hospitals)) {
      return(NA)
    }
  }
