### Advent of Code - 2024
#     Day  2
#
#     By Patrick Rogers, California Research Bureau
#       December 2024

# Load Packages
library(here)

# Inits
setwd(here::here())

# Functions
safe <- function(row){
  row <- row[!is.na(row)] # Remove NA
  changes <- diff(row)
  
  mono <- length(unique(sign(changes))) == 1 # Check if strictly increasing or decreasing
  differ <- all(abs(changes) <= 3) # Check if change level is 3 or less
  
  return(mono & differ)
}

safe_dampener <- function(row){
  row <- row[!is.na(row)] # Remove N
  status <- rep(FALSE, length(row)) # Initialize values
  
  for(i in seq_along(row)){
    status[i] <- safe(row[-i])
  }
  
  return(any(status))
}

# Data
test <- read.delim(file = file.path("data", "day_02_test.txt"), sep  = " ", header = FALSE)
data <- read.delim(file = file.path("data", "day_02_input.txt"), sep  = " ", header = FALSE)

# Part 1 ####

# Test
apply(test, MARGIN = 1, safe)
apply(test, MARGIN = 1, safe_dampener)

# Run
sum(apply(data, MARGIN = 1, safe))
sum(apply(data, MARGIN = 1, safe_dampener))

# EOF ####