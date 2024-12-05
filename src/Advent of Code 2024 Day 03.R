### Advent of Code - 2024
#     Day  3
#
#     By Patrick Rogers, California Research Bureau
#       December 2024

# Load Packages
library(here)

# Inits
setwd(here::here())

# Functions
extract_mul <- function(text){
  # Find mul(x,x) in text
  matches <- gregexpr(pattern =  "mul\\([0-9]+,[0-9]+\\)",
                      text = text)
  
  # Extract text to clean list
  matches_parsed <- vector(mode = "list", length = length(matches))
  for(i in seq_along(matches)){
    match_values <- data.frame(start = as.vector(matches[[i]]),
                               stop = as.vector(matches[[i]]) + attr(matches[[i]], "match.length") - 1)
    
    matches_parsed[[i]] <- apply(match_values, MARGIN = 1, \(x) substr(text[[i]], start = x[1], stop = x[2]))
  }

  return(unlist(matches_parsed))
}

extract_mul_2 <- function(text){
  # Find mul(x,x) in text
  matches_mul <- gregexpr(pattern = "mul\\([0-9]+,[0-9]+\\)",
                          text = text)
  matches_do <- as.numeric(gregexpr(pattern = "do\\(\\)", text = text)[[1]])
  matches_dont <- as.numeric(gregexpr(pattern = "don't\\(\\)", text = text)[[1]])
  
  # Find do() and dont() instructions
  status_change <- rbind(data.frame(index = matches_do, status = TRUE),
                         data.frame(index = matches_dont, status = FALSE))
  status_change <- status_change[order(status_change$index),]
  
  # Extract text to clean list
  match_values <- data.frame(start = as.vector(matches_mul[[1]]),
                             stop = as.vector(matches_mul[[1]]) + attr(matches_mul[[1]], "match.length") - 1,
                             status = TRUE)
  
  # Update status column based on do() and dont() instructions
  for(i in seq_along(status_change$index)){
    # Update status code for any entries with an index larger than status_change$index[i] to match status_change$status[i]
    match_values$status[match_values$start > status_change$index[i]] <- status_change$status[i]
  }
  
  # Drop any with FALSE status
  match_values <- match_values[match_values$status,]
  
  # Extract mul instructions
  matches_parsed <- apply(match_values, MARGIN = 1, \(x) substr(text, start = x[1], stop = x[2]))
  
  return(matches_parsed)
}

# Read in Data
test <- readLines(con = file.path("data", "day_03_test.txt"))
test_2 <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
data <- paste0(readLines(con = file.path("data", "day_03_input.txt")), collapse = "")

# Test ####
mul_data <- extract_mul(test)
mul_data <- data.frame(v1 = as.integer(gsub("mul\\(([0-9]+),([0-9]+)\\)", "\\1", mul_data)),
                        v2 = as.integer(gsub("mul\\(([0-9]+),([0-9]+)\\)", "\\2", mul_data)))
sum(apply(mul_data, MARGIN = 1, \(x) x[1] * x[2]))
# 161

# Part 1 ####
mul_data <- extract_mul(data)
mul_data <- data.frame(v1 = as.integer(gsub("mul\\(([0-9]+),([0-9]+)\\)", "\\1", mul_data)),
                       v2 = as.integer(gsub("mul\\(([0-9]+),([0-9]+)\\)", "\\2", mul_data)))
sum(apply(mul_data, MARGIN = 1, \(x) x[1] * x[2]))
# 192767529

# Part 2 ####
mul_data <- extract_mul_2(data)
mul_data <- data.frame(v1 = as.integer(gsub("mul\\(([0-9]+),([0-9]+)\\)", "\\1", mul_data)),
                       v2 = as.integer(gsub("mul\\(([0-9]+),([0-9]+)\\)", "\\2", mul_data)))
sum(apply(mul_data, MARGIN = 1, \(x) x[1] * x[2]))
# 104083373

# EOF ####
