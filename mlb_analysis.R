# Load necessary libraries

library(rvest)
library(tidyverse)

# Function to scrape salary data
scrape_salaries <- function(year) {
  url <- paste0("https://www.spotrac.com/mlb/rankings/player/_/year/", year, "/sort/cap_total")
  webpage <- read_html(url)
  
  Name <- html_nodes(webpage, ".col-md-8 .link") %>%
    html_text() %>%
    sapply(function(x) gsub("\n", "", x)) %>%
    trimws()
  
  Salary <- html_nodes(webpage, ".col-md-8 .medium") %>%
    html_text() %>%
    sapply(function(x) gsub("\n","",x)) %>%
    trimws()
  
  data.frame(Name, Salary)
}

# Function to scrape stat data (HR, RBI, Hits, etc.)
scrape_stats <- function(stat_type, year) {
  url <- paste0("https://www.espn.com/mlb/stats/player/_/view/batting/season/", year, "/seasontype/2/table/batting/sort/", stat_type, "/dir/desc")
  webpage <- read_html(url)
  
  Stat <- html_nodes(webpage, ".Table__TD:nth-child(9)") %>%
    html_text()
  
  Name <- html_nodes(webpage, ".mr7 .AnchorLink") %>%
    html_text()
  
  data.frame(Name, Stat)
}

# Function to process and clean salary data
clean_salary_data <- function(data) {
  data$Salary <- as.numeric(gsub("\\$", "", gsub(",", "", gsub(" ", "", data$Salary))))
  data
}

# Function to process and clean stat data
clean_stat_data <- function(data, stat_name) {
  data[[stat_name]] <- as.numeric(gsub("\\$", "", gsub(",", "", gsub(" ", "", data[[stat_name]]))))
  data
}

# Function to merge data by Name
merge_data <- function(salary_data, stat_data, stat_name) {
  colnames(stat_data) <- c("Name", stat_name)
  merged_data <- merge(salary_data, stat_data, by = "Name", all = FALSE)
  clean_stat_data(merged_data, stat_name)
}

# Main function to execute the analysis for a specific year
perform_analysis <- function(year) {
  # Scrape data
  salary_data <- scrape_salaries(year)
  hr_data <- scrape_stats("homeRuns", year)
  rbi_data <- scrape_stats("RBIs", year)
  hits_data <- scrape_stats("hits", year)
  sb_data <- scrape_stats("stolenBases", year)
  bb_data <- scrape_stats("walks", year)
  runs_data <- scrape_stats("runs", year)
  
  # Clean salary data
  salary_data <- clean_salary_data(salary_data)
  
  # Merge data
  merged_hr <- merge_data(salary_data, hr_data, "HR")
  merged_rbi <- merge_data(salary_data, rbi_data, "RBI")
  merged_hits <- merge_data(salary_data, hits_data, "Hits")
  merged_sb <- merge_data(salary_data, sb_data, "SB")
  merged_bb <- merge_data(salary_data, bb_data, "BB")
  merged_runs <- merge_data(salary_data, runs_data, "Runs")
  
  # Return the merged data
  list(
    HR = merged_hr,
    RBI = merged_rbi,
    Hits = merged_hits,
    SB = merged_sb,
    BB = merged_bb,
    Runs = merged_runs
  )
}

# Run the analysis for the years 2012-2024
MLBresults_2024 <- perform_analysis(2024)
 MLBresults_2023 <- perform_analysis(2023)
MLBresults_2022 <- perform_analysis(2022)
MLBresults_2021 <- perform_analysis(2021)
MLBresults_2020 <- perform_analysis(2020)
MLBresults_2019 <- perform_analysis(2019)
MLBresults_2018 <- perform_analysis(2018)
MLBresults_2017 <- perform_analysis(2017)
MLBresults_2016 <- perform_analysis(2016)
MLBresults_2015 <- perform_analysis(2015)
MLBresults_2014 <- perform_analysis(2014)
MLBresults_2013 <- perform_analysis(2013)
MLBresults_2012 <- perform_analysis(2012)
                                 

# Function to calculate the mean of specific columns for a given year
calculate_means <- function(year) {
  # Create the dataset name dynamically
  dataset_name <- paste0("MLBresults_", year)
  
  # Access the dataset using get() function
  dataset <- get(dataset_name)
  
  # Define a list to store the results
  means <- list()
  
  # Calculate means for each data frame and column
  means$HR_Salary <- mean(dataset$HR$Salary, na.rm = TRUE)
  means$HR_HR <- mean(dataset$HR$HR, na.rm = TRUE)
  means$RBI_RBI <- mean(dataset$RBI$RBI, na.rm = TRUE)
  means$RBI_Salary <- mean(dataset$RBI$Salary, na.rm = TRUE)
  means$Runs_Runs <- mean(dataset$Runs$Runs, na.rm = TRUE)
  means$Runs_Salary <- mean(dataset$Runs$Salary, na.rm = TRUE)
  means$BB_BB <- mean(dataset$BB$BB, na.rm = TRUE)
  means$BB_Salary <- mean(dataset$BB$Salary, na.rm = TRUE)
  means$SB_SB <- mean(dataset$SB$SB, na.rm = TRUE)
  means$SB_Salary <- mean(dataset$SB$Salary, na.rm = TRUE)
  means$Hits_Hits <- mean(dataset$Hits$Hits, na.rm = TRUE)
  means$Hits_Salary <- mean(dataset$Hits$Salary, na.rm = TRUE)
  
  return(means)
}

# Define the function
calculate_means <- function(year) {
  # Create the dataset name dynamically
  dataset_name <- paste0("MLBresults_", year)
  
  # Access the dataset using get() function
  dataset <- get(dataset_name)
  
  # Calculate means for each data frame and column
  HR_Salary <- mean(as.numeric(dataset$HR$Salary), na.rm = TRUE)
  HR_HR <- mean(as.numeric(dataset$HR$HR), na.rm = TRUE)
  RBI_RBI <- mean(as.numeric(dataset$RBI$RBI), na.rm = TRUE)
  RBI_Salary <- mean(as.numeric(dataset$RBI$Salary), na.rm = TRUE)
  Runs_Runs <- mean(as.numeric(dataset$Runs$Runs), na.rm = TRUE)
  Runs_Salary <- mean(as.numeric(dataset$Runs$Salary), na.rm = TRUE)
  BB_BB <- mean(as.numeric(dataset$BB$BB), na.rm = TRUE)
  BB_Salary <- mean(as.numeric(dataset$BB$Salary), na.rm = TRUE)
  SB_SB <- mean(as.numeric(dataset$SB$SB), na.rm = TRUE)
  SB_Salary <- mean(as.numeric(dataset$SB$Salary), na.rm = TRUE)
  Hits_Hits <- mean(as.numeric(dataset$Hits$Hits), na.rm = TRUE)
  Hits_Salary <- mean(as.numeric(dataset$Hits$Salary), na.rm = TRUE)
  
  # Return the results as a named numeric vector
  means <- c(
    HR_Salary = HR_Salary,
    HR_HR = HR_HR,
    RBI_RBI = RBI_RBI,
    RBI_Salary = RBI_Salary,
    Runs_Runs = Runs_Runs,
    Runs_Salary = Runs_Salary,
    BB_BB = BB_BB,
    BB_Salary = BB_Salary,
    SB_SB = SB_SB,
    SB_Salary = SB_Salary,
    Hits_Hits = Hits_Hits,
    Hits_Salary = Hits_Salary
  )
  
  return(means)
}

# Define the range of years
years <- 2012:2024

# Create an empty list to store each year's results
resultsByYear <- list()

# Loop through each year and apply the function
for (year in years) {
  # Calculate the means for the current year
  results <- calculate_means(year)
  
  # Store the results in a list named by the year
  resultsByYear[[as.character(year)]] <- results
}

# Print the results to check
print(resultsByYear)

# Define the function to calculate ratios
calculate_ratios <- function(year) {
  # Extract means for the current year
  means <- resultsByYear[[as.character(year)]]
  
  # Calculate the ratios
  valuePerHR <- means["HR_Salary"] / means["HR_HR"]
  valuePerRBI <- means["RBI_Salary"] / means["RBI_RBI"]
  valuePerRuns <- means["Runs_Salary"] / means["Runs_Runs"]
  valuePerBB <- means["BB_Salary"] / means["BB_BB"]
  valuePerSB <- means["SB_Salary"] / means["SB_SB"]
  valuePerHits <- means["Hits_Salary"] / means["Hits_Hits"]
  
  # Return the calculated ratios as a list
  return(list(
    valuePerHR = valuePerHR,
    valuePerRBI = valuePerRBI,
    valuePerRuns = valuePerRuns,
    valuePerBB = valuePerBB,
    valuePerSB = valuePerSB,
    valuePerHits = valuePerHits
  ))
}

# Define the range of years
years <- 2012:2024

# Create an empty list to store each year's ratios
ratiosByYear <- list()

# Loop through each year and apply the function
for (year in years) {
  # Calculate the ratios for the current year
  ratios <- calculate_ratios(year)
  
  # Store the ratios in a list named by the year
  ratiosByYear[[as.character(year)]] <- ratios
}

# Function to calculate the player's value based on the year and stats
calculate_player_value <- function(HR, Runs, RBI, BB, Hits, SB, Year) {
  # Ensure the year is within the valid range
  if (Year < 2012 || Year > 2024) {
    stop("Year must be between 2012 and 2024")
  }
  
  # Calculate the player's value based on the year
  valueOfPlayer <- (
    (as.numeric(ratiosByYear[[as.character(Year)]][["valuePerHR"]]) * HR / 6) +
      (as.numeric(ratiosByYear[[as.character(Year)]][["valuePerRBI"]]) * RBI / 6) +
      (as.numeric(ratiosByYear[[as.character(Year)]][["valuePerRuns"]]) * Runs / 6) +
      (as.numeric(ratiosByYear[[as.character(Year)]][["valuePerBB"]]) * BB / 6) +
      (as.numeric(ratiosByYear[[as.character(Year)]][["valuePerSB"]]) * SB / 6) +
      (as.numeric(ratiosByYear[[as.character(Year)]][["valuePerHits"]]) * Hits / 6)
  )
  
  return(valueOfPlayer)
}

# Example usage
HR <- as.numeric(readline("How many homeruns did the player have? "))
Runs <- as.numeric(readline("How many runs did the player have? "))
RBI <- as.numeric(readline("How many RBIs did the player have? "))
BB <- as.numeric(readline("How many walks did the player have? "))
Hits <- as.numeric(readline("How many hits did the player have? "))
SB <- as.numeric(readline("How many stolen bases did the player have? "))
Year <- as.numeric(readline("What year did they have these stats? "))

playerValue <- calculate_player_value(HR, Runs, RBI, BB, Hits, SB, Year)
cat("The value of the player for the year", Year, "is:", playerValue, "\n")
print(valueOfPlayer24)

# Function to plot LSRL and return a summary of the linear model
plotLSRL <- function(statType, salaryColumn, years = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024)) {
  
  # Extract the ratios for each year
  ratios <- sapply(years, function(year) {
    ratiosByYear[[as.character(year)]][[statType]][[salaryColumn]]
  })
  
  # Create a linear model (LSRL)
  model <- lm(ratios ~ years)
  
  # Plot the data points
  plot(years, ratios, main = paste("LSRL for", statType), 
       xlab = "Year", ylab = "Ratio", pch = 19, col = "blue")
  
  # Add the regression line
  abline(model, col = "red", lwd = 2)
  
  # Return the summary of the linear model
  return(summary(model))
}

# Example usage:
summaryHR <- plotLSRL("valuePerHR", "HR_Salary")
summaryRBI <- plotLSRL("valuePerRBI", "RBI_Salary")
summaryRuns <- plotLSRL("valuePerRuns", "Runs_Salary")
summaryBB <- plotLSRL("valuePerBB", "BB_Salary")
summarySB <- plotLSRL("valuePerSB", "SB_Salary")
summaryHits <- plotLSRL("valuePerHits", "Hits_Salary")

# Print summaries for review
print(summaryHR)
print(summaryRBI)
print(summaryRuns)
print(summaryBB)
print(summarySB)
print(summaryHits)

