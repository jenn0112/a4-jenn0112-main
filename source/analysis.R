library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# library("dplyr")
library("dplyr")
library("tidyr")
library("ggplot2")
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
#----------------------------------------------------------------------------#
get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <-function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)
}

View(plot_jail_pop_for_us)

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}

get_jail_pop_by_states <-function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)
}

plot_jail_pop_by_states <- function(df) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p == 0) %>%
    select(state, p) %>%
    pull(state)
  return(t)
}

View(get_jail_pop_by_states)

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_basic_info <- function(df) {
  t <- "Basic Info on `incarceration_trends`\n"
  t <- paste0(t, "  No. regions               ", length(unique(df$region)), "\n")
  t <- paste0(t, "  No. divisions             ", length(unique(df$division)), "\n")
  t <- paste0(t, "  No. states                ", length(unique(df$state)), "\n")
  t <- paste0(t, "  No. counties              ", length(unique(df$county_name)), "\n")
  t <- paste0(t, "  No. fips (county IDs)     ", length(unique(df$fips)), "\n")
  t <- paste0(t, "  No. different urbanicity  ", length(unique(df$urbanicity)), "\n")
  
  t <- paste0(t, "Divisions ...\n")
  t <- paste0(t, format_region_info("Midwest"), "\n")
  t <- paste0(t, format_region_info("Northeast"), "\n")
  t <- paste0(t, format_region_info("South"), "\n")
  t <- paste0(t, format_region_info("West"), "\n")
  
  cat(t) 
}

Some races are disproportionally represented in jail in some areas of the country, it shows that the means number of incarcerations for different races go from white races having the highest mean of incarcerations, to black being next, then latinx, then native, then natives having the smallest mean of incarcerations.
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


