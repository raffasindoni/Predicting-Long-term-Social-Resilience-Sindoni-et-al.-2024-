
######### CODE OVERVIEW

# Script ID = 004
# Code Author = Raffa Sindoni, PhD Candidate '28 Yale University (raffaele.sindoni@yale.edu)
# PROJECT = IC Analysis
# Co-Authors = Farrell, Herring

# Overview = Prepare IC Scrape of 2024 (Present) For Analysis

rm(list = ls(all.names = TRUE)) 

# Install Packges and fetch library -----------------------------------------------------

# install.packages("tigris")
# install.packages('devtools')
# install.packages('zipcodeR')
# install.packages("tidyverse")
# install.packages("writexl")
# install.packages("lubridate")
# install.packages("haven")
# install.packages("rjson") 

library("tigris")
library('devtools')
library('zipcodeR')
library("tidyverse")
library("glue")
library("readxl")
library(ggplot2)
library("writexl")
library(lubridate)
library(haven)
library(scales)
library(readr)

options(scipen=999)
######
######## SECTION 0 - Read in Datasets

############## 

#0 - #Full list of IC (with descriptions) from 2024


#This is an exhaustive scrape of all IC
#from Ic.org in the present day (not wayback Machine)
ic_2024_present <- read_csv("/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/raw/willis_ic_scrape_results_2024_raw.csv")

######## 
######## Section A
############ Prepare Full Detailed IC Listings of 2024

#####
###### A1 Prepare Variables Names

# Rename variables to lowercase and remove spaces and symbols
# Function to clean variable names
clean_names <- function(name) {
  name <- tolower(name)  # Convert to lowercase
  name <- gsub("[[:punct:]&&[^_]]", "", name)  # Remove punctuation except underscores
  name <- gsub("\\s+", "_", name)  # Replace spaces with underscores
  return(name)
}

# Rename variables using the custom function, then manually correct a few
cleaned_ic_2024_present <- ic_2024_present %>%
  rename_with(clean_names, everything()) %>% 
  rename("econ_join_fee" = `econ_join_fee($)`,
         "life_education_styles" = `life_education_style(s)`,
         "about_type" = `about_type(s)`,
         "location_by_ic" = "location",
         "life_spiritual_practice_expected" = `life_spiritual_practice_expected?`,
         "econ_regular_fees" = `econ_dues,_fees,_or_shared_expenses`,
         "coho_number_of_units" = "coho_number_of_house_units",
         "coho_number_of_residences" = "house_current_number_of_residences",
         "gen_year_established" = "gen_started_living_together",
         "gen_year_formed" = "gen_started_planning",
         "life_which_spiritual_traditions" = `life_common_spiritual_practice(s)`
  )


#####
###### B2 Add New Important Variables

#These are the dates that the IC was captured in the Wayback scrape
cleaned_ic_2024_present <- cleaned_ic_2024_present %>%
  #Parse out yr,mtnh,day for scrape
  mutate(yr = 2024,
         mnth = 2,
         dy = 1,
         #Clean names of the IC
         cleaned_ic_name = tolower(str_replace_all(name, "[^a-zA-Z]", "")),
         city_by_ic = str_extract(location_by_ic, "^[^,]+"),  # Extract text before the first comma (multi-word cities)
         state_by_ic = str_extract(location_by_ic, "(?<=,\\s)[^,]+(?=,\\s[^,]+$)"),  # Extract text after the first comma
         country_by_ic = str_extract(location_by_ic, "[^,]+$"),  # Extract text after the last comma
         # Clean up city, state, and country
         city_by_ic = str_replace_all(city_by_ic, "[^a-zA-Z]", "") %>% 
           str_trim(),  # Keep only letters and spaces, then trim any extra spaces
         state_by_ic = str_replace_all(state_by_ic, "[^a-zA-Z]", "") %>% 
           str_trim(), 
         country_by_ic = str_replace_all(country_by_ic, "[^a-zA-Z]", "") %>% 
           str_replace_all("Forming", "")) %>%
  #Create a join name using first 6 letters of name PLUS first 5 letters of city
  mutate(ic_name_for_join = tolower(paste(substr(cleaned_ic_name, 1,6),
                                          substr(city_by_ic, 1,5),
                                          sep = "_"))) %>% 
  #unify total_member_variable
  mutate(mbrsp_adult_members = ifelse(is.na(mbrsp_adult_members),
                                      0,
                                      mbrsp_adult_members),
         mbrsp_child_members = ifelse(is.na(mbrsp_child_members),
                                      0,
                                      mbrsp_child_members)) %>%
  mutate(mbrsp_total_members = mbrsp_adult_members + mbrsp_child_members) 
  

# 
######## Clean up Gender Variables

#First begin with the "percent_men variable"

# Function to extract percentage or range
extract_percentage_or_range <- function(text) {
  # Extract the percentage or range using regular expression
  result <- str_extract(text, "\\b\\d{1,2}%|\\d{1,2}-\\d{1,2}%")
  return(result)
}

cleaned_ic_2024_present <- cleaned_ic_2024_present %>% 
  mutate(cleaned_mbrsp_percent_men  = extract_percentage_or_range(mbrsp_percent_men))

# Function to extract male percentage
extract_male_percentage <- function(text) {
  # Extract the male percentage using regular expression
  result <- str_extract(text, "\\b\\d{1,2}%\\s*M")
  # Remove the ' M' part to keep only the number and percentage sign
  result <- str_replace(result, "\\s*M", "")
  return(result)
}

# # Apply the function to the mbrsp_gender_balance varibale and store it in the cleaned_mbrsp_percent_men varibale we just created, if it has no value
# cleaned_ic_2024_present <- cleaned_ic_2024_present %>% 
#   mutate(cleaned_mbrsp_percent_men = if_else(is.na(cleaned_mbrsp_percent_men),
#                                              extract_male_percentage(mbrsp_gender_balance),
#                                              cleaned_mbrsp_percent_men))

#Correct range values to the midpoint of the range 
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "1-10%"] <- "5%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "11-20%"] <- "15%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "21-30%"] <- "25%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "31-40%"] <- "35%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "41-49%"] <- "45%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "51-60%"] <- "55%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "61-70%"] <- "65%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "71-80%"] <- "75%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "81-90%"] <- "85%"
cleaned_ic_2024_present$cleaned_mbrsp_percent_men[cleaned_ic_2024_present$cleaned_mbrsp_percent_men == "91-99%"] <- "95%"

#Parse percent as number
cleaned_ic_2024_present <- cleaned_ic_2024_present %>%
  mutate(mbrsp_percent_men_numberic = as.numeric(str_replace(cleaned_mbrsp_percent_men, "%", "")) / 100) %>%
  dplyr::select(-cleaned_mbrsp_percent_men,
         -mbrsp_percent_men) %>%
  rename("mbrsp_percent_men"= "mbrsp_percent_men_numberic")




##Select relevant variables for analysis

selected_24ic <- cleaned_ic_2024_present %>%
  #Reorganize
  dplyr::select("dy",
         "yr",
         "mnth",
         "mission_statement",
         community_description,
         "name",
         "cleaned_ic_name",
         "about_type",
         "location_by_ic",
         "city_by_ic",
         "country_by_ic",
         gen_community_address,
         "mbrsp_total_members",
         "coho_number_of_residences",
         "coho_number_of_units",
         "econ_join_fee",
         "econ_regular_fees",
         "econ_required_weekly_labor_contribution",
         "econ_shared_income",
         "env_current_renewable_energy_generation",
         "env_energy_infrastructure",
         "gen_year_established",
         "gen_year_formed",
         "gov_decision_making",
         "gov_identified_leader",
         "gov_leadership_core_group",
         "house_area",
         "house_current_residence_types",
         "house_land_owned_by",
         "ic_name_for_join",
         "life_alcohol_use",
         "life_dietary_practices",
         "life_education_styles",
         "life_shared_meals",
         "life_spiritual_practice_expected",
         "life_which_spiritual_traditions",
         "mbrsp_adult_members",
         "mbrsp_child_members",
         "mbrsp_percent_men",
         "network_affiliations"
  ) %>%
  mutate(data_source = "IC_org_webpage_scrape_2024")

#write to build

write_csv(selected_24ic, "/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/B_present_IC_24_build.csv")



