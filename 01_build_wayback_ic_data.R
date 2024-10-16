
######### CODE OVERVIEW

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


#0 - #Full list of IC (with descriptions)


#These are scrapes of WayBack machine's detailed listings of each commuity
#These are dependent on Wayback's own data collection mechanism and do not
#represent an exhaustive list of all IC during the years present 
#Details could only be pulled when wayback scraped a community's detailed page

#This is a collection of IC with all of their full details (part 1 of WayBack Scrape)
full_ic_part1 <- read_csv("/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/raw/Krut_ic_wayback_scrape_part_1_final.csv")

#This is a collection of IC with all of their full details from roughly 2017 to 2024
full_ic_part2  <- read_csv("/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/raw/Krut_ic_wayback_scrape_part_2_final.csv")

######## 
######## Section A
############ Prepare Part ONE of Full Detailed IC Listings (mostly 2004 - 2017)

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
cleaned_full_part1 <- full_ic_part1 %>%
  rename_with(clean_names, everything()) %>% 
  rename("econ_join_fee" = `econ_join_fee($)`,
         "life_education_styles_x" = `life_education_style(s)`,
         "life_education_styles_y" = `life_education_style(s).1`,
         "about_type" = `about_type(s)`,
         "location_by_ic" = "location",
         "life_spiritual_practice_expected" = `life_spiritual_practice_expected?`
  )

#####
###### A2 Add New Important Variables 

#These are the dates that the IC was captured in the Wayback scrape
cleaned_full_part1 <- cleaned_full_part1 %>%
  #Parse out yr,mtnh,day for scrape
  mutate(yr = as.numeric(substr(timestamp, 1, 4)),
         mnth = as.numeric(substr(timestamp, 5, 6)),
         dy = as.numeric(substr(timestamp, 7, 8)),
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
  #unify_education_style_ variable
  mutate(life_education_styles = ifelse(!is.na(life_education_styles_x),
                                        life_education_styles_x,
                                        life_education_styles_y)) %>%
  #unify the shared spiritual path variable)
  mutate(life_spiritual_practice_expected = ifelse(!is.na(life_spiritual_practice_expected),
                                                   life_spiritual_practice_expected,
                                                   life_shared_spiritual_path))%>%
  #unify the shared income variable
  mutate(econ_shared_income = ifelse(!is.na(econ_shared_income),
                                     econ_shared_income,
                                     econ_financial_style)) %>% 
  #unify the weekly labor contribution variable
  mutate(econ_required_weekly_labor_contribution = ifelse(!is.na(econ_required_weekly_labor_contribution),
                                                          econ_required_weekly_labor_contribution,
                                                          econ_required_labor_contribution))%>% 
  mutate(mbrsp_adult_members = parse_number(mbrsp_adult_members),
         mbrsp_child_members = parse_number(mbrsp_child_members)) %>%
  mutate(mbrsp_adult_members = ifelse(is.na(mbrsp_adult_members),
                                      0,
                                      mbrsp_adult_members),
         mbrsp_child_members = ifelse(is.na(mbrsp_child_members),
                                      0,
                                      mbrsp_child_members)) %>%
  #unify total_member_variable
  mutate(mbrsp_total_members = ifelse(!is.na(mbrsp_total_members),
                                      mbrsp_total_members,
                                      mbrsp_adult_members + mbrsp_child_members)) %>%
  mutate(mission_statement = ifelse(is.na(mission_statement),
                                    community_description,
                                    mission_statement))



######## Clean up Gender Variables

#First begin with the "percent_men variable"

# Function to extract percentage or range
extract_percentage_or_range <- function(text) {
  # Extract the percentage or range using regular expression
  result <- str_extract(text, "\\b\\d{1,2}%|\\d{1,2}-\\d{1,2}%")
  return(result)
}

cleaned_full_part1 <- cleaned_full_part1 %>% 
  mutate(cleaned_mbrsp_percent_men  = extract_percentage_or_range(mbrsp_percent_men))

# Function to extract male percentage
extract_male_percentage <- function(text) {
  # Extract the male percentage using regular expression
  result <- str_extract(text, "\\b\\d{1,2}%\\s*M")
  # Remove the ' M' part to keep only the number and percentage sign
  result <- str_replace(result, "\\s*M", "")
  return(result)
}

# Apply the function to the mbrsp_gender_balance varibale and store it in the cleaned_mbrsp_percent_men varibale we just created, if it has no value
cleaned_full_part1 <- cleaned_full_part1 %>% 
  mutate(cleaned_mbrsp_percent_men = if_else(is.na(cleaned_mbrsp_percent_men),
                                             extract_male_percentage(mbrsp_gender_balance),
                                             cleaned_mbrsp_percent_men))

#Correct range values to the midpoint of the range 
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "1-10%"] <- "5%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "11-20%"] <- "15%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "21-30%"] <- "25%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "31-40%"] <- "35%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "41-49%"] <- "45%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "51-60%"] <- "55%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "61-70%"] <- "65%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "71-80%"] <- "75%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "81-90%"] <- "85%"
cleaned_full_part1$cleaned_mbrsp_percent_men[cleaned_full_part1$cleaned_mbrsp_percent_men == "91-99%"] <- "95%"

#Parse percent as number
cleaned_full_part1 <- cleaned_full_part1 %>%
  mutate(mbrsp_percent_men_numberic = as.numeric(str_replace(cleaned_mbrsp_percent_men, "%", "")) / 100) %>%
  dplyr::select(-cleaned_mbrsp_percent_men,
         -mbrsp_percent_men,
         - mbrsp_gender_balance) %>%
  rename("mbrsp_percent_men"= "mbrsp_percent_men_numberic")


#####
###### A3 Select Relevant Variables for Analysis

selected_full_part1 <- cleaned_full_part1 %>%
    dplyr::select("mission_statement",
           community_description,
           "about_type",
           "yr",
           "mnth",
           "dy",
           "location_by_ic",
           "city_by_ic",
           "country_by_ic",
           "name",
           "cleaned_ic_name",
           "ic_name_for_join",
           gen_community_address,
           "gen_year_formed",
           "gen_year_established",
           "gov_decision_making",
           "life_dietary_practices",
           "life_education_styles",
           "life_shared_meals",
           "env_current_renewable_energy_generation",
           "env_energy_infrastructure",
           "econ_join_fee",
           "gov_leadership_core_group",
           "gov_identified_leader",
           "econ_shared_income",
           "econ_required_weekly_labor_contribution",
           "life_alcohol_use",
           "life_spiritual_practice_expected",
           "life_which_spiritual_traditions",
           "house_current_residence_types",
           "house_land_owned_by",
           "econ_regular_fees",
           "house_area",
           "mbrsp_total_members",
           "mbrsp_adult_members",
           "mbrsp_child_members",
           "coho_number_of_units",
           "coho_number_of_residences",
           "mbrsp_percent_men",
           "network_affiliations")
         


######## 
######## Section B
############ Prepare Full Detailed IC Listings of 2019 - 2023 years

#####
###### B1 Prepare Variables Names

# Rename variables using the custom function, then manually correct a few
cleaned_full_part2 <- full_ic_part2 %>%
  rename_with(clean_names, everything()) %>% 
  rename("econ_join_fee" = `econ_join_fee($)`,
         "life_education_styles" = `life_education_style(s)`,
         "about_type" = `about_type(s)`,
         "econ_regular_fee_per_month" = `econ_monthly_fees($)`,
         "econ_regular_fees" = `econ_dues,_fees,_or_shared_expenses`,
         "life_which_spiritual_traditions" = `life_common_spiritual_practice(s)`,
         "location_by_ic" = "location_hdr",
         "gen_year_established" = "gen_started_living_together",
         "gen_year_formed" = "gen_started_planning",
         "life_spiritual_practice_expected" = `life_spiritual_practice_expected?` ,
         "coho_number_of_units" = "number_of_housing_units",
         "coho_number_of_residences" = "house_current_number_of_residences"
  )


#####
###### B2 Add New Important Variables

#These are the dates that the IC was captured in the Wayback scrape
cleaned_full_part2 <- cleaned_full_part2 %>%
  #Parse out yr,mtnh,day for scrape
  mutate(yr = as.numeric(substr(timestamp, 1, 4)),
         mnth = as.numeric(substr(timestamp, 5, 6)),
         dy = as.numeric(substr(timestamp, 7, 8)),
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
                                          sep = "_")))%>%
  #unify total_member_variable
  mutate(mbrsp_adult_members = ifelse(is.na(mbrsp_adult_members),
                                      0,
                                      mbrsp_adult_members),
         mbrsp_child_members = ifelse(is.na(mbrsp_child_members),
                                      0,
                                      mbrsp_child_members)) %>%
  mutate(mbrsp_total_members = mbrsp_adult_members + mbrsp_child_members)

#####
###### B3 Select Relevant Variables for Analysis

selected_full_part2 <- cleaned_full_part2 %>%
  dplyr::select("mission_statement",
         community_description,
         "about_type",
         "yr",
         "mnth",
         "dy",
         "location_by_ic",
         "city_by_ic",
         "country_by_ic",
         "name",
         "cleaned_ic_name",
         "ic_name_for_join",
         gen_community_address,
         "gen_year_formed",
         "gen_year_established",
         "gov_decision_making",
         "life_dietary_practices",
         "life_shared_meals",
         "life_education_styles",
         "env_current_renewable_energy_generation",
         "env_energy_infrastructure",
         "econ_join_fee",
         "gov_leadership_core_group",
         "gov_identified_leader",
         "econ_shared_income",
         "econ_required_weekly_labor_contribution",
         "life_alcohol_use",
         "life_spiritual_practice_expected",
         "life_which_spiritual_traditions",
         "house_current_residence_types",
         "house_land_owned_by",
         "econ_regular_fees",
         "house_area",
         "mbrsp_total_members",
         "mbrsp_adult_members",
         "mbrsp_child_members",
         "coho_number_of_units",
         "coho_number_of_residences",
         "mbrsp_percent_men") %>%
  mutate(network_affiliations = NA)


#######################
#######################
#######################
###### C3 Stack Together Data (and Temp Export)

combined_04_24 <- rbind(selected_full_part1,
                     selected_full_part2) %>%
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
         "life_shared_meals",
         "life_dietary_practices",
         "life_education_styles",
         "life_spiritual_practice_expected",
         "life_which_spiritual_traditions",
         "mbrsp_adult_members",
         "mbrsp_child_members",
         "mbrsp_percent_men",
         "network_affiliations"
         ) %>%
  mutate(data_source = "wayback_machine")

#write to build

write_csv(combined_04_24, "/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/A_wayback_build.csv")



