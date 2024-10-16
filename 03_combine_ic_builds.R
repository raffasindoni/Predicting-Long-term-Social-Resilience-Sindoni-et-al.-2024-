
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
# install.packages(c("tidycensus", "tidyverse", "lubridate"))

# Load the packages
library(tidycensus)


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
library(dplyr)

options(scipen=999)

# Set up your Census API key
#census_api_key("64ee452f6c75e8fa2bcfa61cf7511c22833bb907", install = TRUE, overwrite = TRUE)

######
######## SECTION 0 - Read in Datasets

wayback_ic_data <- read_csv("/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/A_wayback_build.csv")

ic_2024_present <- read_csv("/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/B_present_IC_24_build.csv")


######
######## SECTION 1 - Stack Variables
combined_ic <- rbind(wayback_ic_data, 
                     ic_2024_present )

######
######## SECTION 2 - Add Varibale on Length of Existance

#Group communities by together by unique id (name, city)
#Then find out the last year that they exist in dataset
#then subtract that against year founded

lenght_of_community_existance <- combined_ic %>%
  #remove observations that don't list year established
  filter(!is.na(gen_year_established)) %>%
  group_by(ic_name_for_join) %>%
  summarise(yr_established = max(gen_year_established),
            most_recent_scrape_date = max(yr)) %>%
  ungroup () %>%
  #calculate yrs in existance as the date last scraped on wayback minus the year listed it was established
  mutate(yrs_in_existance = most_recent_scrape_date - yr_established) %>%
  dplyr::select(ic_name_for_join,
         yrs_in_existance)

#join back onto main dataset
combined_ic <- combined_ic %>%
  left_join(.,
            lenght_of_community_existance)


######
######## SECTION 3 - Add Filtering Flags and Clean Variables 

# Clean the `mbrsp_total_members` variable
clean_members <- function(x) {
  # Remove text in parentheses
  x_clean <- gsub("\\s*\\(.*\\)", "", x)
  # Convert to numeric
  as.numeric(x_clean)
}

combined_ic_w_filter_flags <- combined_ic %>%
  mutate(scrape_as_date = make_date(yr, mnth, dy)) %>%
#Flag for Esablished or Not (do they list an establishment data)
mutate(flag_has_est_year = ifelse(!is.na(gen_year_established),
                                  1,
                                  0),
#Some amount of land (acrage)
      flag_has_acrage = ifelse(!is.na(house_area),
                               1,
                               0),
#Some amount of units or housing (acrage)
flag_has_units_or_housing = ifelse(!is.na(coho_number_of_residences) | 
                                     !is.na(coho_number_of_units),
                                   1,
                                   0),
#Some listing of an address? 
      flag_has_address = ifelse(!is.na(gen_community_address),
                               1,
                               0),
#Flag for IC LIsted country (not geocoded)
      flag_USA_listed_by_IC = ifelse(country_by_ic == "UnitedStates",
                                1,
                                0)) %>%
  #clean up total membrs field 
  mutate(mbrsp_total_members_cleaned = clean_members(mbrsp_total_members),
         #Flag for has members
         flag_has_four_plus_members = ifelse(mbrsp_total_members_cleaned>= 4,
                                             1,
                                             0)) %>%
  dplyr::select(-mbrsp_total_members)
######
######## SECTION 4 - Export TEMP for geocoding in ArcGIS
# Function to find the mode
# get_mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# # Summarize the data
# for_geocode <- combined_ic_w_filter_flags %>%
#   dplyr::select(ic_name_for_join,
#          gen_community_address) %>%
#   group_by(ic_name_for_join) %>%
#   summarize(most_common_address = get_mode(gen_community_address)) %>%
#   # summarize(
#   #   earliest_year_established = min(gen_year_established, na.rm = TRUE),
#   #   most_common_address = get_mode(gen_community_address),
#   #   median_total_members = median(mbrsp_total_members_cleaned, na.rm = TRUE)
#   ungroup()
# 
# write_csv(for_geocode, "/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/temps/all_ic_for_geocode.csv")

######
######## SECTION 6 - Re-Inport GIS GEoCoded data 

geocoded_all_ic <- read_xls("/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/temps/B_geocoded_ic_output.xls")
  
selected_geocoded_est_ic <- geocoded_all_ic %>% 
  rename("ic_name_for_join" = USER_ic_name_for_join,
         "geocoded_address" = LongLabel,
         "geocoded_city" = City,
         "geocoded_county_region" = Subregion,
         "geocoded_state" = Region,
         "geocoded_state_abrv" = RegionAbbr,
         "geocoded_country" = Country,
         "m_from_city_over_50k" = NEAR_DIST,
         "urban_area_code" = UA_CODE,
         "urban_area_name" = "NAME",
         "pop_of_urban_area" = POPULATION) %>%
  mutate(urban_flag = ifelse(!is.na(urban_area_code),
                             1,
                             0),
         gecoded_flag  =ifelse(!is.na(geocoded_address),
                                               1,
                                               0),
         confidence_in_geocoding_flag  =ifelse(Score > 80,
                                               1,
                                               0)) %>%
  dplyr::select(ic_name_for_join,
         urban_area_code,
         urban_area_name,
         pop_of_urban_area,
         m_from_city_over_50k,
         gecoded_flag,
         urban_flag,
         confidence_in_geocoding_flag,
         geocoded_address,
         geocoded_city,
         geocoded_county_region,
         geocoded_state,
         geocoded_state_abrv,
         geocoded_country)

######
######## SECTION 7 - Join Geocoded onto Main Dataset by Unique ID; Export for Build

#join back on to dataset
ic_build_w_geocoded <- combined_ic_w_filter_flags %>%
  left_join(.,
            selected_geocoded_est_ic,
            by = join_by("ic_name_for_join"))

#write to build
write_csv(ic_build_w_geocoded, "/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/C_all_ic_w_geocode.csv")


######
######## SECTION 8 - Make build subection of each community hving only one observation


# Custom function to calculate the mode excluding NA
calculate_mode <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to return the first non-NA observation or fallback to the mode
first_or_mode <- function(x) {
  if (!is.na(x[1])) {
    return(x[1])
  } else {
    return(calculate_mode(x))
  }
}

# Group by ic_name_for_join, arrange by yr in descending order, and summarize
aggregated_data <- ic_build_w_geocoded %>%
  group_by(ic_name_for_join) %>%
  arrange(desc(yr)) %>%
  #Take either the most recent information listed by the IC, or its most common information
  dplyr::summarize(
    yr_most_recently_scraped = first_or_mode(yr),
    mission_statement = first_or_mode(mission_statement),
    community_description = first_or_mode(community_description),
    name = first_or_mode(name),
    cleaned_ic_name = first_or_mode(cleaned_ic_name),
    about_type = first_or_mode(about_type),
    location_by_ic = first_or_mode(location_by_ic),
    city_by_ic = first_or_mode(city_by_ic),
    country_by_ic = first_or_mode(country_by_ic),
    gen_community_address = first_or_mode(gen_community_address),
    coho_number_of_residences = first_or_mode(coho_number_of_residences),
    coho_number_of_units = first_or_mode(coho_number_of_units),
    econ_join_fee = first_or_mode(econ_join_fee),
    econ_regular_fees = first_or_mode(econ_regular_fees),
    econ_required_weekly_labor_contribution = first_or_mode(econ_required_weekly_labor_contribution),
    econ_shared_income = first_or_mode(econ_shared_income),
    env_current_renewable_energy_generation = first_or_mode(env_current_renewable_energy_generation),
    env_energy_infrastructure = first_or_mode(env_energy_infrastructure),
    gen_year_established = first_or_mode(gen_year_established),
    gen_year_formed = first_or_mode(gen_year_formed),
    gov_decision_making = first_or_mode(gov_decision_making),
    gov_identified_leader = first_or_mode(gov_identified_leader),
    gov_leadership_core_group = first_or_mode(gov_leadership_core_group),
    house_area = first_or_mode(house_area),
    house_current_residence_types = first_or_mode(house_current_residence_types),
    house_land_owned_by = first_or_mode(house_land_owned_by),
    life_shared_meals = first_or_mode(life_shared_meals),
    life_alcohol_use = first_or_mode(life_alcohol_use),
    life_dietary_practices = first_or_mode(life_dietary_practices),
    life_education_styles = first_or_mode(life_education_styles),
    life_spiritual_practice_expected = first_or_mode(life_spiritual_practice_expected),
    life_which_spiritual_traditions = first_or_mode(life_which_spiritual_traditions),
    mbrsp_adult_members = first_or_mode(mbrsp_adult_members),
    mbrsp_child_members = first_or_mode(mbrsp_child_members),
    mbrsp_percent_men = first_or_mode(mbrsp_percent_men),
    network_affiliations = first_or_mode(network_affiliations),
    data_source = first_or_mode(data_source),
    yrs_in_existance = first_or_mode(yrs_in_existance),
    flag_has_est_year = first_or_mode(flag_has_est_year),
    flag_has_acrage = first_or_mode(flag_has_acrage),
    flag_has_units_or_housing = first_or_mode(flag_has_units_or_housing),
    flag_has_address = first_or_mode(flag_has_address),
    flag_USA_listed_by_IC = first_or_mode(flag_USA_listed_by_IC),
    mbrsp_total_members_cleaned = first_or_mode(mbrsp_total_members_cleaned),
    flag_has_four_plus_members = first_or_mode(flag_has_four_plus_members),
    urban_area_code = first_or_mode(urban_area_code),
    urban_area_name = first_or_mode(urban_area_name),
    pop_of_urban_area = first_or_mode(pop_of_urban_area),
    m_from_city_over_50k = first_or_mode(m_from_city_over_50k),
    gecoded_flag = first_or_mode(gecoded_flag),
    urban_flag = first_or_mode(urban_flag),
    confidence_in_geocoding_flag = first_or_mode(confidence_in_geocoding_flag),
    geocoded_address = first_or_mode(geocoded_address),
    geocoded_city = first_or_mode(geocoded_city),
    geocoded_county_region = first_or_mode(geocoded_county_region),
    geocoded_state = first_or_mode(geocoded_state),
    geocoded_state_abrv = first_or_mode(geocoded_state_abrv),
    geocoded_country = first_or_mode(geocoded_country)
  )

write_csv(aggregated_data, "/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/aggregated_ic_data.csv")
