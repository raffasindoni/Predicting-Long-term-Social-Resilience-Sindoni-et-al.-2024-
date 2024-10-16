

######### CODE OVERVIEW

# Script ID = 004
# Code Author = Raffa Sindoni, PhD Candidate '28 Yale University (raffaele.sindoni@yale.edu)
# PROJECT = IC Analysis
# Co-Authors = Farrell, Herring

# Overview = Build IC regression

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
# install.packages("MASS")
# install.packages("glmnet")
# install.packages("caret")
# install.packages("xgboost")



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
library(MASS)
library(caret)
library(glmnet)
library(randomForest)
library(xgboost)
library(dplyr)

options(scipen=999)

######
######## SECTION 0 - Read in Datasets

ag_build <- read_csv("/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/aggregated_ic_data.csv")

######
######## SECTION 8 - Add Regression Flags to Build

##Filtered down to US observations w/ est year and establishment date
filt_ab <- ag_build %>% 
  filter(geocoded_country == "USA",
         country_by_ic == "UnitedStates") %>% 
  #Filter to Established IC ONLY between 1900 and 2024
  filter(flag_has_est_year == 1) %>% 
  filter(gen_year_established > 1900 & gen_year_established < 2024)


# 1. decision_council_sociocracy
filt_ab <- filt_ab %>%
  mutate(decision_council_sociocracy = if_else(
      str_detect(gov_decision_making, regex("\\b(sociocracy)\\b", ignore_case = TRUE)) |
      str_detect(gov_decision_making, regex("\\b(council)\\b", ignore_case = TRUE))|
      str_detect(gov_decision_making, regex("\\b(board)\\b", ignore_case = TRUE)) |
      str_detect(gov_decision_making, regex("\\b(group of elders)\\b", ignore_case = TRUE)) & 
      !str_detect(gov_decision_making, regex("\\b(consensus)\\b", ignore_case = TRUE)) & 
      !str_detect(gov_decision_making, regex("\\b(majority)\\b", ignore_case = TRUE)),
    1, 0))

# 2. decision_consensus
filt_ab <- filt_ab %>%
  mutate(decision_consensus = if_else(
    str_detect(gov_decision_making, regex("\\b(consensus)\\b", ignore_case = TRUE)) &
      !str_detect(gov_decision_making, regex("\\b(majority)\\b", ignore_case = TRUE)) &
      decision_council_sociocracy == 0,
    1, 0))

# 3. decision_democracy_majority
filt_ab <- filt_ab %>%
  mutate(decision_democracy_majority = if_else(str_detect(gov_decision_making, regex("\\b(majority|democracy)\\b", ignore_case = TRUE)) & 
                                                 decision_council_sociocracy == 0 &
                                                 decision_consensus == 0,
                                               1, 0))

# 4. diet_vegan_veg
filt_ab <- filt_ab %>%
  mutate(diet_vegan_veg = if_else(str_detect(life_dietary_practices, regex("\\b(vegetarian|vegetarian only|vegan only|primarily vegetarian|primarily vegan)\\b", ignore_case = TRUE)) & 
                                  !str_detect(life_dietary_practices, regex("\\b(Omnivorous)\\b", ignore_case = TRUE)),
                                  1, 0))
  
# 5. governance_core_group
filt_ab <- filt_ab  %>%
  mutate(governance_core_group = if_else(str_detect(gov_leadership_core_group, regex("\\b(yes)\\b", ignore_case = TRUE)), 1, 0))

# 6. governance_one_leader
filt_ab <- filt_ab  %>%
  mutate(governance_one_leader = if_else(str_detect(gov_identified_leader, regex("\\b(yes)\\b", ignore_case = TRUE)), 1, 0))

# 7. income_sharing_some_or_all
filt_ab <- filt_ab  %>%
  mutate(income_sharing_some_or_all = if_else(str_detect(econ_shared_income, regex("\\b(partial|100%|all)\\b", ignore_case = TRUE)) & 
                                                !str_detect(econ_shared_income, regex("\\b(independent)\\b", ignore_case = TRUE)),
                                              1, 0))

# 8. labor_expectation
filt_ab <- filt_ab %>%
  mutate(labor_expectation = if_else(
    str_detect(econ_required_weekly_labor_contribution, regex("\\b[1-9]\\d*\\b|expected", ignore_case = TRUE)), 
    1, 
    0
  ))

# 9. no_alc
filt_ab <- filt_ab  %>%
  mutate(no_alc = if_else(str_detect(life_alcohol_use, regex("\\b(prohibited|No)\\b", ignore_case = TRUE)), 1, 0))

# 10. relig_spirit_or_no
filt_ab <- filt_ab  %>%
  mutate(relig_spirit_or_no = if_else(str_detect(life_spiritual_practice_expected, regex("\\b(Yes)\\b", ignore_case = TRUE)), 1, 0))

# 11. relig_non_sec_spirituality
filt_ab <- filt_ab  %>%
  mutate(relig_non_sec_spirituality = if_else(str_detect(life_which_spiritual_traditions, regex("\\b(Christian|Buddhist|Jewish|Catholic|Quaker|Protestant|Lutheran|Hindu|Hare Krishna)\\b", ignore_case = TRUE)) & 
                                                !str_detect(life_which_spiritual_traditions, regex("\\b(spiritual|Ecumenical)\\b", ignore_case = TRUE)), 1, 0))

# 12. land_individual_ownership
filt_ab <- filt_ab  %>%
  mutate(land_individual = if_else(str_detect(house_land_owned_by, regex("\\b(Individual community|subgroup of community|individual|individuals|landlord)\\b", ignore_case = TRUE)), 1, 0))

# 13. land_community
filt_ab <- filt_ab  %>%
  mutate(land_community = if_else(str_detect(house_land_owned_by, regex("\\b(The Entire Community|The Community|The Whole Community|Community-controlled|Community controlled|Community-controlled land trust|Non-profit|not for profit|non profit|land trust)\\b", ignore_case = TRUE)) 
                                               & land_individual == 0,
                                  1, 0))

# 14. fees_reoccuring
filt_ab <- filt_ab  %>%
  mutate(fees_reoccuring = if_else(str_detect(econ_regular_fees, regex("\\b(Yes|\\d+)\\b", ignore_case = TRUE)) & 
                                     !str_detect(econ_regular_fees, regex("\\b(No)\\b", ignore_case = TRUE)), 1, 0))

# 15. Area (acrage) of community
filt_ab <- filt_ab  %>%
  # Step 1: Remove everything in parentheses
  mutate(house_area_clean = str_replace_all(house_area, "\\s*\\([^\\)]+\\)", "")) %>%
  # Step 2: Extract the first number and the word next to it
  mutate(
    acreage_of_community = case_when(
      str_detect(house_area_clean, regex("\\d+\\s*acres", ignore_case = TRUE)) ~ as.numeric(str_extract(house_area_clean, "\\d+")),
      str_detect(house_area_clean, regex("\\d+\\s*hectares", ignore_case = TRUE)) ~ as.numeric(str_extract(house_area_clean, "\\d+")) * 2.47105,
      TRUE ~ 0  # If no match, return NA
    ))

# 16. Student flag
filt_ab <- filt_ab  %>%
  mutate(student_or_university_flag = if_else(str_detect(cleaned_ic_name, regex("student|university|college", ignore_case = TRUE)) |
                                                cleaned_ic_name == "sherwoodcoop" , 1, 0))

# 17. Network Communities flag
filt_ab <- filt_ab  %>%
  mutate(network_flag = ifelse(cleaned_ic_name %in% c("infinitestarlightofferingvisionaryecovillages"),
                               1,
                               0))
           

###########

# Convert specified variables to factors and ensure numerical variables are numeric
filt_ab <- filt_ab %>%
  mutate(
    STRUCTURE_POWER_decision_council_sociocracy_FACTOR = as.factor(decision_council_sociocracy),
    STRUCTURE_POWER_decision_consensus_FACTOR = as.factor(decision_consensus),
    STRUCTURE_POWER_decision_democracy_majority_FACTOR = as.factor(decision_democracy_majority),
    MORAL_CULTURE_diet_vegan_veg_FACTOR = as.factor(diet_vegan_veg),
    STRUCTURE_POWER_governance_core_group_FACTOR = as.factor(governance_core_group),
    STRUCTURE_POWER_governance_one_leader_FACTOR = as.factor(governance_one_leader),
    COLLECTIVITY_income_sharing_some_or_all_FACTOR = as.factor(income_sharing_some_or_all),
    COLLECTIVITY_labor_expectation_FACTOR = as.factor(labor_expectation),
    MORAL_CULTURE_no_alc_FACTOR = as.factor(no_alc),
    MORAL_CULTURE_relig_spirit_or_no_FACTOR = as.factor(relig_spirit_or_no),
    MORAL_CULTURE_relig_non_sec_spirituality_FACTOR = as.factor(relig_non_sec_spirituality),
    COLLECTIVITY_land_individual_FACTOR = as.factor(land_individual),
    COLLECTIVITY_land_community_FACTOR = as.factor(land_community),
    COLLECTIVITY_fees_reoccuring_FACTOR = as.factor(fees_reoccuring),
    DEMOG_acreage_of_community= as.numeric(acreage_of_community),
    student_or_university_flag = as.numeric(student_or_university_flag),
    DEMOG_mbrsp_percent_men = as.numeric(mbrsp_percent_men),
    DEMOG_urban_flag_FACTOR = as.factor(urban_flag),
    DEMOG_mbrsp_total_members_cleaned = as.numeric(mbrsp_total_members_cleaned),
    DEMOG_m_from_city_over_50k = as.numeric(m_from_city_over_50k),
  ) %>%
  #create variable that determines newer scrapes vs. older scrapes (vs the mean)
  mutate(ADJUSTMENT_centered_year = yr_most_recently_scraped - mean(yr_most_recently_scraped, na.rm = TRUE))


write_csv(filt_ab, "/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/regression_build_ic.csv")

