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
#install.packages("knitr")
#install.packages("rmarkdown")


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
#library(caret)
library(glmnet)
library(randomForest)
library(xgboost)
library(dplyr)
library(car)
library(knitr)
library(rmarkdown)
library(broom)

options(scipen=999)

# Set up your Census API key
#census_api_key("64ee452f6c75e8fa2bcfa61cf7511c22833bb907", install = TRUE, overwrite = TRUE)

######
######## SECTION 0 - Read in Datasets

filt_ab <- read_csv( "PATH/data/built/regression_build_ic.csv")

# Prepare data for Correlation Analaysis

reg_filtered <- filt_ab %>%
  dplyr::select(
    cleaned_ic_name,
    gen_year_established,
    ic_name_for_join,
    yrs_in_existance,
    STRUCTURE_POWER_decision_council_sociocracy_FACTOR, 
    STRUCTURE_POWER_decision_consensus_FACTOR, 
    STRUCTURE_POWER_decision_democracy_majority_FACTOR,
    STRUCTURE_POWER_governance_core_group_FACTOR, 
    STRUCTURE_POWER_governance_one_leader_FACTOR, 
    COLLECTIVITY_income_sharing_some_or_all_FACTOR, 
    COLLECTIVITY_labor_expectation_FACTOR, 
    COLLECTIVITY_fees_reoccuring_FACTOR, 
    MORAL_CULTURE_diet_vegan_veg_FACTOR, 
    MORAL_CULTURE_no_alc_FACTOR, 
    MORAL_CULTURE_relig_spirit_or_no_FACTOR, 
    MORAL_CULTURE_relig_non_sec_spirituality_FACTOR, 
    COLLECTIVITY_land_individual_FACTOR, 
    COLLECTIVITY_land_community_FACTOR, 
    DEMOG_acreage_of_community, 
    student_or_university_flag, 
    DEMOG_urban_flag_FACTOR, 
    DEMOG_mbrsp_total_members_cleaned,
    DEMOG_m_from_city_over_50k,
    DEMOG_mbrsp_percent_men,
    ADJUSTMENT_centered_year
  ) %>% 
  #Filter to IC between 4 members and 400 members (align to literature definition of IC, and Reduce Noise)
  #Remove student communities
  #Remove IC that have no estbalishment date
  filter(yrs_in_existance > 0, 
         DEMOG_mbrsp_total_members_cleaned >= 4 & DEMOG_mbrsp_total_members_cleaned < 400, 
         student_or_university_flag == 0) %>% 
  # remove (a) anomaly data point and (b) two communities that are duplicates but changed locations
  filter(!cleaned_ic_name %in% c("bryngweledhomesteads"),
         !ic_name_for_join %in% c("global_sedon", "karenc_saint") ) %>% 
  #create a binary varaible if gender is +/- than 67% one direction or another
  mutate(DEMOG_gender_homog = ifelse(abs(0.5 - DEMOG_mbrsp_percent_men >= 0.17), 
                                     1, 
                                     0)) %>%
  mutate(DEMOG_log_transformed_acreage_of_community = log1p(DEMOG_acreage_of_community))%>%
  filter(
    !is.na(ADJUSTMENT_centered_year) &
      !is.na(MORAL_CULTURE_diet_vegan_veg_FACTOR) &
      !is.na(MORAL_CULTURE_no_alc_FACTOR) &
      !is.na(MORAL_CULTURE_relig_spirit_or_no_FACTOR) &
      !is.na(COLLECTIVITY_land_community_FACTOR) &
      !is.na(COLLECTIVITY_land_individual_FACTOR) &
      !is.na(COLLECTIVITY_income_sharing_some_or_all_FACTOR) &
      !is.na(COLLECTIVITY_labor_expectation_FACTOR) &
      !is.na(COLLECTIVITY_fees_reoccuring_FACTOR) &
      !is.na(STRUCTURE_POWER_governance_core_group_FACTOR) &
      !is.na(STRUCTURE_POWER_governance_one_leader_FACTOR) &
      !is.na(STRUCTURE_POWER_decision_council_sociocracy_FACTOR) &
      !is.na(STRUCTURE_POWER_decision_democracy_majority_FACTOR) &
      !is.na(DEMOG_mbrsp_total_members_cleaned) &
      !is.na(DEMOG_urban_flag_FACTOR) &
      !is.na(DEMOG_log_transformed_acreage_of_community)  &
      !is.na(DEMOG_gender_homog) & 
      !is.na(yrs_in_existance)
  ) 



# Define the variables for the correlation matrix
variables <- c("yrs_in_existance",
               "ADJUSTMENT_centered_year",
               "MORAL_CULTURE_diet_vegan_veg_FACTOR",
               "MORAL_CULTURE_no_alc_FACTOR",
               "MORAL_CULTURE_relig_spirit_or_no_FACTOR",
               "COLLECTIVITY_land_community_FACTOR",
               "COLLECTIVITY_land_individual_FACTOR",
               "COLLECTIVITY_income_sharing_some_or_all_FACTOR",
               "COLLECTIVITY_labor_expectation_FACTOR",
               "COLLECTIVITY_fees_reoccuring_FACTOR",
               "STRUCTURE_POWER_governance_core_group_FACTOR",
               "STRUCTURE_POWER_governance_one_leader_FACTOR",
               "STRUCTURE_POWER_decision_council_sociocracy_FACTOR",
               "STRUCTURE_POWER_decision_democracy_majority_FACTOR",
               "DEMOG_mbrsp_total_members_cleaned",
               "DEMOG_urban_flag_FACTOR",
               "DEMOG_log_transformed_acreage_of_community",
               "DEMOG_gender_homog")

# Filter the dataframe to include only the specified variables
df_for_corr <- reg_filtered %>% 
  dplyr::select(yrs_in_existance, DEMOG_mbrsp_total_members_cleaned, all_of(variables))

# Initialize an empty data frame to store results
cor_results <- data.frame(variable = character(),
                          correlation = numeric(),
                          std_error = numeric(),
                          p_value = numeric(),
                          significance = character())

# Function to calculate weighted correlation and standard error
calculate_weighted_correlation <- function(x, y, weights) {
  wx_bar <- sum(weights * x) / sum(weights)
  wy_bar <- sum(weights * y) / sum(weights)
  
  wx_var <- sum(weights * (x - wx_bar)^2) / sum(weights)
  wy_var <- sum(weights * (y - wy_bar)^2) / sum(weights)
  
  w_cov_xy <- sum(weights * (x - wx_bar) * (y - wy_bar)) / sum(weights)
  
  weighted_correlation <- w_cov_xy / sqrt(wx_var * wy_var)
  
  # Calculate standard error (using an alternative method)
  n <- length(x)
  se_correlation <- sqrt((1 - weighted_correlation^2) / (n - 2))
  
  return(list(correlation = weighted_correlation, se = se_correlation))
}

# Initialize an empty data frame to store results
cor_results_alt <- data.frame(variable = character(),
                              correlation = numeric(),
                              std_error = numeric(),
                              p_value = numeric(),
                              significance = character())

# Calculate correlations and SEs
for (var in variables) {
  x <- df_for_corr$yrs_in_existance
  y <- df_for_corr[[var]]
  weights <- df_for_corr$DEMOG_mbrsp_total_members_cleaned
  
  # Remove NA values
  complete_idx <- complete.cases(x, y, weights)
  x <- x[complete_idx]
  y <- y[complete_idx]
  weights <- weights[complete_idx]
  
  # Calculate the weighted correlation and its SE
  result <- calculate_weighted_correlation(x, y, weights)
  
  # Compute p-value (approximation using t-distribution)
  n <- length(x)
  t_value <- result$correlation / result$se
  p_value <- 2 * pt(-abs(t_value), df = n - 2)
  
  # Determine significance
  significance <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
  
  cor_results_alt <- rbind(cor_results_alt, data.frame(variable = var,
                                                       correlation = round(result$correlation, 3),
                                                       std_error = round(result$se, 3),
                                                       p_value = round(p_value, 3),
                                                       significance = significance))
}

# Print the alternative correlation results in a table format
print(cor_results_alt)



write_csv(cor_results_alt, "PATH/exports/regressions/pearson_coeff.csv")
