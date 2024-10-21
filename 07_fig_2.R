
# Overview = Summary stats for mission statement analysis

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
#install.packages("extrafont")
library(extrafont)

options(scipen=999)

######
######## SECTION 0 - Read in Datasets

filt_ab <- read_csv( "/Users/raffaelesindoni/Library/Mobile Documents/com~apple~CloudDocs/yale_PHD/doctoral_research/community/data/built/regression_build_ic.csv")


# (A) Prepare data 
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
  #Remove IC that have no establishment date
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


# Assuming your dataframe is named only_reg_obs and has the required columns
only_reg_obs <- reg_filtered %>%
  mutate(community_size = case_when(
    DEMOG_mbrsp_total_members_cleaned <= 10 ~ "0-10",
    DEMOG_mbrsp_total_members_cleaned <= 50 ~ "10-50",
    DEMOG_mbrsp_total_members_cleaned <= 100 ~ "50-100",
    DEMOG_mbrsp_total_members_cleaned <= 200 ~ "100-200",
    DEMOG_mbrsp_total_members_cleaned > 200 & DEMOG_mbrsp_total_members_cleaned < 400 ~ "200-400"
  ))

# Create a new column for community size categories (for point sizes)
only_reg_obs <- only_reg_obs %>%
  mutate(community_size = factor(community_size, levels = c("0-10", "10-50", "50-100", "100-200", "200-400")),
  year_category = case_when(
    gen_year_established < 1960 ~ "Before 1960",
    gen_year_established >= 1960 & gen_year_established <= 1970 ~ "1960-1970",
    gen_year_established >= 1971 & gen_year_established <= 1980 ~ "1970-1980",
    gen_year_established >= 1981 & gen_year_established <= 1990 ~ "1981-1990",
    gen_year_established >= 1991 & gen_year_established <= 2000 ~ "1991-2000",
    gen_year_established >= 2001 & gen_year_established <= 2010 ~ "2001-2010",
    gen_year_established >= 2011 & gen_year_established <= 2020 ~ "2011-2020",
    gen_year_established >= 2021 ~ "2021 onwards",
    TRUE ~ "Unknown"
  ))

# Custom color palette for year_category
custom_colors <- c(
  "Before 1960" = "#084594",
  "1960-1970" = "#2171b5",
  "1970-1980" = "#4292c6",
  "1981-1990" = "#6baed6",
  "1991-2000" = "#9ecae1",
  "2001-2010" = "#c6dbef",
  "2011-2020" = "#deebf7",
  "2021 onwards" = "#f7fbff"
)

# Summarize total members for Rural and Urban categories
total_members <- only_reg_obs %>%
  group_by(DEMOG_urban_flag_FACTOR) %>%
  summarise(total_members = sum(DEMOG_mbrsp_total_members_cleaned))


# Create labels with total members, formatting with commas
rural_label <- paste("Rural\n(N =", format(sum(only_reg_obs$DEMOG_urban_flag_FACTOR == 0), big.mark = ","), ", # =", format(total_members$total_members[total_members$DEMOG_urban_flag_FACTOR == 0], big.mark = ","), ")")
urban_label <- paste("Urban\n(N =", format(sum(only_reg_obs$DEMOG_urban_flag_FACTOR == 1), big.mark = ","), ", # =", format(total_members$total_members[total_members$DEMOG_urban_flag_FACTOR == 1], big.mark = ","), ")")
updated_labels <- c(rural_label, urban_label)

# Plot
ggplot(only_reg_obs, aes(x = factor(DEMOG_urban_flag_FACTOR), y = yrs_in_existance)) +
  geom_violin(fill = "grey80", alpha = 0.3, color = NA) +
  geom_jitter(aes(size = community_size, color = year_category), shape = 16, alpha = 0.7, width = 0.2) +
  scale_size_manual(
    name = "Total Community Members", 
    values = c("0-10" = 2, "10-50" = 4, "50-100" = 6, "100-200" = 8, "200-400" = 10)
  ) +
  scale_color_manual(values = custom_colors, name = "Year Founded Category", breaks = c("Before 1960", "1960-1970", "1970-1980", "1981-1990", "1991-2000", "2001-2010", "2011-2020", "2021 onwards")) +
  scale_x_discrete(labels = updated_labels) +  # Correct labels on x-axis
  theme_minimal() +
  labs(
    x = "",
    y = "Years in Existence"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    legend.position = "right"
  )




