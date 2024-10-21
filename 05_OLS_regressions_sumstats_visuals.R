
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
#install.packages("car")



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
library(knitr)
library(rmarkdown)
library(broom)
library(extrafont)

options(scipen=999)


######
######## SECTION 0 - Read in and Prepare Dataset

filt_ab <- read_csv( "PATH/data/built/regression_build_ic.csv")

# Prepare data for regression

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
                                   0))

# Factor Variables that we can make sure that they are in a factor format
variables <- c(
  "STRUCTURE_POWER_decision_council_sociocracy_FACTOR", 
  "STRUCTURE_POWER_decision_consensus_FACTOR", 
  "STRUCTURE_POWER_decision_democracy_majority_FACTOR",
  "STRUCTURE_POWER_governance_core_group_FACTOR", 
  "STRUCTURE_POWER_governance_one_leader_FACTOR", 
  "COLLECTIVITY_income_sharing_some_or_all_FACTOR", 
  "COLLECTIVITY_labor_expectation_FACTOR", 
  "COLLECTIVITY_fees_reoccuring_FACTOR", 
  "MORAL_CULTURE_diet_vegan_veg_FACTOR", 
  "MORAL_CULTURE_no_alc_FACTOR", 
  "MORAL_CULTURE_relig_spirit_or_no_FACTOR", 
  "MORAL_CULTURE_relig_non_sec_spirituality_FACTOR", 
  "COLLECTIVITY_land_individual_FACTOR", 
  "COLLECTIVITY_land_community_FACTOR", 
  "DEMOG_urban_flag_FACTOR",
  "DEMOG_gender_homog"
)

# Ensure that they are seen as factors
reg_filtered <- reg_filtered %>%
  mutate(across(all_of(variables), ~ factor(.x, levels = c(0, 1, NA))))

# Adjust Acrage variable so that it fits in with regression variables 
reg_filtered <- reg_filtered %>%
  mutate(DEMOG_log_transformed_acreage_of_community = log1p(DEMOG_acreage_of_community)) 


######
######## SECTION 1 - Complete Individual Regression Models for Each Category (1-4)
######
######
######


############# 
# (D1) Prepare STRUCTURE_POWER VARIABLES for REGRESSION
formula_d1 <- yrs_in_existance ~ 
  STRUCTURE_POWER_governance_core_group_FACTOR +
  STRUCTURE_POWER_governance_one_leader_FACTOR +
  STRUCTURE_POWER_decision_council_sociocracy_FACTOR +
  STRUCTURE_POWER_decision_democracy_majority_FACTOR +
  ADJUSTMENT_centered_year

model_d1 <- lm(formula_d1, data = reg_filtered, weights = DEMOG_mbrsp_total_members_cleaned)
summary(model_d1)

# (D2) Prepare MORAL_CULTURE_ VARIABLES for REGRESSION
formula_d2 <- yrs_in_existance ~ 
  MORAL_CULTURE_diet_vegan_veg_FACTOR +
  MORAL_CULTURE_no_alc_FACTOR +
  MORAL_CULTURE_relig_spirit_or_no_FACTOR +
  ADJUSTMENT_centered_year

model_d2 <- lm(formula_d2, data = reg_filtered, weights = DEMOG_mbrsp_total_members_cleaned)
summary(model_d2)

# (D3) Prepare COLLECTIVITY variables for REGRESSION
formula_d3 <- yrs_in_existance ~ 
  COLLECTIVITY_land_individual_FACTOR +
  COLLECTIVITY_land_community_FACTOR +
  COLLECTIVITY_income_sharing_some_or_all_FACTOR +
  COLLECTIVITY_labor_expectation_FACTOR + 
  COLLECTIVITY_fees_reoccuring_FACTOR +
  ADJUSTMENT_centered_year

model_d3 <- lm(formula_d3, data = reg_filtered, weights = DEMOG_mbrsp_total_members_cleaned)
summary(model_d3)

# (D4) Prepare DEMOG VARIABLES for REGRESSION
formula_d4 <- yrs_in_existance ~ 
  DEMOG_urban_flag_FACTOR + 
  DEMOG_mbrsp_total_members_cleaned + 
  DEMOG_log_transformed_acreage_of_community + 
  DEMOG_gender_homog +
  ADJUSTMENT_centered_year

model_d4 <- lm(formula_d4, data = reg_filtered, weights = DEMOG_mbrsp_total_members_cleaned)
summary(model_d4)

######
## Loop for Printing D1-D4 and associated Tests
output_directory <- "PATH/exports/regressions/"

# Define function to extract model details
extract_model_details <- function(model, model_name) {
  model_summary <- summary(model)
  r_squared <- model_summary$adj.r.squared 
  coefficients <- model_summary$coefficients
  coef_table <- data.frame(
    Variable = rownames(coefficients),
    Coefficient = coefficients[, "Estimate"],
    Std.Error = coefficients[, "Std. Error"],
    t.value = coefficients[, "t value"],
    p.value = coefficients[, "Pr(>|t|)"]
  )
  coef_table$Significance <- symnum(coef_table$p.value, corr = FALSE, na = FALSE,
                                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                    symbols = c("***", "**", "*", ".", " "))
  list(model_name = model_name, r_squared = r_squared, coef_table = coef_table)
}

# Extract details from each model
model_details <- list(
  extract_model_details(model_d1, "STRUCTURE_POWER VARIABLES"),
  extract_model_details(model_d2, "MORAL_CULTURE_ VARIABLES"),
  extract_model_details(model_d3, "COLLECTIVITY VARIABLES"),
  extract_model_details(model_d4, "DEMOG VARIABLES")
)

# Create a summary table
summary_table <- data.frame(
  Model = character(),
  Variable = character(),
  Coefficient = numeric(),
  Std.Error = numeric(),
  t.value = numeric(),
  p.value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

for (details in model_details) {
  model_name <- details$model_name
  r_squared <- details$r_squared
  coef_table <- details$coef_table
  coef_table <- cbind(Model = model_name, coef_table)
  coef_table$R_squared <- c(r_squared, rep(NA, nrow(coef_table) - 1))
  summary_table <- rbind(summary_table, coef_table)
}

# Print the summary table
print(summary_table)

write_csv(summary_table, "PATH/exports/regressions/summary_table_d1_d4.csv"
)

# Install necessary packages if not already installed
if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
library(lmtest)

# Function to perform White test
perform_white_test <- function(model, model_name) {
  cat("\nWhite Test for", model_name, ":\n", sep = "")
  
  # Extract the model matrix (excluding the intercept)
  X <- model.matrix(model)[, -1]
  
  # Create the squared and interaction terms
  X_squared <- X^2
  interaction_terms <- combn(ncol(X), 2, function(idx) X[, idx[1]] * X[, idx[2]])
  
  # Combine the original, squared, and interaction terms
  X_all <- cbind(X, X_squared, interaction_terms)
  
  # Run the auxiliary regression of squared residuals on all terms
  aux_model <- lm(resid(model)^2 ~ X_all)
  
  # Perform the White test using the auxiliary regression
  white_test_stat <- summary(aux_model)$r.squared * nrow(X)
  p_value <- 1 - pchisq(white_test_stat, df = ncol(X_all))
  
  # Display the results
  cat("White test statistic:", white_test_stat, "\n")
  cat("p-value:", p_value, "\n")
}

# Run the White test for each model
perform_white_test(model_d1, "STRUCTURE_POWER VARIABLES")
perform_white_test(model_d2, "MORAL_CULTURE_ VARIABLES")
perform_white_test(model_d3, "COLLECTIVITY VARIABLES")
perform_white_test(model_d4, "DEMOG VARIABLES")



######
######## SECTION 2 - Complete FULL Regression Model
######
######
######

# Define the regression formula with the specified variables
formula <- yrs_in_existance ~ 
  ADJUSTMENT_centered_year +
  MORAL_CULTURE_diet_vegan_veg_FACTOR +
  MORAL_CULTURE_no_alc_FACTOR +
  MORAL_CULTURE_relig_spirit_or_no_FACTOR +
  COLLECTIVITY_land_community_FACTOR +
  COLLECTIVITY_land_individual_FACTOR +
  COLLECTIVITY_income_sharing_some_or_all_FACTOR +
  COLLECTIVITY_labor_expectation_FACTOR + 
  COLLECTIVITY_fees_reoccuring_FACTOR +
  STRUCTURE_POWER_governance_core_group_FACTOR +
  STRUCTURE_POWER_governance_one_leader_FACTOR +
  STRUCTURE_POWER_decision_council_sociocracy_FACTOR +
  STRUCTURE_POWER_decision_democracy_majority_FACTOR +
  DEMOG_mbrsp_total_members_cleaned + 
  DEMOG_urban_flag_FACTOR +  
  DEMOG_log_transformed_acreage_of_community + 
  DEMOG_gender_homog
  

# Run the linear regression model, weighting model by size (larger IC have more weight in model)
model_full <- lm(formula, data = reg_filtered, weights = DEMOG_mbrsp_total_members_cleaned)

summary(model_full)

##MAKE BOX PLOTS FOR COEFFIENCTS
# Extract the summary of the model using broom::tidy
library(broom)
library(dplyr)
library(ggplot2)

# Extract the summary of the model using broom::tidy
model_summary <- tidy(model_full)

# Add the subgroup information
model_summary$Subgroup <- case_when(
  str_detect(model_summary$term, "MORAL_CULTURE_") ~ "Moral Culture",
  str_detect(model_summary$term, "COLLECTIVITY") ~ "Collectivity",
  str_detect(model_summary$term, "STRUCTURE_POWER") ~ "Structure & Power",
  str_detect(model_summary$term, "DEMOG") ~ "Demography & Geography",
  TRUE ~ "Other"
)

# Define a named vector for beautified names
beautified_names <- c(
  "MORAL_CULTURE_diet_vegan_veg_FACTOR1" = "Vegetarian or vegan diet",
  "MORAL_CULTURE_no_alc_FACTOR1" = "Prohibition on alcohol",
  "MORAL_CULTURE_relig_spirit_or_no_FACTOR1" = "Religious or spiritual",
  "STRUCTURE_POWER_decision_council_sociocracy_FACTOR1" = "Sociocracy",
  "STRUCTURE_POWER_governance_core_group_FACTOR1" = "Core group of leaders",
  "STRUCTURE_POWER_governance_one_leader_FACTOR1" = "One leader",
  "STRUCTURE_POWER_decision_democracy_majority_FACTOR1" = "Democracy",
  "COLLECTIVITY_land_community_FACTOR1" = "Land owned collectively",
  "COLLECTIVITY_land_individual_FACTOR1" = "Land owned individually",
  "COLLECTIVITY_income_sharing_some_or_all_FACTOR1" = "Some or all income shared",
  "COLLECTIVITY_fees_reoccuring_FACTOR1" = "Reoccurring fees imposed",
  "COLLECTIVITY_labor_expectation_FACTOR1" = "Labor expectation",
  "DEMOG_urban_flag_FACTOR1" = "Urban location",
  "DEMOG_gender_homog1" = "Gender Homogenous"
)

# Define a function to beautify names
beautify_name <- function(name) {
  if (name %in% names(beautified_names)) {
    return(beautified_names[[name]])
  } else {
    return(name)
  }
}

# Filter significant variables (p.value < 0.05) and exclude members and acreage
significant_vars <- model_summary %>%
  filter(p.value < 0.05) %>%
  filter(!term %in% c("(Intercept)", "ADJUSTMENT_centered_year", "DEMOG_mbrsp_total_members_cleaned", "DEMOG_log_transformed_acreage_of_community"))

# Beautify the term names
significant_vars$term <- sapply(significant_vars$term, beautify_name)

# Create a coefficient plot
ggplot(significant_vars, aes(x = estimate, y = reorder(term, estimate), color = Subgroup)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0.2) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("Moral Culture" = "#1b9e77", "Collectivity" = "#d95f02", "Structure & Power" = "#7570b3", "Demography & Geography" = "#e7298a")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.5),  # Add horizontal grid lines
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "right",
    legend.title = element_blank(),
    axis.line.x = element_line(color = "gray"),  # Add axis lines
    axis.line.y = element_line(color = "gray"),  # Add axis lines
    axis.ticks.x = element_line(color = "gray"),  # Add axis ticks
    axis.ticks.y = element_line(color = "gray")   # Add axis ticks
  ) +
  labs(x = "Estimated Effect on Resiliency (Years)")


#############
##Extract Model Details and Export
#############

  # Define function to extract model details
  extract_model_details <- function(model, model_name) {
    model_summary <- summary(model)
    r_squared <- model_summary$adj.r.squared 
    coefficients <- model_summary$coefficients
    coef_table <- data.frame(
      Variable = rownames(coefficients),
      Coefficient = coefficients[, "Estimate"],
      Std.Error = coefficients[, "Std. Error"],
      t.value = coefficients[, "t value"],
      p.value = coefficients[, "Pr(>|t|)"]
    )
    coef_table$Significance <- symnum(coef_table$p.value, corr = FALSE, na = FALSE,
                                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                      symbols = c("***", "**", "*", ".", " "))
    list(model_name = model_name, r_squared = r_squared, coef_table = coef_table)
  }
  
  # Extract details from the full model
  model_details <- list(
    extract_model_details(model_full, "FULL MODEL")
  )
  
  # Create a summary table
  summary_table <- data.frame(
    Model = character(),
    Variable = character(),
    Coefficient = numeric(),
    Std.Error = numeric(),
    t.value = numeric(),
    p.value = numeric(),
    Significance = character(),
    stringsAsFactors = FALSE
  )
  
  for (details in model_details) {
    model_name <- details$model_name
    r_squared <- details$r_squared
    coef_table <- details$coef_table
    coef_table <- cbind(Model = model_name, coef_table)
    coef_table$R_squared <- c(r_squared, rep(NA, nrow(coef_table) - 1))
    summary_table <- rbind(summary_table, coef_table)
  }
  
  # Print the summary table
  print(summary_table)
  
  write_csv(summary_table, "PATH/exports/regressions/summary_table_full_model.csv")
  
  

  
  ############
  ######## 3. Regression Model Error / Validity Testing
  ############
library(car)
  
  #colinearity
  vif_values <- vif(model_full)
  print(vif_values)
  
  # Calculate AIC and BIC for Model
  AIC(model_full)
  BIC(model_full)
  
  # Calculate the studentized residuals
  studentized_residuals <- rstudent(model_full)
  
  # Residual plots using studentized residuals
  par(mfrow = c(2, 2))
  
  # Residuals vs Fitted
  plot(fitted(model_full), studentized_residuals, 
       xlab = "Fitted values", 
       ylab = "Studentized Residuals", 
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red", lty = 2)
  # Add a green trend line
  lines(lowess(fitted(model_full), studentized_residuals), col = "green")
  
  # Q-Q plot
  qqnorm(studentized_residuals, main = "Q-Q Plot")
  qqline(studentized_residuals, col = "red", lty = 2)
  
  # Scale-Location Plot
  plot(fitted(model_full), abs(studentized_residuals)^(1/2), 
       xlab = "Fitted values", 
       ylab = "Square Root of Standardized Residuals", 
       main = "Scale-Location")
  abline(h = 0, col = "red", lty = 2)
  # Add a green trend line
  lines(lowess(fitted(model_full), abs(studentized_residuals)^(1/2)), col = "green")
  
  # Residuals vs Leverage
  plot(hatvalues(model_full), studentized_residuals, 
       xlab = "Leverage", 
       ylab = "Studentized Residuals", 
       main = "Residuals vs Leverage")
  abline(h = 0, col = "red", lty = 2)
  abline(v = 2 * mean(hatvalues(model_full)), col = "blue", lty = 2)
  abline(h = c(-2, 2), col = "blue", lty = 2)
  # Add a green trend line
  lines(lowess(hatvalues(model_full), studentized_residuals), col = "green")
  
  # Install necessary packages if not already installed
  if (!require("lmtest")) install.packages("lmtest", dependencies = TRUE)
  
  # Complete the White Test for Heteroskedasticity 
  
  # Extract the model matrix (excluding the intercept)
  X <- model.matrix(model_full)[, -1]
  
  # Create the squared and interaction terms
  X_squared <- X^2
  interaction_terms <- combn(ncol(X), 2, function(idx) X[, idx[1]] * X[, idx[2]])
  
  # Combine the original, squared, and interaction terms
  X_all <- cbind(X, X_squared, interaction_terms)
  
  # Run the auxiliary regression of squared residuals on all terms
  aux_model <- lm(resid(model_full)^2 ~ X_all)
  
  # Perform the White test using the auxiliary regression
  white_test_stat <- summary(aux_model)$r.squared * nrow(X)
  p_value <- 1 - pchisq(white_test_stat, df = ncol(X_all))
  
  # Display the results
  cat("White test statistic:", white_test_stat, "\n")
  cat("p-value:", p_value, "\n")
  
  ############
  ######## 4. Accuracy Checks & Summary Stats
  ############
  
  # Create a filtered dataset with no missing values for the variables used in the model 
  only_reg_obs <- reg_filtered %>%
    mutate(across(
      c(MORAL_CULTURE_diet_vegan_veg_FACTOR, 
        MORAL_CULTURE_no_alc_FACTOR, 
        MORAL_CULTURE_relig_spirit_or_no_FACTOR, 
        COLLECTIVITY_land_community_FACTOR, 
        COLLECTIVITY_land_individual_FACTOR, 
        COLLECTIVITY_income_sharing_some_or_all_FACTOR, 
        COLLECTIVITY_labor_expectation_FACTOR, 
        STRUCTURE_POWER_governance_core_group_FACTOR, 
        STRUCTURE_POWER_governance_one_leader_FACTOR, 
        STRUCTURE_POWER_decision_council_sociocracy_FACTOR, 
        STRUCTURE_POWER_decision_democracy_majority_FACTOR, 
        COLLECTIVITY_fees_reoccuring_FACTOR, 
        DEMOG_urban_flag_FACTOR), 
      ~ as.numeric(as.character(.)))) %>%
    filter(
      !is.na(ADJUSTMENT_centered_year) &
        !is.na(MORAL_CULTURE_diet_vegan_veg_FACTOR) &
        !is.na(MORAL_CULTURE_no_alc_FACTOR) &
        !is.na(MORAL_CULTURE_relig_spirit_or_no_FACTOR) &
        !is.na(COLLECTIVITY_land_community_FACTOR) &
        !is.na(COLLECTIVITY_land_individual_FACTOR) &
        !is.na(COLLECTIVITY_income_sharing_some_or_all_FACTOR) &
        !is.na(COLLECTIVITY_labor_expectation_FACTOR) &
        !is.na(STRUCTURE_POWER_governance_core_group_FACTOR) &
        !is.na(STRUCTURE_POWER_governance_one_leader_FACTOR) &
        !is.na(STRUCTURE_POWER_decision_council_sociocracy_FACTOR) &
        !is.na(STRUCTURE_POWER_decision_democracy_majority_FACTOR) &
        !is.na(COLLECTIVITY_fees_reoccuring_FACTOR) &
        !is.na(DEMOG_mbrsp_total_members_cleaned) &
        !is.na(DEMOG_urban_flag_FACTOR) &
        !is.na(DEMOG_log_transformed_acreage_of_community)  &
        !is.na(DEMOG_gender_homog) & 
        !is.na(yrs_in_existance)
    ) 
  
  df_for_reg_stats <- filt_ab %>% 
    #filter observations to those used in the regression model
    filter(ic_name_for_join %in% only_reg_obs$ic_name_for_join) %>%
    mutate(DEMOG_gender_homog = ifelse(abs(0.5 - DEMOG_mbrsp_percent_men >= 0.17), 
                                     1, 
                                     0)) %>%
    mutate(gen_year_formed = ifelse(is.na(gen_year_formed), 2024 - yrs_in_existance, gen_year_formed))

  
  #export this dataframe 
  write_csv(df_for_reg_stats, 
            "PATH/data/built/df_of_all_IC_in_regression_model.csv")
  
  
  
  # Add a column for the time periods
  df_for_reg_stats <- df_for_reg_stats %>%
    mutate(gen_year_bucket = case_when(
      gen_year_established < 1960 ~ "Before 1960",
      gen_year_established >= 1960 & gen_year_established <= 1970 ~ "1960-1970",
      gen_year_established >= 1971 & gen_year_established <= 1980 ~ "1971-1980",
      gen_year_established >= 1981 & gen_year_established <= 1990 ~ "1981-1990",
      gen_year_established >= 1901 & gen_year_established <= 2000 ~ "1991-2000",
      gen_year_established >= 2001 & gen_year_established <= 2010 ~ "2001-2010",
      gen_year_established >= 2011 & gen_year_established <= 2020 ~ "2011-2020",
      gen_year_established >= 2021 & gen_year_established <= 2024 ~ "2021-2024",
      TRUE ~ "Unknown"
    ))
  
  # Output dataset summary statistics for dep/indep variables
  df_for_quant_reg_stats<- df_for_reg_stats %>%
    dplyr::select(
      yr_most_recently_scraped, 
      gen_year_established,
      yrs_in_existance,
      ADJUSTMENT_centered_year,
      MORAL_CULTURE_diet_vegan_veg_FACTOR,
      MORAL_CULTURE_no_alc_FACTOR,
      MORAL_CULTURE_relig_spirit_or_no_FACTOR,
      COLLECTIVITY_land_community_FACTOR,
      COLLECTIVITY_land_individual_FACTOR,
      COLLECTIVITY_income_sharing_some_or_all_FACTOR,
      COLLECTIVITY_labor_expectation_FACTOR,
      STRUCTURE_POWER_governance_core_group_FACTOR,
      STRUCTURE_POWER_governance_one_leader_FACTOR,
      STRUCTURE_POWER_decision_council_sociocracy_FACTOR,
      STRUCTURE_POWER_decision_democracy_majority_FACTOR,
      COLLECTIVITY_fees_reoccuring_FACTOR,
      DEMOG_mbrsp_total_members_cleaned,
      DEMOG_urban_flag_FACTOR,
      DEMOG_acreage_of_community,
      DEMOG_gender_homog
    ) 
  

            
  # Calculate means, medians, standard deviations, and number of observations
  summary_stats <- df_for_quant_reg_stats %>%
    summarise_all(list(mean = mean, median = median, sd = sd, num_obs = ~sum(!is.na(.)), sum = sum), na.rm = TRUE) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    separate(variable, into = c("variable", "stat"), sep = "_(?=mean|median|sd|num_obs|sum)") %>%
    pivot_wider(names_from = stat, values_from = value)
  
  # Output dataset summary statistics for dep/indep variables
  df_for_qual_reg_stats<- df_for_reg_stats %>%
    dplyr::select(
      yr_most_recently_scraped, 
      gen_year_established,
      gen_year_bucket,
      yrs_in_existance,
      ADJUSTMENT_centered_year,
      MORAL_CULTURE_diet_vegan_veg_FACTOR,
      MORAL_CULTURE_no_alc_FACTOR,
      MORAL_CULTURE_relig_spirit_or_no_FACTOR,
      COLLECTIVITY_land_community_FACTOR,
      COLLECTIVITY_land_individual_FACTOR,
      COLLECTIVITY_income_sharing_some_or_all_FACTOR,
      COLLECTIVITY_labor_expectation_FACTOR,
      STRUCTURE_POWER_governance_core_group_FACTOR,
      STRUCTURE_POWER_governance_one_leader_FACTOR,
      STRUCTURE_POWER_decision_council_sociocracy_FACTOR,
      STRUCTURE_POWER_decision_democracy_majority_FACTOR,
      COLLECTIVITY_fees_reoccuring_FACTOR,
      DEMOG_mbrsp_total_members_cleaned,
      DEMOG_urban_flag_FACTOR,
      DEMOG_acreage_of_community,
      DEMOG_gender_homog
    ) 
  
  print(summary_stats)
  
  write_csv(summary_stats, "PATH/exports/regressions/sum_stats_IC_in_reg_model.csv")
  
  #### Create Bar Chart
  # Filter for indicator variables only
  indicator_vars <- summary_stats %>%
    filter(variable %in% c(
      "MORAL_CULTURE_diet_vegan_veg_FACTOR",
      "MORAL_CULTURE_no_alc_FACTOR",
      "MORAL_CULTURE_relig_spirit_or_no_FACTOR",
      "COLLECTIVITY_land_community_FACTOR",
      "COLLECTIVITY_land_individual_FACTOR",
      "COLLECTIVITY_income_sharing_some_or_all_FACTOR",
      "COLLECTIVITY_labor_expectation_FACTOR",
      "STRUCTURE_POWER_governance_core_group_FACTOR",
      "STRUCTURE_POWER_governance_one_leader_FACTOR",
      "STRUCTURE_POWER_decision_council_sociocracy_FACTOR",
      "STRUCTURE_POWER_decision_democracy_majority_FACTOR",
      "COLLECTIVITY_fees_reoccuring_FACTOR",
      "DEMOG_urban_flag_FACTOR",
      "DEMOG_gender_homog"
    ))
  
  # Beautify the names for the plot
  beautified_names <- c(
    "MORAL_CULTURE_diet_vegan_veg_FACTOR" = "Vegetarian or vegan diet",
    "MORAL_CULTURE_no_alc_FACTOR" = "Prohibition on alcohol",
    "MORAL_CULTURE_relig_spirit_or_no_FACTOR" = "Religious or spiritual",
    "STRUCTURE_POWER_decision_council_sociocracy_FACTOR" = "Sociocracy",
    "STRUCTURE_POWER_governance_core_group_FACTOR" = "Core group of leaders",
    "STRUCTURE_POWER_governance_one_leader_FACTOR" = "One leader",
    "STRUCTURE_POWER_decision_democracy_majority_FACTOR" = "Democracy",
    "COLLECTIVITY_land_community_FACTOR" = "Land owned collectively",
    "COLLECTIVITY_land_individual_FACTOR" = "Land owned individually",
    "COLLECTIVITY_income_sharing_some_or_all_FACTOR" = "Some or all income shared",
    "COLLECTIVITY_fees_reoccuring_FACTOR" = "Reoccurring fees imposed",
    "COLLECTIVITY_labor_expectation_FACTOR" = "Labor expectation",
    "DEMOG_urban_flag_FACTOR" = "Urban location",
    "DEMOG_gender_homog" = "Gender Homogenous"
  )
  
  # Add the subgroup information
  indicator_vars$Subgroup <- case_when(
    indicator_vars$variable %in% c(
      "MORAL_CULTURE_diet_vegan_veg_FACTOR",
      "MORAL_CULTURE_no_alc_FACTOR",
      "MORAL_CULTURE_relig_spirit_or_no_FACTOR"
    ) ~ "Moral Culture",
    indicator_vars$variable %in% c(
      "COLLECTIVITY_land_community_FACTOR",
      "COLLECTIVITY_land_individual_FACTOR",
      "COLLECTIVITY_income_sharing_some_or_all_FACTOR",
      "COLLECTIVITY_fees_reoccuring_FACTOR",
      "COLLECTIVITY_labor_expectation_FACTOR"
    ) ~ "Collectivity",
    indicator_vars$variable %in% c(
      "STRUCTURE_POWER_governance_core_group_FACTOR",
      "STRUCTURE_POWER_governance_one_leader_FACTOR",
      "STRUCTURE_POWER_decision_council_sociocracy_FACTOR",
      "STRUCTURE_POWER_decision_democracy_majority_FACTOR"
    ) ~ "Structure & Power",
    indicator_vars$variable %in% c(
      "DEMOG_urban_flag_FACTOR",
      "DEMOG_gender_homog"
    ) ~ "Demography & Geography",
    TRUE ~ "Other"
  )
  
  # Beautify the term names
  indicator_vars$variable <- sapply(indicator_vars$variable, beautify_name)
  
  # Define colors for each subgroup
  colors <- c(
    "Moral Culture" = "#66c2a5",
    "Collectivity" = "#fc8d62",
    "Structure & Power" = "#8da0cb",
    "Demography & Geography" = "#e78ac3"
  )
  
  # Create the bar chart
  ggplot(indicator_vars, aes(x = mean * 100, y = reorder(variable, mean), fill = Subgroup)) +
    geom_bar(stat = "identity", show.legend = TRUE) +
    geom_text(aes(label = paste0(round(mean * 100, 1), "%")), hjust = -0.1, color = "black") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 12, color = "black"),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      panel.grid.major.y = element_line(color = "grey80"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "right", 
      text = element_text(family = "Times New Roman")
    ) +
    scale_fill_manual(values = colors) +
    labs(
      x = "% of all IC (N = 348)",
      y = NULL,
      fill = NULL
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100))
  
  #####
  ##### Acrage and Member Examples
  #####
  
  # Assuming you have already extracted and stored the model summary in a dataframe called model_summary
  
  # Extract the coefficients and standard errors for members and acres
  beta_members <- model_summary %>% 
    filter(term == "DEMOG_mbrsp_total_members_cleaned") %>% 
    pull(estimate)
  
  se_members <- model_summary %>% 
    filter(term == "DEMOG_mbrsp_total_members_cleaned") %>% 
    pull(std.error)
  
  beta_acres <- model_summary %>% 
    filter(term == "DEMOG_log_transformed_acreage_of_community") %>% 
    pull(estimate)
  
  se_acres <- model_summary %>% 
    filter(term == "DEMOG_log_transformed_acreage_of_community") %>% 
    pull(std.error)
  
  # Define the number of members and acres to add
  members_to_add <- c(10, 25, 50, 100, 200)
  acres_to_add <- c(10, 25, 50, 100, 200)
  
  # Starting conditions
  current_members <- 10
  current_acres <- 10
  
  # Calculate the effect of adding more members
  effect_members <- beta_members * members_to_add
  
  # Calculate the effect of adding more acres
  effect_acres <- beta_acres * (log1p(current_acres + acres_to_add) - log1p(current_acres))
  
  # Calculate the 95% confidence intervals
  ci_members <- 1.96 * se_members * members_to_add
  ci_acres <- 1.96 * se_acres * (log1p(current_acres + acres_to_add) - log1p(current_acres))
  
  # Create dataframes for members and acres
  results_members <- data.frame(
    Added = members_to_add,
    Effect_on_Years = effect_members,
    Lower_CI = effect_members - ci_members,
    Upper_CI = effect_members + ci_members
  )
  
  results_acres <- data.frame(
    Added = acres_to_add,
    Effect_on_Years = effect_acres,
    Lower_CI = effect_acres - ci_acres,
    Upper_CI = effect_acres + ci_acres
  )
  

  
  # Custom color palette with shades of blue
  blue_palette <- c("#1f77b4", "#6baed6", "#9ecae1", "#c6dbef")
  
  # Plot for members
  p_members <- ggplot(results_members, aes(x = Effect_on_Years, y = factor(Added))) +
    geom_errorbar(aes(xmin = Lower_CI, xmax = Upper_CI), width = 0.2, color = blue_palette[2]) +
    geom_point(size = 3, color = blue_palette[1]) +
    labs(x = "Effect on Years in Existence", y = "Members Added", title = "Effect of Adding Members on Years in Existence") +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
  
  # Plot for acres
  p_acres <- ggplot(results_acres, aes(x = Effect_on_Years, y = factor(Added))) +
    geom_errorbar(aes(xmin = Lower_CI, xmax = Upper_CI), width = 0.2, color = blue_palette[4]) +
    geom_point(size = 3, color = blue_palette[3]) +
    labs(x = "Effect on Years in Existence", y = "Acres Added", title = "Effect of Adding Acres on Years in Existence") +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
  
  # Print the plots
  print(p_members)
  print(p_acres)
  
  
  ############
  ######## 5. Year of Establishment Breakdown
  ############
  
  # Create a breakdown of communities by gen_year_established
  year_bucket_summary <- df_for_qual_reg_stats %>%
    group_by(gen_year_bucket) %>%
    summarise(num_communities = n(),
              num_members = sum(DEMOG_mbrsp_total_members_cleaned)) %>%
    ungroup() %>% 
    mutate(avg_member_per_community = num_members/num_communities)

  
  print(year_bucket_summary)
  
  write_csv(year_bucket_summary, "PATH/exports/regressions/yearly_details_IC_in_reg_model.csv")
  



