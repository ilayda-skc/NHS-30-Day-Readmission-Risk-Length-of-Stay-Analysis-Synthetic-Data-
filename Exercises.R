library(tidyverse)
library(lubridate)

########################
###### Exercise 1 ######
########################

# Load data
# df <- read_csv("synthetic_data.csv")

# Data quality report function
data_quality_report <- function(data) {
  
  # Convert dates
  data_clean <- data %>% # Data goes through the mutate function (i.e. %>%)
    mutate(
      admission_date = dmy(admission_date),
      discharge_date = dmy(discharge_date),
      los = as.numeric(discharge_date - admission_date) # Length of stay calculated
    )
  
  # 1. Impossible values
  impossible_values <- data_clean %>% # This is the improved data used again
    summarise(
      discharge_before_admission = sum(discharge_date < admission_date, na.rm = TRUE), # Incorrect dates
      negative_los = sum(los < 0, na.rm = TRUE), # Due to incorrect dates
      zero_los = sum(los == 0, na.rm = TRUE), # No in hospital stay
      age_negative = sum(age < 0, na.rm = TRUE), # Count how many patients with negative age, you can further improve this line by adding mutate function to create a column with flagged entries.
      age_over_120 = sum(age > 120, na.rm = TRUE) # Count how many patients are age over 120 
    )
  
  # 2. Missing data pattern
  missing_data <- data_clean %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
    mutate(missing_percent = round(missing_count / nrow(data) * 100, 1))
  
  # 3. Outlier detection (IQR method)
  outlier_summary <- data_clean %>%
    summarise(
      los_outliers = sum(los > (quantile(los, 0.75, na.rm = TRUE) + 1.5 * IQR(los, na.rm = TRUE)), na.rm = TRUE),
      age_outliers = sum(age > (quantile(age, 0.75, na.rm = TRUE) + 1.5 * IQR(age, na.rm = TRUE)), na.rm = TRUE),
      comorbidity_outliers = sum(num_comorbidities > 8, na.rm = TRUE)
    )
  
  list(
    impossible = impossible_values,
    missing = missing_data,
    outliers = outlier_summary,
    total_records = nrow(data)
  )
}

# Run report
quality_report <- data_quality_report(synthetic_data) # Call the function
print(quality_report)

# Create cleaned dataset for subsequent exercises
df_clean <- synthetic_data %>%
  mutate(
    admission_date = dmy(admission_date),
    discharge_date = dmy(discharge_date),
    los = as.numeric(discharge_date - admission_date)
  ) %>%
  filter(
    los > 0,
    !is.na(los),
    age > 0,
    age < 120
  ) %>%
  mutate(
    age_group = case_when(
      age < 40 ~ "18-40",
      age < 66 ~ "41-65",
      age < 81 ~ "66-80",
      TRUE ~ "80+"
    ),
    comorbidity_group = case_when(
      num_comorbidities == 0 ~ "0",
      num_comorbidities <= 2 ~ "1-2",
      TRUE ~ "3+"
    )
  )

# Save cleaned data
write_csv(df_clean, "patient_admissions_clean.csv")

#######################################################
###### Optional Solutions to handle missing data ######
#######################################################

# Note: Change data_clean to df_clean

################### Option 1 ##

# Remove any row with ANY missing value
data_complete <- data_clean %>%
  na.omit()

# You'll lose about: 80 + 70 + 50 = 200 missing instances
# But these are spread across different rows, so actual row loss is less
# Approximately 2-3% of your data (assuming 5000 total rows)

################### Option 2 ##

# Keep all data, but remove rows missing key variables
data_clean <- data_clean %>%
  filter(!is.na(num_comorbidities)) %>%
  filter(!is.na(previous_emergency_admissions_12m)) %>%
  filter(!is.na(discharge_destination))


### OR

data_clean <- data_clean %>%
  filter(if_all(c(num_comorbidities,
                  previous_emergency_admissions_12m,
                  discharge_destination),
                ~ !is.na(.x)))

### OR

data_clean <- data_clean %>%
  filter(
    !is.na(num_comorbidities) &
      !is.na(previous_emergency_admissions_12m) &
      !is.na(discharge_destination)
  )


### OR

data_clean <- data_clean %>%
  filter(
    !is.na(num_comorbidities),
    !is.na(previous_emergency_admissions_12m),
    !is.na(discharge_destination)
  )


# You'll lose exactly 200 total rows across all three

################### Option 3 ##

# For numeric variables: fill with median
data_clean <- data_clean %>%
  mutate(
    num_comorbidities = ifelse(is.na(num_comorbidities), 
                               median(num_comorbidities, na.rm = TRUE), 
                               num_comorbidities),
    previous_emergency_admissions_12m = ifelse(is.na(previous_emergency_admissions_12m),
                                               median(previous_emergency_admissions_12m, na.rm = TRUE),
                                               previous_emergency_admissions_12m)
  )

# For categorical variables: fill with mode (most common value)
mode_discharge <- names(sort(table(data_clean$discharge_destination), decreasing = TRUE))[1]
data_clean <- data_clean %>%
  mutate(discharge_destination = ifelse(is.na(discharge_destination),
                                        mode_discharge,
                                        discharge_destination))

################### Option 4 ##

# For discharge destination (likely categorical)
data_clean <- data_clean %>%
  mutate(
    discharge_destination = ifelse(is.na(discharge_destination),
                                   "Unknown",
                                   discharge_destination)
  )

##############################
## Visualising Missing Data ##
##############################

# See which rows have missing data together
library(naniar)

# Visualise missing pattern
gg_miss_upset(df_clean)  # Shows combinations of missing values

# Or a simple summary
miss_var_summary(df_clean)  # Similar to your output

# Which rows have multiple missing values?
df_clean %>%
  filter(is.na(num_comorbidities) | 
           is.na(previous_emergency_admissions_12m) | 
           is.na(discharge_destination)) %>%
  select(patient_id, num_comorbidities, previous_emergency_admissions_12m, discharge_destination) %>%
  head(10)

###############################################################################
###### Exercise 2 ######
# Context: The Trust wants to understand which patient groups drive long stays.
###############################################################################

# Tasks:
# 1. Calculate median LOS by primary_diagnosis and gender
# 2. Create a boxplot of LOS by diagnosis (log scale)
# 3. Identify top 10 patients with longest LOS and their characteristics

# 1. Median LOS by diagnosis and gender
los_summary <- df_clean %>%
  group_by(primary_diagnosis, gender) %>%
  summarise(
    median_los = median(los, na.rm = TRUE),
    mean_los = round(mean(los, na.rm = TRUE), 1),
    iqr_los = IQR(los, na.rm = TRUE),
    n_patients = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(median_los))

print("Median LOS by Diagnosis and Gender:")
print(los_summary)

# 2. Boxplot of LOS by diagnosis (log scale)
los_boxplot <- df_clean %>%
  ggplot(aes(x = reorder(primary_diagnosis, los, FUN = median), y = los, fill = primary_diagnosis)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Length of Stay Distribution by Primary Diagnosis",
    subtitle = "Log scale - Boxplots show median, IQR, and outliers",
    x = "Primary Diagnosis (ICD-10)",
    y = "Length of Stay (days, log scale)",
    caption = "Source: Synthetic NHS Admissions Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip()

print(los_boxplot)

# 3. Top 10 longest stays
top_10_long_stays <- df_clean %>%
  arrange(desc(los)) %>%
  select(patient_id, age, gender, primary_diagnosis, los, num_comorbidities, discharge_destination) %>%
  head(10)

print("Top 10 Longest Hospital Stays:")
print(top_10_long_stays)

# Answer specific questions
long_stay_summary <- df_clean %>%
  summarise(
    highest_median_los_diagnosis = names(sort(tapply(los, primary_diagnosis, median), decreasing = TRUE)[1]),
    median_los_heart_failure = median(los[primary_diagnosis == "I50"], na.rm = TRUE),
    median_los_head_injury = median(los[primary_diagnosis == "S06"], na.rm = TRUE),
    pct_los_gt_7days = round(mean(los > 7, na.rm = TRUE) * 100, 1),
    gender_los_diff_heart_failure = median(los[primary_diagnosis == "I50" & gender == "F"], na.rm = TRUE) -
      median(los[primary_diagnosis == "I50" & gender == "M"], na.rm = TRUE)
  )

print("Key Findings:")
print(long_stay_summary)

####
# Which diagnosis has the highest median LOS?
# GI Bleed 
# Is there a gender difference in LOS for heart failure (I50)?
# No
# What percentage of patients have LOS > 7 days?
# 13.7 %
#### 

###############################################################################
###### Exercise 3 ######
# Context: Build a profile of patients at high risk of 30-day readmission.
###############################################################################

# Tasks:
# 1. Calculate readmission rate by:
#    - Age group (18-40, 41-65, 66-80, 80+)
#    - Discharge destination
#    - Number of comorbidities (0, 1-2, 3+)
#
# 2. Create a faceted bar chart showing readmission rates
# 3. Run a logistic regression to identify significant predictors

# Your code here:

# Check for any value greater than 1
any(df_clean$readmitted_30d > 1)

# 1. Readmission rates by different groups
# By age group
readmit_age <- df_clean %>%
  group_by(age_group) %>%
  summarise(
    readmission_rate = round(mean(readmitted_30d) * 100, 1),
    n_patients = n(),
    .groups = "drop"
  )

# By discharge destination
readmit_dest <- df_clean %>%
  group_by(discharge_destination) %>%
  summarise(
    readmission_rate = round(mean(readmitted_30d, na.rm = TRUE) * 100, 1),
    n_patients = n(),
    .groups = "drop"
  )

# 1. Readmission rates by different groups
# By age group
readmit_age <- df_clean %>%
  group_by(age_group) %>%
  summarise(
    readmission_rate = round(mean(readmitted_30d, na.rm = TRUE) * 100, 1),
    n_patients = n(),
    .groups = "drop"
  )

# By discharge destination
readmit_dest <- df_clean %>%
  group_by(discharge_destination) %>%
  summarise(
    readmission_rate = round(mean(readmitted_30d, na.rm = TRUE) * 100, 1),
    n_patients = n(),
    .groups = "drop"
  )

# By comorbidity group
readmit_comorbid <- df_clean %>%
  group_by(comorbidity_group) %>%
  summarise(
    readmission_rate = round(mean(readmitted_30d, na.rm = TRUE) * 100, 1),
    n_patients = n(),
    .groups = "drop"
  )

print("Readmission Rates by Age Group:")
print(readmit_age)
print("\nReadmission Rates by Discharge Destination:")
print(readmit_dest)
print("\nReadmission Rates by Comorbidity Count:")
print(readmit_comorbid)

# 2. Faceted bar chart
risk_factors_plot <- df_clean %>%
  pivot_longer(cols = c(age_group, discharge_destination, comorbidity_group),
               names_to = "risk_factor", values_to = "category") %>%
  group_by(risk_factor, category) %>%
  summarise(
    readmission_rate = mean(readmitted_30d, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(category)) %>%
  ggplot(aes(x = category, y = readmission_rate, fill = risk_factor)) +
  geom_col(alpha = 0.8, position = position_dodge()) +
  geom_text(aes(label = paste0(round(readmission_rate, 1), "%")), 
            vjust = -0.5, size = 3) +
  facet_wrap(~risk_factor, scales = "free_x") +
  labs(
    title = "30-Day Readmission Rates by Risk Factor",
    subtitle = "Age, discharge destination, and comorbidity burden all show strong associations",
    x = "Category",
    y = "Readmission Rate (%)",
    caption = "Error bars show 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(risk_factors_plot)

# 3. Logistic regression
logistic_model <- glm(readmitted_30d ~ age + los + num_comorbidities + 
                        previous_emergency_admissions_12m + gender + 
                        discharge_destination + primary_diagnosis,
                      data = df_clean, family = binomial())

summary(logistic_model)

# Odds ratios with confidence intervals
odds_ratios <- exp(cbind(OR = coef(logistic_model), confint(logistic_model)))
print("Odds Ratios for Readmission:")
print(round(odds_ratios, 2))

# Most significant predictors (p < 0.05)
significant_predictors <- summary(logistic_model)$coefficients %>%
  as.data.frame() %>%
  filter(`Pr(>|z|)` < 0.05) %>%
  arrange(`Pr(>|z|)`)

print("\nMost Significant Predictors (p < 0.05):")
print(significant_predictors)

  

