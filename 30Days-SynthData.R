
.libPaths()

find.package("pillar")
find.package("dplyr")

# Step 1: Set CRAN mirror (choose a fast one)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Step 2: Define required packages
required_packages <- c(
  "dplyr",
  "tidyr", 
  "lubridate",
  "ggplot2",
  "readr",
  "pillar",    # This was causing the error
  "tibble",
  "magrittr"   # For the %>% pipe operator
)

install.packages("pillar")

# Step 3: Check and install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    cat("Package already loaded:", pkg, "\n")
  }
}

# Step 4: Verify everything loaded correctly
cat("\n========================================\n")
cat("??? All packages loaded successfully!\n")
cat("========================================\n\n")

# Check versions
cat("Package versions:\n")
cat("  dplyr:", packageVersion("dplyr"), "\n")
cat("  ggplot2:", packageVersion("ggplot2"), "\n")
cat("  lubridate:", packageVersion("lubridate"), "\n")
cat("  pillar:", packageVersion("pillar"), "\n")

# Step 5: Test the pipe operator
test_pipe <- c(1, 2, 3) %>% mean()
cat("\n??? Pipe operator (%>%) working. Test result:", test_pipe, "\n\n")

# Step 6: Generate the synthetic dataset (run this separately or continue)
cat("Ready to generate synthetic dataset. Run the data generation script now.\n")


# Synthetic Patient Admissions Dataset for Edge Health UK Intern Exercise
# Simulates 5,000 emergency admissions with realistic patterns

library(tidyverse)
library(lubridate)

set.seed(2025)  # Reproducibility

# Number of patients (some may have multiple admissions, but for simplicity,
# we create unique admissions with patient_id repeated)
n_records <- 5000

# Generate patient IDs (some patients appear multiple times)
patient_ids <- sample(1:3500, n_records, replace = TRUE)

# Age distribution (skewed towards older adults - typical for emergency admissions)
age <- round(rbeta(n_records, 2, 5) * 90 + 10)  # Range 10-100, mean ~37
# But hospital admissions skew older - adjust for clinical reality
age <- ifelse(age < 65, age + rnorm(n_records, 15, 10), age)
age <- pmax(18, pmin(age, 105))  # Cap at 105, min 18

# Gender (slight female predominance in older age groups)
gender <- ifelse(age > 70, 
                 sample(c("M", "F"), n_records, replace = TRUE, prob = c(0.4, 0.6)),
                 sample(c("M", "F"), n_records, replace = TRUE, prob = c(0.48, 0.52)))

# Primary diagnosis (ICD-10 chapters - common emergency admission reasons)
diagnosis <- sample(
  c("I48", "I50", "J44", "J15", "N39", "K92", "R55", "S06"),
  n_records,
  replace = TRUE,
  prob = c(0.12, 0.10, 0.15, 0.13, 0.08, 0.10, 0.07, 0.25)  # Head injury common
)

# Diagnosis labels for readability (optional)
diagnosis_labels <- c(
  "I48" = "Atrial fibrillation",
  "I50" = "Heart failure",
  "J44" = "COPD",
  "J15" = "Pneumonia",
  "N39" = "UTI",
  "K92" = "GI bleed",
  "R55" = "Syncope",
  "S06" = "Head injury"
)

# Number of comorbidities (Elixhauser count)
num_comorbidities <- rpois(n_records, lambda = 1.5)
num_comorbidities <- pmin(num_comorbidities, 8)  # Cap at 8

# Previous emergency admissions in last 12 months (zero-inflated)
previous_emergency_admissions_12m <- rpois(n_records, lambda = 0.8)
previous_emergency_admissions_12m <- ifelse(runif(n_records) < 0.6, 0, previous_emergency_admissions_12m)
previous_emergency_admissions_12m <- pmin(previous_emergency_admissions_12m, 10)

# Admission dates (spread over 2 years)
admission_date <- as.Date("2023-01-01") + sample(0:730, n_records, replace = TRUE)

# Length of stay (days) - depends on diagnosis, age, comorbidities
base_los <- case_when(
  diagnosis %in% c("I50", "J15") ~ rlnorm(n_records, log(5), 0.6),   # Heart failure, pneumonia ~5-8 days
  diagnosis == "I48" ~ rlnorm(n_records, log(2), 0.5),               # AF ~2 days
  diagnosis == "J44" ~ rlnorm(n_records, log(4), 0.7),               # COPD ~4-6 days
  diagnosis == "N39" ~ rlnorm(n_records, log(3), 0.6),               # UTI ~3 days
  diagnosis == "K92" ~ rlnorm(n_records, log(6), 0.8),               # GI bleed ~6-8 days
  diagnosis == "R55" ~ rlnorm(n_records, log(1.5), 0.4),             # Syncope ~1-2 days
  diagnosis == "S06" ~ rlnorm(n_records, log(2), 0.7)                # Head injury ~2-3 days
)

# Adjust LOS for age and comorbidities
los <- base_los * (1 + (age - 70)/200) * (1 + num_comorbidities/15)
los <- round(pmax(1, los))  # Minimum 1 day, round to integer
los <- pmin(los, 60)  # Cap at 60 days (long stay outliers)

# Discharge date
discharge_date <- admission_date + los

# Discharge destination (depends on age and LOS)
discharge_destination <- case_when(
  age > 85 & los > 14 ~ sample(c("Home", "Care home", "Other hospital"), n_records, 
                               replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  age > 75 & los > 7 ~ sample(c("Home", "Care home", "Other hospital"), n_records,
                              replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  TRUE ~ sample(c("Home", "Care home", "Other hospital"), n_records,
                replace = TRUE, prob = c(0.85, 0.1, 0.05))
)

# 30-day readmission (target variable) - depends on multiple factors
# Higher risk: older, more comorbidities, prior admissions, longer LOS, care home discharge
risk_score <- 
  (age - 50) / 50 * 0.3 +
  num_comorbidities * 0.4 +
  previous_emergency_admissions_12m * 0.5 +
  (los - 3) / 10 * 0.2 +
  ifelse(discharge_destination == "Care home", 0.8, 0) +
  ifelse(discharge_destination == "Other hospital", 0.4, 0) +
  ifelse(diagnosis %in% c("I50", "J44"), 0.3, 0)

# Convert to probability (logistic function)
prob_readmit <- 1 / (1 + exp(-(risk_score - 1.2)))
readmitted_30d <- rbinom(n_records, 1, prob_readmit)

# Create final dataframe
synthetic_data <- data.frame(
  patient_id = patient_ids,
  age = age,
  gender = gender,
  admission_date = format(admission_date, "%d/%m/%Y"),
  discharge_date = format(discharge_date, "%d/%m/%Y"),
  primary_diagnosis = diagnosis,
  num_comorbidities = num_comorbidities,
  previous_emergency_admissions_12m = previous_emergency_admissions_12m,
  discharge_destination = discharge_destination,
  readmitted_30d = readmitted_30d
)

# Add some missing data (realistic NHS data has gaps)
missing_rows <- sample(1:n_records, 200)
synthetic_data$num_comorbidities[missing_rows[sample(1:200, 80)]] <- NA
synthetic_data$discharge_destination[missing_rows[sample(1:200, 50)]] <- NA
synthetic_data$previous_emergency_admissions_12m[missing_rows[sample(1:200, 70)]] <- NA

# Save to CSV
write.csv(synthetic_data, "patient_admissions.csv", row.names = FALSE)

# Validation summary
cat("\n=== Synthetic Dataset Generated ===\n")
cat("Records:", nrow(synthetic_data), "\n")
cat("Readmission rate:", round(mean(synthetic_data$readmitted_30d, na.rm = TRUE) * 100, 1), "%\n")
cat("Median LOS:", median(synthetic_data$los_calc <- as.numeric(difftime(
  as.Date(synthetic_data$discharge_date, format = "%d/%m/%Y"),
  as.Date(synthetic_data$admission_date, format = "%d/%m/%Y"),
  units = "days"
)), na.rm = TRUE), "days\n")

# Quick preview
print(head(synthetic_data))

# Optional: Check realistic patterns
synthetic_data_with_los <- synthetic_data %>%
  mutate(
    admission_date_d = dmy(admission_date),
    discharge_date_d = dmy(discharge_date),
    los = as.numeric(discharge_date_d - admission_date_d)
  )

cat("\n=== Mean LOS by Diagnosis ===\n")
print(synthetic_data_with_los %>%
        group_by(primary_diagnosis) %>%
        summarise(mean_los = round(mean(los, na.rm = TRUE), 1), n = n()) %>%
        arrange(desc(mean_los)))

cat("\n=== Readmission Rate by Discharge Destination ===\n")
print(synthetic_data_with_los %>%
        group_by(discharge_destination) %>%
        summarise(readmit_rate = round(mean(readmitted_30d, na.rm = TRUE) * 100, 1), n = n()))
