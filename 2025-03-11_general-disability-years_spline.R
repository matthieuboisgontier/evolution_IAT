# Load libraries
library(haven)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(effects)
library(mgcv)
library(ggplot2)
library(broom)
library(viridis)
library(patchwork)
library(visreg)
library(gratia)

# Load data
#data_iat_physdis_or1 <- read_sav(here("Disability_IAT.public.2022-2024.sav"))
#data_iat_gendis_or1 <- read_sav(here("Disability_IAT.public.2004-2021.sav"))
data_iat_2006 <- read_sav(here("Disability_IAT.public.2006.sav"))
data_iat_2007 <- read_sav(here("Disability_IAT.public.2007.sav"))
data_iat_2008 <- read_sav(here("Disability_IAT.public.2008.sav"))
data_iat_2009 <- read_sav(here("Disability_IAT.public.2009.sav"))
data_iat_2010 <- read_sav(here("Disability_IAT.public.2010.sav"))
data_iat_2011 <- read_sav(here("Disability_IAT.public.2011.sav"))
data_iat_2012 <- read_sav(here("Disability_IAT.public.2012.sav"))
data_iat_2013 <- read_sav(here("Disability_IAT.public.2013.sav"))
data_iat_2014 <- read_sav(here("Disability_IAT.public.2014.sav"))
data_iat_2015 <- read_sav(here("Disability_IAT.public.2015.sav"))
data_iat_2016 <- read_sav(here("Disability_IAT.public.2016.sav"))
data_iat_2017 <- read_sav(here("Disability_IAT.public.2017.sav"))
data_iat_2018 <- read.csv(here("Disability_IAT.public.2018.csv")) # sav: Unable to convert string to the requested encoding (invalid byte sequence).
data_iat_2019 <- read_sav(here("Disability_IAT.public.2019.sav"))
data_iat_2020 <- read_sav(here("Disability_IAT.public.2020.sav"))
data_iat_2021 <- read_sav(here("Disability_IAT.public.2021.sav"))
data_iat_2022 <- read_sav(here("Disability_IAT.public.2022.sav"))
data_iat_2023 <- read_sav(here("Disability_IAT.public.2023.sav"))
data_iat_2024 <- read_sav(here("Disability_IAT.public.2024.sav"))

# Loop to display the number of rows for each dataset from 2006 to 2024
# and compute the total number of rows across all datasets
total_rows <- 0
for (year in 2006:2024) {
  dataset_name <- paste0("data_iat_", year)
  if (exists(dataset_name)) {
    n_rows <- nrow(get(dataset_name))
    total_rows <- total_rows + n_rows
    cat("Number of rows in", dataset_name, ":", n_rows, "\n")
  } else {
    cat("Dataset", dataset_name, "does not exist.\n")
  }
}
cat("\nTotal number of rows across all datasets:", total_rows, "\n")

# Count and remove the non-NA and non-empty values of 'occupation' in 2006
colnames(data_iat_2006)
nrow(data_iat_2006)
sum(!is.na(data_iat_2006$occupation) & data_iat_2006$occupation != "")
data_iat_2006 <- data_iat_2006 %>%
  filter(!is.na(occupation) & occupation != "")
nrow(data_iat_2006)

colnames(data_iat_2015)
unique(data_iat_2006$age)

# Rename variables to make it consistent across datasets
for (year in 2006:2021) {
  dataset_name <- paste0("data_iat_", year)
  assign(
    dataset_name,
    get(dataset_name) %>% rename(D_score = D_biep.Abled_Good_all, explicit_attitude = att_7)
  )
}

for (year in 2022:2024) {
  dataset_name <- paste0("data_iat_", year)
  assign(
    dataset_name,
    get(dataset_name) %>% rename(D_score = D_biep.PhysAbled_Good_all, explicit_attitude = att_7)
  )
}

for (year in 2006:2015) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name)
  
  if ("sex" %in% colnames(dataset)) {
    
    # Recode 'sex' values before renaming
    dataset$sex <- dplyr::recode(dataset$sex,
                                 "m" = "1",
                                 "f" = "2")
    
    dataset <- dataset %>%
      rename(birthsex = sex)  # Renaming the 'sex' column to 'birthsex'
    
    if ("occupation" %in% colnames(dataset)) {
      dataset$occupation_code <- gsub("[^0-9]", "", dataset$occupation)
    }
    
    assign(dataset_name, dataset)
  } else {
    message("Skipping ", dataset_name, ": column 'sex' not found.")
  }
}

# 2016 seems to be a transition year in the data collection
# multiple variables changed name during the year or were removed
if (all(c("occupation", "occuselfdetail") %in% colnames(data_iat_2016))) {
  # Create occupation_code from occupation
  data_iat_2016$occupation_code <- gsub("[^0-9]", "", data_iat_2016$occupation)
  
  # If occupation_code is missing or invalid, use occuselfdetail
  data_iat_2016$occupation_code <- ifelse(
    is.na(data_iat_2016$occupation_code) |
      data_iat_2016$occupation_code == "" |
      !grepl("^[0-9]+$", data_iat_2016$occupation_code),
    gsub("[^0-9]", "", data_iat_2016$occuselfdetail),
    data_iat_2016$occupation_code
  )
  
  # Ensure 'birthsex' and 'sex' are treated as characters for comparison
  data_iat_2016$birthsex <- as.character(data_iat_2016$birthsex)
  data_iat_2016$sex <- as.character(data_iat_2016$sex)
  
  # Recode 'sex' from "m"/"f" to "1"/"2"
  data_iat_2016$sex <- dplyr::recode(data_iat_2016$sex,
                                     "m" = "1",
                                     "f" = "2")
  
  # If 'birthsex' is missing or invalid, use the value from 'sex'
  data_iat_2016$births <- ifelse(
    is.na(data_iat_2016$birthsex) | 
      data_iat_2016$birthsex == "" | 
      !grepl("^[0-9]+$", data_iat_2016$birthsex), 
    ifelse(
      is.na(data_iat_2016$sex) | 
        data_iat_2016$sex == "" | 
        !grepl("^[0-9]+$", data_iat_2016$sex),
      # If both 'sex' and 'birthsex' are missing or invalid, use 'genderidentity' (only if 1 or 2)
      ifelse(
        data_iat_2016$genderidentity %in% c(1, 2),
        as.character(data_iat_2016$genderidentity),
        NA_character_
      ),
      data_iat_2016$sex
    ),
    data_iat_2016$birthsex
  )
  
} else {
  message("One or more required columns ('occupation', 'occuselfdetail', 'birthsex', 'sex', 'genderidentity') are missing in data_iat_2016.")
}

# Replace missing or invalid age values with year - birthyear
if (all(c("age", "birthyear", "year") %in% colnames(data_iat_2016))) {
  data_iat_2016$age <- ifelse(
    is.na(data_iat_2016$age) |
      data_iat_2016$age == "" |
      !grepl("^[0-9]+$", as.character(data_iat_2016$age)),
    as.numeric(data_iat_2016$year) - as.numeric(data_iat_2016$birthyear),
    as.numeric(data_iat_2016$age)
  )
} else {
  message("One or more required columns ('age', 'birthyear', 'year') are missing in data_iat_2016.")
}

for (year in 2017:2024) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name)  
  
  # Create the 'age' variable
  dataset <- dataset %>%
    mutate(age = year - birthyear)
  
  # Save the modified dataset back to the environment
  assign(dataset_name, dataset)
}

for (year in 2017:2018) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name)
  
  if ("occuselfdetail" %in% colnames(dataset)) {
    dataset$occupation_code <- gsub("[^0-9]", "", dataset$occuselfdetail)
    
    # Ensure 'birthsex' is treated as a character for comparison
    dataset$birthsex <- as.character(dataset$birthsex)
    
    # If 'birthsex' is missing or invalid, use the value from 'genderidentity' (only if it is 1 or 2)
    dataset$births <- ifelse(
      is.na(dataset$birthsex) | 
        dataset$birthsex == "" | 
        !grepl("^[0-9]+$", dataset$birthsex),
      ifelse(
        dataset$genderidentity %in% c(1, 2),  # Check if 'genderidentity' is 1 or 2
        as.character(dataset$genderidentity), # Use 'genderidentity' if valid
        NA_character_  # If 'genderidentity' is not valid, assign NA
      ),
      dataset$birthsex
    )
    assign(dataset_name, dataset)
  } else {
    message("Skipping ", dataset_name, ": column 'occuselfdetail' not found.")
  }
}

for (year in 2019:2023) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name)
  
  if ("birthSex" %in% colnames(dataset)) {
    # Rename 'birthSex' to 'birthsex'
    dataset$birthsex <- gsub("[^0-9]", "", dataset$birthSex)
    
    # If 'birthsex' is missing or invalid, use 'genderIdentity' (only if 1 or 2)
    dataset$birthsex <- ifelse(
      is.na(dataset$birthsex) | 
        dataset$birthsex == "" | 
        !grepl("^[0-9]+$", dataset$birthsex),  # Invalid or missing birthsex
      ifelse(
        dataset$genderIdentity %in% c(1, 2),  # Check if genderIdentity is 1 or 2
        as.character(dataset$genderIdentity),  # Use genderIdentity if valid
        NA_character_  # If not valid, assign NA
      ),
      dataset$birthsex  # Use birthsex if it's valid
    )
    
    assign(dataset_name, dataset)
  } else {
    message("Skipping ", dataset_name, ": column 'birthSex' not found.")
  }
}

for (year in 2024:2024) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name)
  
  if ("genderIdentity_0002" %in% colnames(dataset)) {
    # Create a new column that takes the value of 'genderIdentity_0002' if it's 1 or 2
    dataset$birthsex <- ifelse(
      dataset$genderIdentity_0002 %in% c(1, 2),  # Check if the value is 1 or 2
      dataset$genderIdentity_0002,  # Use 'genderIdentity_0002' if valid
      NA_integer_  # If not 1 or 2, assign NA
    )
    
    assign(dataset_name, dataset)
  } else {
    message("Skipping ", dataset_name, ": column 'genderIdentity_0002' not found.")
  }
}

for (year in 2019:2024) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name)
  
  if ("occuSelfDetail" %in% colnames(dataset)) {
    dataset$occupation_code <- gsub("[^0-9]", "", dataset$occuSelfDetail)
    assign(dataset_name, dataset)
  } else {
    message("Skipping ", dataset_name, ": column 'occuSelfDetail' not found.")
  }
}

# Recode occupation_code and remove rows where occupation_code is NA
for (year in 2006:2024) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name) %>%
    mutate(occupation_code = case_when(
      occupation_code == "291000" ~ "clin",
      occupation_code == "312000" ~ "rehab",
      occupation_code == "" ~ NA_character_,
      TRUE ~ "other"
    )) %>%
    filter(!is.na(occupation_code))
  assign(dataset_name, dataset)
}

# Loop to create 'date' and 'date_numeric' variables for datasets from 2006 to 2024,
# after checking for the presence of 'day', 'month', and 'year' columns and filtering out missing dates
for (year in 2006:2024) {
  dataset_name <- paste0("data_iat_", year)
  if (exists(dataset_name)) {
    dataset <- get(dataset_name)
    required_cols <- c("day", "month", "year")
    if (all(required_cols %in% colnames(dataset))) {
      dataset <- dataset %>%
        mutate(
          date = make_date(year, month, day),
          date_numeric = as.numeric(date)
        )
      assign(dataset_name, dataset)
    } else {
      message("Skipping ", dataset_name, ": missing 'day', 'month', or 'year' column.")
    }
  } else {
    message("Skipping ", dataset_name, ": dataset does not exist.")
  }
}

# Select columns of interest
for (year in 2006:2024) {
  dataset_name <- paste0("data_iat_", year)
  assign(
    dataset_name,
    get(dataset_name) %>%
      select(D_score, explicit_attitude, year, occupation_code, date, 
             date_numeric, birthsex, age)
  )
}  

# Harmonize the 'birthsex' variable across all datasets (2006–2024)
# Convert 'birthsex' to character type to ensure consistent data type before merging
for (year in 2006:2024) {
  dataset_name <- paste0("data_iat_", year)
  dataset <- get(dataset_name)
  
  if ("birthsex" %in% colnames(dataset)) {
    dataset$birthsex <- as.character(dataset$birthsex)
  }
  
  assign(dataset_name, dataset)
}

# Merge all yearly datasets (2006–2024) into a single dataset
data_iat_merged <- bind_rows(
  lapply(2006:2024, function(year) get(paste0("data_iat_", year)))
)

# Remove rows for age younger than 20 and older than 70
data_iat_merged <- data_iat_merged %>%
  filter(age >= 20, age <= 70)

# Create a new variable "disability_type" based on the year: 
# 0 for years 2006-2021 (general disability), 1 for years 2022-2024 (physical disability).
data_iat_merged <- data_iat_merged %>%
  mutate(disability_type = case_when(
    year >= 2006 & year <= 2021 ~ 0,
    year >= 2022 & year <= 2024 ~ 1,
    TRUE ~ NA_real_
  ))

# Convert occupation_code to a categorical variable (factor) for analysis
data_iat_merged$occupation_code <- factor(data_iat_merged$occupation_code)
data_iat_merged$disability_type <- factor(data_iat_merged$disability_type)

# Remove "nu" invalid values from birthsex
sum(is.na(data_iat_merged$birthsex))
table(data_iat_merged$birthsex)
data_iat_merged %>% filter(birthsex == "nu") %>% count(year)
data_iat_merged %>%
  filter(birthsex == ".") %>%
  summarise(total = n())
data_iat_merged <- data_iat_merged %>%
  filter(!is.na(birthsex) & !birthsex %in% c(".", "nu"))

# Reorder 'occupation_code' to set 'other' as the reference category and test interaction between 'birthsex' and 'occupation_code'
data_iat_merged$occupation_code <- factor(data_iat_merged$occupation_code, levels = c("other", setdiff(levels(data_iat_merged$occupation_code), "other")))
data_iat_merged $occupation_code <- factor(data_iat_merged $occupation_code)
data_iat_merged $birthsex <- factor(data_iat_merged $birthsex)

# Standardize continuous explanatory variables
data_iat_merged$date_numeric_c <- scale(data_iat_merged$date_numeric, scale = FALSE)
data_iat_merged$age_c <- scale(data_iat_merged$age, scale = FALSE)
data_iat_merged$explicit_attitude_c <- scale(data_iat_merged$explicit_attitude, scale = FALSE)
data_iat_merged$D_score_c <- scale(data_iat_merged$D_score, scale = FALSE)


### DESCRIPTIVE RESULTS ### 
# Remove rows with NA
data_iat_merged <- data_iat_merged %>% drop_na()

# number of observations by occupation group
table(data_iat_merged$occupation_code)

# Age and explicit attitudes as a function of occupation category
data_iat_merged %>%
  group_by(occupation_code) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    
    mean_explicit_attitude = mean(explicit_attitude, na.rm = TRUE),
    sd_explicit_attitude = sd(explicit_attitude, na.rm = TRUE),
    min_explicit_attitude = min(explicit_attitude, na.rm = TRUE),
    max_explicit_attitude = max(explicit_attitude, na.rm = TRUE),
    median_explicit_attitude = median(explicit_attitude, na.rm = TRUE),
    
    mean_D_score = mean(D_score, na.rm = TRUE),
    sd_D_score = sd(D_score, na.rm = TRUE),
    min_D_score = min(D_score, na.rm = TRUE),
    max_D_score = max(D_score, na.rm = TRUE),
    median_D_score = median(D_score, na.rm = TRUE),
    
    n = n()  # Number of participants per occupation category
  ) %>%
  arrange(mean_age) %>%
  print(width = Inf)  # Forces full column display

# number of observations per value of the variables sex and disability type
count_percentage <- function(data, group_var) {
  data %>%
    group_by(occupation_code, {{ group_var }}) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(occupation_code) %>%
    mutate(perc = round(100 * n / sum(n), 2)) %>%
    arrange(occupation_code, desc(n))  # Optional: Order by occupation and count
}

birthsex_counts <- count_percentage(data_iat_merged, birthsex)
disability_type_counts <- count_percentage(data_iat_merged, disability_type)

print(birthsex_counts, n = Inf)
print(disability_type_counts, n = Inf)


### STATISTICAL MODELS ###
## Spline analysis
# Implicit attitudes
model_spline_implicit <- gam(D_score ~ 1 +
                         te(date_numeric_c, occupation_code, birthsex, bs = c("cr", "re", "re")) +
                         date_numeric_c + occupation_code + birthsex + explicit_attitude_c +
                         factor(disability_type) + age_c, data = data_iat_merged
)

print(summary(model_spline_implicit))


# 95% confidence intervals 
coefs <- coef(model_spline_implicit)
se <- summary(model_spline_implicit)$se
lower_ci <- coefs - 1.96 * se
upper_ci <- coefs + 1.96 * se
ci_implicit <- data.frame(Estimate = coefs, Lower_CI = lower_ci, Upper_CI = upper_ci)
print(ci_implicit)

# Explicit attitudes
model_spline_explicit <- gam(explicit_attitude ~ 1 + 
                         te(date_numeric_c, occupation_code, birthsex, bs = c("cr", "re", "re")) +
                         date_numeric_c + occupation_code + birthsex + D_score_c +
                         factor(disability_type) + age_c, data = data_iat_merged
)

print(summary(model_spline_explicit))

# 95% confidence intervals 
coefs <- coef(model_spline_explicit)
se <- summary(model_spline_explicit)$se
lower_ci <- coefs - 1.96 * se
upper_ci <- coefs + 1.96 * se
ci_explicit <- data.frame(Estimate = coefs, Lower_CI = lower_ci, Upper_CI = upper_ci)
print(ci_explicit)


### PLOTS ###
## 1. IMPLICIT ##
spline_implicit <- gam(D_score ~ 1 +
                         te(date_numeric, occupation_code, birthsex, bs = c("cr", "re", "re")) +
                         date_numeric + occupation_code + birthsex + explicit_attitude +
                         factor(disability_type) + age, data = data_iat_merged
)

### 1.1. Plot all the effects (implicit attitudes) ###
# Extract fixed effects (parametric coefficients) as a tidy data frame
fixed_effects <- tidy(spline_implicit, parametric = TRUE)

# Remove the intercept
fixed_effects <- fixed_effects %>%
  filter(term != "(Intercept)") %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

### Plot fixed effects without intercept ###
ggplot(fixed_effects, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(
    title = "Fixed Effects from spline_implicit Model (Intercept Removed)",
    x = "Fixed Effect",
    y = "Estimate (with 95% CI)"
  ) +
  theme_minimal(base_size = 14)


### 1.2. Effect of AGE on D-Score ###
# Create a grid for age
age_grid_implicit <- data.frame(
  age = seq(min(data_iat_merged$age, na.rm = TRUE), max(data_iat_merged$age, na.rm = TRUE), length.out = 100),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  explicit_attitude = mean(data_iat_merged$explicit_attitude, na.rm = TRUE),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict D-score for age
pred_implicit_age <- predict(spline_implicit, newdata = age_grid_implicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
age_grid_implicit$D_score_pred <- pred_implicit_age$fit
age_grid_implicit$lower_CI <- pred_implicit_age$fit - 1.96 * pred_implicit_age$se.fit
age_grid_implicit$upper_CI <- pred_implicit_age$fit + 1.96 * pred_implicit_age$se.fit

# Plot: Effect of Age on D-score
plot_age_implicit <- ggplot(age_grid_implicit, aes(x = age, y = D_score_pred)) +
  geom_line(size = 1, color = "blue") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(
    title = "Age",
    x = "Age",
    y = "Estimated D-Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### 1.3.Effect of DISABILITY TYPE on D-Score ###

# Create a grid for disability_type
disability_grid_implicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  explicit_attitude = mean(data_iat_merged$explicit_attitude, na.rm = TRUE),
  disability_type = factor(c(0, 1), levels = levels(data_iat_merged$disability_type))
)

# Predict D-score for disability type
pred_implicit_disability <- predict(
  spline_implicit,
  newdata = disability_grid_implicit,
  type = "response",
  se.fit = TRUE
)

# Add predictions and confidence intervals
disability_grid_implicit$D_score_pred <- pred_implicit_disability$fit
disability_grid_implicit$lower_CI <- pred_implicit_disability$fit - 1.96 * pred_implicit_disability$se.fit
disability_grid_implicit$upper_CI <- pred_implicit_disability$fit + 1.96 * pred_implicit_disability$se.fit

# Convert disability_type to readable labels
disability_grid_implicit$disability_type <- factor(
  disability_grid_implicit$disability_type,
  levels = c(0, 1),
  labels = c("No Disability", "Disability")
)

# Plot: Effect of Disability Type on D-score
plot_disability_implicit <- ggplot(disability_grid_implicit, aes(x = disability_type, y = D_score_pred)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "blue") +
  labs(
    title = "Disability Type",
    x = "Disability Type",
    y = "Estimated D-Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### 1.4.Effect of EXPLICIT ATTITUDE on D-Score ###
# Create a grid for explicit_attitude
explicit_grid_implicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  explicit_attitude = seq(min(data_iat_merged$explicit_attitude, na.rm = TRUE), 
                          max(data_iat_merged$explicit_attitude, na.rm = TRUE), length.out = 100),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict D-score for explicit attitude
pred_implicit_explicit <- predict(spline_implicit, newdata = explicit_grid_implicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
explicit_grid_implicit$D_score_pred <- pred_implicit_explicit$fit
explicit_grid_implicit$lower_CI <- pred_implicit_explicit$fit - 1.96 * pred_implicit_explicit$se.fit
explicit_grid_implicit$upper_CI <- pred_implicit_explicit$fit + 1.96 * pred_implicit_explicit$se.fit

# Plot: Effect of Explicit Attitude on D-score
plot_explicit_implicit <- ggplot(explicit_grid_implicit, aes(x = explicit_attitude, y = D_score_pred)) +
  geom_line(size = 1, color = "blue") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(
    title = "Explicit Attitude",
    x = "Explicit Attitude",
    y = "Estimated D-Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### Combine All Three Plots ###
combined_plot_implicit <- plot_age_implicit + plot_disability_implicit + plot_explicit_implicit + plot_layout(ncol = 3)

# Print the combined plot
print(combined_plot_implicit)

## 1.5. Plot the 3-way interaction (implicit attitudes) ###
# Get the most frequent category (mode) of disability_type
mode_disability_type <- names(sort(table(data_iat_merged$disability_type), decreasing = TRUE))[1]

# Get the mean values for the other predictors
mean_explicit_attitude <- mean(data_iat_merged$explicit_attitude, na.rm = TRUE)
mean_age <- mean(data_iat_merged$age, na.rm = TRUE)

# Generate a grid of values for date_numeric, occupation_code, and birthsex
predict_data <- expand.grid(
  date_numeric = seq(min(data_iat_merged$date_numeric), max(data_iat_merged$date_numeric), length.out = 100),
  occupation_code = unique(data_iat_merged$occupation_code),
  birthsex = unique(data_iat_merged$birthsex)
)

# Add the other covariates with default values (mean or mode values)
predict_data$explicit_attitude <- mean_explicit_attitude
predict_data$disability_type <- mode_disability_type
predict_data$age <- mean_age

# Predict D_score for each combination of predictors and get standard errors (se.fit = TRUE)
predictions <- predict(spline_implicit, newdata = predict_data, type = "response", se.fit = TRUE)

# Add predictions and CI bounds to the data
predict_data$D_score <- predictions$fit
predict_data$CI_lower <- predictions$fit - 1.96 * predictions$se.fit
predict_data$CI_upper <- predictions$fit + 1.96 * predictions$se.fit

# Convert date_numeric to years for plotting purposes
predict_data$year <- as.integer(format(as.Date(predict_data$date_numeric, origin = "1970-01-01"), "%Y"))

# Calculate date_numeric for the desired years (2006, 2009, 2012, 2015, 2018, 2021, 2024)
years_to_label <- c(2006, 2009, 2012, 2015, 2018, 2021, 2024)
date_numeric_labels <- as.numeric(as.Date(paste0(years_to_label, "-01-01"), format = "%Y-%m-%d"))

# Generate intermediary years for minor breaks (e.g., 2007, 2008, etc.)
intermediate_years <- seq(min(years_to_label), max(years_to_label), by = 1)
intermediate_date_numeric <- as.numeric(as.Date(paste0(intermediate_years, "-01-01"), format = "%Y-%m-%d"))

# Create the plot using date_numeric for the model but display years on the x-axis
ggplot(predict_data, aes(x = date_numeric, y = D_score, color = birthsex)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = birthsex), alpha = 0.2) +  # Add CI ribbon
  facet_wrap(~ occupation_code) +
  labs(title = "Variation of D_score across Date_numeric for each Occupation and Birthsex with 95% CI",
       x = "Year", y = "Estimated D-score") +
  theme_minimal() +
  scale_x_continuous(
    labels = function(x) {
      # Convert date_numeric (days since 1970) to years
      as.integer(format(as.Date(x, origin = "1970-01-01"), "%Y"))
    },
    breaks = date_numeric_labels,  # Major breaks for the specified years (2006, 2009, etc.)
    minor_breaks = intermediate_date_numeric,  # Minor breaks for intermediary years
    expand = c(0.05, 0.05),  # Add some padding on both ends
    limits = c(min(date_numeric_labels), max(date_numeric_labels))  # Ensure the limits match the labels
  ) +
  scale_color_viridis(discrete = TRUE) +  # Apply viridis scale for 'color' aesthetic
  scale_fill_viridis(discrete = TRUE, alpha = 0.2) +  # Apply viridis scale for 'fill' aesthetic with transparency
  theme(
    # Adjust the strip (facet labels) spacing and position
    strip.text = element_text(size = 10),  # Adjust font size for the facet labels
    panel.spacing = unit(1, "lines"),  # Increase space between panels (figures)
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


### 2.EXPLICIT ###
spline_explicit <- gam(explicit_attitude ~ 1 + 
                               te(date_numeric, occupation_code, birthsex, bs = c("cr", "re", "re")) +
                               date_numeric + occupation_code + birthsex + D_score +
                               factor(disability_type) + age, data = data_iat_merged
)


## 2.1. Plot all the effects (explicit attitudes) ###
# Extract fixed effects (parametric coefficients) as a tidy data frame
fixed_effects <- tidy(spline_explicit, parametric = TRUE)

# Remove the intercept
fixed_effects <- fixed_effects %>%
  filter(term != "(Intercept)") %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Plot fixed effects without intercept
ggplot(fixed_effects, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(
    title = "Fixed Effects from spline_explicit Model (Intercept Removed)",
    x = "Fixed Effect",
    y = "Estimate (with 95% CI)"
  ) +
  theme_minimal(base_size = 14)

### 2.2. Effect of AGE on Explicit Attitudes ###
# Create a grid for age
age_grid_explicit <- data.frame(
  age = seq(min(data_iat_merged$age, na.rm = TRUE), max(data_iat_merged$age, na.rm = TRUE), length.out = 100),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  D_score = mean(data_iat_merged$D_score, na.rm = TRUE),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict explicit attitudes for age
pred_explicit <- predict(spline_explicit, newdata = age_grid_explicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
age_grid_explicit$explicit_attitude_pred <- pred_explicit$fit
age_grid_explicit$lower_CI <- pred_explicit$fit - 1.96 * pred_explicit$se.fit
age_grid_explicit$upper_CI <- pred_explicit$fit + 1.96 * pred_explicit$se.fit

# Plot: Effect of Age on Explicit Attitude
plot_age <- ggplot(age_grid_explicit, aes(x = age, y = explicit_attitude_pred)) +
  geom_line(size = 1, color = "red") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "red") +
  labs(
    title = "Age",
    x = "Age",
    y = "Estimated Likert Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank()) 

### 2.3.Effect of DISABILITY TYPE on Explicit Attitudes ###
# Create a grid for disability_type
disability_grid_explicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  D_score = mean(data_iat_merged$D_score, na.rm = TRUE),
  disability_type = factor(c(0, 1), levels = levels(data_iat_merged$disability_type))
)

# Predict explicit attitudes for disability type
explicit_pred_disability <- predict(
  spline_explicit,
  newdata = disability_grid_explicit,
  type = "response",
  se.fit = TRUE
)

# Add predictions and confidence intervals
disability_grid_explicit$explicit_attitude_pred <- explicit_pred_disability$fit
disability_grid_explicit$lower_CI <- explicit_pred_disability$fit - 1.96 * explicit_pred_disability$se.fit
disability_grid_explicit$upper_CI <- explicit_pred_disability$fit + 1.96 * explicit_pred_disability$se.fit

# Convert disability_type to readable labels
disability_grid_explicit$disability_type <- factor(
  disability_grid_explicit$disability_type,
  levels = c(0, 1),
  labels = c("No Disability", "Disability")
)

# Plot: Effect of Disability Type on Explicit Attitude
plot_disability <- ggplot(disability_grid_explicit, aes(x = disability_type, y = explicit_attitude_pred)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "red") +
  labs(
    title = "Disability Type",
    x = "Disability Type",
    y = "Estimated Likert Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### 2.4.Effect of D-SCORE on Explicit Attitudes ###
# Create a grid for D-score
dscore_grid_explicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  D_score = seq(min(data_iat_merged$D_score, na.rm = TRUE), max(data_iat_merged$D_score, na.rm = TRUE), length.out = 100),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict explicit attitudes for D-score
pred_dscore <- predict(spline_explicit, newdata = dscore_grid_explicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
dscore_grid_explicit$explicit_attitude_pred <- pred_dscore$fit
dscore_grid_explicit$lower_CI <- pred_dscore$fit - 1.96 * pred_dscore$se.fit
dscore_grid_explicit$upper_CI <- pred_dscore$fit + 1.96 * pred_dscore$se.fit

# Plot: Effect of D-score on Explicit Attitude
plot_dscore <- ggplot(dscore_grid_explicit, aes(x = D_score, y = explicit_attitude_pred)) +
  geom_line(size = 1, color = "red") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "red") +
  labs(
    title = "D-Score",
    x = "D-Score",
    y = "Estimated Likert Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### Combine All Three Plots ###
combined_plot <- plot_age + plot_disability + plot_dscore + plot_layout(ncol = 3)

# Print the combined plot
print(combined_plot)

### 2.5.Plot the 3-way interaction (explicit attitudes) ###
# Predict explicit_attitude for each combination of predictors and get standard errors (se.fit = TRUE)
predictions_explicit <- predict(spline_explicit, newdata = predict_data, type = "response", se.fit = TRUE)

# Add predictions and CI bounds to the data
predict_data$explicit_attitude <- predictions_explicit$fit
predict_data$CI_lower <- predictions_explicit$fit - 1.96 * predictions_explicit$se.fit
predict_data$CI_upper <- predictions_explicit$fit + 1.96 * predictions_explicit$se.fit

# Convert date_numeric to years for plotting purposes
predict_data$year <- as.integer(format(as.Date(predict_data$date_numeric, origin = "1970-01-01"), "%Y"))

# Create the plot using date_numeric for the model but display years on the x-axis
ggplot(predict_data, aes(x = date_numeric, y = explicit_attitude, color = birthsex)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = birthsex), alpha = 0.2) +  # Add CI ribbon
  facet_wrap(~ occupation_code) +
  labs(title = "Variation of Explicit Attitude across Date_numeric for each Occupation and Birthsex with 95% CI",
       x = "Year", y = "Estimated Explicit Attitude") +
  theme_minimal() +
  scale_x_continuous(
    labels = function(x) {
      # Convert date_numeric (days since 1970) to years
      as.integer(format(as.Date(x, origin = "1970-01-01"), "%Y"))
    },
    breaks = date_numeric_labels,  # Major breaks for the specified years (2006, 2009, etc.)
    minor_breaks = intermediate_date_numeric,  # Minor breaks for intermediary years
    expand = c(0.05, 0.05),  # Add some padding on both ends
    limits = c(min(date_numeric_labels), max(date_numeric_labels))  # Ensure the limits match the labels
  ) +
  scale_color_viridis(discrete = TRUE) +  # Apply viridis scale for 'color' aesthetic
  scale_fill_viridis(discrete = TRUE, alpha = 0.2) +  # Apply viridis scale for 'fill' aesthetic with transparency
  theme(
    
    # Adjust the strip (facet labels) spacing and position
    strip.text = element_text(size = 10),  # Adjust font size for the facet labels
    panel.spacing = unit(1, "lines"),  # Increase space between panels (figures)
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )

#### WITH THE SAME Y AXIS LIMITS
#### IMPLICIT 1.2-4
### 1.2. Effect of AGE on D-score ###
# Create a grid for age
age_grid_implicit <- data.frame(
  age = seq(min(data_iat_merged$age, na.rm = TRUE), max(data_iat_merged$age, na.rm = TRUE), length.out = 100),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  explicit_attitude = mean(data_iat_merged$explicit_attitude, na.rm = TRUE),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict D-score for age
pred_implicit_age <- predict(spline_implicit, newdata = age_grid_implicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
age_grid_implicit$D_score_pred <- pred_implicit_age$fit
age_grid_implicit$lower_CI <- pred_implicit_age$fit - 1.96 * pred_implicit_age$se.fit
age_grid_implicit$upper_CI <- pred_implicit_age$fit + 1.96 * pred_implicit_age$se.fit

# Plot: Effect of Age on D-score
plot_age_implicit <- ggplot(age_grid_implicit, aes(x = age, y = D_score_pred)) +
  geom_line(size = 1, color = "blue") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(
    title = "Age",
    x = "Age",
    y = "Estimated D-Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### 1.3. Effect of DISABILITY TYPE on D-score ###
# Create a grid for disability_type
disability_grid_implicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  explicit_attitude = mean(data_iat_merged$explicit_attitude, na.rm = TRUE),
  disability_type = factor(c(0, 1), levels = levels(data_iat_merged$disability_type))
)

# Predict D-score for disability type
pred_implicit_disability <- predict(
  spline_implicit,
  newdata = disability_grid_implicit,
  type = "response",
  se.fit = TRUE
)

# Add predictions and confidence intervals
disability_grid_implicit$D_score_pred <- pred_implicit_disability$fit
disability_grid_implicit$lower_CI <- pred_implicit_disability$fit - 1.96 * pred_implicit_disability$se.fit
disability_grid_implicit$upper_CI <- pred_implicit_disability$fit + 1.96 * pred_implicit_disability$se.fit

# Convert disability_type to readable labels
disability_grid_implicit$disability_type <- factor(
  disability_grid_implicit$disability_type,
  levels = c(0, 1),
  labels = c("No Disability", "Disability")
)

# Plot: Effect of Disability Type on D-score
plot_disability_implicit <- ggplot(disability_grid_implicit, aes(x = disability_type, y = D_score_pred)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "blue") +
  labs(
    title = "Disability Type",
    x = "Disability Type",
    y = "Estimated D-Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### 1.4. Effect of EXPLICIT ATTITUDE on D-score ###
# Create a grid for explicit_attitude
explicit_grid_implicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  explicit_attitude = seq(min(data_iat_merged$explicit_attitude, na.rm = TRUE), 
                          max(data_iat_merged$explicit_attitude, na.rm = TRUE), length.out = 100),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict D-score for explicit attitude
pred_implicit_explicit <- predict(spline_implicit, newdata = explicit_grid_implicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
explicit_grid_implicit$D_score_pred <- pred_implicit_explicit$fit
explicit_grid_implicit$lower_CI <- pred_implicit_explicit$fit - 1.96 * pred_implicit_explicit$se.fit
explicit_grid_implicit$upper_CI <- pred_implicit_explicit$fit + 1.96 * pred_implicit_explicit$se.fit

# Plot: Effect of Explicit Attitude on D-score
plot_explicit_implicit <- ggplot(explicit_grid_implicit, aes(x = explicit_attitude, y = D_score_pred)) +
  geom_line(size = 1, color = "blue") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "blue") +
  labs(
    title = "Explicit Attitude",
    x = "Explicit Attitude",
    y = "Estimated D-Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### Step 1: Find Global Y-Axis Limits ###
# Determine global min and max for y-axis
global_y_min_implicit <- min(
  c(age_grid_implicit$lower_CI, disability_grid_implicit$lower_CI, explicit_grid_implicit$lower_CI),
  na.rm = TRUE
)
global_y_max_implicit <- max(
  c(age_grid_implicit$upper_CI, disability_grid_implicit$upper_CI, explicit_grid_implicit$upper_CI),
  na.rm = TRUE
)

### Step 2: Apply the Global Limits to All Plots ###
plot_age_implicit <- plot_age_implicit + ylim(global_y_min_implicit, global_y_max_implicit)
plot_disability_implicit <- plot_disability_implicit + ylim(global_y_min_implicit, global_y_max_implicit)
plot_explicit_implicit <- plot_explicit_implicit + ylim(global_y_min_implicit, global_y_max_implicit)

### Combine All Three Plots ###
combined_plot_implicit <- plot_age_implicit + plot_disability_implicit + plot_explicit_implicit + plot_layout(ncol = 3)

# Print the combined plot
print(combined_plot_implicit)


#### EXPLICIT 2.2-4
# Create a grid for age
age_grid_explicit <- data.frame(
  age = seq(min(data_iat_merged$age, na.rm = TRUE), max(data_iat_merged$age, na.rm = TRUE), length.out = 100),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  D_score = mean(data_iat_merged$D_score, na.rm = TRUE),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict explicit attitudes for age
pred_explicit <- predict(spline_explicit, newdata = age_grid_explicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
age_grid_explicit$explicit_attitude_pred <- pred_explicit$fit
age_grid_explicit$lower_CI <- pred_explicit$fit - 1.96 * pred_explicit$se.fit
age_grid_explicit$upper_CI <- pred_explicit$fit + 1.96 * pred_explicit$se.fit

# Plot: Effect of Age on Explicit Attitude
plot_age <- ggplot(age_grid_explicit, aes(x = age, y = explicit_attitude_pred)) +
  geom_line(size = 1, color = "red") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "red") +
  labs(
    title = "Age",
    x = "Age",
    y = "Estimated Likert Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank()) 

### 2.3. Effect of DISABILITY TYPE on Explicit Attitudes ###
# Create a grid for disability_type
disability_grid_explicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  D_score = mean(data_iat_merged$D_score, na.rm = TRUE),
  disability_type = factor(c(0, 1), levels = levels(data_iat_merged$disability_type))
)

# Predict explicit attitudes for disability type
explicit_pred_disability <- predict(
  spline_explicit,
  newdata = disability_grid_explicit,
  type = "response",
  se.fit = TRUE
)

# Add predictions and confidence intervals
disability_grid_explicit$explicit_attitude_pred <- explicit_pred_disability$fit
disability_grid_explicit$lower_CI <- explicit_pred_disability$fit - 1.96 * explicit_pred_disability$se.fit
disability_grid_explicit$upper_CI <- explicit_pred_disability$fit + 1.96 * explicit_pred_disability$se.fit

# Convert disability_type to readable labels
disability_grid_explicit$disability_type <- factor(
  disability_grid_explicit$disability_type,
  levels = c(0, 1),
  labels = c("No Disability", "Disability")
)

# Plot: Effect of Disability Type on Explicit Attitude
plot_disability <- ggplot(disability_grid_explicit, aes(x = disability_type, y = explicit_attitude_pred)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "red") +
  labs(
    title = "Disability Type",
    x = "Disability Type",
    y = "Estimated Likert Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### 2.4. Effect of D-SCORE on Explicit Attitudes ###
# Create a grid for D-score
dscore_grid_explicit <- data.frame(
  age = mean(data_iat_merged$age, na.rm = TRUE),
  date_numeric = mean(data_iat_merged$date_numeric, na.rm = TRUE),
  occupation_code = "other",
  birthsex = 1,
  D_score = seq(min(data_iat_merged$D_score, na.rm = TRUE), max(data_iat_merged$D_score, na.rm = TRUE), length.out = 100),
  disability_type = factor(0, levels = levels(data_iat_merged$disability_type))
)

# Predict explicit attitudes for D-score
pred_dscore <- predict(spline_explicit, newdata = dscore_grid_explicit, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals
dscore_grid_explicit$explicit_attitude_pred <- pred_dscore$fit
dscore_grid_explicit$lower_CI <- pred_dscore$fit - 1.96 * pred_dscore$se.fit
dscore_grid_explicit$upper_CI <- pred_dscore$fit + 1.96 * pred_dscore$se.fit

# Plot: Effect of D-score on Explicit Attitude
plot_dscore <- ggplot(dscore_grid_explicit, aes(x = D_score, y = explicit_attitude_pred)) +
  geom_line(size = 1, color = "red") +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.2, fill = "red") +
  labs(
    title = "D-Score",
    x = "D-Score",
    y = "Estimated Likert Score"
  ) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

### Step 1: Find Global Y-Axis Limits ###
# Determine global min and max for y-axis
global_y_min <- min(
  c(age_grid_explicit$lower_CI, disability_grid_explicit$lower_CI, dscore_grid_explicit$lower_CI),
  na.rm = TRUE
)
global_y_max <- max(
  c(age_grid_explicit$upper_CI, disability_grid_explicit$upper_CI, dscore_grid_explicit$upper_CI),
  na.rm = TRUE
)

### Step 2: Apply the Global Limits to All Plots ###
plot_age <- plot_age + ylim(global_y_min, global_y_max)
plot_disability <- plot_disability + ylim(global_y_min, global_y_max)
plot_dscore <- plot_dscore + ylim(global_y_min, global_y_max)

### Combine All Three Plots ###
combined_plot <- plot_age + plot_disability + plot_dscore + plot_layout(ncol = 3)

# Print the combined plot
print(combined_plot)
