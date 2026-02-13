library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(fixest)
library(tidyverse)
library(modelsummary)

raw2009 <- read_sav("NFCS 2009 State Data 220712 v2.sav")
raw2012 <- read_sav("NFCS 2012 State Data 130503.sav")
raw2015 <- read_sav("NFCS 2015 State Data 160619.sav")
raw2018 <- read_sav("NFCS 2018 State Data 190603.sav")
raw2021 <- read_sav("NFCS 2021 State Data 220627.sav")
raw2024 <- read_sav("NFCS 2024 State Data 250623.sav")

# 1. Define the Standardized Target Columns
std_cols <- c("NFCSID", "STATEQ", "A50A", "A50B", "F1", "F2_1", "F2_2", 
              "F2_3", "F2_4", "F2_5", "F2_6", "G1", "G30", "G35")

# 2. Create the Rename Map
var_map <- list(
  "2018" = c("A50A" = "A3", "A50B" = "A3B"),
  "2015" = c("A50A" = "A3", "A50B" = "A3B", "G30" = "G30_1"),
  "2012" = c("A50A" = "A3", "A50B" = "A3B", "G30" = "G21"),
  "2009" = c("A50A" = "A3", "A50B" = "A3B", "C1_2012" = "C1")
)

raw_data_list <- list(
  "2024" = raw2024,
  "2021" = raw2021,
  "2018" = raw2018,
  "2015" = raw2015,
  "2012" = raw2012,
  "2009" = raw2009
)

# 3. Data constant definitions
years <- c("2024", "2021", "2018", "2015", "2012", "2009")
state_codes <- c(47, 43, 26, 25, 15, 1)
state_labels <- c(
  "47" = "VA", 
  "43" = "TN", 
  "26" = "MO", 
  "25" = "MS", 
  "15" = "IA", 
  "1"  = "AL"
)

policy_dates <- data.frame(
  STATEQ = state_codes,
  Treat_Year = c(2015, 2013, 2010, 2021, 2022, 2017) 
)

nfcs_composite <- map_dfr(names(raw_data_list), function(yr) {
  
  # A. Extract and Rename
  df <- raw_data_list[[yr]]
  mapping <- var_map[[yr]]
  
  # Rename columns if mapping exists for that year
  if (length(mapping) > 0) {
    df <- df %>% rename(any_of(mapping))
  }
  
  # B. Select and Tag Year
  df_selected <- df %>% 
    filter(A50B %in% c(1, 7)) %>%
    select(any_of(std_cols)) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(year = as.integer(yr)) %>%
    mutate(F1 = if_else(F1 == 7, 0, F1)) %>%
    mutate(Has_Card = if_else(F1 > 0, 1, 0))
  
  return(df_selected)
  
})

nfcs_final <- nfcs_composite %>%
  mutate(
    # Create a Numeric Approximation (Trapezoidal Method)
    F1_Estimated = case_when(
      F1 == 0 ~ 0,    
      F1 == 1 ~ 1,    
      F1 == 2 ~ 2.5,  
      F1 == 3 ~ 6,   
      F1 == 4 ~ 10.5, 
      F1 == 5 ~ 16.5, 
      F1 == 6 ~ 22,   
      TRUE ~ NA_real_ 
    ),
    
    # Keep as Ordinal (for Charts/tables)
    F1_Category = factor(F1, 
                         levels = c(0, 1, 2, 3, 4, 5, 6),
                         labels = c("None", "1", "2-3", "4-8", "9-12", "13-20", ">20"),
                         ordered = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    # 1. Calculate the Score directly
    # F2_1: Did they FAIL to pay in full? (Value 2 = Yes, fail)
    # F2_2-6: Did they COMMIT a delinquency? (Value 1 = Yes, commit)
    Composite_F2 = sum(
      F2_1 == 2,         # 1 point if they didn't pay in full
      F2_2 == 1,         # 1 point for late fee
      F2_3 == 1,         # 1 point for over limit
      F2_4 == 1,         # 1 point for cash advance
      F2_5 == 1,         # 1 point for min payment only
      F2_6 == 1,         # 1 point for collection agency
      na.rm = TRUE       # Treat NA (Don't Know) as 0 points
    )
  ) %>%
  ungroup()


### FROM HERE ON OUT, WE'LL USE nfcs_final AS OUR DATA FRAME

state_stats <- nfcs_final %>%
  filter(STATEQ %in% state_codes) %>%
  group_by(year, STATEQ) %>%
  
  summarise(
    N_Respondents = n(),
    Card_Percentage = mean(Has_Card, na.rm = TRUE),
    Avg_F1_Score = mean(F1_Estimated, na.rm = TRUE),
    Median_F1 = median(F1_Estimated, na.rm = TRUE),
    Sd_F1 = sd(F1_Estimated, na.rm = TRUE),
    Avg_F2_Score = mean(Composite_F2, na.rm = TRUE),
    Median_F2 = median(Composite_F2, na.rm = TRUE),
    Sd_F2 = sd(Composite_F2, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarizing
  ) %>%
  
  mutate(State_Name = state_labels[as.character(STATEQ)]) %>%
  
  select(year, State_Name, STATEQ, N_Respondents, Card_Percentage, Avg_F1_Score, Median_F1, Sd_F1, Avg_F2_Score, everything())

state_stats_male <- nfcs_final %>%
  filter(STATEQ %in% state_codes) %>%
  filter(A50A == 1) %>%
  group_by(year, STATEQ) %>%
  
  summarise(
    N_Respondents = n(),
    Card_Percentage = mean(Has_Card, na.rm = TRUE),
    Avg_F1_Score = mean(F1_Estimated, na.rm = TRUE),
    Median_F1 = median(F1_Estimated, na.rm = TRUE),
    Sd_F1 = sd(F1_Estimated, na.rm = TRUE),
    Avg_F2_Score = mean(Composite_F2, na.rm = TRUE),
    Median_F2 = median(Composite_F2, na.rm = TRUE),
    Sd_F2 = sd(Composite_F2, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarizing
  ) %>%
  
  mutate(State_Name = state_labels[as.character(STATEQ)]) %>%
  
  select(year, State_Name, STATEQ, N_Respondents, Card_Percentage, Avg_F1_Score, Median_F1, Sd_F1, Avg_F2_Score, everything())

state_stats_female <- nfcs_final %>%
  filter(STATEQ %in% state_codes) %>%
  filter(A50A == 2) %>%
  group_by(year, STATEQ) %>%
  
  summarise(
    N_Respondents = n(),
    Card_Percentage = mean(Has_Card, na.rm = TRUE),
    Avg_F1_Score = mean(F1_Estimated, na.rm = TRUE),
    Median_F1 = median(F1_Estimated, na.rm = TRUE),
    Sd_F1 = sd(F1_Estimated, na.rm = TRUE),
    Avg_F2_Score = mean(Composite_F2, na.rm = TRUE),
    Median_F2 = median(Composite_F2, na.rm = TRUE),
    Sd_F2 = sd(Composite_F2, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarizing
  ) %>%
  
  mutate(State_Name = state_labels[as.character(STATEQ)]) %>%
  
  select(year, State_Name, STATEQ, N_Respondents, Card_Percentage, Avg_F1_Score, Median_F1, Sd_F1, Avg_F2_Score, everything())



print(state_stats)
print(national_stats)
state_stats %>% filter(year == 2024)

# DO THE REGRESSSION!!!!!

did_data <- state_stats %>%
  left_join(policy_dates, by = "STATEQ") %>%
  mutate(
    time_to_treat = year - Treat_Year,
    Treat_Year = as.numeric(Treat_Year)
  )

res_f1_i <- feols(
  Avg_F1_Score ~ sunab(Treat_Year, year) |
  STATEQ + year,
  cluster = ~STATEQ,
  data = did_data
)

res_f2_i <- feols(
  Avg_F2_Score ~ sunab(Treat_Year, year) |
  STATEQ + year,
  cluster = ~STATEQ,
  data = did_data
)

iplot(list(res_f1_i, res_f2_i), 
      sep = 0.5, 
      ref.line = 0,
      main = "Event Study Estimates: Impact of Law Adoption",
      xlab = "Time to Treatment (Years)",
      ylab = "Estimate (95% CI)")

did_data_male <- state_stats_male %>%
  left_join(policy_dates, by = "STATEQ") %>%
  mutate(
    time_to_treat = year - Treat_Year,
    Treat_Year = as.numeric(Treat_Year)
  )

did_data_female <- state_stats_female %>%
  left_join(policy_dates, by = "STATEQ") %>%
  mutate(
    time_to_treat = year - Treat_Year,
    Treat_Year = as.numeric(Treat_Year)
  )

res_f1_im <- feols(
  Avg_F1_Score ~ sunab(Treat_Year, year) |
    STATEQ + year,
  data = did_data_male
)
res_f2_im <- feols(
  Avg_F2_Score ~ sunab(Treat_Year, year) |
    STATEQ + year,
  data = did_data_male
)

res_f2_if <- feols(
  Avg_F2_Score ~ sunab(Treat_Year, year) |
    STATEQ + year,
  data = did_data_female
)

res_f1_if <- feols(
  Avg_F1_Score ~ sunab(Treat_Year, year) |
    STATEQ + year,
  data = did_data_female
)

iplot(list(res_f1_im, res_f2_im), 
      sep = 0.5, 
      ref.line = -1,
      main = "Event Study Estimates: Impact of Law Adoption on Men",
      xlab = "Time to Treatment (Years)",
      ylab = "Estimate (95% CI)")

legend("topright", col = c(1, 2), pch = c(20, 17), 
      legend = c("Credit Cards (F1)", "Delinquency (F2)"))

iplot(list(res_f1_im, res_f2_im), 
      sep = 0.5, 
      ref.line = -1,
      main = "Event Study Estimates: Impact of Law Adoption on Men",
      xlab = "Time to Treatment (Years)",
      ylab = "Estimate (95% CI)")

# Add a legend to distinguish outcomes
legend("topright", col = c(1, 2), pch = c(20, 17), 
       legend = c("Credit Cards (F1)", "Delinquency (F2)"))

es_data <- state_stats %>%
  left_join(policy_dates, by = "STATEQ") %>%
  mutate(
    rel_year = year - Treat_Year,
    rel_year = if_else(is.infinite(Treat_Year), -1000, rel_year)
  )



estimate_event_study <- function(outcome_var) {
  fml <- as.formula(paste(outcome_var, "~ i(rel_year, ref = -3) | STATEQ + year"))
  model <- feols(fml, data = es_data, cluster = "STATEQ")
  return(model)
}

res_f1 <- estimate_event_study("Avg_F1_Score")
print(res_f1)

res_f2 <- estimate_event_study("Avg_F2_Score")
print(res_f2)

iplot(res_f1, 
      main = "Impact on Credit Card Volume (F1) on Men",
      xlab = "Years Relative to Policy Implementation (k)",
      ylab = "Estimate (tau_k)")

iplot(res_f2, 
      main = "Impact on Financial Distress (F2) on Men",
      xlab = "Years Relative to Policy Implementation (k)",
      ylab = "Estimate (tau_k)")

ci_f1 <- state_stats %>% 
  rowwise() %>%
  groups() %>%
  #mutate(CI_Upper=Avg_F1_Score+1.96*)

# DRAW PLOTS

# Plot 1: Average Delinquency
plot1 <- ggplot(state_stats, aes(x = year, y = Avg_F2_Score, color = State_Name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # Set x-axis to show every data year clearly
  scale_x_continuous(breaks = c(2009, 2012, 2015, 2018, 2021, 2024)) +
  
  labs(
    title = "Credit Card Distress by State (2009–2024)",
    subtitle = "Composite Delinquency Score (0 = Perfect, 6 = High Distress)\nIncludes: Late fees, Over limit, Min payments, Cash advances, Collections",
    y = "Avg. Delinquency Score",
    x = "Year",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(), # Cleaner look
    plot.title = element_text(face = "bold", size = 14)
  )

# Plot 2: Average Cards Held
plot2 <- ggplot(state_stats, aes(x = year, y = Avg_F1_Score, color = State_Name)) +
  geom_line(linewidth = 1.2, linetype = "dashed") + 
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(2009, 2012, 2015, 2018, 2021, 2024)) +
  labs(
    title = "Credit Card Volume by State",
    subtitle = "Average estimated number of credit cards per respondent",
    y = "Avg. Number of Cards",
    x = "Year",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )






# ==============================================================================
# SETUP: Define Treatment Years
# ==============================================================================
# Merge dates into main data
df_analysis <- state_stats %>%
  left_join(policy_dates, by = "STATEQ")

# Identify treated states
treated_states <- policy_dates %>% 
  filter(is.finite(Treat_Year)) %>% 
  pull(STATEQ)

results_list <- list()

# ==============================================================================
# EXECUTION: The Loop
# ==============================================================================
for (target_state in treated_states) {
  
  target_year <- policy_dates$Treat_Year[policy_dates$STATEQ == target_state]
  
  # 1. Construct the Specific Control Group
  df_iter <- df_analysis %>%
    filter(
      STATEQ == target_state |                # Target State
        is.infinite(Treat_Year) |               # Clean Controls
        (Treat_Year > target_year & year < Treat_Year) # Future-treated (only pre-years)
    ) %>%
    mutate(
      rel_year = year - target_year,
      is_treated = if_else(STATEQ == target_state, 1, 0)
    )
  
  # 2. Safety Check: Does the reference period (-3) exist for this state?
  # If the state was treated in 2009, 'rel_year' starts at 0. We can't use -3.
  
  # 3. Run Event Study
  # We use ref = -3 because your data is triennial (2009, 2012, 2015...)
  # so '1 year prior' (-1) is missing. '3 years prior' (-3) is the correct baseline.
  model <- feols(Avg_F2_Score ~ i(rel_year, is_treated, ref = 0) | year + STATEQ,
                 data = df_iter,
                 cluster = "STATEQ")
  
  # Save using the State Name (for better plots later)
  st_name <- state_labels[as.character(target_state)]
  results_list[[st_name]] <- model
  
  cat("Finished:", st_name, "\n")
}

# ==============================================================================
# VISUALIZATION: The Corrected Plot
# ==============================================================================

# 1. Check if we have results
if (length(results_list) > 0) {
  
  # 2. Use iplot directly (Do not assign to a variable)
  # The list names (VA, TN, MO) will automatically become the legend
  iplot(results_list, 
        main = "Impact of Policy on Financial Distress (F2)",
        xlab = "Years Relative to Policy (k)",
        ylab = "Estimate (Difference from Control)",
        ref.line = 0,  # Visual line at the reference period
        pt.join = TRUE) # Connects the dots
  
} else {
  print("No models were successfully generated.")
}




# write.csv(x = sd_normalized, "~/Documents/econometrics_game/data_composite.csv")