# Load required libraries
library(dplyr)
library(lubridate)
library(readr)

# Load datasets
bup_case <- read_rds('bup_case_raw.rds')
control <- read_rds('control_all_raw.rds')

# Exclude patients with ASA classification 5 and emergency surgery
bup_case <- bup_case %>%
  filter(asaclas <= 4 & is.na(ASAINDEX))

# Find patients with repeat surgery within 30 days
bup_case <- bup_case %>%
  arrange(scrssn, opdt)

# Calculate time differences between consecutive surgeries for the same patient
time_diff <- as.numeric(difftime(bup_case$opdt[-1], bup_case$opdt[-nrow(bup_case)], units = 'days'))
bup_case <- bup_case %>%
  mutate(
    t_delta = c(0, time_diff),
    new_pt = c(TRUE, scrssn[-1] != scrssn[-nrow(bup_case)])
  )

# Adjust t_delta for new patients
bup_case <- bup_case %>%
  mutate(t_delta = ifelse(new_pt, -100, t_delta))

# Remove surgeries within 30 days
bup_case <- bup_case %>%
  filter(t_delta == -100 | t_delta > 30)

# Repeat the process for the control group
control <- control %>%
  arrange(scrssn, oprymd)

time_diff_control <- as.numeric(difftime(control$oprymd[-1], control$oprymd[-nrow(control)], units = 'days'))
control <- control %>%
  mutate(
    t_delta = c(0, time_diff_control),
    new_pt = c(TRUE, scrssn[-1] != scrssn[-nrow(control)])
  )

control <- control %>%
  mutate(t_delta = ifelse(new_pt, -100, t_delta)) %>%
  filter(t_delta == -100 | t_delta > 30)

# Filter and adjust race categories
bup_case <- bup_case %>%
  filter(!is.na(race1)) %>%
  mutate(race1 = ifelse(race1 == 'D', 'C', race1))

control <- control %>%
  mutate(race1 = ifelse(race1 == 'D', 'C', race1))

# Adjust ASA class if needed
bup_case <- bup_case %>%
  mutate(asaclas = ifelse(asaclas == 1, 2, asaclas))

# Add a year column based on surgery date
bup_case <- bup_case %>%
  mutate(yr = year(oprymd))

# Load and prepare PSS dataset
pss <- read_csv('./PSS_v3.csv', lazy = FALSE) %>%
  mutate(across(PRNCPTX, as.character))

# Join PSS data with case and control groups
bup_case <- inner_join(bup_case, pss, by = 'PRNCPTX')
control <- inner_join(control, pss, by = 'PRNCPTX')

# Summarize cases for matching
case_summary <- bup_case %>%
  group_by(asaclas, sex, age, race1, yr, smoke, PSS) %>%
  summarise(n = n(), .groups = 'drop')

# Initialize control lists for matching
control_list <- data.frame(matrix(ncol = ncol(control), nrow = 0))
names(control_list) <- names(control)

control_list_none <- case_summary[0,]
control_list_partial <- case_summary[0,]

control <- control %>% mutate(used = 0)

# Match control cases based on defined criteria
for (i in seq_len(nrow(case_summary))) {
  case <- case_summary[i, ]
  
  matched_controls <- control %>%
    filter(
      asaclas == case$asaclas,
      sex == case$sex,
      race1 == case$race1,
      between(age, case$age - 5, case$age + 5),
      PSS == case$PSS,
      smoke == case$smoke,
      between(yr, case$yr - 1, case$yr + 1)
    )
  
  if (nrow(matched_controls) == 0) {
    print(paste(i, 'no match'))
    control_list_none <- bind_rows(control_list_none, case)
  } else if (nrow(matched_controls) < case$n * 3) {
    print(paste(i, 'partial match', nrow(matched_controls), 'of', case$n * 3))
    control_list_partial <- bind_rows(control_list_partial, case)
    control_list <- bind_rows(control_list, matched_controls)
  } else {
    print(paste(i, 'full match'))
    sampled_controls <- matched_controls[sample(nrow(matched_controls), case$n * 3), ]
    control_list <- bind_rows(control_list, sampled_controls)
    control <- control %>% mutate(used = ifelse(scrssn %in% sampled_controls$scrssn, 1, used))
  }
}

# Filter out initial empty rows from control lists
control_list <- control_list %>% filter(!is.na(scrssn))
control_list_none <- control_list_none %>% filter(!is.na(asaclas))
control_list_partial <- control_list_partial %>% filter(!is.na(asaclas))

# Format scrssn with leading zeros and save datasets if necessary
control_list <- control_list %>%
  mutate(scrssn_tmp = scrssn, scrssn = sprintf('%09i', scrssn))

if (FALSE) {
  write_rds(control_list, 'control_list.rds')
  control_list <- read_rds('control_list.rds')
  write_rds(bup_case, 'bup_case_tmp.rds')
  bup_case <- read_rds('bup_case_tmp.rds')
}

if (FALSE) {
  control_list %>% select(scrssn, oprymd) %>% write_csv('control_list.csv')
}
