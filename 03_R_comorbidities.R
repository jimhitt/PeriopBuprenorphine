# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)

# Load datasets
bup_case <- read_rds('PATHNAME/bup_case_tmp.rds')
bup_ctrl <- read_rds('PATHNAME/bup_ctrl.rds')

# Check for multiple entries in bup_case ----------------------------------

# Group by patient ID (scrssn) and operation date (oprymd) to identify duplicates
bup_case_check <- bup_case %>%
  group_by(scrssn, oprymd) %>%
  summarise(n = n(), opdt = min(opdt))

# No duplicate entries in bup_case
nrow(filter(bup_case_check, n > 1))

# Check for multiple entries in bup_ctrl ----------------------------------

# Similar process for bup_ctrl to find duplicates and calculate delta (time difference)
bup_ctrl_check <- bup_ctrl %>%
  group_by(scrssn, oprymd) %>%
  summarise(n = n(), delta = as.numeric(difftime(max(opdt), min(opdt), units = 'days')))

# Find patients with reoperation in bup_ctrl
nrow(filter(bup_ctrl_check, n > 1))

# Filter for unique entries only
bup_ctrl_unique <- bup_ctrl_check %>% filter(n == 1)

# Merge filtered control data back with original dataset
bup_ctrl <- inner_join(bup_ctrl, bup_ctrl_unique, by = c('scrssn', 'oprymd'))

# Remove temporary columns
bup_ctrl <- bup_ctrl %>% select(-n, -delta)

# Load PSS data and merge with bup_ctrl -----------------------------------

pss <- read_csv('PATHNAME/PSS_V3.csv', lazy = FALSE)
pss <- pss %>% mutate(across(PRNCPTX, as.character))

# Merge PSS data
bup_ctrl <- inner_join(bup_ctrl, pss, by = 'PRNCPTX')

# Add year and race columns to datasets -----------------------------------

bup_ctrl <- bup_ctrl %>% mutate(yr = year(oprymd))
bup_case <- bup_case %>% mutate(yr = year(oprymd))

# Adjust race column to group unknowns ('D') with declined ('C')
bup_ctrl <- bup_ctrl %>%
  mutate(race1 = ifelse(race1 == 'D', 'C', race1))

# Create race lookup table for explanation
race_lookup <- tibble(
  race1 = c('3', '8', '9', 'C', 'A', 'B'),
  race = c('AmericanIndian/Alaskan', 'Asian', 'Black', 'Declined/Unk', 'PacificIsland', 'White')
)

# Merge race explanations
bup_ctrl <- inner_join(bup_ctrl, race_lookup, by = 'race1')
bup_case <- inner_join(bup_case, race_lookup, by = 'race1')
rm(race_lookup)

# Remove unnecessary columns in bup_case ----------------------------------

bup_case <- bup_case %>% select(-t_delta, -new_pt)

# Combine case and control datasets ---------------------------------------

bup_case <- bup_case %>% mutate(type = 'case')
bup_ctrl <- bup_ctrl %>% mutate(type = 'ctrl')

bup_all <- bind_rows(bup_case, bup_ctrl)

bup_all %>% count(asaclas)

# Save/load datasets if necessary -----------------------------------------

if (FALSE) {
  write_rds(bup_ctrl, 'PATHNAME/bup_ctrl_final.rds')
  write_rds(bup_case, 'PATHNAME/bup_case_final.rds')
  write_rds(bup_all, 'PATHNAME/bup_all.rds')
  
  bup_ctrl <- read_rds('PATHNAME/bup_ctrl_final.rds')
  bup_case <- read_rds('PATHNAME/bup_case_final.rds')
  bup_all <- read_rds('PATHNAME/bup_all.rds')
}

# Load and prepare all diagnoses data -------------------------------------

all_dx <- read_rds('PATHNAME/all_dx.rds')

# Arrange and create index for patient matching
bup_all <- bup_all %>% arrange(PatientICN, oprymd)
bup_all$idx <- 1:nrow(bup_all)

idx_lookup <- bup_all %>% select(idx, PatientICN, oprymd)

all_dx <- inner_join(all_dx, idx_lookup, by = c('PatientICN', 'oprymd'))

# Filter diagnosis codes within a specific time window for comorbidity calculation
case_dx <- all_dx %>%
  filter(between(delta, -365, -1)) %>%
  group_by(idx, oprymd, ICD_type, Code) %>%
  summarise(n = n(), .groups = 'drop')

# Calculate comorbidities using a custom function (e.g., calc_comorbidity2)
elix_case <- calc_comorbidity2(case_dx, FALSE)

bup_all_dx <- left_join(select(bup_all, idx, PatientICN, oprymd, type), elix_case, by = 'idx')

# Save/load bup_all_dx if necessary ---------------------------------------

if (FALSE) {
  write_rds(bup_all_dx, 'PATHNAME/bup_all_dx.rds')
  bup_all_dx <- read_rds('PATHNAME/bup_all_dx.rds')
  
  write_rds(bup_all, 'PATHNAME/bup_all.rds')
  bup_all <- read_rds('PATHNAME/bup_all.rds')
}

# Examine comorbidities ---------------------------------------------------

tibble(
  n = 1:ncol(bup_all_dx),
  name = colnames(bup_all_dx)
) %>% print(n = 100)

elix_res <- tibble(
  names = colnames(bup_all_dx)[5:35],
  case_n = NA,
  case_y = NA,
  ctrl_n = NA,
  ctrl_y = NA,
  p_value = NA
)

# Perform chi-square tests for each comorbidity and populate results
for (i in 5:35) {
  t <- table(bup_all_dx[[4]], bup_all_dx[[i]])
  res <- chisq.test(t)
  elix_res[[2]][[i-4]] <- t[1,1]
  elix_res[[3]][[i-4]] <- t[1,2]
  elix_res[[4]][[i-4]] <- t[2,1]
  elix_res[[5]][[i-4]] <- t[2,2]
  elix_res[[6]][[i-4]] <- res$p.value
}
elix_res$p_adjust <- p.adjust(elix_res$p_value, method = 'bonferroni')

# Save the results to a CSV file if necessary
if (FALSE) {
  write_csv(elix_res, 'PATHNAME/elix_res.csv')
}

# Calculate statistics for weighted scores (wscore_vw) by type (case/control)
bup_all_dx %>%
  filter(!is.na(wscore_vw)) %>%
  group_by(type) %>%
  summarise(mean = mean(wscore_vw),
            sd = sd(wscore_vw),
            median = median(wscore_vw),
            iqr = IQR(wscore_vw),
            n = n())

# Perform t-test on weighted scores between cases and controls
t.test(
  filter(bup_all_dx, !is.na(wscore_vw) & type == 'case')$wscore_vw,
  filter(bup_all_dx, !is.na(wscore_vw) & type == 'ctrl')$wscore_vw
)

# Repeat comparison for "continued" vs. "late" groups ======================
# Merge datasets and filter for cases only, then convert 'late' to factor
df <- inner_join(bup_all_dx, select(bup_all, PatientICN, oprymd, late), by = c('PatientICN', 'oprymd')) %>%
  filter(type == 'case') %>%
  mutate(late = factor(late, levels = c('continued', 'late')))

# Summary statistics by 'late' status
df %>%
  group_by(late) %>%
  summarise(mean = mean(wscore_vw),
            sd = sd(wscore_vw),
            median = median(wscore_vw),
            iqr = IQR(wscore_vw),
            n = n())

# Perform t-test between "continued" and "late" groups
t.test(
  filter(df, late == 'continued')$wscore_vw,
  filter(df, late == 'late')$wscore_vw
)

# Chi-square tests for comorbidities =======================================
elix_res <- tibble(
  names = colnames(df)[4:35],
  case_n = NA, case_y = NA, ctrl_n = NA, ctrl_y = NA, p_value = NA
)

for (i in 4:35) {
  t <- table(df[[36]], df[[i]])
  res <- chisq.test(t)
  if (ncol(t) == 2) {
    elix_res[[2]][[i-3]] <- t[1,1]
    elix_res[[3]][[i-3]] <- t[1,2]
    elix_res[[4]][[i-3]] <- t[2,1]
    elix_res[[5]][[i-3]] <- t[2,2]
    elix_res[[6]][[i-3]] <- res$p.value
  } else {
    elix_res[[2]][[i-3]] <- 0
    elix_res[[3]][[i-3]] <- t[1]
    elix_res[[4]][[i-3]] <- 0
    elix_res[[5]][[i-3]] <- t[2]
    elix_res[[6]][[i-3]] <- 1
  }
}

elix_res$p_adjust <- p.adjust(elix_res$p_value, 'bonferroni')
print(elix_res, n = 100)

# Additional comparisons for smoking and gender ============================
# Compare smoking status by type
t <- table(bup_all$type, bup_all$smoke)
res <- chisq.test(t)
elix_res_tmp <- tibble(
  names = 'smoking', 
  case_n = t[1,1], case_y = t[1,2], 
  ctrl_n = t[2,1], ctrl_y = t[2,2], 
  p_value = res$p.value
)
elix_res <- bind_rows(elix_res, elix_res_tmp)

# Compare gender by type
t <- table(bup_all$type, bup_all$sex)
res <- chisq.test(t)
elix_res_tmp <- tibble(
  names = 'gender', 
  case_n = t[1,1], case_y = t[1,2], 
  ctrl_n = t[2,1], ctrl_y = t[2,2], 
  p_value = res$p.value
)
elix_res <- bind_rows(elix_res, elix_res_tmp)

# Save results to CSV file
write_csv(elix_res, 'PATHNAME/elix_res.csv')

# Further demographic and clinical comparisons ============================

# ASA classification by type
t <- table(bup_all$type, bup_all$asaclas)
prop.table(t, margin = 1)
chisq.test(t)

# Race distribution by type
t <- table(bup_all$type, bup_all$race)
prop.table(t, margin = 1)
chisq.test(t)

# Age comparison between cases and controls
t.test(
  filter(bup_all, type == 'case')$age,
  filter(bup_all, type == 'ctrl')$age
)

# Summary statistics for age by type
bup_all %>%
  group_by(type) %>%
  summarise(mean = mean(age), sd = sd(age), median = median(age))

# PSS distribution by type
t <- table(bup_all$type, bup_all$PSS)
round(prop.table(t, 1) * 10000) / 100
chisq.test(t)

# Other demographics (smoking, age, ASA class, race, surgical specialty) ===

# Smoking status by type
t <- table(bup_all$type, bup_all$smoke)
round(prop.table(t, margin = 1) * 10000) / 100
chisq.test(t)

# ASA class distribution by type
t <- table(bup_all$asaclas, bup_all$type)
round(prop.table(t, margin = 2) * 10000) / 100
chisq.test(t)

# Race distribution by type
t <- table(bup_all$race, bup_all$type)
round(prop.table(t, margin = 2) * 10000) / 100
chisq.test(t)

# Surgical specialty distribution by type
if (FALSE) {
  surg_spec <- tibble(
    speccode = as.character(c(50, 500, 501, 51, 52, 53, 54, 541, 55, 56, 57, 58,
                              59, 591, 60, 61, 62, 65, 97)),
    surg_spec = c('general_surgery', 'cardiac surgery', 'transplant surgery', 'gynecology',
                  'neurosurgery', 'opthalmology', 'orthopedic surgery', 'hand surgery',
                  'otolaryngology', 'plastic surgery', 'proctology', 'thoracic surgery',
                  'urology', 'kidney transplant', 'oral surgery', 'podiatry', 'peripheral vascular surgery', 
                  'spinal cord surgery', 'hand surgery')
  )
  bup_all <- left_join(bup_all, surg_spec, by = 'speccode')
}

# Surgical specialty chi-square test
t <- table(bup_all$surg_spec, bup_all$type)
round(prop.table(t, margin = 2) * 10000) / 100
chisq.test(t)

# Load/save datasets ======================================================
if (FALSE) {
  write_rds(bup_all, 'PATHNAME/bup_all.rds')
  bup_all <- read_rds('PATHNAME/bup_all.rds')
}

# Outcome analysis ========================================================
df <- bup_all %>%
  mutate(PSS = factor(PSS),
         type = factor(type, levels = c('ctrl', 'case')),
         asaclas = factor(asaclas))

df <- left_join(df, select(bup_all_dx, PatientICN, oprymd, wscore_vw), by = c('PatientICN', 'oprymd')) %>%
  mutate(wscore_fct = cut(wscore_vw, breaks = c(-20, -7, 0, 5, 12, 50)))

# Print column names and indices
tibble(idx = 1:ncol(df), name = colnames(df)) %>% print(n = 50)

# Calculate VASQIP outcomes ===============================================
for (i in c(22, 26, 31, 33, 35, 40, 41, 32)) {
  t <- table(df[[45]], df[[i]])
  print(paste('Analyzing:', names(df[i])))
  print(t)
  print(round(prop.table(t, margin = 1) * 10000) / 100)
  res <- chisq.test(t)
  print(res)
}

# Logistic regression (LR) for outcomes ====================================
outcome_vars <- c('failwean', 'returnor', 'reintub', 'oupneumo', 'wndinfd', 'POSTCODE')
for (outcome in outcome_vars) {
  model <- glm(reformulate(c('type', 'OSS', 'wscore_fct'), outcome), family = 'binomial', data = df)
  print(summary(model))
}

# Analysis of length of stay (LOS) variables ===============================
df <- bup_all %>%
  mutate(pacu_time = as.numeric(difftime(pacu, tpatout, units = 'hours')))

# Print column names and indices
tibble(idx = 1:ncol(df), name = colnames(df)) %>% print(n = 50)

# Handle outliers in LOS variables
los_vars <- c('TOTHLOS', 'TOTSLOS', 'pacu_time')
for (var in los_vars) {
  d <- df %>% pull(var) %>% na.omit()
  upper_bound <- median(d) + 1.5 * IQR(d)
  df <- df %>%
    mutate(!!var := ifelse(!!sym(var) > upper_bound, ceiling(upper_bound) + 1, !!sym(var)))
}

# Set negative pacu_time to NA
df <- df %>% mutate(pacu_time = ifelse(pacu_time < 0, NA, pacu_time))

# Visualize LOS variables
df %>%
  pivot_longer(cols = c('pacu_time', 'TOTHLOS', 'TOTSLOS'), names_to = 'LOS_var', values_to = 'LOS_time') %>%
  group_by(type, LOS_var) %>%
  summarise(n = n(), mean = mean(LOS_time, na.rm = TRUE), sd = sd(LOS_time, na.rm = TRUE)) %>%
  ggplot(aes(type, mean)) +
  geom_col(fill = 'steelblue') +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .4) +
  facet_wrap(~ LOS_var, nrow = 1)

# t-tests for LOS variables by type
for (var in los_vars) {
  print(t.test(filter(df, type == 'case')[[var]], filter(df, type == 'ctrl')[[var]]))
}
