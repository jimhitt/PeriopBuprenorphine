# Load necessary libraries
library(dplyr)
library(stringr)
library(ggplot2)

# Load datasets
bup_all <- read_rds('PATHNAME/bup_all3.rds')
rx_ip <- read_rds('PATHNAME/rx_ip_raw.rds')

# Summarize date differences in rx_ip dataset
summary(rx_ip$date_diff)

# Fix date_diff by merging with bup_all data and calculating new date_diff
rx_ip <- inner_join(rx_ip, select(bup_all, PatientICN, idx, oprymd, opdt), by = c('PatientICN', 'oprymd')) %>%
  mutate(date_diff = as.numeric(difftime(ActionDateTime, opdt, units = 'days')))

# Filter rx_ip for date differences between -30 and 30 days
rx_ip <- rx_ip %>%
  filter(between(date_diff, -30, 30))

rx_ip %>% head(100) %>% View()

# Temporary variable for route of administration processing
tmp <- rx_ip

# Clean up MedicationRoute column
tmp %>% count(MedicationRoute) %>%
  arrange(desc(n)) %>%
  print(n = 100)

if (1) {
  # Initialize new column for cleaned routes
  tmp$MedRoute2 <- NA
  
  # Define regex patterns for various routes of administration
  oral_pattern <- '(subling|oral|mouth|tube|nostril|peg|buccal|po|tongue)'
  transdermal_pattern <- '(transderm|topical|skin|patch|trunk|externally)'
  rectal_pattern <- '(rectal|rectum)'
  iv_pattern <- '(intraven|iv|intramusc|subcutan|ivp|pca|patient controlled)'
  epidural_pattern <- 'epidural'
  intrathecal_pattern <- '(intrathec)'
  im_sq_pattern <- '(intramuscular|subcutaneous|sc|im inj)'
  
  # Apply patterns to classify routes
  tmp[str_detect(tmp$MedicationRoute, oral_pattern) & !is.na(tmp$MedicationRoute), 'MedRoute2'] <- 'oral'
  tmp[str_detect(tmp$MedicationRoute, transdermal_pattern) & !is.na(tmp$MedicationRoute), 'MedRoute2'] <- 'transdermal'
  tmp[str_detect(tmp$MedicationRoute, rectal_pattern) & !is.na(tmp$MedicationRoute), 'MedRoute2'] <- 'rectal'
  tmp[str_detect(tmp$MedicationRoute, iv_pattern) & !is.na(tmp$MedicationRoute), 'MedRoute2'] <- 'iv'
  tmp[str_detect(tmp$MedicationRoute, epidural_pattern) & !is.na(tmp$MedicationRoute), 'MedRoute2'] <- 'epidural'
  tmp[str_detect(tmp$MedicationRoute, intrathecal_pattern) & !is.na(tmp$MedicationRoute), 'MedRoute2'] <- 'intrathecal'
  tmp[str_detect(tmp$MedicationRoute, im_sq_pattern) & !is.na(tmp$MedicationRoute), 'MedRoute2'] <- 'im/sq'
  
  # Further classify based on LocalDrugNameWithDose if MedRoute2 is still NA
  if (0) {
    tmp %>% filter(is.na(MedRoute2)) %>%
      count(MedicationRoute) %>%
      arrange(desc(n)) %>%
      print(n = 200)
    
    tmp %>% filter(is.na(MedRoute2)) %>%
      select(LocalDrugNameWithDose, MedicationRoute, MedRoute2) %>%
      print(n = 100)
  }
  
  # Use LocalDrugNameWithDose to fill in missing route classifications
  inj_pattern <- '(inj|syr|tubex|carpuject)'
  oral_sol_pattern <- '(tab|oral|sol|solution|film|subling| sl | cup)'
  specific_oral_drugs <- '(hydrocodone|oxycodone|oramorph)'
  
  tmp[str_detect(tmp$LocalDrugNameWithDose, inj_pattern) & is.na(tmp$MedRoute2), 'MedRoute2'] <- 'iv'
  tmp[str_detect(tmp$LocalDrugNameWithDose, oral_sol_pattern) & is.na(tmp$MedRoute2), 'MedRoute2'] <- 'oral'
  tmp[str_detect(tmp$LocalDrugNameWithDose, specific_oral_drugs) & is.na(tmp$MedRoute2), 'MedRoute2'] <- 'oral'
  
  # Examine unclassified drugs
  tmp %>% filter(is.na(MedRoute2)) %>%
    count(LocalDrugNameWithDose) %>%
    arrange(desc(n)) %>%
    print(n = 200)
  
  # Clean up and finalize tmp dataset
  tmp <- tmp %>%
    select(-MedicationRoute) %>%
    rename(MedRoute = MedRoute2) %>%
    filter(!is.na(MedRoute))
  rx_ip <- tmp
  rm(tmp)
}

# Separate oral medications into tablets and solutions
solution_pattern <- '(ml|soln|solution|inj|exlixir|liquid)'

rx_ip_tab <- rx_ip %>%
  filter(MedRoute == 'oral' & !str_detect(LocalDrugNameWithDose, solution_pattern))
rx_ip_soln <- rx_ip %>%
  filter(MedRoute == 'oral' & str_detect(LocalDrugNameWithDose, solution_pattern))

# Processing oral tablet medications ======================================
df <- rx_ip_tab

opioid_patterns <- c('morphine', 'hydrocodone', '(oxycod|percocet)', 'codeine', 'methadone', 
                     'hydromorphone', 'tramadol', '(buprenor|bupren|suboxone)', 'fentanyl', 'oxymorphone')

# Replace "1/2" with ".5" in dosage
df <- df %>%
  mutate(LocalDrugNameWithDose = str_replace(LocalDrugNameWithDose, '\\s*1\\/2', '\\.5'))

# Extract dose information
df <- get_dose(opioid_patterns, df, T)

# Filter out entries with missing strength values
df <- df %>% filter(!is.na(Strength))

# Correct drug names based on identified patterns
opioid_names <- tibble(
  search_name = opioid_patterns,
  drug_name = c('morphine', 'hydrocodone', 'oxycodone', 'codeine', 'methadone',
                'hydromorphone', 'tramadol', 'buprenorphine', 'fentanyl', 'oxymorphone')
)

# Update Drug names
for (i in 1:nrow(opioid_names)) {
  pattern <- opioid_names$search_name[i]
  name <- opioid_names$drug_name[i]
  df <- df %>% mutate(Drug = ifelse(Drug == pattern, name, Drug))
}

# Specific adjustments for dosages
df <- df %>%
  mutate(
    Strength = case_when(
      Drug == 'buprenorphine' & Strength > 12 ~ Strength / 1000,
      Drug == 'oxycodone' & Strength == 325 ~ 5,
      Drug == 'codeine' & Strength == 300 ~ 30,
      TRUE ~ Strength
    )
  )

# Join with MED conversion factors and calculate MED given
df <- left_join(df, get_MED(), by = 'Drug') %>%
  mutate(
    MED = case_when(
      Drug == 'buprenorphine' & Strength >= 25 ~ .03,
      Drug == 'buprenorphine' & Strength < 25 ~ 30,
      Drug == 'methadone' ~ 4.7,
      TRUE ~ MED
    ),
    MEDgiven = DosesGiven * Strength * MED
  )

rx_ip_tab <- df

# Processing oral solutions ===============================================
df <- rx_ip_soln
opioid_patterns_sol <- c('morphine', '(hydroco|lortab)', '(oxycod|percocet|roxicet)', 'codeine', 'methadone', 'hydromorphone')

# Extract dose information for solutions
df <- get_dose_soln(opioid_patterns_sol, df, return_no_match = TRUE)

# Correct drug names
opioid_names_sol <- tibble(
  search_name = opioid_patterns_sol,
  drug_name = c('morphine', 'hydrocodone', 'oxycodone', 'codeine', 'methadone', 'hydromorphone')
)

for (i in 1:nrow(opioid_names_sol)) {
  pattern <- opioid_names_sol$search_name[i]
  name <- opioid_names_sol$drug_name[i]
  df <- df %>% mutate(Drug = ifelse(Drug == pattern, name, Drug))
}

# Merge with MED conversion factors and calculate MED given
df <- left_join(df, get_MED(), by = 'Drug') %>%
  mutate(
    MED = ifelse(Drug == 'methadone', 4.7, MED),
    DosesGiven = replace_na(DosesGiven, 0),
    MEDgiven = DosesGiven * Dose * MED
  )

rx_ip_soln <- df %>% select(-Strength) %>%
  rename(Strength = Dose)

# Combine oral tablets and solutions into a single dataset
rx_ip_oral <- bind_rows(rx_ip_tab, rx_ip_soln)

# Save combined oral dataset if necessary
if (FALSE) {
  write_rds(rx_ip_oral, 'PATHNAME/rx_ip_oral.rds')
  rx_ip_oral <- read_rds('PATHNAME/rx_ip_oral.rds')
}

# Processing intravenous (IV) medications =================================
rx_ip_iv <- rx_ip %>%
  filter(MedRoute == 'iv')

df <- rx_ip_iv
iv_opioids <- c('morphine', 'hydromorphone', 'fentanyl', 'methadone', 'buprenorphine')

df <- get_dose_soln(iv_opioids, df, return_no_match = TRUE)

# Fix fentanyl dose in mg and merge with MED conversion factors
df <- df %>%
  mutate(
    Dose = ifelse(Drug == 'fentanyl' & Strength > 1, Dose / 1000, Dose),
    MED = case_when(
      Drug == 'morphine' ~ 3,
      Drug == 'hydromorphone' ~ 40,
      Drug == 'fentanyl' ~ 150,
      Drug == 'methadone' ~ 4.7,
      TRUE ~ MED
    )
  ) %>%
  select(-Strength) %>%
  rename(Strength = Dose) %>%
  mutate(
    DosesGiven = replace_na(DosesGiven, 0),
    MEDgiven = Strength * DosesGiven * MED
  )

# Combine IV and oral datasets into a single dataset
rx_ip_iv <- df %>% rename(MEDgiven = MEDgiven)
rx_ip_all <- bind_rows(rx_ip_oral, rx_ip_iv)

# Include additional variables and fix missing values
rx_ip_all <- inner_join(rx_ip_all, select(bup_all, idx, late), by = 'idx') %>%
  mutate(DosesGiven = replace_na(DosesGiven, 0))

# Save final dataset for IP oral and IV if necessary
if (FALSE) {
  write_rds(rx_ip_all, 'PATHNAME/rx_ip_all.rds')
  rx_ip_all <- read_rds('PATHNAME/rx_ip_all.rds')
}

# Recalculate the daily dose and summarize
rx_ip_all %>%
  filter(!is.na(MEDgiven)) %>%
  group_by(MedRoute, Drug) %>%
  summarise(
    count = n(),
    dose_Q1 = quantile(MEDgiven / MED, probs = .25),
    dose_median = median(MEDgiven / MED),
    dose_Q3 = quantile(MEDgiven / MED, probs = .75),
    dose_min = min(MEDgiven / MED),
    dose_max = max(MEDgiven / MED)
  ) %>%
  print(n = 100)

# Further processing for IP opioids over time =============================
ip_all <- rx_ip_all %>%
  filter(between(date_diff, -60, 60)) %>%
  mutate(DailyMED = MEDgiven, DaysSupply = 1, QtyNumeric = 1) %>%
  select(idx, PatientICN, oprymd, type, MedRoute, date_diff, DaysSupply, QtyNumeric, DailyMED, Drug) %>%
  group_by(PatientICN, oprymd, type, MedRoute, Drug) %>%
  nest()

ip_all <- opioid_calc_by_time(ip_all, by_week = FALSE, n_days = 30)
ip_all <- inner_join(ip_all, select(bup_all, PatientICN, oprymd, late), by = c('PatientICN', 'oprymd'))

# Combine IP and OP datasets and plot
tmp <- ip_all %>%
  unnest(cols = c(data)) %>%
  filter(between(date_diff, -28, 27))

tmp2 <- rx_by_day %>%
  unnest(cols = c(data)) %>%
  filter(between(date_diff, -28, 27))

tmp <- bind_rows(tmp, tmp2)

# Visualization and aggregation
tmp %>%
  filter(late != 'control') %>%
  ggplot(aes(date_diff, dose, color = late)) +
  geom_line() +
  geom_vline(xintercept = -.5) +
  facet_wrap(~ Drug, scale = 'free_y')

s <- bup_all %>%
  group_by(late) %>%
  summarise(n = length(unique(PatientICN)))

for (i in 1:nrow(s)) {
  tmp <- tmp %>%
    mutate(dose = ifelse(late == s$late[i], dose / s$n[i], dose))
}

p <- tmp %>%
  filter(between(date_diff, -14, 27)) %>%
  ggplot(aes(date_diff, dose, color = late)) +
  geom_line(size = 1) +
  geom_vline(xintercept = -.5) +
  facet_wrap(~ Drug, scale = 'free_y', ncol = 2) +
  labs(y = 'Normalized Dose (MED / n in cohort)', x = 'Days from Surgery')

p

ggsave('PATHNAME/results/opioid_by_day_JCA.jpg', p, width = 6, height = 9, units = 'in', dpi = 300)

# Final save operations ===================================================
if (FALSE) {
  rx_ip_iv <- df
  rx_ip_pca <- df_pca
  
  write_rds(ip_all, 'PATHNAME/ip_opioid_by_day.rds')
  ip_all <- read_rds('PATHNAME/ip_opioid_by_day.rds')
  
  write_rds(rx_ip, 'PATHNAME/rx_ip.rds')
  rx_ip <- read_rds('PATHNAME/rx_ip.rds')
  
  write_rds(rx_ip_iv, 'PATHNAME/rx_ip_iv.rds')
  rx_ip_iv <- read_rds('PATHNAME/rx_ip_iv.rds')
  
  write_rds(rx_ip_pca, 'PATHNAME/rx_ip_pca.rds')
  rx_ip_pca <- read_rds('PATHNAME/rx_ip_pca.rds')
}
