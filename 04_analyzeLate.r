# new steps
# 1. split the pre and post outpatient rx
# 2. Find patients with inpatient rx
# 3. Determine the buprenorphine managment (continued vs. late)
# 4. Query postop rx SIG for holding instructions
# 5. Examine timeliness of postop prescription
#      - look at patients with long hospital admission
# 6. Calculate pre and postop rx_by_day
# saves bup_all4.rds


# calculate timely and late -----------------------------------------------
bup_all <- read_rds('bup_all3.rds')
# from 01_opRx_Dose.R
rx_op <- read_rds('rx_out_process.rds')
# from 03_ipRx_Dose
rx_ip_all <- read_rds('rx_ip_all.rds')



bup_bup = bup_all %>%
  filter(type == 'case')

# preop buprenorphine ---------------------------------------------------------------

rx_op = inner_join(
  rx_op,
  select(bup_bup, idx, PatientICN, oprymd),
  by = c('PatientICN', 'oprymd')
)

# find preop bup dose closest to surgery ----------------------------------

df <- rx_op %>%
  filter(Drug == 'buprenorphine' & MedRoute %in% c('oral', 'iv') &
           between(date_diff, -35, 0))

df$DailyDoseMG <- df$DailyDose * df$Strength

# filter out low daily dose
df = df %>%
  filter(DailyDoseMG > 0.5)
summary(df$DailyDoseMG)

df_group = df %>%
  filter(between(date_diff, -35, -1)) %>%
  group_by(idx, oprymd, DispensedDate) %>%
  summarise(DailyDoseMG = sum(DailyDoseMG),
            DaysSupply = max(DaysSupply))

df_preop <- df_group %>%
  group_by(idx) %>%
  summarise(DispensedDate = max(DispensedDate))


# join to get the last dose and duration
df_preop = left_join(
  df_preop,
  select(df, idx, DispensedDate, oprymd, Drug, DailyDoseMG, DaysSupply),
  by = c("idx", "DispensedDate")
)


# filter and group
df_preop = df_preop %>%
  filter(Drug == 'buprenorphine') %>%
  group_by(idx) %>%
  summarise(DispensedDate = max(DispensedDate),
            bup_dose = max(DailyDoseMG))

summary(df_preop)

#join the bup_dose on the bup_bup outcome table
bup_bup$bup_dose_old = bup_bup$bup_dose
bup_bup = bup_bup %>% select(-bup_dose)
df <- left_join(bup_bup, df_preop, by = c('idx'))    #, 'oprymd'))

df %>% 
  mutate(bup_dose = floor(bup_dose)) %>%
  count(type, bup_dose) %>% print(n = 100)

df_missing <- df %>%
  filter(is.na(bup_dose))
df_missing <- df_missing$idx

# remove cases with missing preop BUP data
rx_op %>% 
  filter(idx %in% df_missing & date_diff > 0) %>%
  arrange(idx, date_diff) %>%
  select(idx, PatientICN, date_diff, Drug, MedRoute, Sig, DailyDose, Strength, QtyNumeric, DaysSupply) %>%
  view()

# remove cases
df = df %>% filter(! idx %in% df_missing)
df = df %>% select(-bup_dose_old)
#bup_all_dx = bup_all_dx %>% filter(! PatientICN %in% df_missing)

bup_bup <- df


# calculate late postop dosing --------------------------------------------
# 1. Find patients with inpatient prescriptions
# 2. Look at Sig to find hold instructions
# 3. Analyze the timeliness of outpatient rx
df = bup_bup

df$late_tmp = df$late
df$late = NA

# look at inpatient prescription --------------------
rx_tmp = rx_ip_all %>%
  filter(Drug == 'buprenorphine' &
           between(date_diff, 0, 7))

rx_tmp = inner_join(
  rx_tmp,
  select(bup_bup, idx, PSS),
  by = "idx"
)

rx_tmp = rx_tmp %>%
  group_by(idx) %>%
  summarise(first_dose = min(date_diff),
            last_dose = max(date_diff),
            total_dose = sum(MEDgiven / MED),
            avg_dose = total_dose / (last_dose - first_dose))

rx_tmp = rx_tmp %>%
  filter(first_dose <= 1.5)

bup_bup$late1 = NA
bup_bup[bup_bup$idx %in% rx_tmp$idx, 'late1'] = 'continued'

# 107 patients were reclassified from late to continued
bup_bup %>% filter(late1 == 'continued') %>% count(late_tmp)

# look at outpatient rx Sig ---------------------

rx_tmp = inner_join(
  rx_op, 
  select(bup_bup, idx, late1),
  by = 'idx')

tmp = rx_tmp %>%
  filter(#Drug != 'buprenorphine' &
           str_detect(Sig, '(stop|(tapering|bridge|while|transition) off|weaning|discontinue|in place of|do not take with|withdrawal)') &
           str_detect(Sig, '(buprenorphine|suboxone)')) %>%
  mutate(s1 = str_extract(Sig, '(stop|(tapering|bridge|while|transition) off|weaning|discontinue|in place of|do not take with|withdrawal)'),
         s2 = str_extract(Sig, '(buprenorphine|suboxone)'))
view(tmp)

bup_bup$late2 = NA
bup_bup[bup_bup$idx %in% tmp$idx, 'late2'] = 'late'

bup_bup %>% filter(late2 == 'late') %>% count(late_tmp)
tmp = bup_bup %>% filter(!is.na(late1) & !is.na(late2))

rx_op %>% filter(idx %in% tmp$idx) %>% arrange(idx, date_diff) %>% view()

# calculate days late ----------------------------
df <- rx_op %>%
  filter(Drug == 'buprenorphine' & MedRoute %in% c('oral', 'iv') &
           between(date_diff, -35, 60)) %>%
  arrange(idx, oprymd, DispensedDate)

# fix multiple dispense on same day
df = df %>%
  group_by(idx, DispensedDate, oprymd, date_diff) %>%
  summarise(DailyDose = max(DailyDose),
            DaysSupply = max(DaysSupply)) %>%
  arrange(idx, DispensedDate)

# get dispensed date times
t <- df$DispensedDate
#shift down one and subtract to get delta
t_delta <- as.numeric(difftime(t[2:length(t)], t[1:length(t) - 1], units = 'days'))
t_delta <- c(t_delta, NA)

# repeat for idx
t <- df$idx
t_filter <- t[2 : length(t)] == t[1 : length(t) - 1]
t_filter <- c(t_filter, F)

# make 0 = NA
t_filter[t_filter == 0] <- NA

# make fill_delta column
tmp2 = t_filter * t_delta
df$fill_delta <- c(NA, tmp2[1:length(tmp2) - 1])

# make days_late column
tmp2 = (t_filter * t_delta) - df$DaysSupply
df$days_late <- c(NA, tmp2[1:length(tmp2) - 1])

# view output
if (0) {
  df %>% view(n = 100)
  
}

# find first rx after surgery
df_group <- df %>%
  filter(date_diff >= 0) %>%
  group_by(idx, oprymd) %>%
  summarise(date_diff = min(date_diff))

tmp <- left_join(bup_bup, df_group, by = c('idx', 'oprymd'))

tmp1 <- tmp %>%
  filter(!is.na(TOTHLOS))
tmp2 <- tmp %>%
  filter(is.na(TOTHLOS))


summary(tmp1$days_late)
tmp1$days_late <- tmp1$days_late - tmp1$TOTHLOS
summary(tmp1$days_late)
tmp <- rbind(tmp1, tmp2)
rm(tmp1, tmp2)

# create logical late column for an patient w/rx more than 1 day late
tmp$late3 <- tmp$days_late > 3

# make NA == TRUE
tmp[is.na(tmp$late3), 'late3'] <- T

# add late3 column
bup_bup$late3 = NA
tmp_late = tmp %>%
  filter(late3 == T)
bup_bup[bup_bup$idx %in% tmp_late$idx, 'late3'] = 'late'

bup_bup[bup_bup$idx %in% filter(tmp, late3 == F)$idx, 'late3'] = 'continued'


bup_bup %>% count(late1)

bup_bup$late = bup_bup$late3
bup_bup[bup_bup$late1 == 'continued' & !is.na(bup_bup$late1), 'late'] = 'continued'
bup_bup[bup_bup$late2 == 'late' & !is.na(bup_bup$late2), 'late'] = 'late'

bup_bup %>% count(late)

(t = table(bup_bup$type, bup_bup$late))
round(prop.table(t, 1) * 10000) / 100

(t = table(bup_bup$yr, bup_bup$late))
round(prop.table(t, 1) * 10000) / 100

(t = table(bup_bup$Complexity, bup_bup$late))
round(prop.table(t, 1) * 10000) / 100

# combine and save new bup_all
bup_bup = bup_bup %>%
  select(-late1, -late2, -late3, -DispensedDate)
bup_all$late = as.character(bup_all$late)

bup_all = bup_all %>%
  filter(type == 'ctrl')

bup_all = rbind(bup_all, bup_bup)
bup_all = bup_all %>%
  mutate(late = factor(late))

levels(bup_all$late)

write_rds(bup_all, 'bup_all4.rds')
bup_all = read_rds('bup_all4.rds')

