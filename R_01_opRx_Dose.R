
rx_op <- read_rds('rx_out_raw.rds')

# remove duplicate RxSID dispensed on same date ---------------------------

rx_op2 = rx_op %>%
  group_by(RxOutpatFillSID, DispensedDate) %>%
  summarise(PatientICN = min(PatientICN),
            oprymd = min(oprymd),
            type = min(type),
            QtyNumeric = max(QtyNumeric),
            DaysSupply = max(DaysSupply),
            LocalDrugNameWithDose = max(LocalDrugNameWithDose),
            Strength = max(Strength),
            MedRoute = max(MedRoute),
            Sig = max(Sig),
            # this code is used to verify duplicate entries were identical
            eopry = min(oprymd) != max(oprymd),
            etype = min(type) != max(type),
            eqty = min(QtyNumeric) != max(QtyNumeric),
            eds = min(DaysSupply) != max(DaysSupply),
            eld = min(LocalDrugNameWithDose) != max(LocalDrugNameWithDose),
            #estr = min(Strength) != max(Strength),
            eroute = min(MedRoute) != max(MedRoute),
            esig = min(Sig) != max(Sig)
  ) %>%
  ungroup() %>%
  relocate(RxOutpatFillSID, .after = type)

# check for true values
sum(rx_op2[,12:18], na.rm = T)

rx_op = rx_op2 %>%
  select(PatientICN, oprymd, type, DispensedDate, QtyNumeric, DaysSupply, LocalDrugNameWithDose, Strength, MedRoute, Sig)
rm(rx_op2)
rx_op = rx_op %>%
  mutate(date_diff = as.numeric(difftime(DispensedDate, oprymd, units = 'days')))

# select from +/- 60 days
rx_op <- rx_op %>%
  filter(between(date_diff, -60, 60))

# this routine combines the various MedRoute entries
# creates temporary column, MedRoute2 to allow error checking
# then it moves it to MeRoute column
if (1) {
  rx_op$MedRoute2 <- NA
  
  s <- '(subling|oral|mouth|tube|nostril|peg|buccal|po|tongue)'
  rx_op[str_detect(rx_op$MedRoute, s) & !is.na(rx_op$MedRoute), 'MedRoute2'] <- 'oral'
  s <- '(transderm|topical|skin|patch|trunk|externally)'
  rx_op[str_detect(rx_op$MedRoute, s) & !is.na(rx_op$MedRoute), 'MedRoute2'] <- 'transdermal'
  s <- '(rectal|rectum)'
  rx_op[str_detect(rx_op$MedRoute, s) & !is.na(rx_op$MedRoute), 'MedRoute2'] <- 'rectal'
  s <- '(intraven|iv|intramusc|subcutan|ivp)'
  rx_op[str_detect(rx_op$MedRoute, s) & !is.na(rx_op$MedRoute), 'MedRoute2'] <- 'iv'
  s <- '(intrathec)'
  rx_op[str_detect(rx_op$MedRoute, s) & !is.na(rx_op$MedRoute), 'MedRoute2'] <- 'intrathecal'
  
  # fix few missclassified
  s <- '(patch|transdermal)'
  rx_op[str_detect(rx_op$LocalDrugNameWithDose, s) & is.na(rx_op$MedRoute2), 'MedRoute2'] <- 'transdermal'
  
  s <- '(buprenorphine|oxycodone|morphine|hydrocodone)'
  rx_op[str_detect(rx_op$LocalDrugNameWithDose, s) & is.na(rx_op$MedRoute2), 'MedRoute2'] <- 'oral'
  
  # examine nulls in MedRout2
  if (0) {
    rx_op %>% filter(is.na(MedRoute2)) %>%
      select(LocalDrugNameWithDose, Sig, MedRoute, MedRoute2) %>%
      print(n = 100)
  }
  
  #remove Medroute and move MedRoute2
  rx_op <- rx_op %>% select(-MedRoute)
  rx_op <- rx_op %>% rename(MedRoute = MedRoute2)
  rm(s)
}

(t <- table(rx_op$type, rx_op$MedRoute))

round(prop.table(t, margin = 1) * 10000) / 100

s <- '(ml|soln|solution|inj|exlixir|liquid|elixir)'
rx_op_tab <- rx_op %>%
  filter(MedRoute == 'oral' & !str_detect(LocalDrugNameWithDose, s))
rx_op_soln <- rx_op %>%
  filter(MedRoute == 'oral' & str_detect(LocalDrugNameWithDose, s))

# look at results
if(0) {
  rx_op_tab %>%
    filter(str_detect(LocalDrugNameWithDose, '(hydrocod|oxycod|morph)')) %>%
    count(LocalDrugNameWithDose) %>%
    filter(n > 25) %>%
    arrange(desc(n)) %>%
    print(n = 200)
  
  rx_op_soln %>%
    filter(str_detect(LocalDrugNameWithDose, '(hydrocod|oxycod|morph)')) %>%
    count(LocalDrugNameWithDose) %>%
    arrange(desc(n)) %>%
    print(n = 200)
}

s_drugs <- c('morphine', 'hydrocodone', '(oxycod|percocet)', 'codeine', 'methadone', 
             'hydromorphone', 'tramadol', '(buprenor|bupren|suboxone)', 'fentanyl', 'oxymorphone')

rx_op_tab <- get_dose(s_drugs, rx_op_tab, T)

rx_op_tab %>%
  filter(is.na(Strength)) %>%
  group_by(LocalDrugNameWithDose) %>%
  summarise(n = n(),
            min_date = min(DispensedDate),
            max_date = max(DispensedDate)) %>%
  arrange(desc(n)) %>%
  print(n = 50)

rx_op_tab = rx_op_tab %>%
  filter(!is.na(Strength))

s_drugs <- tibble(
  search_name = s_drugs,
  drug_name = c('morphine', 'hydrocodone', 'oxycodone', 'codeine', 'methadone',
                'hydromorphone', 'tramadol', 'buprenorphine', 'fentanyl', 'oxymorphone')
)

# fix drug names
for (i in 1:nrow(s_drugs)) {
  s1 <- s_drugs[[1]][[i]]
  s2 <- s_drugs[[2]][[i]]
  rx_op_tab[rx_op_tab$Drug == s1, 'Drug'] <- s2
  rm(s1, s2)
}

# look at results
rx_op_tab %>%
  count(Drug, Strength) %>%
  arrange(Drug, Strength) %>%
  print(n = 100)


# fix codeine 300, codeine 4
rx_op_tab[rx_op_tab$Drug == 'codeine' & rx_op_tab$Strength == 300, 'Strength'] <- 30
rx_op_tab[rx_op_tab$Drug == 'codeine' & rx_op_tab$Strength == 4, 'Strength'] <- 60

# calculate daily dose (in tabs)
rx_op_tab$DailyDose <- rx_op_tab$QtyNumeric / rx_op_tab$DaysSupply
rx_op_tab <- left_join(rx_op_tab, get_MED(), by = 'Drug')

rx_op_tab$DailyMED <- rx_op_tab$DailyDose * rx_op_tab$Strength * rx_op_tab$MED


# fix buprenorphine > 25 (ie, Belbuca)
# fix Suboxone (conversion is 30 not 12.6 [which is for Butrans])
rx_op_tab[rx_op_tab$Drug == 'buprenorphine' & rx_op_tab$Strength >= 25, 'MED'] <- .03
rx_op_tab[rx_op_tab$Drug == 'buprenorphine' & rx_op_tab$Strength < 25, 'MED'] <- 30

rx_op_tab$DailyMED <- rx_op_tab$DailyDose * rx_op_tab$Strength * rx_op_tab$MED

rx_op_tab = rx_op_tab %>%
  filter(!is.na(Strength))

# look at high daily dose
if (0) {
  summary(rx_op_tab$DailyDose)
  # very high
  tmp <- rx_op_tab %>%
    filter(DailyDose > 12) %>%
    select(date_diff, type, QtyNumeric, DaysSupply, LocalDrugNameWithDose, Sig, Drug, Strength, MED, DailyDose, DailyMED) %>%
    view()
  # very low
  tmp <- rx_op_tab %>%
    filter(DailyDose < 1) %>%
    select(date_diff, type, QtyNumeric, DaysSupply, LocalDrugNameWithDose, Sig, Drug, Strength, MED, DailyDose, DailyMED) %>%
    view()
  rm(tmp)
  
}


# Get strength for solutions ---------------------------------------------------

# skipping methadone solution (methadone clinics)
# the documentation is not interpretable
s_drugs <- c('hydrocodone', 'oxycodone', 'codeine', 'morphine')

rx_op_soln <- get_dose_soln(s_drugs, rx_op_soln, T)

# look at missing
rx_op_soln %>%
  filter(is.na(Dose)) %>%
  count(LocalDrugNameWithDose) %>%
  arrange(desc(n)) %>% print(n = 100)

rx_op_soln <- rx_op_soln %>%
  filter(!is.na(Dose))



# calculate DailyDose and add MED column
rx_op_soln$DailyDose <- rx_op_soln$QtyNumeric / rx_op_soln$DaysSupply
rx_op_soln <- left_join(rx_op_soln, get_MED(), by = 'Drug')

rx_op_soln$DailyMED <- rx_op_soln$DailyDose * rx_op_soln$Strength * rx_op_soln$MED



# Look at transdermal ----------------------------------------------------------
rx_op_patch <- rx_op %>%
  filter(MedRoute == 'transdermal')

s_drugs <- c('fentanyl', 'buprenorphine')
rx_op_patch <- get_dose(s_drugs, rx_op_patch, T)

rx_op_patch = rx_op_patch %>%
  filter(!is.na(Strength))

rx_op_patch$DailyDose <- 1
rx_op_patch$MED <- NA
rx_op_patch[rx_op_patch$Drug == 'fentanyl', 'MED'] <- 2.4
rx_op_patch[rx_op_patch$Drug == 'buprenorphine' & rx_op_patch$Strength <= 20, 'MED'] <- 12.6

rx_op_patch$DailyMED <- rx_op_patch$DailyDose * rx_op_patch$Strength * rx_op_patch$MED
rx_op_patch <- rx_op_patch %>%
  filter(!is.na(DailyMED))

# Look at IV --------------------------------------------------------------------------
rx_op_iv = rx_op %>%
  filter(MedRoute == 'iv')

s_drugs <- c('morphine', 'hydromorphone', 'fentanyl', 'buprenorphine')

rx_op_iv <- get_dose_soln(s_drugs, rx_op_iv, T)

# just missing meperidine 
rx_op_iv %>%
  filter(is.na(Strength)) %>%
  count(LocalDrugNameWithDose) %>%
  arrange(desc(n)) %>% print(n = 100)

rx_op_iv = rx_op_iv %>%
  filter(! is.na(Strength))

# fix buprenorphine
rx_op_iv[rx_op_iv$Drug == 'buprenorphine', 'DaysSupply'] = 28
rx_op_iv[rx_op_iv$Drug == 'buprenorphine' &
           str_detect(rx_op_iv$LocalDrugNameWithDose, '300'), 'Strength'] = 300
rx_op_iv[rx_op_iv$Drug == 'buprenorphine' &
           str_detect(rx_op_iv$LocalDrugNameWithDose, '100'), 'Strength'] = 100


# add MED
rx_op_iv$MED <- NA
rx_op_iv[rx_op_iv$Drug == 'morphine', 'MED'] <- 3
rx_op_iv[rx_op_iv$Drug == 'hydromorphine', 'MED'] <- 40
# assume 100 mcg (0.1 mg) of fentanyl ~ 15 mg morphine
rx_op_iv[rx_op_iv$Drug == 'fentanyl', 'MED'] <- 150
rx_op_iv[rx_op_iv$Drug == 'methadone', 'MED'] <- 1
rx_op_iv[rx_op_iv$Drug == 'buprenorphine', 'MED'] <- 1

# fix fentanyl listed as mcg
rx_op_iv[rx_op_iv$Drug == 'fentanyl' &
           rx_op_iv$Strength > 1, 'Strength'] =
  rx_op_iv[rx_op_iv$Drug == 'fentanyl' &
             rx_op_iv$Strength > 1, 'Strength'] / 1000

# calculate DailyDose and add MED column
rx_op_iv$DailyDose <- rx_op_iv$QtyNumeric / rx_op_iv$DaysSupply
rx_op_iv$DailyMED <- rx_op_iv$DailyDose * rx_op_iv$Strength * rx_op_iv$MED


# End IV --------------------------------------------------------------------------

# Still have some errors in solution; need to put all tab and patch together
# skipping IV
rx_op <- rbind(rx_op_tab,
               rx_op_patch, select(rx_op_soln, -Dose)) #, select(rx_op_iv, -Dose))



# read/write ----------------------------------------------------------
if (0) {
  rf = './results_v1/'
  rm(rx_op_patch, rx_op_soln, rx_op_tab)
  rm(i, s, s_drugs)
  
  write_rds(rx_op, 'rx_out_process.rds')
  rx_op <- read_rds('rx_out_process.rds')
}

# prints summary table
# removed the MED conversion for dose range
rx_op %>%
  filter(between(date_diff, -7, 13)) %>%
  group_by(type, Drug) %>%
  summarise(count = n(),
            dose_Q1 = quantile(DailyMED / MED, probs = .25),
            dose_median = median(DailyMED / MED),
            dose_Q3 = quantile(DailyMED / MED, probs = .75),
            dose_min = min(DailyMED / MED),
            dose_max = max(DailyMED / MED))

