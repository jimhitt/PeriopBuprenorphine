library(tidyverse)
library(lubridate)
library(VINCI)
library(modelr)
library(mgcv)
library(patchwork)
library(ggsignif)

# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
# reworking analysis
# 1. Removing statistical notation from charts
# 2. Finidng preop opioid use in controls
# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

bup_all <- read_rds('bup_all4.rds')
# from 01_opRx_Dose.R
rx_op <- read_rds('rx_out_process.rds')
# from 03_ipRx_Dose
rx_ip_all <- read_rds('rx_ip_all.rds')

bup_all_dx = read_rds('bup_all_dx2.rds')

# reorder bup$late
bup_all = bup_all %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late')))

#write_csv(head(bup_all, 100), paste0(resultsFolder, 'bup_all.csv'))


# fix idx for both rx_op and rx_ip_all
rx_op = inner_join(
  rx_op,
  select(bup_all, PatientICN, oprymd, idx),
  by = c('PatientICN', 'oprymd')
)

rx_ip_all = rx_ip_all %>%
  select(-idx)
rx_ip_all = inner_join(
  rx_ip_all,
  select(bup_all, PatientICN, oprymd, idx),
  by = c('PatientICN', 'oprymd')
)

all_rx_by_day = read_rds('all_rx_by_day2.rds')
# reorder late
all_rx_by_day = all_rx_by_day %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late')))

all_rx_by_day %>%
  unnest(cols = data) %>%
  mutate(week = floor(date_diff / 7)) %>%
  filter(between(week, -1, 2),
         dose > 0) %>%
  group_by(week, late) %>%
  summarise(mean_dose = mean(dose),
            tot_dose = sum(dose),
            n = n(),
            ms_dev = var(dose) * n,
            sd = sd(dose),
            CI = sd * qt(.975, df = n - 1)/ sqrt(n))
# Mean Postop Opioid Dose Plot all ----------------------------------------
# removed statistical notations from plots
# included standard deviation rather than standard error

df = all_rx_by_day %>%
  unnest(cols = c(data)) %>%
  #filter(between(date_diff, 0, 27)) %>%
  group_by(idx, late, date_diff) %>%
  summarise(dose = sum(dose))

bup_all = bup_all %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 8, 16, 24, 40)))

df = df %>%
  inner_join(select(bup_all, idx, PSS, bup_dose, bup_fct), by = 'idx')

tmp = df %>%
  filter(between(date_diff, 0, 13)) %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late'))) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'Buprenorphine\nContinued' = 'continued',
                           'Buprenorphine\nInterrupted' = 'late')) %>%
  group_by(idx, late) %>%
  summarise(dose = mean(dose))
#summarise(dose = log10(mean(dose) + 1))


p = tmp %>%
  group_by(late) %>%
  summarise(sd = sd(dose),
            dose = mean(dose),
            n = n(),
            se = sd/sqrt(n)) %>%
  ggplot(aes(late, dose)) +
  geom_col(fill = 'steelblue', color = 'black') +
  geom_errorbar(aes(ymin = dose, ymax = dose + sd), width = .4) +
  # this block adds significance markers to the plot (removed for journal)
  #geom_signif(data = tmp,
  #            comparisons = list(c('Control', 'BUP Continued')), y_position = 425, tip_length = c(.002, .002)) + #,map_signif_level = T)
  #geom_signif(data = tmp,
  #            comparisons = list(c('BUP Continued', 'BUP Interrupted')), y_position = 450, tip_length = c(.002, .002)) +
  #geom_signif(data = tmp,
  #            comparisons = list(c('Control', 'BUP Interrupted')), y_position = 500, tip_length = c(.002, .002)) +
  labs(x = 'Cohort', y = 'Average Dose (mg Morphine Equivalency)')
# this code can put it on a log axis if needed
# scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#               labels = scales::trans_format("log10", scales::math_format(10^.x)))
p

ggsave('PATHNAME/avg_opioid_bar_all.jpg', p,
       width = 4, height = 5, units = 'in', dpi = 300)
ggsave('PATHNAME/avg_opioid_bar_all.jpg.svg', p,
       width = 4, height = 5, units = 'in', dpi = 300)

# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
# opioid data output and significance
# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
tmp %>%
  group_by(late) %>%
  summarise(sd = sd(dose),
            dose = mean(dose),
            n = n(),
            se = sd/sqrt(n))

cohort = c('Control', 'Buprenorphine\nContinued', 'Buprenorphine\nInterrupted')
print('===== significance testing NON-transformed =====')
(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[2])$dose))
pValue = res$p.value

(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[3])$dose))
pValue = c(pValue, res$p.value)

(res = t.test(filter(tmp, late == cohort[2])$dose,
              filter(tmp, late == cohort[3])$dose))
(pValue = c(pValue, res$p.value))
p.adjust(pValue, method = 'bonferroni')

# repeating with log dose  .-.-.-.-.-.-.-.-.-.-.-.-.-.-
tmp$dose_log = log10(tmp$dose + 1)
print('===== significance testing LOG-transformed =====')
(res = t.test(filter(tmp, late == cohort[1])$dose_log,
              filter(tmp, late == cohort[2])$dose_log))
pValue = res$p.value

(res = t.test(filter(tmp, late == cohort[1])$dose_log,
              filter(tmp, late == cohort[3])$dose_log))
pValue = c(pValue, res$p.value)

(res = t.test(filter(tmp, late == cohort[2])$dose_log,
              filter(tmp, late == cohort[3])$dose_log))
(pValue = c(pValue, res$p.value))
p.adjust(pValue, method = 'bonferroni')

# Mean Postop Opioid Dose Plot no bup -------------------------------------

df = all_rx_by_day %>%
  filter(Drug != 'buprenorphine') %>%
  unnest(cols = c(data)) %>%
  group_by(idx, late, date_diff) %>%
  summarise(dose = sum(dose))

bup_all = bup_all %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 8, 16, 24, 40)))

df = df %>%
  inner_join(select(bup_all, idx, PSS, bup_dose, bup_fct), by = 'idx')

tmp = df %>%
  filter(between(date_diff, 0, 13)) %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late'))) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'Buprenorphine\nContinued' = 'continued',
                           'Buprenorphine\nInterrupted' = 'late')) %>%
  group_by(idx, late) %>%
  summarise(dose = mean(dose))



p = tmp %>%
  group_by(late) %>%
  summarise(sd = sd(dose),
            dose = mean(dose),
            n = n(),
            se = sd/sqrt(n)) %>%
  ggplot(aes(late, dose)) +
  geom_col(fill = 'steelblue', color = 'black') +
  geom_errorbar(aes(ymin = dose, ymax = dose + sd), width = .4) +
  #geom_signif(data = tmp,
  #            comparisons = list(c('Control', 'BUP Continued')), y_position = -20, tip_length = c(.002, .002)) + #,map_signif_level = T)
  #geom_signif(data = tmp,
  #            comparisons = list(c('BUP Continued', 'BUP Interrupted')), y_position = -5, tip_length = c(.002, .002)) +
  #geom_signif(data = tmp,
  #            comparisons = list(c('Control', 'BUP Interrupted')), y_position = 5, tip_length = c(.002, .002)) +
  labs(x = 'Cohort', y = 'Average Dose (mg Morphine Equivalency)')
p

ggsave('PATHNAME/avg_opioid_bar_noBup.jpg', p,
       width = 4, height = 5, units = 'in', dpi = 300)
ggsave('PATHNAME/avg_opioid_bar_all.noBup.svg', p,
       width = 4, height = 5, units = 'in', dpi = 300)

# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
# opioid data output and significance
# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
tmp %>%
  group_by(late) %>%
  summarise(sd = sd(dose),
            dose = mean(dose),
            n = n(),
            se = sd/sqrt(n))

cohort = c('Control', 'Buprenorphine\nContinued', 'Buprenorphine\nInterrupted')
print('===== significance testing NON-transformed =====')
(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[2])$dose))
pValue = res$p.value

(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[3])$dose))
pValue = c(pValue, res$p.value)

(res = t.test(filter(tmp, late == cohort[2])$dose,
              filter(tmp, late == cohort[3])$dose))
(pValue = c(pValue, res$p.value))
p.adjust(pValue, method = 'bonferroni')

# repeating with log dose (shouldn't matter) .-.-.-.-.-.-.-.-.-.-.-.-.-.-
tmp$dose_log = log10(tmp$dose + 1)
print('===== significance testing LOG-transformed =====')

# not sure why the log10 dose is HIGHER for controls; the mean dose is lower.
# is it because there are more high outliers that contribute to the average?
(res = t.test(filter(tmp, late == cohort[1])$dose_log,
              filter(tmp, late == cohort[2])$dose_log))
pValue = res$p.value

(res = t.test(filter(tmp, late == cohort[1])$dose_log,
              filter(tmp, late == cohort[3])$dose_log))
pValue = c(pValue, res$p.value)

(res = t.test(filter(tmp, late == cohort[2])$dose_log,
              filter(tmp, late == cohort[3])$dose_log))
(pValue = c(pValue, res$p.value))
p.adjust(pValue, method = 'bonferroni')


# Postop Opioid by Bup Dose -----------------------------------------------

# this is using 2 weeks (14 days); similar to the analysis above
tmp = df %>%
  filter(late != 'control') %>%
  filter(between(date_diff, 0, 13)) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'Buprenorphine Continued' = 'continued',
                           'Buprenorphine Interrupted' = 'late')) %>%
  group_by(idx, late, bup_fct) %>%
  summarise(dose = sum(dose) / 14)


p = tmp %>%
  group_by(bup_fct, late) %>%
  summarise(mean_dose = mean(dose),
            se = sd(dose)) %>% #  / sqrt(n() - 1)) %>%
  mutate(Cohort = factor(late)) %>%
  mutate(bup_fct = recode(bup_fct,
                          "(0,8]" = "< 8 mg",
                          "(8,16]" = '8 to 15 mg',
                          "(16,24]" = '16 to 23 mg',
                          "(24,40]" = '24 to 40 mg')) %>%
  ggplot(aes(bup_fct, mean_dose, fill = Cohort)) +
  geom_col(position = 'dodge2', color = 'black') +
  geom_errorbar(aes(ymin = mean_dose, ymax = mean_dose + se),
                position = 'dodge2') +
  labs(x = 'Burenorphine Dose', y = 'Mean Opioid Dose (mg Morphine Equivalency)') +
  theme(legend.position = c(0.3, 0.85))

p
ggsave('PATHNAME/bup_dose_bar.jpg', p,
       width = 4, height = 4, units = 'in', dpi = 300)
ggsave('PATHNAME/bup_dose_bar.svg', p,
       width = 4, height = 4, units = 'in', dpi = 300)

# stats for opioid dose by bup dose

tmp = df %>%
  filter(late != 'control') %>%
  filter(between(date_diff, 0, 13)) %>%
  group_by(idx, late, bup_fct) %>%
  summarise(dose = log((sum(dose) / 14) + 1))

# looking at just continued
res_aov = aov(dose ~ bup_fct,
              data = filter(tmp, late == 'continued'))
summary(res_aov)

# looking at just late
res_aov = aov(dose ~ bup_fct,
              data = filter(tmp, late == 'late'))
summary(res_aov)

# modeling both late and continued with bup_fct
# unbalanced ANOVA
res_aov = aov(dose ~ bup_fct * late,
              data = tmp)
summary(res_aov)

library(car)
Anova(res_aov, type = 'III') # using type III sum of squares (not sure why)

tmp %>%
  ggplot(aes(late, dose, color = bup_fct)) +
  geom_boxplot()

# look at the preop opioid dose for controls ------------------------------

df = all_rx_by_day %>%
  filter(Drug != 'buprenorphine') %>%
  unnest(cols = c(data)) %>%
  group_by(idx, late, date_diff) %>%
  summarise(dose = sum(dose))

bup_all = bup_all %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 8, 16, 24, 40)))

df = df %>%
  inner_join(select(bup_all, idx, PSS, bup_dose, bup_fct), by = 'idx')

# create tmp variable for controls from 60 to 1 day BEFORE surgery
tmp = df %>%
  filter(late == 'control') %>%
  filter(between(date_diff, -60, -1))

# define high dose here
# average MED of 40 for 60 days is reasonable for "high" or chronic
highMED = 40

tmp = tmp %>%
  group_by(idx) %>%
  summarise(avg_dose = mean(dose))


tmp = tmp %>%
  mutate(dosefct = cut(avg_dose, breaks = c(0, 1, highMED, Inf), right = F, labels = c('ctrl_zero', 'ctrl_low', 'ctrl_high')))

tmp %>%
  count(dosefct)

tmp %>%
  ggplot(aes(avg_dose)) +
  geom_histogram(binwidth = highMED)

# rename column
tmp = tmp %>%
  rename(preop_dose = dosefct,
         preop_avg_dose = avg_dose)

# add column to bup_all
bup_all = bup_all %>%
  left_join(tmp, by = 'idx')

# fix NAs
bup_all$preop_dose = as.character(bup_all$preop_dose)
bup_all[is.na(bup_all$preop_dose), 'preop_dose'] = 'case'
bup_all$preop_dose = factor(bup_all$preop_dose, levels = c('case', 'ctrl_zero', 'ctrl_low', 'ctrl_high'))

# GAM Analysis ------------------------------------------------------------
df = all_rx_by_day %>%
  filter(Drug != 'buprenorphine') %>%
  unnest(cols = c(data)) %>%
  #filter(between(date_diff, 0, 27)) %>%
  group_by(idx, late, date_diff) %>%
  summarise(dose = sum(dose))

bup_all = bup_all %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 8, 16, 24, 40)))

df = df %>%
  inner_join(select(bup_all, idx, PSS, bup_dose, bup_fct), by = 'idx')

tmp = df %>%
  filter(between(date_diff, 0, 13)) %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late'))) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'BUP Continued' = 'continued',
                           'BUP Interrupted' = 'late')) %>%
  group_by(idx, late) %>%
  summarise(dose = mean(dose))
#summarise(dose = log10(mean(dose) + 1))
pt = df %>%
  ungroup() %>%
  filter(between(date_diff, 0, 13)) %>%
  select(idx, date_diff, dose)

pt = inner_join(
  pt,
  select(bup_all, idx, late, race, sex, smoke, bup_fct, bup_dose, asaclas, PSS, yr, Complexity, preop_dose),
  by = 'idx'
)

pt$bup_fct = as.character(pt$bup_fct)
pt[is.na(pt$bup_fct), 'bup_fct']  = 'none'
pt = pt %>%
  mutate(bup_fct = factor(bup_fct,
                          levels = c('none', "(0,8]", "(8,16]", "(16,24]",  "(24,40]")))
pt = pt %>%
  mutate(race = factor(race),
         race = fct_recode(race,
                           White = 'White',
                           Black = 'Black',
                           Other = 'AmericanIndian/Alaskan',
                           Other = 'Asian',
                           Other = 'Declined/Unk',
                           Other = 'PacificIsland'),
         sex = factor(sex),
         sex = fct_recode(sex,
                          'Male' = '1',
                          'Female' = '0'),
         asaclas = factor(asaclas),
         PSS = factor(PSS),
         smoke = factor(smoke))

pt = pt %>%
  mutate(race = factor(race, levels = c('White', 'Black', 'Other')))

pt = pt %>%
  mutate(log_dose = log10(dose + 1))

pt[is.na(pt$bup_dose), 'bup_dose'] = 0

pt$year = factor(pt$yr)

pt$Complexity = factor(pt$Complexity)

# rounding bup_dose
pt$bup_dose = round(pt$bup_dose)

# adding preop_dose to late column for controls .-.-.-.-.-.-.-.-.-.-..-.-.-.-.-.-
pt$late = as.character(pt$late)
pt[pt$late == 'control', 'late'] = pt[pt$late == 'control', 'preop_dose']
pt$late = factor(pt$late,
                 levels = c('ctrl_zero', 'ctrl_low', 'ctrl_high', 'continued', 'late'))

# redoing GAM
pt = pt %>%
  mutate(POD = date_diff,
         BUP_Dose = bup_dose,
         Cohort = late,
         Smoking_Status = smoke,
         ASA_Class = asaclas,
         log_dose = log10(dose + 1))
res = gam(dose ~ s(BUP_Dose) + s(POD) + Cohort + PSS + Smoking_Status + sex + ASA_Class + race + year + Complexity,
          data = pt, method = 'REML')
summary(res)

jpeg('./results_v12_AnesRevis/GAM_all_nolog.jpg', width = 8, height = 4, units = 'in',
     res = 300, quality = 100, pointsize = 14)
plot(res, shade = T, shade.col = 'lightblue', seWithMean = T, shift = coef(res)[1], pages = 1)
dev.off()

plot(res, all.terms = T, se = T, pages = 1)

# repeat sig testing for postop opioids removing high dose controls -------

df = all_rx_by_day %>%
  filter(Drug != 'buprenorphine') %>%
  unnest(cols = c(data)) %>%
  #filter(between(date_diff, 0, 27)) %>%
  group_by(idx, late, date_diff) %>%
  summarise(dose = sum(dose))

bup_all = bup_all %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 8, 16, 24, 40)))

df = df %>%
  inner_join(select(bup_all, idx, PSS, bup_dose, bup_fct, preop_dose), by = 'idx')

tmp = df %>%
  filter(between(date_diff, 0, 13)) %>%
  filter(! preop_dose == 'ctrl_high') %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late'))) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'Buprenorphine\nContinued' = 'continued',
                           'Buprenorphine\nInterrupted' = 'late')) %>%
  group_by(idx, late) %>%
  summarise(dose = mean(dose))


# there is now a small, significant difference between controls and BUP continued
# the BUP continued have HIGHER postop requirements
# this makes sense
# buprenorphine is has an effect similar to low dose opioids
# that can be seen in the GAM coefficiencts as well

cohort = c('Control', 'Buprenorphine\nContinued', 'Buprenorphine\nInterrupted')
print('===== significance testing NON-transformed =====')
(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[2])$dose))
pValue = res$p.value

(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[3])$dose))
pValue = c(pValue, res$p.value)

(res = t.test(filter(tmp, late == cohort[2])$dose,
              filter(tmp, late == cohort[3])$dose))
(pValue = c(pValue, res$p.value))
p.adjust(pValue, method = 'bonferroni')





# pain analysis -----------------------------------------------------------

pain = read_rds('pain2.rds')

# histogram of pain entries
if (0) {
  tmp = pain %>% unnest(cols = data)
  tmp$time_delta = difftime(
    tmp$VitalSignEnteredDateTime,
    tmp$opdt,
    units = 'hours'
  )
  tmp$time_delta = as.numeric(tmp$time_delta)
  
  p = tmp %>%
    filter(time_delta < 48) %>%
    ggplot(aes(time_delta))+
    geom_histogram(binwidth = 1)
  p
  ggsave('PATHNAME/pain_histo.jpg', p,
         width = 4, height = 5, units = 'in', dpi = 300)
}

# right join is new
# this keeps all patients in the study
pain = right_join(
  select(pain, PatientICN, oprymd, n, TimeWindow, TWpain),
  bup_all,
  by = c('PatientICN', 'oprymd')
)

pain = pain %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 16, 40)))

# change NAs in the count (n) column
pain[is.na(pain$n), 'n'] = 0

# examine missing data ~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# missing pain data is more common for procedures with low PSS values
if (0) {
  # analysis of missing pain data
  # by patient group
  pain$missing = pain$n < 3
  (t = table(pain$late, pain$missing))
  round(prop.table(t, margin = 1) * 10000) / 100
  chisq.test(t)
  
  # by in versus out
  (t = table(pain$inout, pain$missing))
  round(prop.table(t, margin = 1) * 10000) / 100
  chisq.test(t)
  

  
  # this is old code that seems strange
  # by PSS
  painMissingPSS = pain %>%
    ungroup() %>%
    filter(is.na(n) | n < 3) %>%
    group_by(PSS) %>%
    summarize(missing = n())
  
  tmp = pain %>%
    ungroup() %>%
    filter(n >= 3) %>%
    group_by(PSS) %>%
    summarize(captured = n())
  
  painMissingPSS = painMissingPSS %>%
    inner_join(tmp, by = 'PSS')
  
  painMissingPSS$total = painMissingPSS$missing + painMissingPSS$captured
  painMissingPSS$missingPCNT = painMissingPSS$missing / painMissingPSS$total
  
  
  # by CPT
  painMissingCPT = pain %>%
    ungroup() %>%
    filter(is.na(n) | n < 3) %>%
    group_by(PRNCPTX) %>%
    summarize(missing = n())
  
  tmp = pain %>%
    ungroup() %>%
    filter(n >= 3) %>%
    group_by(PRNCPTX) %>%
    summarize(captured = n())
  
  painMissingCPT = painMissingCPT %>%
    right_join(tmp, by = 'PRNCPTX')
  # set missing NA = 0
  painMissingCPT[is.na(painMissingCPT$missing), 'missing'] = 0
  
  painMissingCPT$total = painMissingCPT$missing + painMissingCPT$captured
  painMissingCPT$missingPCNT = painMissingCPT$missing / painMissingCPT$total
}


# impute missing pain data by PSS (separate column) -----------------------

pain$TWpainImpute = pain$TWpain
pain[pain$n < 3, 'TWpain'] = NA

pain %>%
  ungroup() %>%
  filter(n >= 3) %>%
  group_by(PSS, late) %>%
  summarize(mean_pain = mean(TWpain),
            sd_pain = sd(TWpain),
            median_pain = median(TWpain),
            p25 = quantile(TWpain, 0.25),
            p75 = quantile(TWpain, 0.75))

pain %>%
  ungroup() %>%
  filter(n >= 3) %>%
  group_by(late) %>%
  summarize(mean_pain = mean(TWpain),
            sd_pain = sd(TWpain),
            median_pain = median(TWpain),
            p25 = quantile(TWpain, 0.25),
            p75 = quantile(TWpain, 0.75))

avgPainPSS = pain %>%
  ungroup() %>%
  filter(n >= 3) %>%
  group_by(PSS, late) %>%
  summarize(mean_pain = mean(TWpain))

# imputing based on average pain for PSS and late cohort
pain = pain %>%
  left_join(avgPainPSS, by = c('PSS', 'late')) %>%
  mutate(TWpainImpute = if_else(is.na(TWpain), mean_pain, TWpain)) %>%
  select(-mean_pain)

# handle categorical variables ~_~_~_~_~_~_~_~_~_~_~_~_~_
pt = pain %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late'))) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'BUP Continued' = 'continued',
                           'BUP Interrupted' = 'late'))


pt$bup_fct = as.character(pt$bup_fct)
pt[is.na(pt$bup_fct), 'bup_fct']  = 'none'
pt = pt %>%
  mutate(bup_fct = factor(bup_fct,
                          levels = c('none', "low", "high")))
pt = pt %>%
  mutate(race = factor(race),
         race = fct_recode(race,
                           White = 'White',
                           Black = 'Black',
                           Other = 'AmericanIndian/Alaskan',
                           Other = 'Asian',
                           Other = 'Declined/Unk',
                           Other = 'PacificIsland'),
         sex = factor(sex),
         sex = fct_recode(sex,
                          'Male' = '1',
                          'Female' = '0'),
         asaclas = factor(asaclas),
         PSS = factor(PSS))

pt = pt %>%
  mutate(race = factor(race, levels = c('White', 'Black', 'Other')))


pt[is.na(pt$bup_dose), 'bup_dose'] = 0

pt$year = factor(pt$yr)

pt$PSS = factor(pt$PSS, levels = c('1', '2', '3', '4', '5'))

pt$Complexity = factor(pt$Complexity)

# rounding bup_dose
pt$bup_dose = round(pt$bup_dose)


# average pain by BUP dose ------------------------------------------------


p = pt %>%
  mutate(late = fct_recode(late,
                           'Buprenorphine\nContinued' = 'BUP Continued',
                           'Buprenorphine\nInterrupted' = 'BUP Interrupted')) %>%
  filter(n >= 3) %>%
  group_by(late) %>%
  summarise(sd = sd(TWpain),
            CI = sd / sqrt(n()) * qnorm(1 - (1 - 0.95) / 2),
            TWpain = mean(TWpain)) %>%
  ggplot(aes(late, TWpain)) +
  geom_col(fill = 'steelblue') +
  geom_errorbar(aes(ymin = TWpain - sd, ymax = TWpain + sd), width = .4) + 
  labs(x = 'Cohort', y = 'Mean (Standard Deviation)\nTime-Weighted Average Pain')
p

ggsave('PATHNAME/avg_pain.jpg', p,
       width = 4, height = 5, units = 'in', dpi = 300)
ggsave('PATHNAME/avg_pain.svg', p,
       width = 4, height = 5, units = 'in', dpi = 300)

# pain stats
tmp2 = pt %>%
  filter(n >= 3) 

(res = t.test(filter(tmp2, late == 'Control')$TWpain,
              filter(tmp2, late == 'BUP Continued')$TWpain))
pValue = res$p.value

(res = t.test(filter(tmp2, late == 'Control')$TWpain,
              filter(tmp2, late == 'BUP Interrupted')$TWpain))
pValue = c(pValue, res$p.value)

(res = t.test(filter(tmp2, late == 'BUP Continued')$TWpain,
              filter(tmp2, late == 'BUP Interrupted')$TWpain))
pValue = c(pValue, res$p.value)

pValue
p.adjust(pValue, method = 'bonferroni')

tmp2 %>%
  group_by(late) %>%
  summarize(avg_pain = mean(TWpain),
            sd = sd(TWpain),
            n = n())

# pain by bup dose .-.--..-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
pt %>%
  filter(type == 'case') %>%
  filter(n >= 3) %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 16, 40))) %>%
  group_by(bup_fct, late) %>%
  summarize(avg_pain = mean(TWpain),
            sd_pain = sd(TWpain))

p = pt %>%
  filter(type == 'case') %>%
  filter(n >= 3) %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 16, 40))) %>%
  group_by(bup_fct, late) %>%
  summarize(avg_pain = mean(TWpain),
            sd_pain = sd(TWpain)) %>%
  mutate(Cohort = fct_recode(late,
                             'Continued' = 'BUP Continued',
                             'Interrupted' = 'BUP Interrupted')) %>%
  mutate(bup_fct = fct_recode(bup_fct,
                              '16 mg or less' = '(0,16]', 
                              'greater than 16 mg' = '(16,40]')) %>%
  ggplot(aes(bup_fct, avg_pain, fill = Cohort)) +
  geom_col(position = 'dodge2', color = 'black') +
  geom_errorbar(aes(ymin = avg_pain - sd_pain, ymax = avg_pain + sd_pain),
                position = 'dodge2') +
  labs(x = 'Buprenorphine Dose', y = 'Average Pain')
p

ggsave('PATHNAME/avg_pain_dose.jpg', p,
       width = 4, height = 5, units = 'in', dpi = 300)
ggsave('PATHNAME/avg_pain_dose.svg', p,
       width = 4, height = 5, units = 'in', dpi = 300)

# modeling both late and continued with bup_fct
# unbalanced ANOVA
# the differences related to dose are not significant

pain_tmp = pt %>%
  filter(type == 'case') %>%
  filter(n >= 3) %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 16, 40)))

res_aov = aov(TWpain ~ bup_fct * late,
              data = pain_tmp)
summary(res_aov)

# repeating anova with imputed pain
pain_tmp = pt %>%
  filter(type == 'case') %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 16, 40)))

res_aov = aov(TWpain ~ bup_fct * late,
              data = pain_tmp)

summary(res_aov)



# imputed values .-.-.--..-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
p = pt %>%
  mutate(late = fct_recode(late,
                           'Buprenorphine\nContinued' = 'BUP Continued',
                           'Buprenorphine\nInterrupted' = 'BUP Interrupted')) %>%
  group_by(late) %>%
  summarise(sd = sd(TWpainImpute),
            CI = sd / sqrt(n()) * qnorm(1 - (1 - 0.95) / 2),
            TWpain = mean(TWpainImpute)) %>%
  ggplot(aes(late, TWpain)) +
  geom_col(fill = 'steelblue') +
  geom_errorbar(aes(ymin = TWpain - sd, ymax = TWpain + sd), width = .4) + 
  labs(x = 'Cohort', y = 'Mean (Standard Deviation)\nTime-Weighted Average Pain')
p

# imputed stats .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

pt %>%
  group_by(late) %>%
  summarize(avg_pain = mean(TWpainImpute),
            sd = sd(TWpainImpute),
            n = n())

(res = t.test(filter(pt, late == 'Control')$TWpainImpute,
              filter(pt, late == 'BUP Continued')$TWpainImpute))
pValue = res$p.value

(res = t.test(filter(pt, late == 'Control')$TWpainImpute,
              filter(pt, late == 'BUP Interrupted')$TWpainImpute))
pValue = c(pValue, res$p.value)

(res = t.test(filter(pt, late == 'BUP Continued')$TWpainImpute,
              filter(pt, late == 'BUP Interrupted')$TWpainImpute))
pValue = c(pValue, res$p.value)

pValue
p.adjust(pValue, method = 'bonferroni')

# print pain tables
pt %>%
  filter(n >= 3) %>%
  group_by(late) %>%
  summarize(avg_pain = mean(TWpain),
            sd = sd(TWpain))

# imputed
pt %>%
  group_by(late) %>%
  summarize(avg_pain = mean(TWpainImpute),
            sd = sd(TWpainImpute))


# Pain ANOVA / ANCOVA -----------------------------------------------------

pt = pt %>%
  mutate(smoke = factor(smoke))
# combine late and preop_dose
pt$late = as.character(pt$late)
pt$preop_dose = as.character(pt$preop_dose)
pt$late3 = pt$late
pt[pt$late == 'Control', 'late3'] = pt[pt$late == 'Control', 'preop_dose']
pt[pt$late3 == 'case', 'late3'] = 'ctrl_zero'
pt = pt %>%
  mutate(late = factor(late),
         late3 = factor(late3,
                        levels = c('ctrl_zero', 'ctrl_low', 'ctrl_high', 'BUP Continued', 'BUP Interrupted')))
pt$preop_dose = factor(pt$preop_dose)

pt = pt %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(-1, 0, 16, 48)))

pt_tmp = pt %>%
  filter(n >= 3)


ancova_mod = aov(TWpain ~ late + preop_dose + PSS + smoke + sex + asaclas + Complexity + bup_fct,
                 data = pt_tmp)
summary(ancova_mod)

ancova_mod = aov(TWpainImpute ~ late + preop_dose + PSS + smoke + sex + asaclas + Complexity + bup_fct,
                 data = pt)
summary(ancova_mod)

# redo opioid analysis and GAM w/o missing pain subjects ------------------

pain$missing_pain = pain$n < 3

bup_all_pain = bup_all %>%
  inner_join(select(pain, PatientICN, oprymd, missing_pain),
             by = c('PatientICN', 'oprymd'))

df = all_rx_by_day %>%
  filter(Drug != 'buprenorphine') %>%
  unnest(cols = c(data)) %>%
  #filter(between(date_diff, 0, 27)) %>%
  group_by(idx, late, date_diff) %>%
  summarise(dose = sum(dose))

bup_all_pain = bup_all_pain %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 8, 16, 24, 40)))

df = df %>%
  inner_join(select(bup_all_pain, idx, PSS, bup_dose, bup_fct, missing_pain), by = 'idx')

tmp = df %>%
  filter(between(date_diff, 0, 13)) %>%
  filter(missing_pain == F) %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late'))) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'Buprenorphine\nContinued' = 'continued',
                           'Buprenorphine\nInterrupted' = 'late')) %>%
  group_by(idx, late) %>%
  summarise(dose = mean(dose))



p = tmp %>%
  group_by(late) %>%
  summarise(sd = sd(dose),
            dose = mean(dose),
            n = n(),
            se = sd/sqrt(n)) %>%
  ggplot(aes(late, dose)) +
  geom_col(fill = 'steelblue', color = 'black') +
  geom_errorbar(aes(ymin = dose, ymax = dose + sd), width = .4) +
  #geom_signif(data = tmp,
  #            comparisons = list(c('Control', 'BUP Continued')), y_position = -20, tip_length = c(.002, .002)) + #,map_signif_level = T)
  #geom_signif(data = tmp,
  #            comparisons = list(c('BUP Continued', 'BUP Interrupted')), y_position = -5, tip_length = c(.002, .002)) +
  #geom_signif(data = tmp,
  #            comparisons = list(c('Control', 'BUP Interrupted')), y_position = 5, tip_length = c(.002, .002)) +
  labs(x = 'Cohort', y = 'Average Dose (mg Morphine Equivalency)')
p

# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
# opioid data output and significance
# .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
tmp %>%
  group_by(late) %>%
  summarise(sd = sd(dose),
            dose = mean(dose),
            n = n(),
            se = sd/sqrt(n))

cohort = c('Control', 'Buprenorphine\nContinued', 'Buprenorphine\nInterrupted')
print('===== significance testing NON-transformed =====')
(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[2])$dose))
pValue = res$p.value

(res = t.test(filter(tmp, late == cohort[1])$dose,
              filter(tmp, late == cohort[3])$dose))
pValue = c(pValue, res$p.value)

(res = t.test(filter(tmp, late == cohort[2])$dose,
              filter(tmp, late == cohort[3])$dose))
(pValue = c(pValue, res$p.value))
p.adjust(pValue, method = 'bonferroni')

# note: the pain results (significance and magnitude hold if patients with missing pain are removed)
# the difference between control and BUP continued is small and not significant (P = .123)

# GAM Analysis with missing pain excluded ---------------------------------
df = all_rx_by_day %>%
  filter(Drug != 'buprenorphine') %>%
  unnest(cols = c(data)) %>%
  #filter(between(date_diff, 0, 27)) %>%
  group_by(idx, late, date_diff) %>%
  summarise(dose = sum(dose))

bup_all_pain = bup_all_pain %>%
  mutate(bup_fct = cut(bup_dose, breaks = c(0, 8, 16, 24, 40)))

df = df %>%
  inner_join(select(bup_all_pain, idx, PSS, bup_dose, bup_fct, missing_pain), by = 'idx')

tmp = df %>%
  filter(between(date_diff, 0, 13)) %>%
  filter(missing_pain == F) %>%
  mutate(late = factor(late, levels = c('control', 'continued', 'late'))) %>%
  mutate(late = fct_recode(late,
                           'Control' = 'control',
                           'BUP Continued' = 'continued',
                           'BUP Interrupted' = 'late')) %>%
  group_by(idx, late) %>%
  summarise(dose = mean(dose))
#summarise(dose = log10(mean(dose) + 1))
pt = df %>%
  ungroup() %>%
  filter(between(date_diff, 0, 13)) %>%
  filter(missing_pain == F) %>%
  select(idx, date_diff, dose)

pt = inner_join(
  pt,
  select(bup_all_pain, idx, late, race, sex, smoke, bup_fct, 
         bup_dose, asaclas, PSS, yr, Complexity, preop_dose, missing_pain),
  by = 'idx'
)

pt$bup_fct = as.character(pt$bup_fct)
pt[is.na(pt$bup_fct), 'bup_fct']  = 'none'
pt = pt %>%
  mutate(bup_fct = factor(bup_fct,
                          levels = c('none', "(0,8]", "(8,16]", "(16,24]",  "(24,40]")))
pt = pt %>%
  mutate(race = factor(race),
         race = fct_recode(race,
                           White = 'White',
                           Black = 'Black',
                           Other = 'AmericanIndian/Alaskan',
                           Other = 'Asian',
                           Other = 'Declined/Unk',
                           Other = 'PacificIsland'),
         sex = factor(sex),
         sex = fct_recode(sex,
                          'Male' = '1',
                          'Female' = '0'),
         asaclas = factor(asaclas),
         PSS = factor(PSS),
         smoke = factor(smoke))

pt = pt %>%
  mutate(race = factor(race, levels = c('White', 'Black', 'Other')))

pt = pt %>%
  mutate(log_dose = log10(dose + 1))

pt[is.na(pt$bup_dose), 'bup_dose'] = 0

pt$year = factor(pt$yr)

pt$Complexity = factor(pt$Complexity)

# rounding bup_dose
pt$bup_dose = round(pt$bup_dose)


# adding preop_dose to late column for controls .-.-.-.-.-.-.-.-.-.-..-.-.-.-.-.-
pt$late = as.character(pt$late)
pt[pt$late == 'control', 'late'] = pt[pt$late == 'control', 'preop_dose']
pt$late = factor(pt$late,
                 levels = c('ctrl_zero', 'ctrl_low', 'ctrl_high', 'continued', 'late'))

# redoing GAM
pt = pt %>%
  mutate(POD = date_diff,
         BUP_Dose = bup_dose,
         Cohort = late,
         Smoking_Status = smoke,
         ASA_Class = asaclas,
         log_dose = log10(dose + 1))
res = gam(dose ~ s(BUP_Dose) + s(POD) + Cohort + PSS + Smoking_Status + sex + 
            ASA_Class + race + year + Complexity,
          data = pt, method = 'REML')
summary(res)
