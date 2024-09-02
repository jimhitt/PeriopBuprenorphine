# calculate timely and late -----------------------------------------------
bup_all <- read_rds('bup_all3.rds')
# from 01_opRx_Dose.R
rx_op <- read_rds('rx_out_process.rds')
# from 03_ipRx_Dose
rx_ip_all <- read_rds('rx_ip_all.rds')



bup_bup = bup_all %>%
  filter(type == 'case')
bup_ctrl = bup_all %>%
  filter(type == 'ctrl')

# preop buprenorphine ---------------------------------------------------------------

rx_op_ctrl = inner_join(
  rx_op,
  select(bup_ctrl, idx, PatientICN, oprymd),
  by = c('PatientICN', 'oprymd')) %>%
  filter(date_diff < 0)

rx_op_ctrl_pt = rx_op_ctrl %>%
  group_by(idx) %>%
  summarise(total_dose = sum(Strength * QtyNumeric * MED),
            total_days = sum(DaysSupply),
            avg_dose = total_dose / total_days)


preoprx = select(bup_ctrl, PatientICN, oprymd, PSS, idx) %>%
  left_join(rx_op_ctrl_pt, by = 'idx') %>%
  replace_na(list(total_dose = 0, total_days = 0, avg_dose = 0))

# histogram of all preop average dose for controls
preoprx %>%
  ggplot(aes(avg_dose)) +
  geom_histogram(binwidth = 25)

# percent of controls who were on opioids >= 45 days in the 60 days before surgery
# 15.1%
nrow(filter(preoprx, total_days >= 45)) / nrow(preoprx) * 100

# historgram of duration of outpatient opioid prescriptions in control group
preoprx %>%
  ggplot(aes(total_days)) +
  geom_histogram(binwidth = 15)

# histogram of preop average dose for patients on >= 15 days before surgery
preoprx %>%
  filter(total_days >= 15) %>%
  ggplot(aes(avg_dose)) +
  geom_histogram(binwidth = 15)
