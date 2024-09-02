library(tidyverse)
library(lubridate)
library(VINCI)
library(modelr)
library(mgcv)
# library(ggplot2)
library(patchwork)
library(ggsignif)


# calc_comorbidity2 (uses idx not PatientICN) -----------------------------
calc_comorbidity2 <- function(df, modified_score = T){
  library(comorbidity)
  
  
  # replaced PatientICN with idx
  
  a <- unique(filter(df, ICD_type == 'icd9')$idx)
  b <- unique(filter(df, ICD_type == 'icd10')$idx)
  
  pts_icd9 <- setdiff(a, b)
  pts_icd10 <- setdiff(b, a)
  pts_both <- intersect(a, b)
  rm(a, b)
  
  #get icd9 codes for comorbidity
  # icd9
  tmp <- df %>%
    filter(idx %in% pts_icd9) %>%
    select(idx, Code)
  tmp <- tmp %>%
    mutate(code = str_replace(Code, "\\.", ""))
  elix_9 <- comorbidity(x = tmp, id = "idx", code = "code",
                        icd = "icd9", score = "elixhauser",
                        assign0 = FALSE)
  # elix_9 <- elix_9 %>%
  #   select(idx, wscore_vw)
  
  # icd10
  tmp <- df %>%
    filter(idx %in% pts_icd10) %>%
    select(idx, Code)
  tmp <- tmp %>%
    mutate(code = str_replace(Code, "\\.", ""))
  elix_10 <- comorbidity(x = tmp, id = "idx", code = "code",
                         icd = "icd10", score = "elixhauser",
                         assign0 = FALSE)
  # elix_10 <- elix_10 %>%
  #   select(idx, wscore_vw)
  
  # both icd9 and 10
  tmp <- df %>%
    filter(idx %in% pts_both)
  tmp9 <- tmp %>% filter(ICD_type == 'icd9')
  tmp9 <- tmp9 %>%
    mutate(code = str_replace(Code, "\\.", "")) %>%
    select(idx, code)
  
  elix_b9 <- comorbidity(x = tmp9, id = "idx", code = "code",
                         icd = "icd9", score = "elixhauser",
                         assign0 = FALSE)
  
  
  tmp10 <- tmp %>% filter(ICD_type == 'icd10')
  tmp10 <- tmp10 %>%
    mutate(code = str_replace(Code, "\\.", "")) %>%
    select(idx, code)
  elix_b10 <- comorbidity(x = tmp10, id = "idx", code = "code",
                          icd = "icd10", score = "elixhauser",
                          assign0 = FALSE)
  
  # make sure both are sorted by SSN
  elix_b9 <- elix_b9 %>% arrange(idx)
  elix_b10 <- elix_b10 %>% arrange(idx)
  
  #create matrix pair
  mat_b9 <- elix_b9[ , 2:32]
  mat_b10 <- elix_b10[ , 2:32]
  
  matb <- mat_b9 + mat_b10
  #set values > 1 to 1 using logical test
  matb <- matrix(as.numeric(matb > 0), ncol = 31)
  
  c_name <- colnames(elix_b10)
  c_name <- c_name[2:32]
  
  colnames(matb) <- c_name
  
  # v is scoring weight vector 
  # vanWalraven et al. Med Care. 2009; 47(6):626
  if (modified_score){
    print('using the modified score')
    v <- c(7, 5, -1, 4, 2, 0, 0, 7, 6, 3, 0, 0, 0, 5, 11, 0, 0, 9, 12, 4, 0, 3, -4, 6, 5, -2, -2, 0, 0, 0, -3)
  } else {
    print('using the standard score')
    v <- c(7, 5, -1, 4, 2, 0, 0, 7, 6, 3, 0, 0, 0, 5, 11, 0, 0, 9, 12, 4, 0, 3, -4, 6, 5, -2, -2, 0, -7, 0, -3)
  }
  
  vw_score <- colSums(t(matb) * v)
  
  elix_both <- as_tibble(matb)
  elix_both <- elix_both %>%
    mutate(wscore_vw  = vw_score,
           idx = elix_b9$idx) %>%
    select(idx, c_name, wscore_vw )
  
  
  elix_9 <- elix_9 %>% select(idx, c_name, wscore_vw)
  elix_10 <- elix_10 %>% select(idx, c_name, wscore_vw)
  
  # FINAL creation of elix_all
  elix_all <- rbind(elix_9, elix_10, elix_both)
  
}

comma <- function(x) format(x, digits = 2, nsmall = 1, big.mark = ",")

# FUNCTION that returns MED table -------------------------------------------
get_MED <- function (){
  med <- tibble(
    Drug = c('buprenorphine', 'codeine', 'fentanyl', 'hydrocodone', 'hydromorphone',
             'methadone', 'morphine', 'oxycodone', 'tapentadol', 'tramadol'),
    MED = c(12.6, 0.15, 2.4, 1, 4, 4.7, 1, 1.5, 0.4, 0.2)
  )
}

# FUNCTION that returns Dose from LocalDrugName
# check for some number reversals (ie hydrocodone/apap with apap number returned)
get_dose <- function(s_drugs, df, return_nomatch = T){
  
  df$Drug <- NA
  df$Strength <- NA
  # remove sulfate abbreviation
  df <- df %>%
    mutate(LocalDrugNameWithDose = str_replace(LocalDrugNameWithDose, '(so4|s04)', ''))
  # remove #3 or #4 from codeine
  df <- df %>%
    mutate(LocalDrugNameWithDose = str_replace(LocalDrugNameWithDose, '#(3|4)', ''))
  match <- df %>% filter(!is.na(Strength))
  nomatch <- df %>% filter(is.na(Strength))
  
  for (i in 1:length(s_drugs)){
    s <- paste0(s_drugs[i], '.*\\d+(\\.\\d+)*')
    nomatch <- nomatch %>%
      mutate(tmp = str_extract(LocalDrugNameWithDose, s),
             Strength = as.numeric(str_extract(tmp, '\\d+(\\.\\d+)*')),
             Drug = s_drugs[i])
    # set anything over 125 back to NA (error in capture)
    # nomatch[nomatch$Dose > 125 & !is.na(nomatch$Dose), 'Dose'] <- NA
    nomatch <- nomatch %>% select(-tmp)
    match <- rbind(match, filter(nomatch, !is.na(Strength)))  
    nomatch <- nomatch %>% filter(is.na(Strength))
    print(i)
    print(nrow(match) / (nrow(match) + nrow(nomatch)) * 100)
  }
  
  # take a look
  if (0) {
    nomatch %>% count(LocalDrugNameWithDose) %>%
      arrange(desc(n)) %>%
      print(n = 100)
    
    match %>%
      count(Drug, Strength) %>%
      arrange(Drug, Strength) %>%
      print(n = 200)
    
    # will need to correct buprenorphine 75 to 0.075 (mcg to mg)
    match %>%
      filter(str_detect(Drug, 'bupren') & Strength == 75) %>%
      select(LocalDrugNameWithDose, Sig, Strength, QtyNumeric) %>%
      print(n = 100)
  }
  
  if (return_nomatch) {
    return(rbind(match, nomatch))
  } else {
    return(match)
  }
}

# FUNCTION for dose_soln
# get_dose_soln -- for solution prescriptions -----------------------------------
get_dose_soln <- function(s_drugs, df, return_no_match = T){
  # remove sulfate abbreviation
  df <- df %>%
    mutate(LocalDrugNameWithDose = str_replace(LocalDrugNameWithDose, '(so4|s04)', ''))
  
  df$Strength <- NA
  df$Dose <- NA
  df$Drug <- NA
  
  # replace mg/ml with mg/1ml (add the digit) -- then repeat for mcg/ml
  df <- df %>%
    mutate(LocalDrugNameWithDose = str_replace(LocalDrugNameWithDose, 'mg\\/ml', 'mg/1ml'))
  df <- df %>%
    mutate(LocalDrugNameWithDose = str_replace(LocalDrugNameWithDose, 'mcg\\/ml', 'mcg/1ml'))
  
  match <- df %>% filter(!is.na(Strength))
  nomatch <- df %>% filter(is.na(Strength))
  
  for (i in 1:length(s_drugs)){
    s <- paste0(s_drugs[i], '.*\\d+(\\.\\d+)*')
    nomatch <- nomatch %>%
      mutate(tmp = str_extract(LocalDrugNameWithDose, s),
             Dose = as.numeric(str_extract(tmp, '\\d+(\\.\\d+)*')),
             tmp2 = str_extract(LocalDrugNameWithDose, '\\d+(\\.\\d+)*ml'),
             Strength = Dose / as.numeric(str_extract(tmp2, '\\d+(\\.\\d+)*')),
             Drug = s_drugs[i])
    # set anything over 125 back to NA (error in capture)
    # nomatch[nomatch$Dose > 125 & !is.na(nomatch$Dose), 'Dose'] <- NA
    nomatch <- nomatch %>% select(-tmp, -tmp2)
    match <- rbind(match, filter(nomatch, !is.na(Strength)))  
    nomatch <- nomatch %>% filter(is.na(Dose))
    print(i)
    print(nrow(match) / (nrow(match) + nrow(nomatch)) * 100)
  }
  if (return_no_match) {
    return(rbind(match, nomatch))
  } else {
    return(match)
  }
}

# calculate opioids by time
# columns need to be in this order:
#   PatientICN, oprymd, type, date_diff, DaysSupply, QtyNumeric, DailyMED
opioid_calc_by_time <- function(tmp, by_week = T, n_days = 182) {
  for (j in 1:nrow(tmp)) {
    print(j)
    # select prescriptions for single patient
    tmp2 <- tmp$data[[j]]
    
    tmp3 <- tibble(date_diff = seq(-n_days, n_days), dose = 0)
    
    # iterate over prescriptions and add to sum df (tmp3)
    for (i in 1:nrow(tmp2)) {
      # adding call to floor because some date_diff are floats
      a = floor(tmp2[i,]$date_diff)
      b = floor(tmp2[i,]$DaysSupply) + a - 1
      b = min(b, n_days)
      d = tmp2[i,]$DailyMED
      
      a = a + n_days + 1
      b = b + n_days + 1
      tmp3[a:b,2] <- tmp3[a:b,2] + d
    }
    
    if (by_week) {
      # make week column
      tmp3$week <- floor(tmp3$date_diff / 7)
      
      tmp3 <- tmp3 %>%
        group_by(week) %>%
        summarise(dose = sum(dose) / 7)
    }
    
    # remove NA
    tmp3 <- tmp3 %>% filter(!is.na(dose))
    
    # move the by_week df back to base df (tmp)
    tmp$data[[j]] <- tmp3
  }
  return(tmp)
}
# END calculate opioid dose by week ------------------------------


