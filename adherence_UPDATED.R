library(readxl)
library(tidyverse)

adh <- read_excel("CHURCH CHA ADHERENCE/CHA_BL_adherent_clearning.xlsx")
nonadh <- read_excel("CHURCH CHA ADHERENCE/CHA_BL_nonadherent_cleaning.xlsx")

colnames(nonadh)
colnames(adh)

## CRC SCREENING
## Men and women 50-75 years old only
crc_nonadh <- nonadh %>%
  filter(age >= 50 & age <= 75) %>%
  mutate(fobt_adh = replace(fobt_adh, fobt_adh == 9, 0)) %>%
  mutate(new_fobt_adh = ifelse(ever_fobt == 1 & last_fobt <= 12 & !is.na(last_fobt), 1, 0)) %>%
  mutate(new_sig_adh = ifelse(ever_sig == 1 & last_sig <= 60 & !is.na(last_sig), 1, 0)) %>%
  mutate(new_col_adh = ifelse(ever_col == 1 & last_col <= 120 & !is.na(last_col), 1, 0)) %>%
  mutate(new_CRC_adh = ifelse(new_fobt_adh == 1 |
                                new_sig_adh == 1 |
                                new_col_adh == 1, 1, 0))

fobt <- crc_nonadh %>%
  select(id, church, sex, age, ever_fobt, last_fobt, fobt_adh, new_fobt_adh) %>%
  filter(new_fobt_adh != fobt_adh)

sig <- crc_nonadh %>%
  select(id, church, sex, age, ever_sig, last_sig, sig_adh, new_sig_adh) %>%
  filter(new_sig_adh != sig_adh)

col <- crc_nonadh %>%
  select(id, church, sex, age, ever_col, last_col, col_adh, new_col_adh) %>%
  filter(new_col_adh != col_adh)

test1 <- crc_nonadh %>%
  select(id, church, sex, age, ever_fobt, last_fobt, fobt_adh, new_fobt_adh,
         ever_sig, last_sig, sig_adh, new_sig_adh, ever_col, last_col, col_adh, new_col_adh, CRC_adh, new_CRC_adh) %>%
  filter(new_fobt_adh != fobt_adh |
           new_sig_adh != sig_adh |
           new_col_adh != col_adh |
           new_CRC_adh != CRC_adh)

crc_adh <- adh %>%
  filter(age >= 50 & age <= 75) %>%
  mutate(fobt_adh = replace(fobt_adh, fobt_adh == 9, 0)) %>%
  mutate(new_fobt_adh = ifelse(ever_fobt == 1 & last_fobt <= 12 & !is.na(last_fobt), 1, 0)) %>%
  mutate(new_sig_adh = ifelse(ever_sig == 1 & last_sig <= 60 & !is.na(last_sig), 1, 0)) %>%
  mutate(new_col_adh = ifelse(ever_col == 1 & last_col <= 120 & !is.na(last_col), 1, 0)) %>%
  mutate(new_CRC_adh = ifelse(new_fobt_adh == 1 |
                                new_sig_adh == 1 |
                                new_col_adh == 1, 1, 0))

test2 <- crc_adh %>%
  select(id, church, name, sex, age, ever_fobt, last_fobt, new_fobt_adh,
         ever_sig, last_sig, new_sig_adh, ever_col, last_col, new_col_adh, CRC_adh, new_CRC_adh) %>%
  filter(new_CRC_adh != CRC_adh)

crc_test <- bind_rows(test1, test2)
write.csv(crc_test,'crc_adh_test.csv')

## BREAST CANCER SCREENING
## Women 50-75 only
brc_nonadh <- nonadh %>%
  filter(sex == "F" & age >= 50 & age <= 75) %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_304", 0)) %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_157", 0)) %>%
  mutate(new_mammo_adh = ifelse(ever_mammo == 1 & last_mammo <= 24 & !is.na(last_mammo), 1, 0))

test1 <- brc_nonadh %>%
  select(id, church, sex, age, ever_mammo, last_mammo, mammo_adh, new_mammo_adh) %>%
  filter(new_mammo_adh != mammo_adh)

brc_adh <- adh %>%
  filter(sex == "F" & age >= 50 & age <= 75) %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_304", 0)) %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_157", 0)) %>%
  mutate(new_mammo_adh = ifelse(ever_mammo == 1 & last_mammo <= 24 & !is.na(last_mammo), 1, 0))

test2 <- brc_adh %>%
  select(id, church, sex, age, ever_mammo, last_mammo, mammo_adh, new_mammo_adh) %>%
  filter(new_mammo_adh != mammo_adh)

brc_test <- bind_rows(test1, test2)
write.csv(brc_test,'brc_adh_test.csv')

## CERVICAL CANCER SCREENING
## Women 50-65 only, no hysterectomy
cvc_nonadh <- nonadh %>%
  filter(sex == "F" & age >= 50 & age <= 65 & hystr == 0) %>%
  mutate(new_pap_adh = ifelse((ever_pap == 1 & last_pap <= 36 & !is.na(last_pap)) | 
           (ever_HPV == 1 & last_HPV <= 60 & !is.na(last_HPV)), 1, 0))

test1 <- cvc_nonadh %>%
  select(id, church, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
         pap_adh, new_pap_adh) %>%
  filter(new_pap_adh != pap_adh)

cvc_adh <- adh %>%
  filter(sex == "F" & age >= 50 & age <= 65 & hystr == 0) %>%
  mutate(new_pap_adh = ifelse((ever_pap == 1 & last_pap <= 36 & !is.na(last_pap)) | 
                                (ever_HPV == 1 & last_HPV <= 60 & !is.na(last_HPV)), 1, 0))

test2 <- cvc_adh %>%
  select(id, church, name, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
         pap_adh, new_pap_adh) %>%
  filter(new_pap_adh != pap_adh)

cvc_test <- bind_rows(test1, test2)
write.csv(cvc_test,'cvc_adh_test.csv')

## PROSTATE CANCER SCREENING
## Men 50-75 only, guideline = discuss prostate screening w/ doctor
psc_nonadh <- nonadh %>%
  filter(sex == "M" & age >= 50 & age <= 75)

test1 <- nonadh %>%
  filter(!is.na(ever_psa_discuss) & sex == "F")

