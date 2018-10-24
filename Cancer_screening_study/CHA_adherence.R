library(readxl)
library(tidyverse)

nonadh <- read_excel("~/CHURCH CHA ADHERENCE/Baseline Assessment - From CRC-2.xlsx",
               sheet = 1)
adh <- read_excel("~/CHURCH CHA ADHERENCE/Baseline Assessment - From CRC-2.xlsx",
                  sheet = 2)
#nonadh <- read_excel("~/CHURCH CHA ADHERENCE/CHA_BL_nonadherent_cleaning.xlsx")

colnames(adh)
colnames(nonadh)

## colorectal cancer screening - subset

## stool blood test
sto_adh <- adh %>%
  select(id, church, name, sex, age, ever_fobt, last_fobt, fobt_adh) %>%
  mutate(new_fobt_adh = ifelse(ever_fobt == 1 & last_fobt <= 12, 1, 0)) %>% # some 12 mo are adh, some 12 mo are not?? 
  filter(new_fobt_adh != fobt_adh) # mis-classification of stool blood test adherence status
write.csv(sto_adh,'sto_adh.csv')

# some 12 mo are adh, some 12 mo are not?? Does not matter for adherence group?
sto_adh <- adh %>%
  select(id, church, name, sex, age, ever_fobt, last_fobt, fobt_adh, sig_adh,
         col_adh, CRC_adh, mammo_adh, pap_adh, ever_psa_discuss) %>%
  filter(last_fobt == 12)
#write.csv(sto_adh,'sto_adh.csv')

table(nonadh$fobt_adh)
table(nonadh$sig_adh)
table(nonadh$col_adh)
table(nonadh$CRC_adh)
table(nonadh$mammo_adh)
table(nonadh$hystr)
table(nonadh$pap_adh)
table(nonadh$ever_psa_discuss)

table(sto_nonadh$fobt_adh) # one entry with fobt_adh == 9

sto_nonadh <- nonadh %>%
  select(id, church, name, sex, age, ever_fobt, last_fobt, fobt_adh, sig_adh,
         col_adh, CRC_adh, mammo_adh, pap_adh, ever_psa_discuss) %>%
  mutate(new_fobt_adh = ifelse(ever_fobt == 1 & last_fobt <= 12, 1, 0)) %>%
  mutate(fobt_adh = replace(fobt_adh, fobt_adh == 9, 0)) %>% # replaced 9 with 0 (checked survey)
  filter(new_fobt_adh != fobt_adh)
write.csv(sto_nonadh,'sto_nonadh.csv')

## sigmoidoscopy
sig_nonadh <- nonadh %>%
  select(id, church, name, sex, age, ever_sig, last_sig, sig_adh,
         fobt_adh, col_adh, CRC_adh, mammo_adh, pap_adh, ever_psa_discuss) %>%
  mutate(new_sig_adh = ifelse(ever_sig == 1 & last_sig <= 60, 1, 0)) %>% # some 60 mo are adh, some 60 mo are not?? 
  filter(new_sig_adh != sig_adh)
write.csv(sig_nonadh,'sig_nonadh.csv')

## colonoscopy
col_nonadh <- nonadh %>%
  select(id, church, name, sex, age, ever_col, last_col, col_adh, sig_adh,
         fobt_adh, CRC_adh, mammo_adh, pap_adh, ever_psa_discuss) %>%
  mutate(new_col_adh = ifelse(ever_col == 1 & last_col <= 120, 1, 0)) %>% 
  filter(new_col_adh != col_adh)
write.csv(col_nonadh,'col_nonadh.csv')

## NEW CRC screening adherence status
new_nonadh <- nonadh %>%
  mutate(fobt_adh = replace(fobt_adh, fobt_adh == 9, 0)) %>%
  mutate(fobt_adh = replace(fobt_adh, fobt_adh == 9, 0)) %>%
  mutate(new_fobt_adh = ifelse(ever_fobt == 1 & last_fobt <= 12 & !is.na(last_fobt), 1, 0)) %>%
  mutate(new_sig_adh = ifelse(ever_sig == 1 & last_sig <= 60 & !is.na(last_sig), 1, 0)) %>%
  mutate(new_col_adh = ifelse(ever_col == 1 & last_col <= 120 & !is.na(last_col), 1, 0)) %>%
  mutate(new_CRC_adh = ifelse(new_fobt_adh == 1 |
                                new_sig_adh == 1 |
                                new_col_adh == 1, 1, 0))
  
test <- new_nonadh %>%
  select(id, church, name, sex, age, ever_fobt, last_fobt, fobt_adh, sig_adh,
         col_adh, CRC_adh, new_CRC_adh) %>%
  filter(new_CRC_adh != CRC_adh)

## BREAST CANCER SCREENING
new_nonadh <- new_nonadh %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_304", 0)) %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_157", 0)) %>%
  mutate(new_mammo_adh = ifelse(sex == "M", NA, 
                                ifelse(ever_mammo == 1 & last_mammo <= 24 & !is.na(last_mammo), 1, 0)))

breast_nonadh <- new_nonadh %>%
  select(id, church, name, sex, age, ever_mammo, last_mammo, mammo_adh, new_mammo_adh) %>%
  filter(new_mammo_adh != mammo_adh)
write.csv(breast_nonadh,'breast_nonadh.csv')


## CERVICAL CANCER SCREENING
new_nonadh <- new_nonadh %>%
  mutate(new_pap_adh = ifelse(sex == "M", NA, 
                              ifelse((ever_pap == 1 & last_pap <= 36 & !is.na(last_pap)) | 
                                     (ever_HPV == 1 & last_HPV <= 60 & !is.na(last_HPV)) | 
                                     age < 50 | 
                                     age > 65 | 
                                     (hystr == 1 & !is.na(hystr)), 1, 0)))

cervic_nonadh <- new_nonadh %>%
  select(id, church, name, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
         pap_adh, new_pap_adh) %>%
  filter(new_pap_adh != pap_adh)
write.csv(cervic_nonadh,'cervic_nonadh.csv')

## PROSTATE CANCER SCREENING based on ever_psa_discuss
## NEW ADHERENCE STATUS
new_nonadh <- new_nonadh %>%
  mutate(still_nonadh = ifelse(sex == "M", ifelse(new_CRC_adh == 1 & ever_psa_discuss == 1, "No", "Yes"),
                               ifelse(new_CRC_adh == 1 &
                                        new_mammo_adh == 1 & 
                                        new_pap_adh == 1, "No", "Yes")))

# new_nonadh <- new_nonadh %>%
#   mutate(still_nonadh = ifelse(sex == "M", ifelse(new_CRC_adh == 0 | ever_psa_discuss == 0, "Yes", "No"),
#                                            ifelse(new_CRC_adh == 0 |
#                                                   new_mammo_adh == 0 | 
#                                                   new_pap_adh == 0, "Yes", "No")))

changed_nonadh <- new_nonadh %>%
    select(id, church, name, name, sex, age, new_CRC_adh, new_mammo_adh, 
           new_pap_adh, ever_psa_discuss, still_nonadh) %>%
    filter(still_nonadh == "No")
write.csv(changed_nonadh,'changed_nonadh.csv')

##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## ADHERENCE DATA SET 
colnames(adh)

check <- adh %>%
  select(id, church, name, sex, age, CRC_adh, mammo_adh, pap_adh, ever_psa_discuss)

table(adh$fobt_adh)
table(adh$sig_adh)
table(adh$col_adh)
table(adh$CRC_adh)
table(adh$mammo_adh)
table(adh$hystr)
table(adh$pap_adh)
table(adh$ever_psa_discuss)

## NEW CRC screening adherence status
new_adh <- adh %>%
  mutate(fobt_adh = replace(fobt_adh, fobt_adh == 9, 0)) %>%
  mutate(new_fobt_adh = ifelse(ever_fobt == 1 & last_fobt <= 12 & !is.na(last_fobt), 1, 0)) %>%
  mutate(new_sig_adh = ifelse(ever_sig == 1 & last_sig <= 60 & !is.na(last_sig), 1, 0)) %>%
  mutate(new_col_adh = ifelse(ever_col == 1 & last_col <= 120 & !is.na(last_col), 1, 0)) %>%
  mutate(new_CRC_adh = ifelse(new_fobt_adh == 1 |
                                new_sig_adh == 1 |
                                new_col_adh == 1, 1, 0))

test <- new_adh %>%
  select(id, church, name, sex, age, ever_fobt, last_fobt, fobt_adh, sig_adh,
         ever_col, last_col, col_adh, CRC_adh, new_CRC_adh) %>%
  filter(new_CRC_adh != CRC_adh)

## BREAST CANCER SCREENING
new_adh <- new_adh %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_304", 0)) %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_157", 0)) %>%
  mutate(new_mammo_adh = ifelse(sex == "M", NA, 
                                ifelse(ever_mammo == 1 & last_mammo <= 24 & !is.na(last_mammo), 1, 0)))

test <- new_adh %>%
  select(id, church, name, sex, age, ever_mammo, last_mammo, mammo_adh, new_mammo_adh) %>%
  filter(new_mammo_adh != mammo_adh)

# write.csv(test,'cha_adh_test.csv')

## CERVICAL CANCER SCREENING
new_adh <- new_adh %>%
  mutate(new_pap_adh = ifelse(sex == "M", NA, 
                              ifelse((ever_pap == 1 & last_pap <= 36 & !is.na(last_pap)) | 
                                       (ever_HPV == 1 & last_HPV <= 60 & !is.na(last_HPV)) | 
                                       age < 50 | 
                                       age > 65 | 
                                       (hystr == 1 & !is.na(hystr)), 1, 0)))

test <- new_adh %>%
  select(id, church, name, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
         pap_adh, new_pap_adh) %>%
  filter(new_pap_adh != pap_adh)


## PROSTATE CANCER SCREENING based on ever_psa_discuss
## NEW ADHERENCE STATUS
new_adh <- new_adh %>%
  mutate(still_adh = ifelse(sex == "M", ifelse(new_CRC_adh == 1 & ever_psa_discuss == 1, "Yes", "No"),
                               ifelse(new_CRC_adh == 1 &
                                        new_mammo_adh == 1 & 
                                        new_pap_adh == 1, "Yes", "No")))

# new_nonadh <- new_nonadh %>%
#   mutate(still_nonadh = ifelse(sex == "M", ifelse(new_CRC_adh == 0 | ever_psa_discuss == 0, "Yes", "No"),
#                                            ifelse(new_CRC_adh == 0 |
#                                                   new_mammo_adh == 0 | 
#                                                   new_pap_adh == 0, "Yes", "No")))

changed_adh <- new_adh %>%
  select(id, church, name, sex, age, new_CRC_adh, new_mammo_adh, 
         new_pap_adh, ever_psa_discuss, still_adh) %>%
  filter(still_adh == "No")
write.csv(changed_adh,'changed_adh.csv')

##---------------------------------------------------------------------------##
## NEW CRC screening adherence status
new_adh <- adh %>%
  mutate(new_fobt_adh = ifelse(ever_fobt == 1 & last_fobt <= 12, 1, 0)) %>%
  mutate(new_sig_adh = ifelse(ever_sig == 1 & last_sig <= 60, 1, 0)) %>%
  mutate(new_col_adh = ifelse(ever_col == 1 & last_col <= 120, 1, 0)) %>%
  mutate(new_CRC_adh = ifelse(new_fobt_adh == 1 |
                                new_sig_adh == 1 |
                                new_col_adh == 1, 1, 0))

test <- new_adh %>%
  select(id, church, name, sex, age, CRC_adh, new_CRC_adh) %>%
  filter(new_CRC_adh != CRC_adh)

write.csv(test,'cha_adh_test.csv')

## BREAST CANCER SCREENING
new_adh <- new_adh %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_304", 0)) %>%
  mutate(mammo_adh = replace(mammo_adh, id == "bs_157", 0)) %>%
  mutate(new_mammo_adh = ifelse(ever_mammo == 1 & last_mammo <= 24, 1, 0))

test <- new_adh %>%
  select(id, church, name, sex, age, ever_mammo, last_mammo, mammo_adh, new_mammo_adh) %>%
  filter(new_mammo_adh != mammo_adh)

write.csv(test,'cha_adh_test.csv')

## CERVICAL CANCER SCREENING

# check <- new_adh %>%
#   mutate(new_pap_adh = ifelse(sex == "M", NA,
#                               ifelse(age < 50 | age > 65 | hystr == 1, 1, 
#                                      ifelse((ever_pap == 1 & last_pap <= 36) | (ever_HPV == 1 & last_HPV <= 60), 1, 0)))) %>%
#   select(id, church, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
#          pap_adh, new_pap_adh)

# female_adh <- new_adh %>%
#   filter(sex == "F") %>%
#   select(id, church, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
#          pap_adh) %>%
#   mutate(new_pap_adh = ifelse((ever_pap == 1 & last_pap <= 36) | (ever_HPV == 1 & last_HPV <= 60) | 
#                                 age < 50 | age > 65 | hystr == 1, 1, 0))

new_adh <- new_adh %>%
  mutate(new_pap_adh = ifelse(sex == "F" & ((ever_pap == 1 & last_pap <= 36) | 
                                            (ever_HPV == 1 & last_HPV <= 60) | 
                                            age < 50 | 
                                            age > 65 | 
                                            hystr == 1), 1, 0)) %>%
  select(id, church, name, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
       pap_adh)

test <- new_adh %>%
  select(id, church, name, sex, age, ever_pap, last_pap, ever_HPV, last_HPV, hystr,
         pap_adh, new_pap_adh) %>%
  filter(new_pap_adh != pap_adh)

write.csv(test,'cha_adh_test.csv')

## PROSTATE CANCER SCREENING based on ever_psa_discuss
## NEW ADHERENCE STATUS
new_adh <- new_adh %>%
  mutate(still_adh = ifelse(new_CRC_adh == 0 |
                                 new_mammo_adh == 0 | 
                                 new_pap_adh == 0 | 
                                 ever_psa_discuss == 0, "No", "Yes"))
new_adh %>%
  select(id, church, name, sex, age, ever_psa_discuss) %>%
  filter(ever_psa_discuss == 0)

excluded_vars <- c("new_fobt_adh", "new_sig_adh", "new_col_adh", "date_bs", "name", "cha", 
                   "new_CRC_adh", "new_mammo_adh", "new_pap_adh")

test1 <- new_adh %>%
  filter(still_adh == "No") %>%
  select(-one_of(excluded_vars))
write.csv(test1,'cha_adh_test.csv')
