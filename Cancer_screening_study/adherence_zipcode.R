library(readxl)
library(tidyverse)
# install.packages("htmlTable")
install.packages("maps")
install.packages("devtools")
library(htmlTable)
library(zipcode)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

bl_adh <- read_excel("T:/U54 Pilot Study/DATA/Baseline Assessment.xlsx", sheet = 2)
bl_nonadh <- read_excel("T:/U54 Pilot Study/DATA/Baseline Assessment.xlsx", sheet = 1)

southLA.zip = c(90001, 90002, 90003, 90007, 90008,
                90011, 90016, 90018, 90037, 90043,
                90044, 90047, 90059, 90061, 90062,
                90220, 90221, 90222, 90262, 90723,
                90301, 90302, 90303, 90304, 90305,
                90250)

sclaco = c(90001, 90002, 90044, 90047, 90059)
scla = c(90003)
unila = c(90007, 90011, 90018, 90037, 90062)
crenshawlaco = c(90008, 90043)
crenshawla = c(90016)
westcompt = c(90061)
compton = c(90220, 90221, 90222)
lynwood = c(90262)
paramount = c(90723)
inglewood = c(90301, 90302, 90303, 90305)
inglennox = c(90304)
hawthorne = c(90250)

bl.adh.sLA <- bl_adh %>%
  select(church, name, sex, age, zip_code) %>%
  filter(zip_code %in% southLA.zip)

freqadh <- bl.adh.sLA %>%
  count(zip_code) %>%
  mutate(community = case_when(zip_code %in% sclaco ~ "South Central LA/Co.",
                               zip_code %in% scla ~ "South Central LA",
                               zip_code %in% unila ~ "University LA",
                               zip_code %in% crenshawlaco ~ "Crenshaw LA/Co.",
                               zip_code %in% crenshawla ~ "Crenshaw LA",
                               zip_code %in% westcompt ~ "West Compton LA/Co.",
                               zip_code %in% compton ~ "Compton",
                               zip_code %in% lynwood ~ "Lynwood",
                               zip_code %in% paramount ~ "Paramount",
                               zip_code %in% inglewood ~ "Inglewood",
                               zip_code %in% inglennox ~ "Inglewood (Lennox)",
                               zip_code %in% hawthorne ~ "Hawthorne")) %>%
  rename(adherent = n) %>%
  select(zip_code, community, adherent)

bl.nonadh.sLA <- bl_nonadh %>%
  select(church, name, sex, age, zip_code) %>%
  filter(zip_code %in% southLA.zip)

freqnonadh <- bl.nonadh.sLA %>%
  count(zip_code) %>%
  mutate(community = case_when(zip_code %in% sclaco ~ "South Central LA/Co.",
                               zip_code %in% scla ~ "South Central LA",
                               zip_code %in% unila ~ "University LA",
                               zip_code %in% crenshawlaco ~ "Crenshaw LA/Co.",
                               zip_code %in% crenshawla ~ "Crenshaw LA",
                               zip_code %in% westcompt ~ "West Compton LA/Co.",
                               zip_code %in% compton ~ "Compton",
                               zip_code %in% lynwood ~ "Lynwood",
                               zip_code %in% paramount ~ "Paramount",
                               zip_code %in% inglewood ~ "Inglewood",
                               zip_code %in% inglennox ~ "Inglewood (Lennox)",
                               zip_code %in% hawthorne ~ "Hawthorne")) %>%
  rename(non_adherent = n) %>%
  select(zip_code, community, non_adherent) 

freqtable <- right_join(freqadh, freqnonadh) %>%
  replace_na(list(adherent = 0, non_adherent = 0)) %>%
  mutate(total = adherent + non_adherent)

write.csv(freqtable,'adh_freq.csv')

