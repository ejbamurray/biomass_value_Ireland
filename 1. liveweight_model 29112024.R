#set up####

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(magrittr)
library(writexl)
library(tidyr)
library(tidyverse)
library(segmented)

setwd("C:/Users/Emma-Jane Murray/OneDrive - University College Dublin/Biomass/Data/Edited/Reactor Data/csv")

data <- read.csv("Data.csv")#this is the cleaned data#

#formatting####
data$DOB <- dmy(data$DOB)
data$DOD <- dmy(data$DOD)
data$VALUATION_DATE <- dmy(data$VALUATION_DATE)
data$age <- as.numeric(data$VALUATION_DATE-data$DOB)
data$year <- year(data$VALUATION_DATE)
data$month <- month(data$VALUATION_DATE)

data<-filter(data, !is.na(DOB))
data<-filter(data, age>0)



#coding breeds####
data$br2 <- substr(data$BREED, 1, 2)
data$breedtype <- recode(data$br2, 'AY'="DAIRY", 'BS'="DAIRY", 'FR'="DAIRY", 
                          'JE'="DAIRY", 'NR'="DAIRY", 'GU'="DAIRY", 'LV'="BEEF", 'SR'="DAIRY", 
                          'AA'="BEEF-BI", 'AU'="BEEF", 'BA'="BEEF", 'BB'="BEEF", 'BY'="BEEF-BI", 'CH'="BEEF", 
                          'DX'="BEEF-BI", 'GA'="BEEF-BI", 'HE'="BEEF-BI", 'HI'="BEEF-BI", 'IM'="BEEF-BI", 
                          'KE'="BEEF-BI", 'LM'="BEEF", 'MH'="BEEF", 'MO'="DAIRY", 'MY'="DAIRY", 'PI'="BEEF", 
                          'PT'="BEEF", 'RB'="DAIRY", 'RM'="BEEF", 'SA'="BEEF", 'SH'="BEEF-BI", 'SI'="BEEF", 
                          'BT'="BEEF", 'LU'="BEEF", 'MA'="BEEF", 'RP'="BEEF", 'SP'="BEEF", 'ST'="BEEF", 'WA'="BEEF", 
                          'WP'="BEEF",
                          'BG'="BEEF-BI", 'BW' = "BEEF", 'LH' = "BEEF", 'SL' = "BEEF-BI", 'NO' = "DAIRY", 'LR' = "BEEF",
                          'RD' = "DAIRY", 'AK' = "BEEF", 'AN'="DAIRY", 'BF' = "BEEF", 'BL' = "DAIRY",
                          'BZ' = "BEEF", 'CL' = "BEEF", 'DE' = "BEEF-BI", 'DN' = "BEEF-BI", 'FE' = "DAIRY",
                          'GB' = "DAIRY", 'GS' = "BEEF-BI", 'GY' = "BEEF", 'MS' = "DAIRY", 'PZ' = "BEEF",
                          'SG' = "BEEF", 'VO' = "BEEF", 'WW' = "BEEF",
                          'AL' = "BEEF", 'AM' = "DAIRY", 'BE' = "BEEF", 'BI' = "BEEF", 'BP' = "DAIRY", 'BR' = "BEEF",
                          'CB' = "BEEF", 'CI' = "BEEF", 'EF' = "BEEF", 'EP' = "BEEF-BI", 'GL' = "BEEF-BI", 
                          'LJ' = "DAIRY", 'MK' = "DAIRY", 'OE' = "BEEF-BI", 'PS' = "BEEF", 'RE' = "DAIRY",
                          'SD' = "BEEF-BI", 'SU' = "BEEF-BI", 'TY' = "BEEF-BI", 'VA' = "BEEF", 'VB' = "BEEF", 
                          'WB' = "BEEF-BI", 'WG' = "BEEF-BI", 'YK' = "BEEF", 'ZE' = "BEEF")

table(data$breedtype)

data <- data %>% mutate(sex_breed = paste(SEX, breedtype, sep="-"))
data$age_bin = ntile(data$age, n = 20)

names <- c('month' ,'year', 'SEX', 'breedtype')
data[,names] <- lapply(data[,names] , factor)

#mass prediction####


BULLS = filter(data, ANIMAL_CLASS == "BULL")
COWS = filter(data, ANIMAL_CLASS %in% c("COW"))
HEIFERS = filter(data, ANIMAL_CLASS %in% c("HEIFER"))
PREG_HEIFERS = filter(data, ANIMAL_CLASS %in% c("PREGNANT HEIFER"))
STEERS = filter(data, ANIMAL_CLASS %in% c("STEER"))
CALVES = filter(data, ANIMAL_CLASS %in% c("CALF"))

CALVES <- filter(CALVES, LIVE_WEIGHT >=30 & LIVE_WEIGHT <1400)
BULLS = filter(BULLS, LIVE_WEIGHT >=200 & LIVE_WEIGHT <1400)
COWS = filter(COWS, LIVE_WEIGHT >=200 & LIVE_WEIGHT <1400)
HEIFERS = filter(HEIFERS, LIVE_WEIGHT >=30 & LIVE_WEIGHT <1400)
PREG_HEIFERS = filter(PREG_HEIFERS, LIVE_WEIGHT >=200 & LIVE_WEIGHT <1400)
STEERS = filter(STEERS, LIVE_WEIGHT >=30 & LIVE_WEIGHT <1400)

#merge animal class dataframes#


data_kg <- bind_rows(CALVES, BULLS, COWS, HEIFERS, PREG_HEIFERS, STEERS)

rm(CALVES)
rm(BULLS)
rm(COWS)
rm(HEIFERS)
rm(PREG_HEIFERS)
rm(STEERS)
gc()

table(data_kg$ANIMAL_CLASS)

samp <- data_kg %>% filter(age < 3650) %>% 
  group_by(ANIMAL_CLASS) %>% sample_n(size=600) %>% ungroup() %>%
  dplyr::select(LIVE_WEIGHT, breedtype, year, month, age, SEX)

sampF <- filter(samp, SEX=="F")
sampM <- filter(samp, SEX=="M")

segm_kg_F <- segmented(lmodF <- lm(LIVE_WEIGHT ~ breedtype + month + year + age, 
                                data=sampF), ~ age, npsi=2)
segm_kg_M <- segmented(lmodM <- lm(LIVE_WEIGHT ~ age + month + year + breedtype, 
                                  data=sampM), ~ age, npsi=2)


rm(data, samp, names)
gc()