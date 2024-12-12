#set up####
setwd("C:/Users/Emma-Jane Murray/OneDrive - University College Dublin/Biomass/Data/Edited/Reactor Data/csv")

data <- read.csv("Data.csv")

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


data_eu <- filter(data, !is.na(ELIGIBLE_VALUE) & ELIGIBLE_VALUE > 0)
table(data_eu$ANIMAL_CLASS)

samp_eu <- data_eu %>% filter(age < 3650) %>% 
  mutate(sex_breed = paste(SEX, breedtype, sep="-")) %>%
  group_by(ANIMAL_CLASS) %>% sample_n(1000) %>% ungroup() %>%
  dplyr::select(ELIGIBLE_VALUE, ANIMAL_CLASS, sex_breed, breedtype, year, month, age, SEX,
                HERD_TYPE)


sampF_eu <- filter(samp_eu, SEX=="F")
sampM_eu <- filter(samp_eu, SEX=="M")

segm_eu_F <- segmented(lmodFe <- lm(ELIGIBLE_VALUE ~ breedtype + month + year + age, 
                                  data=sampF_eu), ~ age, npsi=2)
segm_eu_M <- segmented(lmodMe <- lm(ELIGIBLE_VALUE ~ breedtype + month + year + age, 
                                  data=sampM_eu), ~ age, npsi=2)

rm(data, data_eu, samp_eu, sampF_eu, sampM_eu)
gc()
