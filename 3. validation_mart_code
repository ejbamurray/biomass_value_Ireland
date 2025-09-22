
##need to run analysis 31-1-24 first to fit the models

#setwd("#pathway#")#

library(stringr)
library(dplyr)
library(data.table)
library(tidyverse)
library(segmented)


mart <-read.csv("mart/data/Dairy 2011-2022.csv")
non_breed <- fread("mart/data/Nonbreeding 2011-2022.csv")

mart <- filter(mart, Category != "")

sum(mart$Birth.Year!="")
mart$Category = trimws(mart$Category)
mart$Category = gsub(" ", "", mart$Category, fixed = T)

table(mart$Category)
mart$cat <- paste(str_sub(mart$Category, 3, 6), str_sub(mart$Category, -1, -1), sep="")
mart$cat_ln <- str_sub(mart$cat, -1)
mart$cat_ln[mart$cat_ln == "+"] <- 10
mart$cat_ln <- as.numeric(mart$cat_ln)
mart$cat_ln[mart$cat_ln > 3] <- "4+"

mart$cat_sub <- str_sub(mart$Category, 3, 6)

mart$cat_corr <- ifelse(mart$cat_sub == "Lact", paste(mart$cat_sub, mart$cat_ln, sep=""),
                        mart$cat)

table(mart$cat_corr)


mart$cat_corr[mart$cat_corr == "Inca)"] <- "InCalfHeifer"
mart$cat_corr[mart$cat_corr == "Heifs"] <- "HeiferCalf"
mart$cat_corr[mart$cat_corr == "Maidr"] <- "MaidenHeifer"
mart$cat_corr[mart$cat_corr == "Weanr"] <- "WeanedHeifer"

###
mart$Average.Price <- as.numeric(mart$Average.Price)

mart_summ <- mart %>% group_by(cat_corr) %>% summarise(real = mean(Average.Price, na.rm=T))



link <-read.csv("mart/data/category_lookup.csv")
link$age <- link$Age_months * 30
pred <- data.frame(age = 30*seq(1, 120, 1), breedtype="DAIRY", year = "2018", month="6", 
                   ELIGIBLE_VALUE= seq(1, 120, 1))


###READ IN REACTOR DATA, TIDY AND CREATE MODEL

data <- read.csv("Raw Data.csv")

data$DOB <- dmy(data$DOB)
data$DOD <- dmy(data$DOD)
data$VALUATION_DATE <- dmy(data$VALUATION_DATE)
data$age <- as.numeric(data$VALUATION_DATE-data$DOB)
data$year <- year(data$VALUATION_DATE)
data$month <- month(data$VALUATION_DATE)

data<-filter(data, !is.na(DOB))
data<-filter(data, age>0)

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





segm_eu_F <- segmented(lmod <- lm(ELIGIBLE_VALUE ~ breedtype + month + year + age, 
                                  data=sampF_eu), ~ age, npsi=2)

newdf <- pred
X <- model.matrix(lmod, data=newdf)

newdf <- cbind(newdf, X[,setdiff(colnames(X), names(newdf))])

newdf$p1F <- predict(segm_eu_F, newdata = newdf)

pred <- left_join(newdf, link)

mart_pred = pred %>% group_by(Animal_category) %>% summarise(predicted = mean(p1F, na.rm=T)) %>%
  rename(cat_corr = Animal_category)


join = left_join(mart_summ, mart_pred)



###suckler categories
predM <- data.frame(age = 30*seq(1, 120, 1), breedtype=c("BEEF", "BEEF-BI"), 
                   year = "2018", month="6", 
                   ELIGIBLE_VALUE= seq(1, 120, 1))

pred <- data.frame(age = 30*seq(1, 120, 1), breedtype=c("BEEF", "BEEF-BI"), 
                   year = "2018", month="6", 
                   ELIGIBLE_VALUE= seq(1, 120, 1))


segm_eu_M <- segmented(lmod <- lm(ELIGIBLE_VALUE ~ breedtype + month + year + age, 
                                  data=sampM_eu), ~ age, npsi=2)
##male predict
newdfM <- predM
Xm <- model.matrix(lmod, data=newdfM)

newdfM <- cbind(newdfM, X[,setdiff(colnames(Xm), names(newdfM))])

newdfM$p1F <- predict(segm_eu_M, newdata = newdfM)
newdfM$sex="M"


##female predict
newdf <- pred
X <- model.matrix(lmod, data=newdf)

newdf <- cbind(newdf, X[,setdiff(colnames(X), names(newdf))])

newdf$p1F <- predict(segm_eu_F, newdata = newdf)
newdf$sex="F"

newdf <- bind_rows(newdf, newdfM)



link <-read.csv("mart/data/beef_cat_lookup.csv")
link$age <- link$age * 30


pred <- left_join(link, newdf)

mart_pred = pred %>% group_by(Animal_category) %>% summarise(predicted = mean(p1F, na.rm=T)) %>%
  rename(cat_corr = Animal_category)
mart_pred






###suckler mart data
suckler <- read.csv("mart/data/Suckler 2011-2022.csv")
suckler <- dplyr::select(suckler, Category, Breed.Type, Average.Price)
table(suckler$Category)
suckler_cat <- as.data.frame(table(suckler$Category))
suckler_cat
suckler_cat$Animal_category = c("Lact1", "Lact2", 
                                "Lact3", rep("Lact4+", 7),
                                rep("InCalfHeifer", 2))
names(suckler_cat)[1] <- "Category"
suckler <- left_join(suckler, suckler_cat)

nonbreed <- read.csv("mart/data/Nonbreeding 2011-2022.csv")
nonbreed <- dplyr::select(nonbreed, Category, Breed.Type, Average.Price)
nonbreed_cat <- as.data.frame(table(nonbreed$Category))
nonbreed_cat
nonbreed_cat$Animal_category = c(NA, "LightStore", "Finished", rep("CullCow", 3),
                                 rep("Calf", 2), "LightStore", "Weaned", 
                                 rep("YoungBullFeeder", 2), "Finished", rep("ForwardStore", 3),
                                 "Weaned", NA)
names(nonbreed_cat)[1] <- "Category"
nonbreed <- left_join(nonbreed, nonbreed_cat)
nonbreed$Average.Price <- as.numeric(nonbreed$Average.Price)

beef <- bind_rows(suckler, nonbreed)
beef_cumm <- beef %>% group_by(Animal_category) %>% 
  summarise(real = mean(Average.Price, na.rm=T), nReal=n())

names(mart_pred)[1] <- "Animal_category"
comb <- left_join(mart_pred, beef_cumm)




##write outputs
write.csv(join, "dairy_mart_validation.csv")
write.csv(comb, "beef_mart_validation.csv")





