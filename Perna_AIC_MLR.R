#iMLR, AIC, fMLR 01/28/2021
###Model 1:  Settlement, WQ, FDM
###Model 2:  Bycatch, WQ, FDM, FIM BC
#

library(plyr)
library(tidyverse)
library(zoo)
library(ggpubr)
library(MASS)
library(car)
library(broom)

TableOutput <- function(TABLE, ModelNames){
  table.cols <- c("deviance", "r.squared", "adj.r.squared","df.residual", "AIC")
  reported.table <- TABLE[table.cols]
  names(reported.table) <- c("RSS", "R2", "Adj.R2", "Resid.Df", "AIC")
  
  reported.table[['dAIC']] <-  with(reported.table, AIC - min(AIC))
  reported.table[['weight']] <- with(reported.table, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
  reported.table$weight <- round(reported.table$weight, 3)
  reported.table$dAIC <- round(reported.table$dAIC, 3)
  reported.table$RSS <- round(reported.table$RSS, 3)
  reported.table$R2 <- round(reported.table$R2, 3)
  reported.table$Adj.R2 <- round(reported.table$Adj.R2, 3)
  row.names(reported.table) <- ModelNames
  
  return(reported.table)
}
#
####Settlement####
##Spat
Perna <- read.csv("../CSV/Output/Baywide/DetrendedLogSpat.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% dplyr::select(-c(MonYr))
#WQ parameters
SetWQ <- read.csv("../CSV/Output/Baywide/Settlement_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) 
#Crabs
BFDM <- read.csv("../CSV/Output/Baywide/B_FDM_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
SFDM <- read.csv("../CSV/Output/Baywide/S_FDM_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

##Join wq and crabs to Perna

#MTB and LTB
Sett <- full_join(Perna, SetWQ, by = c("Year", "Month")) %>% full_join(BFDM) %>% full_join(SFDM)
#
write.csv(Sett, file = "../CSV/Output/Baywide/Settlement_data.csv", row.names = FALSE)
#
rm(Perna, SetWQ, BFDM, SFDM)
#
####Adults####
##Adults
Bycatch <- read.csv("../CSV/Output/Baywide/Adult_MoYr_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))  %>% dplyr::select(-c(MonYr))
#WQ parameters
ByWQ <- read.csv("../CSV/Output/Baywide/Bycatch_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) 
#Crabs
FIM <- read.csv("../CSV/Output/Baywide/FIM_BC_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
BFDM <- read.csv("../CSV/Output/Baywide/B_FDM_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
SFDM <- read.csv("../CSV/Output/Baywide/S_FDM_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

##Join wq and crabs 
#MTB and LTB
Byc <- full_join(Bycatch, ByWQ, by = c("Year", "Month")) %>% full_join(FIM) %>% full_join(BFDM) %>% full_join(SFDM)
#
write.csv(Byc, file = "../CSV/Output/Baywide/Bycatch_data.csv", row.names = FALSE)
#Clean up dfs/items 
rm(Bycatch, ByWQ, FIM, BFDM, SFDM)
#
####Settlement - Model####
#initial MLR
Sett_fill <- Sett[complete.cases(Sett), ] %>% dplyr::select(-c("Month"))
set.seed(54321)
fullSett <- lm(SpatAdj ~ ., data = Sett_fill)
fullSett_sum <- summary(fullSett)
fullSett_sum
#MLR table with test values
fullSett_tab <- tidy(fullSett)
names(fullSett_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullSett_sum_tab <- glance(fullSett) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullSett_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#

##AIC - Model selection 
Settstep <- stepAIC(fullSett, direction = "backward")
#Build all models
fullSett2 <- update(fullSett, .~. -CHlaadj, data = Sett_fill)
fullSett3 <- update(fullSett2, .~. -Tempadj, data = Sett_fill)
fullSett4 <- update(fullSett3, .~. -pHadj, data = Sett_fill)
fullSett5 <- update(fullSett4, .~. -Saladj, data = Sett_fill)
fullSett6 <- update(fullSett5, .~. -DOadj, data = Sett_fill)
fullSett7 <- update(fullSett6, .~. -Phosadj, data = Sett_fill)
fullSett8 <- update(fullSett7, .~. -DOpadj, data = Sett_fill)

model.names.Sett <- c("Year, Temperature, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM", 
                        "Year, Temperature, Salinity, Secchi, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM", 
                        "Year, Salinity, Secchi, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM", 
                        "Year, Salinity, Secchi, DO, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Secchi, DO, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Secchi, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Secchi, DO%, Nitrogen, BFDM, SFDM", 
                        "Year, Secchi, Nitrogen, BFDM, SFDM")
summ.Sett <- do.call(rbind,lapply(list(fullSett, fullSett2, fullSett3, fullSett4, fullSett5, fullSett6, fullSett7, fullSett8), broom::glance))
summ.Sett

Setttab <- TableOutput(summ.Sett, model.names.Sett)

###Reporting final model
#fullBayWQ
fullSett8
fullSett8_sum <- summary(fullSett8)
#MLR table with test values
fullSett8_tab <- tidy(fullSett8)
names(fullSett8_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullSett8_sum_tab <- glance(fullSett8) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullSett8_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#Save model for figure use
save(fullSett8, file = "../CSV/Output/Baywide/Models/fullSett8_sep.rda")
#
#
Sett <- Sett %>% mutate(MonYr = as.yearmon(paste(Month, Year, sep = "/"), format = "%m/%Y"))
Sett %>% ggplot2::ggplot(aes(x = MonYr, y = SFDM_Adj))+
  geom_line(linetype = "dashed") + geom_smooth(linetype = "dashed", se = F)
#Remove unneeded items
rm(Sett_fill, fullSett, fullSett2, fullSett8_sum, fullSett_tab, fullSett8_sum_tab, fullSett3, fullSett4, fullSett5, fullSett6, fullSett7, Settstep, model.names.Sett, summ.Sett)
#
####Bycatch - Model####
#initial MLR
By_fill <- Byc[complete.cases(Byc), ] %>% dplyr::select(-c("Month"))
set.seed(54321)
fullBy <- lm(PV ~ ., data = By_fill)
fullBy_sum <- summary(fullBy)
fullBy_sum
#MLR table with test values
fullBy_tab <- tidy(fullBy)
names(fullBy_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullBy_sum_tab <- glance(fullBy) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullBy_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#

##AIC - Model selection 
Bystep <- stepAIC(fullBy, direction = "backward")
#Build all models
fullBy2 <- update(fullBy, .~. -Saladj, data = By_fill)
fullBy3 <- update(fullBy2, .~. -Nadj, data = By_fill)
fullBy4 <- update(fullBy3, .~. -DOadj, data = By_fill)
fullBy5 <- update(fullBy4, .~. -pHadj, data = By_fill)
fullBy6 <- update(fullBy5, .~. -Phosadj, data = By_fill)
fullBy7 <- update(fullBy6, .~. -FBCAdj, data = By_fill)
fullBy8 <- update(fullBy7, .~. -PerSecadj, data = By_fill)

model.names.By <- c("Year, Temperature, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM, BC", 
                      "Year, Temperature, Secchi, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM, BC", 
                      "Year, Temperature, Secchi, ChlA, DO, DO%, pH, Phosphorus, BFDM, SFDM, BC", 
                      "Year, Temperature, Secchi, ChlA, DO%, pH, Phosphorus, BFDM, SFDM, BC", 
                      "Year, Temperature, Secchi, ChlA, DO%, Phosphorus, BFDM, SFDM, BC", 
                      "Year, Temperature, Secchi, ChlA, DO%, BFDM, SFDM, BC", 
                      "Year, Temperature, Secchi, ChlA, DO%, BFDM, SFDM", 
                      "Year, Temperature, ChlA, DO%, BFDM, SFDM")
summ.By <- do.call(rbind,lapply(list(fullBy, fullBy2, fullBy3, fullBy4, fullBy5, fullBy6, fullBy7, fullBy8), broom::glance))
summ.By

Bytab <- TableOutput(summ.By, model.names.By)

###Reporting final model
#fullBayWQ
fullBy8
fullBy8_sum <- summary(fullBy8)
#MLR table with test values
fullBy8_tab <- tidy(fullBy8)
names(fullBy8_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullBy8_sum_tab <- glance(fullBy8) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullBy8_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#Save model for figure use
save(fullBy8, file = "../CSV/Output/Baywide/Models/fullBy8_sep.rda")
#
Byc <- Byc %>% mutate(MonYr = as.yearmon(paste(Month, Year, sep = "/"), format = "%m/%Y"))
Byc %>% ggplot2::ggplot(aes(x = MonYr, y = SFDM_Adj))+
  geom_line(linetype = "dashed") + geom_smooth(linetype = "dashed", se = F)
#Remove unneeded items
rm(By_fill, fullBy, fullBy2, fullBy8_sum, fullBy_tab, fullBy8_sum_tab, fullBy3, fullBy4, fullBy5, fullBy6, fullBy7, Bystep, model.names.By, summ.By)
#
#####
#
#
#
####Regional models####
#
#
#
###Settlement####
##Spat
OTB <- read.csv("../CSV/Output/DetrendedLogBays_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% dplyr::select(Month:Year, DlogOTB)
Bays <- read.csv("../CSV/Output/DetrendedLogBays_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% dplyr::select(Month:Year, DlogBoth)
#WQ parameters
TBWQ <- read.csv("../CSV/Output/LogWQdata_TBadjusted.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) 
OTBWQ <- read.csv("../CSV/Output/LogWQdata_OTBadjusted.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#Crabs
FDM <- read.csv("../CSV/Output/FDM_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

##Join wq and crabs to each spat df as needed - 1 value/year

#MTB and LTB
Bays_C <- full_join(Bays, TBWQ, by = c("Year", "Month")) %>% full_join(FDM, by = c("Year", "Month")) 
#OTB
OTB_C <- full_join(OTB, OTBWQ, by = c("Year", "Month")) %>% full_join(FDM, by = c("Year", "Month"))
#
write.csv(Bays_C, file = "../CSV/Output/Bays_crabs_sep.csv", row.names = FALSE)
write.csv(OTB_C, file = "../CSV/Output/OTB_crabs_sep.csv", row.names = FALSE)
#
rm(OTB, Bays, FDM, TBWQ, OTBWQ)
#
###Bycatch####
Lower <- read.csv("../CSV/Output/Log_adult_lower.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% 
  mutate(Year = as.integer(format(as.yearmon(MonYr, format="%b %Y"),"%Y")), Month = as.integer(format(as.yearmon(MonYr, format="%b %Y"),"%m"))) %>%
  dplyr::select(Year, Month, logPV)
Upper <- read.csv("../CSV/Output/Log_adult_upper.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% 
  mutate(Year = as.integer(format(as.yearmon(MonYr, format="%b %Y"),"%Y")), Month = as.integer(format(as.yearmon(MonYr, format="%b %Y"),"%m"))) %>%
  dplyr::select(Year, Month, logPV)
#WQ parameters
LowWQ <- read.csv("../CSV/Output/WQdata_Loweradjusted.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) 
UppWQ <- read.csv("../CSV/Output/WQdata_Upperadjusted.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) 
#Crabs
FDM <- read.csv("../CSV/Output/FDM_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
FIM <- read.csv("../CSV/Output/FIM_BC_adj.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

##Join wq and crabs to each spat df as needed - 1 value/year

#MTB and LTB
Lower_Fs <- full_join(Lower, LowWQ, by = c("Year", "Month")) %>% full_join(FDM, by = c("Year", "Month")) %>% full_join(FIM, by = c("Year", "Month"))
#OTB
Upper_Fs <- full_join(Upper, UppWQ, by = c("Year", "Month")) %>% full_join(FDM, by = c("Year", "Month")) %>% full_join(FIM, by = c("Year", "Month"))
#
write.csv(Lower_Fs, file = "../CSV/Output/Lower_Crabs_sep.csv", row.names = FALSE)
write.csv(Upper_Fs, file = "../CSV/Output/Upper_Crabs_sep.csv", row.names = FALSE)
#Clean up dfs/items 
rm(Lower, Upper, UppWQ, FDM, FIM)
#
####Sett - models####
###LTB + MTB
#Currently using PerSeccDepth
#initial MLR
Bays_fill <- Bays_C[complete.cases(Bays_C), ] %>% dplyr::select(-c("Month"))
set.seed(54321)
fullBayC <- lm(DlogBoth ~ ., data = Bays_fill)
fullBayC_sum <- summary(fullBayC)
fullBayC_sum
#MLR table with test values
fullBay_tab <- tidy(fullBayC)
names(fullBay_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullBay_sum_tab <- glance(fullBayC) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullBay_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#

##AIC - Model selection 
BayCstep <- stepAIC(fullBayC, direction = "backward")
#Build all models
fullBayC2 <- update(fullBayC, .~. -pHadj, data = Bays_fill)
fullBayC3 <- update(fullBayC2, .~. -Tempadj, data = Bays_fill)
fullBayC4 <- update(fullBayC3, .~. -DOadj, data = Bays_fill)
fullBayC5 <- update(fullBayC4, .~. -CHlaadj, data = Bays_fill)
fullBayC6 <- update(fullBayC5, .~. -Saladj, data = Bays_fill)
fullBayC7 <- update(fullBayC6, .~. -Phosadj, data = Bays_fill)

model.names.BaysC <- c("Year, Temperature, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM", 
                        "Year, Temperature, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Salinity, Secchi, ChlA, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Salinity, Secchi, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Secchi, DO%, Nitrogen, Phosphorus, BFDM, SFDM", 
                        "Year, Secchi, DO%, Nitrogen, BFDM, SFDM")
summ.baysC <- do.call(rbind,lapply(list(fullBayC, fullBayC2, fullBayC3, fullBayC4, fullBayC5, fullBayC6, fullBayC7), broom::glance))
summ.baysC

BayCtab <- TableOutput(summ.baysC, model.names.BaysC)

###Reporting final model
#fullBayWQ
fullBayC7
fullBayC7_sum <- summary(fullBayC7)
#MLR table with test values
fullBayC7_tab <- tidy(fullBayC7)
names(fullBayC7_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullBayC7_sum_tab <- glance(fullBayC7) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullBayC7_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#Save model for figure use
save(fullBayC7, file = "../CSV/Output/Models/fullBayC7_sep.rda")
#Remove unneeded items
rm(Bays_fill, fullBayC, fullBayC_sum, fullBay_tab, fullBay_sum_tab, fullBayC2, fullBayC3, fullBayC4, fullBayC5, fullBayC6, BayCstep, model.names.BaysC, summ.baysC)
#

###OTB
#Currently using PerSeccDepth
#initial MLR
OTB_fill <- OTB_C[complete.cases(OTB_C), ] %>% dplyr::select(-c("Month"))
set.seed(54321)
fullOTBC <- lm(DlogOTB ~ ., data = OTB_fill)
fullOTBC_sum <- summary(fullOTBC)
fullOTBC_sum
#MLR table with test values
fullOTB_tab <- tidy(fullOTBC)
names(fullOTB_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullOTB_sum_tab <- glance(fullOTBC) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullOTB_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#

##AIC - Model selection 
OTBCstep <- stepAIC(fullOTBC, direction = "backward")
#Build all models
fullOTBC2 <- update(fullOTBC, .~. -PerSecadj, data = OTB_fill)
fullOTBC3 <- update(fullOTBC2, .~. -Saladj, data = OTB_fill)
fullOTBC4 <- update(fullOTBC3, .~. -pHadj, data = OTB_fill)
fullOTBC5 <- update(fullOTBC4, .~. -DOadj, data = OTB_fill)
fullOTBC6 <- update(fullOTBC5, .~. -Tempadj, data = OTB_fill)
fullOTBC7 <- update(fullOTBC6, .~. -CHlaadj, data = OTB_fill)
fullOTBC8 <- update(fullOTBC7, .~. -DOpadj, data = OTB_fill)
fullOTBC9 <- update(fullOTBC8, .~. -BFDMAdj, data = OTB_fill)
fullOTBC10 <- update(fullOTBC9, .~. -SFDMAdj, data = OTB_fill)


model.names.OTBC <- c("Year, Temperature, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM", #1
                       "Year, Temperature, Salinity, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM", #2
                      "Year, Temperature, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, BFDM, SFDM", #3
                      "Year, Temperature, ChlA, DO, DO%, Nitrogen, Phosphorus, BFDM, SFDM", #4
                      "Year, Temperature, ChlA, DO%, Nitrogen, Phosphorus, BFDM, SFDM", #5
                      "Year, ChlA, DO%, Nitrogen, Phosphorus, BFDM, SFDM", #6
                      "Year, DO%, Nitrogen, Phosphorus, BFDM, SFDM", #7
                      "Year, Nitrogen, Phosphorus, BFDM, SFDM", #8
                      "Year, Nitrogen, Phosphorus, SFDM", #9
                      "Year, Nitrogen, Phosphorus") #10
summ.OTBC <- do.call(rbind,lapply(list(fullOTBC, fullOTBC2, fullOTBC3, fullOTBC4, fullOTBC5, fullOTBC6, fullOTBC7, fullOTBC8, fullOTBC9, fullOTBC10), broom::glance))
summ.OTBC

OTBCtab <- TableOutput(summ.OTBC, model.names.OTBC)

###Reporting final model
#fullOTBC
fullOTBC10
fullOTBC10_sum <- summary(fullOTBC10)
#MLR table with test values
fullOTBC10_tab <- tidy(fullOTBC10)
names(fullOTBC10_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullOTBC10_sum_tab <- glance(fullOTBC10) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullOTBC10_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#Save model for figure use
save(fullOTBC10, file = "../CSV/Output/Models/fullOTBC10_sep.rda")
#Remove unneeded items
rm(OTB_fill, fullOTBC, fullOTBC_sum, fullOTB_tab, fullOTB_sum_tab, fullOTBC2, fullOTBC3, fullOTBC4, fullOTBC5, fullOTBC6, fullOTBC7, fullOTBC8, fullOTBC9, OTBCstep, model.names.OTBC, summ.OTBC)
#
####By - models####
#Lower
Lower_fill2 <- Lower_Fs[complete.cases(Lower_Fs), ] %>% dplyr::select(-c("Month"))
set.seed(54321)
fullLowC <- lm(logPV ~ ., data = Lower_fill2)
fullLowC_sum <- summary(fullLowC)
fullLowC_sum
#MLR table with test values
fullLowC_tab <- tidy(fullLowC)
names(fullLowC_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullLowC_sum_tab <- glance(fullLowC) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullLowC_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#

##AIC - Model selection 
LowCstep <- stepAIC(fullLowC, direction = "backward")
#Build all models
fullLowC2 <- update(fullLowC, .~. -CHlaadj, data = Lower_fill2)
fullLowC3 <- update(fullLowC2, .~. -Tempadj, data = Lower_fill2)
fullLowC4 <- update(fullLowC3, .~. -DOadj, data = Lower_fill2)
fullLowC5 <- update(fullLowC4, .~. -BFDMAdj, data = Lower_fill2)
fullLowC6 <- update(fullLowC5, .~. -FBCAdj, data = Lower_fill2)
fullLowC7 <- update(fullLowC6, .~. -pHadj, data = Lower_fill2)
fullLowC8 <- update(fullLowC7, .~. -PerSecadj, data = Lower_fill2)
fullLowC9 <- update(fullLowC8, .~. -Saladj, data = Lower_fill2)
fullLowC10 <- update(fullLowC9, .~. -SFDMAdj, data = Lower_fill2)

model.names.lowC <- c("Year, Temperature, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM", 
                      "Year, Temperature, Salinity, Secchi, DO, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM", 
                      "Year, Salinity, Secchi, DO, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM", 
                      "Year, Salinity, Secchi, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM", 
                      "Year, Salinity, Secchi, DO%, Nitrogen, pH, Phosphorus, Stone FDM, Blue FIM", 
                      "Year, Salinity, Secchi, DO%, Nitrogen, pH, Phosphorus, Stone FDM", 
                      "Year, Salinity, Secchi, DO%, Nitrogen, Phosphorus, Stone FDM", 
                      "Year, Salinity, DO%, Nitrogen, Phosphorus, Stone FDM", 
                      "Year, DO%, Nitrogen, Phosphorus, Stone FDM", 
                      "Year, DO%, Nitrogen, Phosphorus")
summ.lowC <- do.call(rbind,lapply(list(fullLowC, fullLowC2, fullLowC3, fullLowC4, fullLowC5, fullLowC6, fullLowC7, fullLowC8, fullLowC9, fullLowC10), broom::glance))
summ.lowC

LowCtab <- TableOutput(summ.lowC, model.names.lowC)

###Reporting final model
#fullBayWQ
fullLowC10
fullLowC10_sum <- summary(fullLowC10)
#MLR table with test values
fullLowC10_tab <- tidy(fullLowC10)
names(fullLowC10_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullLowC10_sum_tab <- glance(fullLowC10) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullLowC10_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#Save model for figure use
save(fullLowC10, file = "../CSV/Output/Models/fullLowC10_sep.rda")
#Remove uneeded items
rm(Lower_fill2, fullLowC, fullLowC_sum, fullLowC_tab, fullLowC_sum_tab, fullLowC2, fullLowC3, fullLowC4, fullLowC5, fullLowC6, fullLowC7, fullLowC8, fullLowC9, model.names.lowC, summ.lowC)
#

##Upper
Upper_fill2 <- Upper_Fs[complete.cases(Upper_Fs), ] %>% dplyr::select(-c("Month"))
set.seed(54321)
fullUppC <- lm(logPV ~ ., data = Upper_fill2)
fullUppC_sum <- summary(fullUppC)
fullUppC_sum
#MLR table with test values
fullUppC_tab <- tidy(fullUppC)
names(fullUppC_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullUppC_sum_tab <- glance(fullUppC) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullUppC_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#

##AIC - Model selection 
UppCstep <- stepAIC(fullUppC, direction = "backward")
#Build all models
fullUppC2 <- update(fullUppC, .~. -CHlaadj, data = Upper_fill2)
fullUppC3 <- update(fullUppC2, .~. -PerSecadj, data = Upper_fill2)
fullUppC4 <- update(fullUppC3, .~. -Saladj, data = Upper_fill2)
fullUppC5 <- update(fullUppC4, .~. -Phosadj, data = Upper_fill2)
fullUppC6 <- update(fullUppC5, .~. -Tempadj, data = Upper_fill2)
fullUppC7 <- update(fullUppC6, .~. -DOadj, data = Upper_fill2)

model.names.uppC <- c("Year, Temperature, Salinity, Secchi, ChlA, DO, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM", 
                      "Year, Temperature, Salinity, Secchi, DO, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM",
                      "Year, Temperature, Salinity, DO, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM", 
                      "Year, Temperature, DO, DO%, Nitrogen, pH, Phosphorus, Blue FDM, Stone FDM, Blue FIM",              
                      "Year, Temperature, DO, DO%, Nitrogen, pH, Blue FDM, Stone FDM, Blue FIM",                      
                      "Year, DO, DO%, Nitrogen, pH, Blue FDM, Stone FDM, Blue FIM",                      
                      "Year, DO%, Nitrogen, pH, Blue FDM, Stone FDM, Blue FIM")
summ.uppC <- do.call(rbind,lapply(list(fullUppC, fullUppC2, fullUppC3, fullUppC4, fullUppC5, fullUppC6, fullUppC7), broom::glance))
summ.uppC

UppCtab <- TableOutput(summ.uppC, model.names.uppC)

###Reporting final model
fullUppC7
fullUppC7_sum <- summary(fullUppC7)
#MLR table with test values
fullUppC7_tab <- tidy(fullUppC7)
names(fullUppC7_tab) <- c("term", "Est.", "SE", "t", "p-value")
fullUppC7_sum_tab <- glance(fullUppC7) %>% dplyr::select(r.squared:df, deviance:df.residual)
names(fullUppC7_sum_tab) <- c("R2", "adjR2", "RSE", "F", "p-value", "df", "RSS", "Resid.df")
#Save model for figure use
save(fullUppC7, file = "../CSV/Output/Models/fullUppC7_sep.rda")
#Remove uneeded items
rm(fullUppC_sum, fullUppC_tab, fullUppC_sum_tab, fullUppC2, fullUppC3, fullUppC4, fullUppC5, fullUppC6, model.names.uppC, summ.uppC)
#