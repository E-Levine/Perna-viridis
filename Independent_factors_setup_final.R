#Water quality for settlement, bycatch
#WQ files moved external to project folder due to size
#FDM
#FIM crab bycatch
#
library(plyr)
library(tidyverse)
library(ggpubr)
library(lmPerm)
library(broom)
library(zoo)
library(forecast)
library(rstatix)
#

###Detrend each parameter - additive
detrending <- function(df, param){
  temp <- df %>% ungroup() %>% select(c("MonYr", param))
  temp$MonYr <- as.yearmon(temp$MonYr, format = "%m/%Y")
  temp <- na.interp(as.ts(read.zoo(temp, FUN = as.yearmon)))
  temp %>% decompose("additive") -> decompTemp
  tempAdj <- temp-decompTemp$seasonal
  return(tempAdj)
}
#
####Settlement WQ####
#
#See Perna_adults for WQ with OTB and HB data added
###Spat data
TBWQ <- read.csv("CSV/TB_WQ_bays.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(TBWQ)
WQ <- subset(TBWQ, select = c(Month:Station, Temp:Sal, Secchi:Bay))
str(WQ)
WQ$Per_Secchi <- as.numeric(WQ$Per_Secchi)

WQ_means <- WQ %>% group_by(Month, Year, MonYr) %>% dplyr::summarize(Temp = mean(Temp, na.rm = T),
                                                                     Sal = mean(Sal, na.rm = T),
                                                                     Per_Secchi = mean(Per_Secchi, na.rm = T))
##Temp##
TempAdj <- detrending(WQ_means, "Temp")
plot(TempAdj)

###Sal###
SalAdj <- detrending(WQ_means, "Sal")
plot(SalAdj)

##Percent Secchi Depth##
PerSecAdj <- detrending(WQ_means, "Per_Secchi")
plot(PerSecAdj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(TempAdj))
WQAdj <- data.frame(MonYr, TempAdj, SalAdj, SecAdj, PerSecAdj)
#
write.csv(WQAdj, file = "CSV/Output/WQadj.csv", row.names = FALSE)
#
#
####WQ site###
Wsites <- read.csv("CSV/TB_site_2002_2017.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
keep_cols <- c("MonitoringLocationIdentifier", "OrganizationIdentifier", "OrganizationFormalName",
               "MonitoringLocationName", "MonitoringLocationTypeName", "MonitoringLocationDescriptionText",
               "LatitudeMeasure", "LongitudeMeasure", "Bay", "HorizontalCoordinateReferenceSystemDatumName", "StateCode",
               "CountyCode", "ProviderName")
Wsites <- Wsites[keep_cols]
head(Wsites, 4)
#Result data
Wresults <- read.csv("CSV/TB_results_2002_2017.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
keep_results <- c("MonitoringLocationIdentifier", "ResultIdentifier", "ActivityStartDate", 
                  "ActivityStartTime.Time", "ActivityStartTime.TimeZoneCode", "CharacteristicName", 
                  "ResultMeasureValue", "ResultMeasure.MeasureUnitCode")
Wresults <- Wresults[keep_results]
head(Wresults, 4)
combined <- merge(Wresults, Wsites, by = "MonitoringLocationIdentifier")
head(combined, 5)
#Stations to exclude
excluding <- read.csv("CSV/TB_excluded_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
removelist <- unlist(excluding)
combined <- combined[!grepl(paste0('*',removelist, "$", collapse = "|"), combined$MonitoringLocationIdentifier),]
#
write.csv(combined, file = "CSV/Output/WQ/TB_combined.csv", row.names = FALSE)
#
#Filter combined file to only include specified charactersitics 
Characters <- read.csv("CSV/WQParametersList.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Charkeep <- unlist(Characters$CharacteristicName)

combined_filtered <- combined %>% filter(CharacteristicName %in% Charkeep)
unique(combined_filtered$CharacteristicName)
#
write.csv(combined_filtered, file = "CSV/Output/WQ/TB_combined_filtered.csv", row.names = FALSE)
#Stations have been updated.  Load file to re run stats
#
#Assign group by Character and select wanted columns
modified <- left_join(combined_filtered, Characters, by = "CharacteristicName") %>% 
  dplyr::select(MonitoringLocationIdentifier, ActivityStartDate, CharacteristicName, Type, ResultMeasureValue, 
                ResultMeasure.MeasureUnitCode, LatitudeMeasure, LongitudeMeasure, Bay)
##Add month and year columns, reorder df and drop ActivityStartDate
modified$Month <- format(as.Date(modified$ActivityStartDate, "%m/%d/%Y"), "%m")
modified$Year <- format(as.Date(modified$ActivityStartDate, "%m/%d/%Y"), "%Y")
modified <- modified %>% dplyr::select(-ActivityStartDate) %>% dplyr::select(Month:Year, Bay, everything())
#
write.csv(modified, file = "CSV/OUtput/WQ/TB_combined_filtered_modified.csv", row.names = FALSE)
write.csv(combined_filtered, file = "CSV/Output/WQ/TB_modified.csv", row.names = FALSE)
#
#
###Bin data by month, year, bay###
binbyM <- as.list(colnames(modified) %>% subset(!(colnames(modified) %in% c("ResultMeasureValue", "ResultIdentifier"))))
binbyB <- as.list(colnames(modified) %>% subset(!(colnames(modified) %in% 
                                                    c("MonitoringLocationIdentifier", "ResultMeasureValue", "ResultIdentifier"))))
#all by station
MBS <- modified %>% group_by(.dots = binbyM) %>% 
  summarise(ResultMeasureValue = mean(as.numeric(ResultMeasureValue), na.rm = T)) %>%
  select(Month:MonitoringLocationIdentifier, Type, CharacteristicName, ResultMeasureValue, everything())
#
write.csv(MBS, file = "CSV/Output/WQ/WQ_TB_MBS.csv", row.names = FALSE)

#Just all
MBB <- modified %>% group_by(.dots = binbyB) %>% 
  summarise(ResultMeasureValue = mean(as.numeric(ResultMeasureValue), na.rm = T)) %>%
  select(Month:Bay, Type, CharacteristicName, ResultMeasureValue, everything())

write.csv(MBB, file = "CSV/Output/WQ/WQ_TB_MBB.csv", row.names = FALSE)
#
#
###Detrend site data
#Load file 
WQdata <- read.csv("CSV/Output/WQ/WQ_TB_MBS_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
keepchar <- c("Chl_a", "Depth", "DO", "DO_p", "N", "pH", "Phosphorus", "Salinity", "Secchi", "Temp")
WQdata <- WQdata %>% filter(Type %in% keepchar)

#
#Average by Month, Year, Quarter
WQdata_means_long <- WQdata %>% group_by(Month, Year, Quarter, Type) %>% 
  dplyr::summarize(mean = mean(ResultMeasureValue, na.rm = T))
#Separate into different columns based on type
WQdata_means <- WQdata_means_long %>% spread("Type", "mean")
WQdata_means$MonYr <- paste(WQdata_means$Month, WQdata_means$Year, sep = "/")
WQdata_means$Per_Secchi <- as.numeric((WQdata_means$Secchi/WQdata_means$Depth)*100)
#
write.csv(WQdata_means, file = "CSV/Output/Baywide/MeanWQdata_bays.csv", row.names = FALSE)
#For figure 2
write.csv(WQdata, file = "CSV/Output/Baywide/Settlement_WQ.csv", row.names = FALSE)
#
##Add spat WQ into Tampa Bay's WQ (not OTB)
###Ave temp, sal, secchi, percent secchi - before detrending
#
head(WQ_means,4)
WQ_means$MonYr <- paste(WQ_means$Month, WQ_means$Year, sep = "/")
TBset <- WQdata_means_long %>% group_by(Month, Year, Quarter, Type) %>% dplyr::summarize(mean = mean(mean, na.rm = T)) %>%
  spread("Type", "mean") %>%  mutate(MonYr = paste(Month, Year, sep = "/"), Per_Secchi = as.numeric((Secchi/Depth)*100)) %>% dplyr::select(-c("Depth", "Secchi")) %>% 
  dplyr::select(Month, Year, MonYr, Quarter, Temp, Salinity, Per_Secchi, Chl_a, DO, DO_p, N, pH, Phosphorus)
TBset_comb <- merge(TBset, (ungroup(WQ_means) %>% dplyr::select(MonYr:Per_Secchi)), by = "MonYr") %>% 
  mutate(TempAll = rowMeans(subset(., select = c(Temp.x, Temp.y)), na.rm = T),
         SalAll = rowMeans(subset(., select = c(Salinity, Sal)), na.rm = T),
         PerSecchiAll = rowMeans(subset(., select = c(Per_Secchi.x, Per_Secchi.y)), na.rm = T)) %>%
  dplyr::select(Month, Year, MonYr, Quarter, TempAll, SalAll, PerSecchiAll, Chl_a, DO, DO_p, N, pH, Phosphorus)

###TB detrending
TBMonYr <- as.Date(time(detrendingAd(TBset_comb, "TempAll")))
TBAdjusted <- data.frame(TBMonYr,(ts.union(
  na.interp(detrendingAd(TBset_comb, "TempAll")),
  na.interp(detrendingAd(TBset_comb, "SalAll")),
  na.interp(detrendingAd(TBset_comb, "PerSecchiAll")),
  na.interp(detrendingAd(TBset_comb, "Chl_a")),
  na.interp(detrendingAd(TBset_comb, "DO")),
  na.interp(detrendingAd(TBset_comb, "DO_p")),
  na.interp(detrendingAd(TBset_comb, "N")),
  na.interp(detrendingAd(TBset_comb, "pH")),
  na.interp(detrendingAd(TBset_comb, "Phosphorus"))))) %>% 
  mutate(Year = as.numeric(format(as.yearmon(TBMonYr, format = "%Y-%m-%d"), "%Y")),
         Month = as.numeric(format(as.yearmon(TBMonYr, format = "%Y-%m-%d"), "%m"))) %>%
  dplyr::select(Month, Year, everything()) %>% dplyr::select(-c(TBMonYr))
#
names(TBAdjusted) <- c("Month", "Year", "Tempadj", "Saladj", "PerSecadj", "CHlaadj", "DOadj", "DOpadj", "Nadj", "pHadj", "Phosadj")
#
write.csv(TBAdjusted, file = "CSV/Output/Baywide/Settlement_WQ.csv", row.names = FALSE)
#
#
#File of original data (not detrended), spat + WQ site, with means, min, max for temp and salinity
TBWQ <- read.csv("CSV/TB_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(TBWQ)
WQ <- subset(TBWQ, select = c(Month:MonYr, Temp:Sal))
WQ <- WQ %>% gather("Type", "ResultMeasureValue", Temp, Sal)
#
WQdata <- read.csv("CSV/Output/WQ/TB_combined_filtered_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
keepchar <- c("Salinity", "Tempature")
WQdata <- WQdata %>% filter(Type %in% keepchar)
WQdata$MonYr <- paste(WQdata$Month, WQdata$Year, sep = "/")
WQdata <- WQdata %>% dplyr::select(Month:Year, MonYr, Type, ResultMeasureValue) 
WQdata$Type[WQdata$Type=="Tempature"] <- "Temp"
WQdata$Type[WQdata$Type=="Salinity"] <- "Sal"
WQdata$ResultMeasureValue <- as.numeric(WQdata$ResultMeasureValue)
allWQ <- rbind(WQ, WQdata)
allWQ_sum <- allWQ %>% group_by(Month, Year, MonYr, Type) %>% subset(ResultMeasureValue <= 50) %>%
  summarize(mean = mean(ResultMeasureValue, na.rm = T), min = min(ResultMeasureValue, na.rm = T), max = max(ResultMeasureValue, na.rm = T)) 
allTemps <- allWQ_sum %>% subset(Type == "Temp") %>% rename(T_mean = mean, T_min = min, T_max = max) %>% dplyr::select(-c("Type"))
allSal <- allWQ_sum %>% subset(Type == "Sal") %>% rename(S_mean = mean, S_min = min, S_max = max) %>% dplyr::select(-c("Type"))
allWQ_sum <- full_join(allTemps, allSal, by = c("Year", "Month", "MonYr"))
#
write.csv(allWQ_sum, file = "CSV/Output/Summary/Baywide/Sett_WQsummary.csv", row.names = FALSE)
#
allWQ_sum %>% group_by(Month) %>% summarise(Tmean = mean(T_mean, na.rm = T),
                                            Smean = mean(S_mean, na.rm = T))
#
####Bycatch WQ####
#
#See Perna_adults for WQ with OTB and HB data added
#WQ online#
###Format/edit OTB and HB data - skip to line 354 if not redoing datafile
##WQ Site
Wsites <- read.csv("CSV/HB_OTB_site_2002_2017.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
keep_cols <- c("MonitoringLocationIdentifier", "OrganizationIdentifier", "OrganizationFormalName",
               "MonitoringLocationName", "MonitoringLocationTypeName", "MonitoringLocationDescriptionText",
               "LatitudeMeasure", "LongitudeMeasure", "Bay", "HorizontalCoordinateReferenceSystemDatumName", "StateCode",
               "CountyCode", "ProviderName")
Wsites <- Wsites[keep_cols]
head(Wsites, 4)
#Result data
Wresults <- read.csv("CSV/HB_OTB_results_2002_2017.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
keep_results <- c("MonitoringLocationIdentifier", "ResultIdentifier", "ActivityStartDate", 
                  "ActivityStartTime.Time", "ActivityStartTime.TimeZoneCode", "CharacteristicName", 
                  "ResultMeasureValue", "ResultMeasure.MeasureUnitCode")
Wresults <- Wresults[keep_results]
head(Wresults, 4)
combined <- merge(Wresults, Wsites, by = "MonitoringLocationIdentifier")
head(combined, 5)
#Stations to exclude
excluding <- read.csv("CSV/WQ_site/HB_OTB_excluded_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
removelist <- unlist(excluding)
combined <- combined[!grepl(paste0('*',removelist, "$", collapse = "|"), combined$MonitoringLocationIdentifier),]
#
#Write station datafile with stations excluded
write.csv(combined, file = "CSV/Output/WQ/HB_OTB_combined.csv", row.names = FALSE)
#
#
#Filter combined file to only include specified charactersitics 
Characters <- read.csv("CSV/WQParametersList.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Charkeep <- unlist(Characters$CharacteristicName)

combined_filtered <- combined %>% filter(CharacteristicName %in% Charkeep)
unique(combined_filtered$CharacteristicName)
#
write.csv(combined_filtered, file = "CSV/Output/WQ/HB_OTB_combined_filtered.csv", row.names = FALSE)
#
#Assign group by Character and select wanted columns
modified <- left_join(combined_filtered, Characters, by = "CharacteristicName") %>% 
  dplyr::select(MonitoringLocationIdentifier, ActivityStartDate, CharacteristicName, Type, ResultMeasureValue, 
                ResultMeasure.MeasureUnitCode, LatitudeMeasure, LongitudeMeasure, Bay)
##Add month and year columns, reorder df and drop ActivityStartDate
modified$Month <- format(as.Date(modified$ActivityStartDate, "%m/%d/%Y"), "%m")
modified$Year <- format(as.Date(modified$ActivityStartDate, "%m/%d/%Y"), "%Y")
modified <- modified %>% dplyr::select(-ActivityStartDate) %>% dplyr::select(Month:Year, Bay, everything())
#
#Write file of stations excluded, characters needed
write.csv(modified, file = "CSV/Output/WQ/HB_OTB_combined_filtered_modified.csv", row.names = FALSE)

###Edit units in Excel and reload file
modified <- read.csv("CSV/Output/WQ/HB_OTB_combined_filtered_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#Remove units with NA
modified <- modified[!is.na(modified$ResultMeasure.MeasureUnitCode),]
modified$ResultMeasureValue <- as.numeric(modified$ResultMeasureValue)
#
#Load LTB, MTB, OTB WQ site file, bind with new OTB, HB file
WQsite <- read.csv("../CSV/WQ_site/WQ_TB_MBS_modified3.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
WQ_all <- rbind(modified, WQsite)

#Create MonYr column, convert to right format, and same data type
WQ_all$MonYr <- paste(WQ_all$Month, WQ_all$Year, sep = "/") %>% as.yearmon(format = "%m/%Y") %>% as.character()
#For figure 2
write.csv(MBS, file = "CSV/Output/Bycatch_WQ.csv", row.names = FALSE)
###Detrend site data and save output
###Bin data by month, year, bay###
binbyM <- as.list(colnames(modified) %>% subset(!(colnames(modified) %in% c("ResultMeasureValue", "ResultIdentifier"))))
#all by station
MBS <- WQ_all %>% group_by(.dots = binbyM) %>% 
  dplyr::summarise(ResultMeasureValue = mean(as.numeric(ResultMeasureValue), na.rm = T)) %>%
  dplyr::select(Month:MonitoringLocationIdentifier, Type, CharacteristicName, ResultMeasureValue, everything())
#
write.csv(MBS, file = "CSV/Output/WQ_all_MBS.csv", row.names = FALSE)
#
#
#Select data to keep, detrend and save file to work with
keepchar <- c("Chl_a", "Depth", "DO", "DO_p", "N", "pH", "Phosphorus", "Salinity", "Secchi", "Temp")
MBS <- MBS %>% filter(Type %in% keepchar)

#Average by Month, Year, Quarter
WQ_means <- MBS %>% group_by(Month, Year, Quarter, Type) %>% 
  dplyr::summarize(mean = mean(ResultMeasureValue, na.rm = T))
#

#Average by Month, Year, Quarter
WQ_means_long <- WQ_means %>% group_by(Month, Year, Quarter, Type) %>% 
  dplyr::summarize(mean = mean(mean, na.rm = T))
#Separate into different columns based on type
WQdata_means <- WQ_means_long %>% spread("Type", "mean")
WQdata_means$MonYr <- paste(WQdata_means$Month, WQdata_means$Year, sep = "/")
WQdata_means$Per_Secchi <- as.numeric((WQdata_means$Secchi/WQdata_means$Depth)*100)
#
write.csv(WQdata_means, file = "CSV/Output/Baywide/Bycatch_MeanWQ.csv", row.names = FALSE)
#
BycatchWQ <- WQ_means_long %>% group_by(Month, Year, Quarter, Type) %>% dplyr::summarize(mean = mean(mean, na.rm = T)) %>%
  spread("Type", "mean") %>%  mutate(MonYr = paste(Month, Year, sep = "/"), Per_Secchi = as.numeric((Secchi/Depth)*100)) %>% dplyr::select(-c("Depth", "Secchi")) %>% 
  dplyr::select(Month, Year, MonYr, Quarter, Temp, Salinity, Per_Secchi, Chl_a, DO, DO_p, N, pH, Phosphorus)
#
##Add spat WQ 
###Ave temp, sal, secchi, percent secchi - before detrending
#
###Spat data
TBWQ <- read.csv("CSV/TB_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(TBWQ)
WQ <- subset(TBWQ, select = c(Month:Station, Temp:Sal, Per_Secchi))
str(WQ)
WQ$Per_Secchi <- as.numeric(WQ$Per_Secchi)

WQ_means <- WQ %>% group_by(Month, Year, MonYr) %>% dplyr::summarize(Temp = mean(Temp, na.rm = T),
                                                                     Sal = mean(Sal, na.rm = T),
                                                                     Per_Secchi = mean(Per_Secchi, na.rm = T))
head(WQ_means,4)
Bycatch_comb <- merge(BycatchWQ, (ungroup(WQ_means) %>% dplyr::select(MonYr:Per_Secchi)), by = "MonYr") %>% 
  mutate(TempAll = rowMeans(subset(., select = c(Temp.x, Temp.y)), na.rm = T),
         SalAll = rowMeans(subset(., select = c(Salinity, Sal)), na.rm = T),
         PerSecchiAll = rowMeans(subset(., select = c(Per_Secchi.x, Per_Secchi.y)), na.rm = T)) %>%
  dplyr::select(Month, Year, MonYr, Quarter, TempAll, SalAll, PerSecchiAll, Chl_a, DO, DO_p, N, pH, Phosphorus)


###Detrending
ByMonYr <- as.Date(time(detrendingAd(Bycatch_comb, "TempAll")))
BycatchAdjusted <- data.frame(ByMonYr,(ts.union(
  na.interp(detrendingAd(Bycatch_comb, "TempAll")),
  na.interp(detrendingAd(Bycatch_comb, "SalAll")),
  na.interp(detrendingAd(Bycatch_comb, "PerSecchiAll")),
  na.interp(detrendingAd(Bycatch_comb, "Chl_a")),
  na.interp(detrendingAd(Bycatch_comb, "DO")),
  na.interp(detrendingAd(Bycatch_comb, "DO_p")),
  na.interp(detrendingAd(Bycatch_comb, "N")),
  na.interp(detrendingAd(Bycatch_comb, "pH")),
  na.interp(detrendingAd(Bycatch_comb, "Phosphorus"))))) %>% 
  mutate(Year = as.numeric(format(as.yearmon(ByMonYr, format = "%Y-%m-%d"), "%Y")),
         Month = as.numeric(format(as.yearmon(ByMonYr, format = "%Y-%m-%d"), "%m"))) %>%
  dplyr::select(Month, Year, everything()) %>% dplyr::select(-c(ByMonYr))
#
names(BycatchAdjusted) <- c("Month", "Year", "Tempadj", "Saladj", "PerSecadj", "CHlaadj", "DOadj", "DOpadj", "Nadj", "pHadj", "Phosadj")
#
write.csv(BycatchAdjusted, file = "CSV/Output/Baywide/Bycatch_WQ.csv", row.names = FALSE)
#
#File of original data (not detrended), spat + WQ site, with means, min, max for temp and salinity
TBWQ <- read.csv("CSV/TB_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(TBWQ)
WQ <- subset(TBWQ, select = c(Month:MonYr, Temp:Sal)) %>% gather("Type", "mean", Temp, Sal)
ByWQ <- read.csv("CSV/Output/WQ_all_MBS.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
ByWQ$Type[ByWQ$Type=="Tempature"] <- "Temp"
ByWQ$Type[ByWQ$Type=="Salinity"] <- "Sal"
#Average by Month, Year, Quarter
ByWQ2 <- ByWQ %>% filter(Type %in% c("Sal", "Temp") & ResultMeasureValue <= 50) %>% group_by(Month, Year, Type) %>% 
  dplyr::summarize(mean = mean(ResultMeasureValue, na.rm = T)) %>%
  mutate(MonYr = paste(Month, Year, sep = "/")) 
#
AllWQ <- full_join(WQ, ByWQ2) %>% group_by(Month, Year, MonYr, Type) %>% 
  dplyr::summarize(ave = mean(mean, na.rm = T), min = min(mean, na.rm = T), max = max(mean, na.rm = T))
allTemps <- AllWQ %>% subset(Type == "Temp") %>% rename(T_mean = ave, T_min = min, T_max = max) %>% dplyr::select(-c("Type"))
allSal <- AllWQ %>% subset(Type == "Sal") %>% rename(S_mean = ave, S_min = min, S_max = max) %>% dplyr::select(-c("Type"))
allWQ_sum <- full_join(allTemps, allSal, by = c("Year", "Month", "MonYr"))
#
write.csv(allWQ_sum, file = "CSV/Output/Summary/Baywide/By_WQsummary.csv", row.names = FALSE)
#

####FDM####
##Load file to work with 
FDM <- read.csv("CSV/Crab_FDM.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

###Stone crab FDM
StoneFDM <- FDM %>% filter(Species == "SC") 
head(StoneFDM, 3)
str(StoneFDM)

SFDM_Totals <- StoneFDM %>% group_by(Year, Month, Season, Species) %>% dplyr::summarise(Total = sum(Pounds, na.rm = T),
                                                                                        CPUE = (Total/sum(Trips, na.rm = T)))
#
#Stone_totals = StoneFDM
SFDM_Totals$Season <- as.factor(SFDM_Totals$Season)
#Vizualize
ggboxplot(SFDM_Totals, x = "Season", y = "Total")
ggplot(SFDM_Totals, aes(x = Total))+
  geom_histogram()
#Slightly skewed so transformation required
SFDM_Totals$logTSC <- log10(SFDM_Totals$Total+1)
#
write.csv(SFDM_Totals, file = "CSV/Output/Summary/SCLanding.csv", row.names = FALSE)
#
ggplot(SFDM_Totals, aes(x = logTSC))+
  geom_histogram()
#Summary of log transformed data by season
SFDMs <- SFDM_Totals %>% group_by(Year, Month, Season) %>% rstatix::get_summary_stats(logTSC, type = "mean_sd")

#Perm ANOVA on Season 
set.seed(54321)
aovpSF <- aovp(mean ~ Season, data = SFDMs, perm = "", nperm = 10000)
summary(aovpSF)
aovp_tidy_SF <- tidy(aovpSF)
names(aovp_tidy_SF) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff 
##Pairwise comparisons - season - issues running but not crucial....
set.seed(54321)
seas_pairSF <- SFDMs %>% rstatix::pairwise_t_test(mean ~ Season, p.adjust.method = "holm")
seas_tabSF <- select(seas_pairSF, c("group1", "group2", "p", "p.adj"))
seas_tabSF <- seas_tabSF %>% mutate(Seasons = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Seasons", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
#
#Means of groups, comparisons to show
Season_meansSF <- SFDMs %>% group_by(Season) %>% get_summary_stats(mean, type = "mean_sd") %>%
  select(-c("variable", "n")) 
#
###Detrend##
SFDM_Totals <- SFDM_Totals %>% mutate(MonYr = paste(Month, Year, sep = "/"))
SFDM_Adj <- detrendingAd(SFDM_Totals, "logTSC")
plot(SFDM_Adj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(SFDM_Adj))
SFDMAdj <- data.frame(MonYr, SFDM_Adj) %>% mutate(Year = MonYr %>% format("%Y"), Month = MonYr %>% format("%m"))
#
write.csv((SFDMAdj %>% dplyr::select(Year, Month, SFDM_Adj)), file = "CSV/Output/Baywide/S_FDM_adj.csv", row.names = FALSE)

###Compare detrended among years for change in CPuE over time
##SFDM_Adj
SFDMAdj$Year <- as.factor(SFDMAdj$Year)
#ANOVA
set.seed(54321)
aovSC_FDM <- aovp(SFDM_Adj ~ Year, data = SFDMAdj, perm = "", nperm = 10000)
summary(aovSC_FDM)
aovSC_FDM_tidy <- tidy(aovSC_FDM)
names(aovSC_FDM_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")

#Sig diff among years pairwise_t
set.seed(54321)
SFDM_testing <- SFDMAdj %>% group_by(Month, Year) %>% rstatix::get_summary_stats(SFDM_Adj, type = "mean_sd")
SFDM_pair <- SFDM_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")
SFDM_pair <- select(SFDM_pair, c("group1", "group2", "p", "p.adj"))
SFDM_pair <- SFDM_pair %>% mutate(Years = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Years", everything()) %>% select(-c("group1", "group2"))

#Sig diff among years TUKEY - compare
set.seed(54321)
TUKSC_FDM <- TukeyHSD(aovSC_FDM)
#Get letters, load sumamry table, append letters and save with letters
SFDMLett <- data.frame(Letters = multcompView::multcompLetters4(aovSC_FDM, comp = TUKSC_FDM)$Year$Letters) %>%
  tibble::rownames_to_column("Year")
SFDM_TOT <- SFDM_Totals %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
SFDM_TOT <- merge(SFDM_TOT, SFDMLett, by = "Year")
#
write.csv(SFDM_TOT, file = "CSV/Output/Summary/Baywide/SFDM_yearly_letters.csv", row.names = FALSE)
#


###Blue crab FDM###
BlueFDM <- FDM %>% filter(Species == "BC") 
head(BlueFDM, 3)
str(BlueFDM)

BFDM_Totals <- BlueFDM %>% group_by(Year, Month, Season, Species) %>% dplyr::summarise(Total = sum(Pounds, na.rm = T),
                                                                                       CPUE = (Total/sum(Trips, na.rm = T)))

BFDM_Totals$Season <- as.factor(BFDM_Totals$Season)
#Vizualize
ggboxplot(BFDM_Totals, x = "Season", y = "Total")
ggplot(BFDM_Totals, aes(x = Total))+
  geom_histogram()
#Not really skewed so no transformation needed
write.csv(BFDM_Totals, file = "CSV/Summary/BCLanding.csv", row.names = FALSE)

BFDMs <- BFDM_Totals %>% group_by(Year, Month, Season) %>% rstatix::get_summary_stats(Total, type = "mean_sd")
#Perm ANOVA on Season
set.seed(54321)
aovpBF <- aovp(mean ~ Season, data = BFDMs, perm = "", nperm = 10000)
summary(aovpBF)
aovp_tidyBF <- tidy(aovpBF)
names(aovp_tidyBF) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#
##Pairwise comparisons - season
set.seed(54321)
seas_pairBF <- BFDMs %>% pairwise_t_test(mean ~ Season, p.adjust.method = "holm")
seas_tabBF <- select(seas_pairBF, c("group1", "group2", "p", "p.adj"))
seas_tabBF <- seas_tabBF %>% mutate(Seasons = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Seasons", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
#
#Means of groups, comparisons to show
Season_meansBF <- BFDMs %>% group_by(Season) %>% get_summary_stats(mean, type = "mean_sd") %>%
  select(-c("variable", "n"))

###Detrend##
BFDM_Totals <- BFDM_Totals %>% mutate(MonYr = paste(Month, Year, sep = "/"))
BFDM_Adj <- detrendingAd(BFDM_Totals, "Total")
plot(BFDM_Adj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(BFDM_Adj))
BFDMAdj <- data.frame(MonYr, BFDM_Adj) %>% mutate(Year = MonYr %>% format("%Y"), Month = MonYr %>% format("%m"))
#
write.csv((BFDMAdj %>% dplyr::select(Year, Month, BFDM_Adj)), file = "CSV/Output/Baywide/B_FDM_adj.csv", row.names = FALSE)

###Compare detrended among years for change in CPuE over time
##Stone_Adj = BFDM_Adj
BFDMAdj$Year <- as.factor(BFDMAdj$Year)
#ANOVA
set.seed(54321)
aovBC_FDM <- aovp(BFDM_Adj ~ Year, data = BFDMAdj, perm = "", nperm = 10000)
summary(aovBC_FDM)
aovBC_FDM_tidy <- tidy(aovBC_FDM)
names(aovBC_FDM_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among years - compare
set.seed(54321)
TUKBC_FDM <- TukeyHSD(aovBC_FDM)
TUKBC_FDM_tab <- tidy(TUKBC_FDM) %>% dplyr::select(-c("term"))
names(TUKBC_FDM_tab) <- c("Years", "Estimate", "CIlower", "CIupper", "adj.p-value")    #Move 'Seasons' to front and drop grp1 & grp2
plot(TUKBC_FDM)
#Pairwise_t
set.seed(54321)
BFDM_testing <- BFDMAdj %>% group_by(Month, Year) %>% rstatix::get_summary_stats(BFDM_Adj, type = "mean_sd")
BFDM_pair <- BFDM_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")
BFDM_pair <- select(BFDM_pair, c("group1", "group2", "p", "p.adj"))
BFDM_pair <- BFDM_pair %>% mutate(Years = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Years", everything()) %>% select(-c("group1", "group2"))

#Get letters, load sumamry table, append letters and save with letters
BFDMLett <- data.frame(Letters = multcompView::multcompLetters4(aovBC_FDM, comp = TUKBC_FDM)$Year$Letters) %>%
  tibble::rownames_to_column("Year")
BFDM_TOT <- BFDM_Totals %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
BFDM_TOT <- merge(BFDM_TOT, BFDMLett, by = "Year")
write.csv(BFDM_TOT, file = "CSV/Output/Summary/Baywide/BFDM_yearly_letters.csv", row.names = FALSE)
#
#
####FIM Blue Crabs####
#Load file, map to check stations/Bays
FimBC <- read.csv("CSV/FIM_Crabs_20 12 07.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% dplyr::filter(scientificname == "Callinectes sapidus")
head(FimBC, 3)
str(FimBC)
summary(FimBC)
FimBC$MonYr <- as.yearmon(FimBC$MonYr, format = "%m/%Y")
library(ggplot2)
library(ggmap)
#Set boundaries
bounding <- function(dflatcol, dflongcol, source, type, color){
  boundaries <<- data.frame(
    North <- max(dflatcol+0.03),
    South <- min(dflatcol-0.03),
    East <- max(dflongcol+0.03),
    West <- min(dflongcol-0.03))
  
  maparea <- get_map(location = c(West, South, East, North), source = source, maptype = type, color = color)
  return(maparea)
}
#
register_google(key = "AIzaSyDk2p1evqcHUg7QbOXw44xqzOBIPRFum-M")
#
#df of all stations
Map_area <- bounding(FimBC$latitude, FimBC$longitude, "google", "terrain", "bw")
ggmap(Map_area) +
  geom_point(data = FimBC, aes(x = longitude, y = latitude, color = Bay, shape = Bay), size = 3, alpha = 0.8)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_blank(), axis.text = element_text(color = "black"),
        #legend.position = "none")+
        legend.position = c(0.855, 0.4), #legend.title = element_blank(),
        legend.background = element_rect(fill = "#CCCCCC"),
        legend.key = element_rect(color = "transparent", fill = "transparent"),
        legend.text.align = 0, legend.box.just = "right")+
  geom_point(data = (FimBC %>% filter(reference =="TBM2002060704")), aes(x = longitude, y = latitude), color = "black")

#Create full list of Months Years 05/2002 through 12/2017 filled with 0 for missing Bay/MonYr
ts <- data.frame(MonYr = seq(as.yearmon("05/2002", "%m/%Y"), as.yearmon("12/2017", "%m/%Y"), 1/12))
F_Crabs <- full_join(ts, FimBC) %>%  #join full ist and data to fill gaps
  complete(Bay, MonYr, fill = list(effort = 0, TotalCount = 0)) %>% #Complete df with all bays recorded for all MonYrs
  mutate(Year = format(as.yearmon(MonYr, format="%b %Y"),"%Y"),
         Month = as.integer(format(as.yearmon(MonYr, format="%b %Y"),"%m")),  #Fill months
         Quarter = ifelse(Month <= 3, 1, 
                          ifelse((Month >= 4)&(Month <= 6), 2,
                                 ifelse(Month >= 10, 3, 4))))
#
write.csv(F_Crabs, file = "CSV/Output/FIM_BC_20 12 07.csv", row.names = FALSE)
#Summarize and vizualize
FIMBC_totals <- F_Crabs %>% group_by(Year, Month, MonYr, Quarter, reference) %>% summarise(Effort = mean(effort, na.rm = T),
                                                                                           Total = sum(TotalCount, na.rm = T))
FIMBC_totals$Quarter <- as.factor(FIMBC_totals$Quarter)
#
write.csv(FIMBC_totals, file = "CSV/Output/Summary/FIMBlue_Effort.csv", row.names = FALSE)
#
F_BC_mean <- F_Crabs %>% group_by(Month, Year, Quarter) %>% rstatix::get_summary_stats(effort, type = "mean_sd")
F_BC_mean$Quarter <- as.factor(F_BC_mean$Quarter)
#
write.csv(F_BC_mean, file = "CSV/Output/Summary/FIMBlue_Effort_means.csv", row.names = FALSE)
#Vizualize
ggboxplot(F_BC_mean, x = "Quarter", y = "mean")
gghistogram(F_BC_mean, x = "mean")
ggplot(F_BC_mean, aes(x = mean))+
  geom_histogram()
#Fairly normaly, slight skew
#
#Perm ANOVA on Season
set.seed(54321)
aovp <- aovp(mean ~ Quarter, data = F_BC_mean, perm = "", nperm = 10000)
summary(aovp)
aovp_tidy <- tidy(aovp)
names(aovp_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among Seasons

##Pairwise comparisons - season
seas_pair <- F_BC_mean %>% pairwise_t_test(mean ~ Quarter, p.adjust.method = "BH")
seas_tab <- select(seas_pair, c("group1", "group2", "p", "p.adj"))
seas_tab <- seas_tab %>% mutate(Seasons = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Seasons", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
ggboxplot(F_BC_mean, x = "Quarter", y = "mean")
#All seasons are sig diff 1 & 3 < 2 & 4 so seasonality should be removed from data

#Means of groups, comparisons to show
Season_meansB <- F_BC_mean %>% group_by(Quarter) %>% get_summary_stats(mean, type = "mean_sd") %>%
  select(-c("variable", "n"))
#
###Detrend##
F_BC_mean <- F_BC_mean %>% plyr::mutate(MonYr = paste(Month, Year, sep = "/"))  #Create MonYr column
FBCAdj <- detrendingAd(F_BC_mean, "mean")
plot(FBCAdj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(FBCAdj))
FBC_Adj <- data.frame(MonYr, FBCAdj) %>% mutate(Year = MonYr %>% format("%Y"), Month = MonYr %>% format("%m"))
#
write.csv((FBC_Adj %>% dplyr::select(Year, Month, FBCAdj)), file = "CSV/Output/Baywide/FIM_BC_adj.csv", row.names = FALSE)

###Compare detrended among years for change in CPuE over time
##FBC_Adj
FBC_Adj$Year <- as.factor(FBC_Adj$Year)
#ANOVA
set.seed(54321)
aovFBC <- aovp(FBCAdj ~ Year, data = FBC_Adj, perm = "", nperm = 10000)
summary(aovFBC)
aovFBC_tidy <- tidy(aovFBC)
names(aovFBC_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among years - pairwise_t
set.seed(54321)
FBC_testing <- FBC_Adj %>% group_by(Month, Year) %>% rstatix::get_summary_stats(FBCAdj, type = "mean_sd")
FBC_pair <- FBC_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")
FBC_pair <- select(FBC_pair, c("group1", "group2", "p", "p.adj"))
FBC_pair <- FBC_pair %>% mutate(Years = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Years", everything()) %>% select(-c("group1", "group2"))
#Set up annual mean, sd, Letters for comparison
FBC_letters <- biostat::make_cld(select((FBC_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")), 
                                        c("group1", "group2", "p", "p.adj")) %>% 
                                   mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
                                   select("Comparison", everything()) %>% select(-c("group1", "group2")) %>% rename(p.adjust = p, p.value = p.adj)) %>%
  select(-c("spaced_cld")) %>% rename(Year = group, Letters = cld)
F_Crab_sum <- merge(F_Crabs %>% group_by(Year) %>% get_summary_stats(effort, type = "mean_sd") %>% 
                      dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd),
                    FBC_letters, by = "Year")
ggplot(F_Crab_sum, aes(x = Year, y = mean))+
  geom_point(aes(color = Letters))
#
write.csv(F_Crab_sum, file = "CSV/Output/Summary/Baywide/FIM_crab_letters.csv", row.names = FALSE)
#