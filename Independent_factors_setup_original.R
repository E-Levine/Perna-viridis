###Water quality, Crab CPUEs, FDM data setup
#WQ files moved external to project folder due to size
##Packages used

library(plyr)
library(tidyverse)
library(ggpubr)
library(lmPerm)
library(broom)
library(zoo)
library(forecast)
library(rstatix)
#

####WQ parameters - settlement data####
#See Perna_adults for WQ with OTB and HB data added
###Spat data
TBWQ <- read.csv("CSV/TB_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(TBWQ)
WQ <- subset(TBWQ, select = c(Month:Station, Temp:Sal, Secchi:Per_Secchi))
str(WQ)
WQ$Per_Secchi <- as.numeric(WQ$Per_Secchi)

WQ_means <- WQ %>% group_by(Month, Year, MonYr) %>% summarize(Temp = mean(Temp, na.rm = T),
                                                              Sal = mean(Sal, na.rm = T),
                                                              Secchi = mean(Secchi, na.rm = T),
                                                              Per_Secchi = mean(Per_Secchi, na.rm = T))

###Detrend each parameter
detrending <- function(df, param){
  temp <- df %>% ungroup() %>% select(c("MonYr", param))
  temp$MonYr <- as.yearmon(temp$MonYr, format = "%m/%Y")
  temp <- na.interp(as.ts(read.zoo(temp, FUN = as.yearmon)))
  temp %>% decompose("additive") -> decompTemp
  tempAdj <- temp-decompTemp$seasonal
  return(tempAdj)
}

##Temp##
TempAdj <- detrending(WQ_means, "Temp")
plot(TempAdj)

###Sal###
SalAdj <- detrending(WQ_means, "Sal")
plot(SalAdj)

###Secchi###
SecAdj <- detrending(WQ_means, "Secchi")
plot(SecAdj)

##Percent Secchi Depth##
PerSecAdj <- detrending(WQ_means, "Per_Secchi")
plot(PerSecAdj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(TempAdj))
WQAdj <- data.frame(MonYr, TempAdj, SalAdj, SecAdj, PerSecAdj)
write.csv(WQAdj, file = "CSV/Output/WQ/WQadj.csv", row.names = FALSE)
#
#
#
###WQ site###
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

write.csv(combined, file = "CSV/Output/WQ/TB_combined.csv", row.names = FALSE)
#
#Filter combined file to only include specified charactersitics 
Characters <- read.csv("CSV/WQParametersList.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Charkeep <- unlist(Characters$CharacteristicName)

combined_filtered <- combined %>% filter(CharacteristicName %in% Charkeep)
unique(combined_filtered$CharacteristicName)

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

write.csv(modified, file = "CSV/Output/WQ/TB_combined_filtered_modified.csv", row.names = FALSE)
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

write.csv(MBS, file = "CSV/Output/WQ/WQ_TB_MBS.csv", row.names = FALSE)

#Just all
MBB <- modified %>% group_by(.dots = binbyB) %>% 
  summarise(ResultMeasureValue = mean(as.numeric(ResultMeasureValue), na.rm = T)) %>%
  select(Month:Bay, Type, CharacteristicName, ResultMeasureValue, everything())

write.csv(MBB, file = "CSV/WQ_TB_MBB.csv", row.names = FALSE)
#
#
###Detrend site data
#Load file 
WQdata <- read.csv("CSV/Output/WQ/WQ_TB_MBS_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
keepchar <- c("Chl_a", "Conductance", "Depth", "DO", "DO_p", "N", "pH", "Phosphorus", "Salinity", "Secchi", "Temp", "Turbidity", "heavymetals")
WQdata <- WQdata %>% filter(Type %in% keepchar)

#Average by Month, Year, Quarter, Bay
WQdata_means <- WQdata %>% group_by(Month, Year, Quarter, Type) %>% 
  summarize(mean = mean(ResultMeasureValue, na.rm = T))
#Separate into different columns based on type
WQdata_means <- WQdata_means %>% spread("Type", "mean")
WQdata_means$MonYr <- paste(WQdata_means$Month, WQdata_means$Year, sep = "/")
WQdata_means$Per_Secchi <- as.numeric((WQdata_means$Secchi/WQdata_means$Depth)*100)
#
write.csv(WQdata_means, file = "CSV/Output/WQ/MeanWQdata.csv", row.names = FALSE)


#Chl_a
CHlaadj <- detrending(WQdata_means, "Chl_a")
plot(CHlaadj)
#
#Conductance
Conadj <- detrending(WQdata_means, "Conductance")
plot(Conadj)
#
#Depth
Depadj <- detrending(WQdata_means, "Depth")
plot(Depadj)
#
#DO
DOadj <- detrending(WQdata_means, "DO")
plot(DOadj)
#
#DO_p
DOpadj <- detrending(WQdata_means, "DO_p")
plot(DOpadj)
#
#N
Nadj <- detrending(WQdata_means, "N")
plot(Nadj)
#
#pH
pHadj <- detrending(WQdata_means, "pH")
plot(pHadj)
#
#Phosphorus
Phosadj <- detrending(WQdata_means, "Phosphorus")
plot(Phosadj)
#
#Salinity
Saladj <- detrending(WQdata_means, "Salinity")
plot(Saladj)
#
#Secchi
Secadj <- detrending(WQdata_means, "Secchi")
plot(Secadj)
#
#Temp
Tempadj <- detrending(WQdata_means, "Temp")
plot(Tempadj)
#
#Turbidity
Turadj <- detrending(WQdata_means, "Turbidity")
plot(Turadj)
#
#Percent Secchi Depth
PerSecadj <- detrending(WQdata_means, "Per_Secchi")
plot(PerSecadj)
#
#Create new df of adjusted data
Adjusted <- ts.union(na.interp(CHlaadj), na.interp(Conadj), na.interp(Depadj), na.interp(DOadj), na.interp(DOpadj), na.interp(Nadj), 
                     na.interp(pHadj), na.interp(Phosadj), na.interp(Saladj), na.interp(Secadj), na.interp(PerSecadj), na.interp(Tempadj), na.interp(Turadj))
MonYr <- as.Date(time(Saladj))
Adjusted <- data.frame(MonYr, Adjusted)
names(Adjusted) <- c("MonYr", "CHlaadj", "Conadj", "Depadj", "DOadj", "DOpadj", "Nadj", "pHadj", "Phosadj", "Saladj", "Secadj","PerSecadj", "Tempadj", "Turadj")
Adjusted$Year <- as.numeric(format(as.yearmon(Adjusted$MonYr, format = "%Y-%m-%d"), "%Y"))                    
Adjusted$Month <- as.numeric(format(as.yearmon(Adjusted$MonYr, format = "%Y-%m-%d"), "%m"))
Adjusted <- Adjusted %>% select("Month", "Year", everything()) %>% select(-c("MonYr"))

write.csv(Adjusted, file = "CSV/Output/WQ/LogWQdata_adjusted.csv", row.names = FALSE)
##
##
##
###Combine WQ and WQ data into one file###
###Ave temp, sal, secchi, percent secchi - before detrending
#Load original WQ and WQdata files
TBWQ <- read.csv("CSV/WQaves.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% 
  dplyr::select(Month:MonYr, Depth_mean:Secchi_mean)
TBWQ$PerSecchi_mean <- as.numeric((TBWQ$Secchi_mean/TBWQ$Depth_mean)*100)
TBWQ <- TBWQ %>% dplyr::select(-Depth_mean)
#
WQdata2 <- read.csv("CSV/Output/WQ/WQ_TB_MBS_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#Create MonYr column, convert to right format, and same data type
WQdata2$MonYr <- paste(WQdata2$Month, WQdata2$Year, sep = "/") %>% as.yearmon(format = "%m/%Y") %>% as.character()
keepchar <- c("Salinity", "Secchi", "Temp", "Depth")
WQdata2 <- WQdata2 %>% filter(Type %in% keepchar)
#Average by Month, Year, Type and spread Types into separate columns
WQdata2_means <- WQdata2 %>% group_by(Month, Year, MonYr, Type) %>% 
  summarize(mean = mean(ResultMeasureValue, na.rm = T)) %>% spread("Type", "mean")
WQdata2_means$PerSecchi <- as.numeric((WQdata2_means$Secchi/WQdata2_means$Depth)*100)
WQdata2_means <- WQdata2_means %>% dplyr::select(-Depth)

#Join WQ and WQdata into 1 df
WQ_combinded <- full_join(TBWQ, WQdata2_means, by = c("Year", "Month", "MonYr"))
WQ_combinded$TempAll <- rowMeans(subset(WQ_combinded, select = c(Temp_mean, Temp)), na.rm = T)
WQ_combinded$SalAll <- rowMeans(subset(WQ_combinded, select = c(Sal_mean, Salinity)), na.rm = T)
WQ_combinded$SecchiAll <- rowMeans(subset(WQ_combinded, select = c(Secchi_mean, Secchi)), na.rm = T)
WQ_combinded$PerSecchiAll <- rowMeans(subset(WQ_combinded, select = c(PerSecchi_mean, PerSecchi)), na.rm = T)
WQ_combinded <- WQ_combinded %>% dplyr::select(Month:MonYr, TempAll:PerSecchiAll)

##Detrend parameters
#Temp
Tempadj2 <- detrending(WQ_combinded, "TempAll")
plot(Tempadj2)
#
#Salinity
Saladj2 <- detrending(WQ_combinded, "SalAll")
plot(Saladj2)
#
#Secchi
Secadj2 <- detrending(WQ_combinded, "SecchiAll")
plot(Secadj2)
#
#Percent Secchi Depth
PerSecadj2 <- detrending(WQ_combinded, "PerSecchiAll")
plot(PerSecadj2)

#Combine into 1 df and add in other WQdata 
CombinedAdj <- ts.union(na.interp(Tempadj2), na.interp(Saladj2), na.interp(Secadj2), na.interp(PerSecadj2), na.interp(CHlaadj), 
                        na.interp(Conadj), na.interp(DOadj), na.interp(DOpadj), na.interp(Nadj), na.interp(pHadj), na.interp(Phosadj),   na.interp(Turadj))
MonYr <- as.Date(time(Tempadj2))
CombinedAdj <- data.frame(MonYr, CombinedAdj)
names(CombinedAdj) <- c("MonYr", "TempAll", "SalAll", "SecAll", "PerSecAll", "CHlaadj", "Conadj", "DOadj", "DOpadj", "Nadj", "pHadj", "Phosadj", "Turadj")
CombinedAdj$Year <- as.numeric(format(as.yearmon(CombinedAdj$MonYr, format = "%Y-%m-%d"), "%Y"))                    
CombinedAdj$Month <- as.numeric(format(as.yearmon(CombinedAdj$MonYr, format = "%Y-%m-%d"), "%m"))
CombinedAdj <- CombinedAdj %>% select("Month", "Year", everything()) %>% select(-c("MonYr"))

write.csv(CombinedAdj, file = "CSV/Output/WQ/AllWQdata_adjusted.csv", row.names = FALSE)

#File of original data (not detrended), spat + WQ site, with means, min, max for temp and salinity
TBWQ <- read.csv("CSV/TB_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(TBWQ)
WQ <- subset(TBWQ, select = c(Month:MonYr, Temp:Sal))
WQ <- WQ %>% gather("Type", "ResultMeasureValue", Temp, Sal)
#
WQdata <- read.csv("CSV/Output/WQ/WQ/TB_combined_filtered_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
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
write.csv(allWQ_sum, file = "CSV/Output/WQ/Summary/WQsummary.csv", row.names = FALSE)
#
allWQ_sum %>% group_by(Month) %>% summarise(Tmean = mean(T_mean, na.rm = T),
                                            Smean = mean(S_mean, na.rm = T))
#######
##
###Crab setup###
#
####Stone Crabs####
Stone <- read.csv("CSV/Stone_ori.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(Stone, 3)
str(Stone)
Stone <- Stone %>% dplyr::select(-c(Temperature,Salinity))
#
Stone_totals <- Stone %>% group_by(Year, Month, Season,Location, Station) %>% drop_na(Station) %>% summarise(Traps = n(), 
                                                                                                             Crabs = sum(Number_Crabs, na.rm = T),
                                                                                                             CPUE = mean(Crabs/Traps, na.rm = T))
Stone_totals$Season <- as.factor(Stone_totals$Season)
write.csv(Stone_totals, file = "CSV/Output/Summary/StoneCPUE.csv", row.names = FALSE)
#Vizualize
ggboxplot(Stone_totals, x = "Season", y = "CPUE")
#Pretty normal so no transformation required
Stones <- Stone_totals %>% group_by(Month, Year, Season) %>% get_summary_stats(CPUE, type = "mean_sd")
Stones$Season <- as.factor(Stones$Season)

#Perm ANOVA on Season (All stations in LTB)
set.seed(54321)
aovp <- aovp(CPUE ~ Season, data = Stone_totals, perm = "", nperm = 10000)
summary(aovp)
aovp_tidy <- tidy(aovp)
names(aovp_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff 
##Pairwise comparisons - season
set.seed(54321)
seas_pair <- Stones %>% pairwise_t_test(mean ~ Season, p.adjust.method = "holm")
seas_tab <- select(seas_pair, c("group1", "group2", "p", "p.adj"))
seas_tab <- seas_tab %>% mutate(Seasons = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Seasons", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
#
#Means of groups, comparisons to show
Season_means <- Stones %>% group_by(Season) %>% get_summary_stats(mean, type = "mean_sd") %>%
  select(-c("variable", "n")) 
#
###Detrend##
Stone_totals <- Stone_totals %>% plyr::mutate(MonYr = paste(Month, Year, sep = "/"))  #Create MonYr column
Stonede <- Stone_totals %>% group_by(MonYr) %>% summarise(CPUE = mean(CPUE, na.rm = T)) #%>% ungroup() %>% select(MonYr, CPUE)
ggboxplot(Stone_totals, x = "MonYr", y = "CPUE") #additive
Stonede$MonYr <- as.yearmon(Stonede$MonYr, format = "%m/%Y")
Stonets <- na.interp(as.ts(read.zoo(Stonede, FUN = as.yearmon)))
Stonets %>% decompose("additive") -> decompStone
autoplot(decompStone)
StoneAdj <- Stonets-decompStone$seasonal
plot(StoneAdj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(StoneAdj))
Stone_Adj <- data.frame(MonYr, StoneAdj)
Stone_Adj$Year <- Stone_Adj$MonYr %>% format("%Y")
Stone_Adj$Month <- Stone_Adj$MonYr %>% format("%m")
write.csv((Stone_Adj %>% dplyr::select(Year, Month, StoneAdj)), file = "CSV/Output/Stoneadj.csv", row.names = FALSE)

###Compare detrended among years for change in CPuE over time
##Stone_Adj
Stone_Adj$Year <- as.factor(Stone_Adj$Year)
#ANOVA
set.seed(54321)
aovSC <- aovp(StoneAdj ~ Year, data = Stone_Adj, perm = "", nperm = 10000)
summary(aovSC)
aovSC_tidy <- tidy(aovSC)
names(aovSC_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among years - compare
set.seed(54321)
TUKSC <- TukeyHSD(aovSC)
TUKSC_tab <- tidy(TUKSC) %>% dplyr::select(-c("term"))
names(TUKSC_tab) <- c("Years", "Estimate", "CIlower", "CIupper", "adj.p-value")    #Move 'Seasons' to front and drop grp1 & grp2
plot(TUKSC)
#No sig diffs - try pairwise_t
set.seed(54321)
Stone_testing <- Stone_Adj %>% group_by(Month, Year) %>% rstatix::get_summary_stats(StoneAdj, type = "mean_sd")
Stones_pair <- Stone_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")
Stones_pair <- select(Stones_pair, c("group1", "group2", "p", "p.adj"))
Stones_pair <- Stones_pair %>% mutate(Years = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Years", everything()) %>% select(-c("group1", "group2"))
#
#
####Blue Crabs####
Blue <- read.csv("CSV/Blue_ori.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
head(Blue, 3)
str(Blue)
Blue <- Blue %>% dplyr::select(-c(Temperature,Salinity)) %>% filter(Station != "SH") %>% filter(Station != "AP")
#
Blue_totals <- Blue %>% group_by(Year, Month, Bay, MonYr, Season, Station) %>% summarise(Traps = sum(Number_Traps, na.rm = T),
                                                                                         Crabs = sum(Number_Crabs, na.rm = T),
                                                                                         CPUE = mean(Crabs/Traps, na.rm = T))
Blue_totals$Season <- as.factor(Blue_totals$Season)
Blue_totals$Bay <- as.factor(Blue_totals$Bay)
#
write.csv(Blue_totals, file = "CSV/Output/Summary/BlueCPUE.csv", row.names = FALSE)
#Vizualize
ggboxplot(Blue_totals, x = "Season", y = "CPUE")
ggplot(Blue_totals, aes(x = CPUE))+
  geom_histogram()
#Slightly skewed so transfrom data
Blue_totals$log <- log10(Blue_totals$CPUE+1)
#
write.csv(Blue_totals, file = "CSV/Output/Summary/BlueCPUE.csv", row.names = FALSE)
#
ggplot(Blue_totals, aes(x = log))+
  geom_histogram()
#Summary of log transformed data
Blues <- Blue_totals %>% group_by(Month, Year, Season, Bay) %>% rstatix::get_summary_stats(log, type = "mean_sd")

#Perm ANOVA on Season
set.seed(54321)
aovpB <- aovp(mean ~ Season, data = Blues, perm = "", nperm = 10000)
summary(aovpB)
aovp_tidyB <- tidy(aovpB)
names(aovp_tidyB) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#
#Perm ANOVA on Season and Bay - just use Season
set.seed(54321)
aovpB <- aovp(mean ~ Season * Bay, data = Blues, perm = "", nperm = 10000)
summary(aovpB)
aovp_tidyB <- tidy(aovpB)
names(aovp_tidyB) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#
#Removal of SH, AP.  AR = OTB.  Bays and seasons not significantly different so no detrendeding required.
Blue_totals %>% ggplot(aes(Year, CPUE, color = Bay))+
  geom_boxplot() + facet_wrap(.~Season)
##Pairwise comparisons - season
set.seed(54321)
seas_pairB <- Blues %>% pairwise_t_test(mean ~ Season, p.adjust.method = "holm")
seas_tabB <- select(seas_pairB, c("group1", "group2", "p", "p.adj"))
seas_tabB <- seas_tabB %>% mutate(Seasons = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Seasons", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
#
#Means of groups, comparisons to show
Season_meansB <- Blues %>% group_by(Season) %>% get_summary_stats(mean, type = "mean_sd") %>%
  select(-c("variable", "n"))
#
Blue_final <- Blue_totals %>% group_by(Year, Month) %>% summarise(BClogAdj = mean(log, na.rm = T))
#save file for use
write.csv(Blue_final, file = "CSV/Output/Blueadj.csv", row.names = FALSE)

###Compare log transformed data among years for change in CPuE over time
##Blue_final
Blue_final$Year <- as.factor(Blue_final$Year)
#ANOVA
set.seed(54321)
aovBC <- aovp(BClogAdj ~ Year, data = Blue_final, perm = "", nperm = 10000)
summary(aovBC)
aovBC_tidy <- tidy(aovBC)
names(aovBC_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among years, compare.
TUKBC <- TukeyHSD(aovBC)
TUKBC_tab <- tidy(TUKBC) %>% dplyr::select(-c("term"))
names(TUKBC_tab) <- c("Years", "Estimate", "CIlower", "CIupper", "adj.p-value")    #Move 'Seasons' to front and drop grp1 & grp2
plot(TUKBC)
#Sig diffs - 2002 lowest
#USE pairwise for consistency
set.seed(54321)
Blue_testing <- Blue_totals %>% group_by(Month, Year) %>% rstatix::get_summary_stats(log, type = "mean_sd")
Blue_pair <- Blue_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")
Blue_pair <- select(Blue_pair, c("group1", "group2", "p", "p.adj"))
Blue_pair <- Blue_pair %>% mutate(Years = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Years", everything()) %>% select(-c("group1", "group2"))

#
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
SFDM_Totals$logTSC <- log10(SFDM_Totals$Total)
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
seas_pairSF <- SFDMs %>% pairwise_t_test(mean ~ Season, p.adjust.method = "holm")
seas_tabSF <- select(seas_pairSF, c("group1", "group2", "p", "p.adj"))
seas_tabSF <- seas_tabSF %>% mutate(Seasons = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Seasons", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
#
#Means of groups, comparisons to show
Season_meansSF <- SFDMs %>% group_by(Season) %>% get_summary_stats(mean, type = "mean_sd") %>%
  select(-c("variable", "n")) 
#
###Detrend##
StoneFDMs <- SFDM_Totals %>% plyr::mutate(MonYr = paste(Month, Year, sep = "/"))  #Create MonYr column
Stonede <- StoneFDMs %>% group_by(MonYr) %>% summarise(logTotal = sum(logTSC, na.rm = T))
ggboxplot(Stonede, x = "MonYr", y = "logTotal") #additive
Stonede$MonYr <- as.yearmon(Stonede$MonYr, format = "%m/%Y")
StoneFts <- na.interp(as.ts(read.zoo(Stonede, FUN = as.yearmon)))
StoneFts %>% decompose("additive") -> decompSFDM
autoplot(decompSFDM)
SFDMAdj <- StoneFts-decompSFDM$seasonal
plot(SFDMAdj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(SFDMAdj))
SFDM_Adj <- data.frame(MonYr, SFDMAdj)
SFDM_Adj$Year <- SFDM_Adj$MonYr %>% format("%Y")
SFDM_Adj$Month <- SFDM_Adj$MonYr %>% format("%m")
write.csv((SFDM_Adj %>% dplyr::select(Year, Month, SFDMAdj)), file = "../CSV/Output/S_FDM_adj.csv", row.names = FALSE)

###Compare detrended among years for change in CPuE over time
##SFDM_Adj
SFDM_Adj$Year <- as.factor(SFDM_Adj$Year)
#ANOVA
set.seed(54321)
aovSC_FDM <- aovp(SFDMAdj ~ Year, data = SFDM_Adj, perm = "", nperm = 10000)
summary(aovSC_FDM)
aovSC_FDM_tidy <- tidy(aovSC_FDM)
names(aovSC_FDM_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among years TUKEY - compare
set.seed(54321)
TUKSC_FDM <- TukeyHSD(aovSC_FDM)
TUKSC_FDM_tab <- tidy(TUKSC_FDM) %>% dplyr::select(-c("term"))
names(TUKSC_FDM_tab) <- c("Years", "Estimate", "CIlower", "CIupper", "adj.p-value")    #Move 'Seasons' to front and drop grp1 & grp2
plot(TUKSC_FDM)
#No sig diffs - try pairwise_t
set.seed(54321)
SFDM_testing <- SFDM_Adj %>% group_by(Month, Year) %>% rstatix::get_summary_stats(SFDMAdj, type = "mean_sd")
SFDM_pair <- SFDM_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")
SFDM_pair <- select(SFDM_pair, c("group1", "group2", "p", "p.adj"))
SFDM_pair <- SFDM_pair %>% mutate(Years = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Years", everything()) %>% select(-c("group1", "group2"))

#Get letters, load sumamry table, append letters and save with letters
SFDMLett <- data.frame(Letters = multcompView::multcompLetters4(aovSC_FDM, comp = TUKSC_FDM)$Year$Letters) %>%
  tibble::rownames_to_column("Year")
SFDM_TOT <- SFDM_Totals %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
SFDM_TOT <- merge(SFDM_TOT, SFDMLett, by = "Year")
write.csv(SFDM_TOT, file = "CSV/Output/Summary/SFDM_yearly_letters.csv", row.names = FALSE)
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
write.csv(BFDM_Totals, file = "CSV/Output/Summary/BCLanding.csv", row.names = FALSE)

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
BlueFDMs <- BFDM_Totals %>% plyr::mutate(MonYr = paste(Month, Year, sep = "/"))  #Create MonYr column
Bluede <- BlueFDMs %>% group_by(MonYr) %>% summarise(Total = sum(Total, na.rm = T))
ggboxplot(Bluede, x = "MonYr", y = "Total") #additive
Bluede$MonYr <- as.yearmon(Bluede$MonYr, format = "%m/%Y")
BlueFts <- na.interp(as.ts(read.zoo(Bluede, FUN = as.yearmon)))
BlueFts %>% decompose("additive") -> decompBFDM
autoplot(decompBFDM)
BFDMAdj <- BlueFts-decompBFDM$seasonal
plot(BFDMAdj)

#New df of detrended data
MonYr <- as.Date(time(BFDMAdj))
BFDM_Adj <- data.frame(MonYr, BFDMAdj)
BFDM_Adj$Year <- BFDM_Adj$MonYr %>% format("%Y")
BFDM_Adj$Month <- BFDM_Adj$MonYr %>% format("%m")
write.csv((BFDM_Adj %>% dplyr::select(Year, Month, BFDMAdj)), file = "../CSV/Output/B_FDM_adj.csv", row.names = FALSE)

###Compare detrended among years for change in CPuE over time
##Stone_Adj = BFDM_Adj
BFDM_Adj$Year <- as.factor(BFDM_Adj$Year)
#ANOVA
set.seed(54321)
aovBC_FDM <- aovp(BFDMAdj ~ Year, data = BFDM_Adj, perm = "", nperm = 10000)
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
BFDM_testing <- BFDM_Adj %>% group_by(Month, Year) %>% rstatix::get_summary_stats(BFDMAdj, type = "mean_sd")
BFDM_pair <- BFDM_testing %>% pairwise_t_test(mean ~ Year, p.adjust.method = "holm")
BFDM_pair <- select(BFDM_pair, c("group1", "group2", "p", "p.adj"))
BFDM_pair <- BFDM_pair %>% mutate(Years = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Years", everything()) %>% select(-c("group1", "group2"))

#Get letters, load sumamry table, append letters and save with letters
BFDMLett <- data.frame(Letters = multcompView::multcompLetters4(aovBC_FDM, comp = TUKBC_FDM)$Year$Letters) %>%
  tibble::rownames_to_column("Year")
BFDM_TOT <- BFDM_Totals %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
BFDM_TOT <- merge(BFDM_TOT, BFDMLett, by = "Year")
write.csv(BFDM_TOT, file = "CSV/Output/Summary/BFDM_yearly_letters.csv", row.names = FALSE)
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
FIMBC_totals <- F_Crabs %>% group_by(Year, Month, Bay, MonYr, Quarter, reference) %>% summarise(Effort = mean(effort, na.rm = T),
                                                                                                Total = sum(TotalCount, na.rm = T))
FIMBC_totals$Quarter <- as.factor(FIMBC_totals$Quarter)
FIMBC_totals$Bay <- as.factor(FIMBC_totals$Bay)
#
write.csv(FIMBC_totals, file = "CSV/Output/Summary/FIMBlue_Effort.csv", row.names = FALSE)
#
F_BC_mean <- F_Crabs %>% group_by(Month, Year, Quarter, Bay) %>% rstatix::get_summary_stats(effort, type = "mean_sd")
F_BC_mean$Quarter <- as.factor(F_BC_mean$Quarter)
F_BC_mean$Bay <- as.factor(F_BC_mean$Bay)
#
write.csv(F_BC_mean, file = "CSV/Output/Summary/FIMBlue_Effort_means.csv", row.names = FALSE)
#Vizualize
ggboxplot(F_BC_mean, x = "Quarter", y = "mean")
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
#All seasons are sig diff 1 < 3 < 2 & 4 so seasonality should be removed from data

#Means of groups, comparisons to show
Season_meansB <- F_BC_mean %>% group_by(Quarter) %>% get_summary_stats(mean, type = "mean_sd") %>%
  select(-c("variable", "n"))
#
###Detrend##
F_BC_mean <- F_BC_mean %>% plyr::mutate(MonYr = paste(Month, Year, sep = "/"))  #Create MonYr column
FBCde <- FIMBC_totals %>% group_by(MonYr) %>% summarise(Effort = mean(Effort, na.rm = T)) #%>% ungroup() %>% select(MonYr, CPUE)
ggboxplot(F_BC_mean, x = "MonYr", y = "mean") #additive
#FBCde$MonYr <- as.yearmon(FBCde$MonYr, format = "%m/%Y")
FBCts <- na.interp(as.ts(read.zoo(FBCde, FUN = as.yearmon)))
FBCts %>% decompose("additive") -> decompFBC
autoplot(decompFBC)
FBCAdj <- FBCts-decompFBC$seasonal
plot(FBCAdj)

#New df combining adjusted data. Compare
MonYr <- as.Date(time(FBCAdj))
FBC_Adj <- data.frame(MonYr, FBCAdj)
FBC_Adj$Year <- FBC_Adj$MonYr %>% format("%Y")
FBC_Adj$Month <- FBC_Adj$MonYr %>% format("%m")
#
write.csv((FBC_Adj %>% dplyr::select(Year, Month, FBCAdj)), file = "CSV/Output/FIM_BC_adj.csv", row.names = FALSE)

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
write.csv(F_Crab_sum, file = "CSV/Output/Summary/FIM_crab_letters.csv", row.names = FALSE)
#
####FIM crab extra####
#Perm ANOVA on Season, Bay
set.seed(54321)
aovp <- aovp(mean ~ Quarter * Bay, data = F_BC_mean, perm = "", nperm = 10000)
summary(aovp)
aovp_tidy <- tidy(aovp)
names(aovp_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among Seasons and Bays

##Pairwise comparisons - season
seas_pair <- F_BC_mean %>% pairwise_t_test(mean ~ Quarter, p.adjust.method = "BH")
seas_tab <- select(seas_pair, c("group1", "group2", "p", "p.adj"))
seas_tab <- seas_tab %>% mutate(Seasons = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Seasons", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
ggboxplot(F_BC_mean, x = "Quarter", y = "mean")
#All seasons are sig diff except 2 & 4 so seasonality should be removed from data

##Pairwise comparisons - Bay
bays_pair <- F_BC_mean %>% pairwise_t_test(mean ~ Bay, p.adjust.method = "holm")
bays_tab <- select(bays_pair, c("group1", "group2", "p", "p.adj"))
bays_tab <- bays_tab %>% mutate(Bay = paste(group1, group2, sep = " vs. ")) %>%   #Add new column of grp v grp
  select("Bay", everything()) %>% select(-c("group1", "group2"))    #Move 'Seasons' to front and drop grp1 & grp2
ggboxplot(F_BC_mean, x = "Bay", y = "mean")

