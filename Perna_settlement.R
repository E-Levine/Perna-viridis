###Perna settlement in TB###
#Settlement on scallop spat traps
#Comparisons among Bays
#
#
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
if (!require("remotes")) install.packages("remotes")
remotes::install_github("GegznaV/biostat")
#
#
#
###Perna spat data
####Set up####
perna_spat <- read.csv("CSV/TB_recruit.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
perna_spat <- perna_spat %>% plyr::mutate(MonYr = paste(Month, Year, sep = "/"))  #Create MonYr column
head(perna_spat, 3) #Check data entered properly
str(perna_spat)     #Check structure of df
#Make Quarter, Bay, IvO = Factors, Spat = numeric
perna_spat$IvO  <- as.factor(perna_spat$IvO)
perna_spat$Bay  <- as.factor(perna_spat$Bay)
perna_spat$Quarter  <- as.factor(perna_spat$Quarter)
perna_spat$Spat <- as.numeric(perna_spat$Spat)

##Vizualize data##
#Get summary mean/sd per grouping Quarter x Bay x IvO
perna <- perna_spat %>% group_by(Month, Year, Quarter, Bay, IvO) %>% rstatix::get_summary_stats(Spat, type = "mean_sd")
bxp <- ggboxplot(perna_spat, x = "Quarter", y = "Spat", color = "IvO", facet.by = "Bay")
bxp
perna_spat %>% ggplot(aes(x = Spat))+
  geom_histogram(aes(y = ..count..))

#Clearly not normal and zero-inflated so log(x+1) transform data
perna_spat$logSpat <- log10(perna_spat$Spat+1)
#Get new summary mean/sd per group for log+1 data
perna_log <- perna_spat %>% group_by(Month, Year, Quarter, Bay, IvO) %>% rstatix::get_summary_stats(logSpat, type = "mean_sd")
ggboxplot(perna_log, x = "Quarter", y = "mean", color = "IvO", facet.by = "Bay") %>%
  ggpar(main = "Log(Spat+1)")
ggsave("Figures/Reference/Log_boxplot.png")
perna_log %>% ggplot(aes(x = mean))+
  geom_histogram(aes(y = ..count..))
#
#
####Permutation based 3 factor ANOVA####
#Using mean log10(spat+1) vs Quarter, Bay, and IvO 
set.seed(402)
aovp <- aovp(mean ~ Quarter * Bay * IvO, data = perna_log, perm = "",  nperm = 10000)
summary(aovp)
sumaovp <- summary(aovp)
#
aovp_tidy <- tidy(aovp)
names(aovp_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")

#Sig diff among quarter and bay, not between IvO --> detrend for quarter/season, group for Bays

##Pairwise comparisons - season
seas_pair <- perna_log %>% pairwise_t_test(mean ~ Quarter, p.adjust.method = "BH")
seas_tab <- select(seas_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  select(" lComparison", everything()) %>% select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Seas_letters <- biostat::make_cld(seas_tab) %>% select(-c("spaced_cld")) %>% rename(Quarter = group, Letters = cld)
#All seasons are sig diff except 3 & 4 so seasonality should be removed from data
Seasons <- merge(perna_spat %>% group_by(Quarter) %>% rstatix::get_summary_stats(Spat, type = "mean_sd") %>% 
                   dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Seas_letters, by = "Quarter")

##Pairwise comparisons - Bay
bays_pair <- perna_log %>% pairwise_t_test(mean ~ Bay, p.adjust.method = "holm")
bays_tab <- select(bays_pair, c("group1", "group2", "p", "p.adj")) %>%
  mutate(Comparison = paste(group1, group2, sep = "-")) %>%   #Add new column of grp v grp
  select("Comparison", everything()) %>% select(-c("group1", "group2")) %>% rename(p.value = p, p.adjust = p.adj)    #Move 'Comparison' to front and drop grp1 & grp2
Bay_letters <- biostat::make_cld(bays_tab) %>% select(-c("spaced_cld")) %>% rename(Bay = group, Letters = cld)
Bays <- merge(perna_spat %>% group_by(Bay) %>% rstatix::get_summary_stats(Spat, type = "mean_sd") %>% 
                dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd), Bay_letters, by = "Bay")

#save tables for stats/figures
write.csv(seas_pair, file = "CSV/Output/Stats/seas_pair.csv", row.names = FALSE)
write.csv(Seasons, file = "CSV/Output/Summary/Perna_seasons_letters.csv", row.names = FALSE)
write.csv(bays_pair, file = "CSV/Output/Stats/bays_pair.csv", row.names = FALSE)
write.csv(Bays, file = "CSV/Output/Summary/Perna_bays_letters.csv", row.names = FALSE)
#
#
####Detrend data####
library(forecast)
#####Use log data for detrending
head(perna_spat,3)
#Separate by bay
logLTB <- perna_spat %>% group_by(MonYr) %>% filter(Bay == "LTB") %>% dplyr::summarise(Spat = mean(logSpat, na.rm = T))
logMTB <- perna_spat %>% group_by(MonYr) %>% filter(Bay == "MTB") %>% dplyr::summarise(Spat = mean(logSpat, na.rm = T))
logOTB <- perna_spat %>% group_by(MonYr) %>% filter(Bay == "OTB") %>% dplyr::summarise(Spat = mean(logSpat, na.rm = T))
logBoth <- perna_spat %>%filter(Bay == "LTB" | Bay == "MTB") %>% group_by(MonYr, Bay) %>% select(Bay, Month:logSpat) %>% 
  dplyr::summarise(logSpat = mean(logSpat, na.rm = T)) %>% spread("Bay", "logSpat")
logBoth$logAve <- rowMeans(subset(logBoth, select = c(LTB, MTB)), na.rm = T)

#logLTB
lLTBts <- logLTB 
lLTBts$MonYr <- as.yearmon(lLTBts$MonYr, format = "%m/%Y")  #Create ts
lLTBts <- na.interp(as.ts(read.zoo(lLTBts, FUN = as.yearmon)))  #Fill NAs by interp
lLTBts %>% decompose("multiplicative") -> decomplLTB #Decompose, remove seasonality, plot
autoplot(decomplLTB)
lLTBadj <- lLTBts/decomplLTB$seasonal
par(mfrow = c(2, 1), oma = c(0,0,1,0))
plot(lLTBts)
plot(lLTBadj)

#logMTB
lMTBts <- logMTB 
lMTBts$MonYr <- as.yearmon(lMTBts$MonYr, format = "%m/%Y")  #Create ts
lMTBts <- na.interp(as.ts(read.zoo(lMTBts, FUN = as.yearmon)))  #Fill NAs by interp
lMTBts %>% decompose("multiplicative") -> decomplMTB #Decompose, remove seasonality, plot
autoplot(decomplMTB)
lMTBadj <- lMTBts/decomplMTB$seasonal
par(mfrow = c(2, 1), oma = c(0,0,1,0))
plot(lMTBts)
plot(lMTBadj)

#logOTB
lOTBts <- logOTB 
lOTBts$MonYr <- as.yearmon(lOTBts$MonYr, format = "%m/%Y")  #Create ts
lOTBts <- na.interp(as.ts(read.zoo(lOTBts, FUN = as.yearmon)))  #Fill NAs by interp
lOTBts %>% decompose("multiplicative") -> decomplOTB #Decompose, remove seasonality, plot
autoplot(decomplOTB)
lOTBadj <- lOTBts/decomplOTB$seasonal
par(mfrow = c(2, 1), oma = c(0,0,1,0))
plot(lOTBts)
plot(lOTBadj)
#
#logBoth
lBothts <- select(logBoth, c(MonYr, logAve))
lBothts$MonYr <- as.yearmon(lBothts$MonYr, format = "%m/%Y")  #Create ts
lBothts <- na.interp(as.ts(read.zoo(lBothts, FUN = as.yearmon)))  #Fill NAs by interp
lBothts %>% decompose("multiplicative") -> decomplBoth #Decompose, remove seasonality, plot
autoplot(decomplBoth)
lBothadj <- lBothts/decomplBoth$seasonal
par(mfrow = c(2, 1), oma = c(0,0,1,0))
plot(lBothts)
plot(lBothadj)
#

#Create new df of adjusted data
Adjusted_log <- ts.union(na.interp(lLTBadj), na.interp(lMTBadj), na.interp(lOTBadj), na.interp(lBothadj))
MonYr <- as.Date(time(lLTBadj))
Adjusted_log <- data.frame(MonYr, Adjusted_log)
names(Adjusted_log) <- c("MonYr", "DlogLTB", "DlogMTB", "DlogOTB", "DlogBoth")
Adjusted_log$Year <- as.numeric(format(as.yearmon(Adjusted_log$MonYr, format = "%Y-%m-%d"), "%Y"))                    
Adjusted_log$Month <- as.numeric(format(as.yearmon(Adjusted_log$MonYr, format = "%Y-%m-%d"), "%m"))
Adjusted_log <- Adjusted_log %>% select("Month", "Year", everything()) %>% select(-c("MonYr"))
#
write.csv(Adjusted_log, file = "CSV/Output/DetrendedLogBays.csv", row.names = FALSE)
#
#
####Comparisons between years####
#Original data to compare all data, LTB+MTB, and OTB
perna <- read.csv("CSV/DetrendedLogBays_modified.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#Create column of All to compare to OTB and Both (Comparisons chosen based on perm3factor) )
perna <- perna %>% mutate(All = rowMeans(.[names(.)[3:5]], na.rm = T)) 
perna <- dplyr::select(perna, c(Month,Year, DlogOTB:All))
perna$Year <- as.factor(perna$Year)

#
##ALL
#Summary stats for each group
All_sum <- perna %>% group_by(Year) %>% summarise(count = n(),
                                                  mean = mean(All, na.rm = T),
                                                  sd = sd(All, na.rm = T),
                                                  median = median(All, na.rm = T),
                                                  IQR = IQR(All, na.rm = T))# %>% arrange(desc(median))
ggboxplot(perna, "Year", "All")

Both_sum <- perna %>% group_by(Year) %>% summarise(count = n(),
                                                   mean = mean(DlogBoth, na.rm = T),
                                                   sd = sd(DlogBoth, na.rm = T),
                                                   median = median(DlogBoth, na.rm = T),
                                                   IQR = IQR(DlogBoth, na.rm = T))# %>% arrange(desc(median))
ggboxplot(perna, "Year", "DlogBoth")

OTB_sum <- perna %>% group_by(Year) %>% summarise(count = n(),
                                                  mean = mean(DlogOTB, na.rm = T),
                                                  sd = sd(DlogOTB, na.rm = T),
                                                  median = median(DlogOTB, na.rm = T),
                                                  IQR = IQR(DlogOTB, na.rm = T))# %>% arrange(desc(median))
ggboxplot(perna, "Year", "DlogOTB")

###Both spat
#ANOVA
set.seed(54321)
aov_yB <- aovp(DlogBoth ~ Year, data = perna, perm = "", nperm = 10000)
summary(aov_yB)
aov_yB_tidy <- tidy(aov_yB)
names(aov_yB_tidy) <- c("Factors", "df", "SS", "MS", "Iter", "Pr")
#Sig diff among years
##TukeyKramer     #TukeyHSD
TUKBoth <- TukeyHSD(aov_yB)
TUKBoth_tab <- tidy(TUKBoth) %>% dplyr::select(-c("term", "null.value"))
names(TUKBoth_tab) <- c("Years", "Estimate", "CIlower", "CIupper", "adj.p-value")    #Move 'Seasons' to front and drop grp1 & grp2
par(mfrow = c(1, 1), oma = c(0,0,1,0))
plot(TUKBoth)
#Get letters, load summary table, append letters and save with letters
BothLett <- data.frame(Letters = multcompView::multcompLetters4(aov_yB, comp = TUKBoth)$Year$Letters) %>%
  tibble::rownames_to_column("Year")
meansBoth <- read.csv("../CSV/Output/Summary/Both_original_meansSD.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
meansBoth <- merge(meansBoth, BothLett, by = "Year")
#
write.csv(meansBoth, file = "CSV/Output/Summary/Both_original_meansSD_Letters.csv", row.names = FALSE)

###OTB spat
#ANOVA
set.seed(54321)
aov_yO <- aovp(DlogOTB ~ Year, data = (ungroup(perna) %>% filter(as.integer(Year) <= 4)), perm = "", nperm = 10000)
summary(aov_yO)
aov_yO_tidy <- tidy(aov_yO)
names(aov_yO_tidy) <- c("Factors", "df", "SS", "MS", "F", "Pr")
#Sig diff among years
##TukeyKramer     #TukeyHSD
TUKOTB <- TukeyHSD(aov_yO)
TUKOTB_tab <- tidy(TUKOTB) %>% dplyr::select(-c("term", "null.value"))
names(TUKOTB_tab) <- c("Years", "Estimate", "CIlower", "CIupper", "adj.p-value")    #Move 'Seasons' to front and drop grp1 & grp2
plot(TUKOTB)
#Get letters, load sumamry table, append letters and save with letters
OTBLett <- data.frame(Letters = multcompView::multcompLetters4(aov_yO, comp = TUKOTB)$Year$Letters) %>%
  tibble::rownames_to_column("Year")
meansOTB <- read.csv("CSV/Output/Summary/OTB_original_meansSD.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
meansOTB <- merge(meansOTB, OTBLett, by = "Year")
#
write.csv(meansOTB, file = "CSV/Output/Summary/OTB_original_meansSD_Letters.csv", row.names = FALSE)
#
