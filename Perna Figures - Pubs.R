##Figures for Perna paper, final for publication

library(ggplot2)
library(ggmap)
library(ggsn)
library(dplyr)
library(ggpubr)
#
####Themes and formatting####
basetheme <- theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12, face = "bold", color = "black"), axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), axis.text.y = element_text(size = 10),
        panel.grid = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "black"))
axistheme <- theme(axis.ticks.length = unit(-0.15, "cm"), axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0, 0.5), "cm")), axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")))
legends <- theme(legend.position = "right", legend.key = element_rect(color = "transparent", fill = "transparent"))
legendON <- theme(legend.position = c(0.897, 0.899), legend.title = element_blank(),
                  legend.background = element_rect(linetype = 1, size = 0.5, color = "black"),
                  legend.key = element_rect(color = "transparent", fill = "transparent"))
bxptheme <- theme_bw() +
  theme(axis.text.x = element_text(size = 12, face = "bold"), axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(t = 0, r = 8, b = 0, l = 0)), 
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "black"))
theme_f <- theme(strip.text.y = element_blank(),
                 strip.background = element_blank(),
                 panel.spacing = unit(0.75, "lines"),
                 strip.text.x = element_blank())
#
Bay_names <- c("LTB" = "Lower TB", "MTB" = "Middle TB", "OTB" = "Old TB")
#Crab names
CrabSp <- c("Blue" = "C. sapidus", "Stone" = "M. mercenaria")
Crabcolor <- c("#000000", "#666666")
names(Crabcolor) <- c("Blue", "Stone")
#Shapes to station type color to IvO
shapesAll <- c(22, 16, 15, 17, 21)
names(shapesAll) <- levels(Spat_crab_stations$Helper)
shapes <- c(16, 15, 17, 23)
names(shapes) <- c("LTB", "MTB", "OTB", "HB")
#Color to IvO
IvOColor <- c("#000000", "#666666", "#000000", "#333333")
names(IvOColor) <- c("I", "O", "N", "A")
#Color to Bay
BayColor <- c("#000000", "#666666", "#999999")
names(BayColor) <- c("LTB", "MTB", "OTB")
#Color to Bay with HB
BayColorH <- c("#000000", "#666666", "#999999", "#000000")
names(BayColorH) <- c("LTB", "MTB", "OTB", "HB")
#
Helpers <- c("BC-TB" = expression(italic("C. sapidus")), "SC-TB" = expression(italic("M. mercenaria")), "GM-LTB" = "Lower TB", "GM-MTB" = "Middle TB", "GM-OTB" = "Old TB")
InandOut <- c("I" = "In", "O" = "Out")
#
#remove top and left box lines
#
#
####Fig 1 - Station Map####
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
Spat_crab_stations <- read.csv("../CSV/Spat_crab_stations.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
str(Spat_crab_stations)
summary(Spat_crab_stations)
Spat_crab_stations$Helper <- as.factor(Spat_crab_stations$Helper)
AllMap_area <- bounding(Spat_crab_stations$Lat, Spat_crab_stations$Long, "google", "terrain", "bw")

all_stations <- ggmap(AllMap_area) +
  geom_point(data = Spat_crab_stations, aes(x = Long, y = Lat, color = IvO, shape = Helper), size = 3, alpha = 0.8)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_blank(), axis.text = element_text(color = "black"),
        #legend.position = "none")+
        legend.position = c(0.855, 0.4), #legend.title = element_blank(),
        legend.background = element_rect(fill = "#CCCCCC"),
        legend.key = element_rect(color = "transparent", fill = "transparent"),
        legend.text.align = 0, legend.box.just = "right")+
  scale_x_continuous(expand = c(0,0), breaks = c(-82.80, -82.70, -82.60, -82.50, -82.40), 
                     labels = c("-82.80°", "-82.70°", "-82.60°", "-85.50°", "-82.40°"))+
  scale_y_continuous(expand = c(0,0), breaks = c(27.50, 27.60,27.70,27.80,27.90), 
                     labels = c("27.50°", "27.60°","27.70°","27.80°","27.90°"))+
  scale_color_manual(name = "Type", values = IvOColor,breaks = c("I", "O"), labels = InandOut)+
    guides(colour = guide_legend(override.aes = list(shape = 18)))+ #overrides shape of IvO colors in key
  scale_shape_manual(name = "Station", values = shapesAll, labels = Helpers)+
  scalebar(dist = 1, dist_unit = "mi", transform = TRUE, model = "WGS84",  #Transform = true:data in decimals so needs transformed 
           st.size = 3.5, st.bottom = FALSE, st.dist = 0.06, #size = bar size, bottom = text location, dist = btw text and bar
           x.min = -82.42, x.max = -82.52,
           y.min = 27.50, y.max = 27.65)

#Add north arrow
all_stations %>% north2(x= 0.732, y = 0.16, scale = 0.06, symbol = 12)
#
####Fig 2 - Temp and Sal####
#AllWQ_final <- read.csv("../CSV/Output/Summary/WQ_MoYr.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
AllWQ_final <- read.csv("../CSV/Output/Summary/WQsummary.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#MonYr
AllWQ_final$MonYr <- zoo::as.yearmon(paste(AllWQ_final$Year, AllWQ_final$Month), "%Y %m")
#

Temps <- AllWQ_final %>% ggplot(aes(x = MonYr, y = T_mean))+
  geom_line(size = 1)+
  basetheme+ axistheme+
  ylab("Temperature (°C)")+
  scale_x_continuous(limits = c(2002, 2017), breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0, 35), expand = c(0,0))+
  geom_hline(yintercept = 26, linetype = "dashed")+
  geom_hline(yintercept = 32, linetype = "dashed")+
  geom_rect(aes(ymin = 26, ymax = 32, xmin = -Inf, xmax = Inf),
            fill = "#999999", alpha = 0.006)+
  geom_line(aes(x = MonYr, y = T_min), linetype = "dashed", color = "#666666")+
  geom_line(aes(x = MonYr, y = T_max), linetype = "dashed", color = "#666666")
Sals <- AllWQ_final %>% ggplot(aes(x = MonYr, y = S_mean))+
  geom_line(size = 1)+
  basetheme+ axistheme+
  ylab("Salinity (ppt)")+
  scale_x_continuous(limits = c(2002, 2017), breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0, 45), expand = c(0,0))+
  geom_hline(yintercept = 27, linetype = "dashed")+
  geom_hline(yintercept = 32, linetype = "dashed")+
  geom_rect(aes(ymin = 27, ymax = 32, xmin = -Inf, xmax = Inf),
            fill = "#999999", alpha = 0.006)+
  geom_line(aes(x = MonYr, y = S_min), linetype = "dashed", color = "#666666")+
  geom_line(aes(x = MonYr, y = S_max), linetype = "dashed", color = "#666666")
#
Figure <- ggpubr::ggarrange(Temps, Sals, labels = c("A", "B"), nrow = 2)
Figure
#
#
####Fig 3 - Season/Bay Averages####
library(lmPerm)
TBrecruit <- read.csv("../CSV/TB_recruit.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
TBrecruit$Quarter <- as.factor(TBrecruit$Quarter)
TBrecruit$Bay <- as.factor(TBrecruit$Bay)

###Season
Seasons <- TBrecruit %>% group_by(Quarter) %>% rstatix::get_summary_stats(Spat, type = "mean_sd") %>% 
  dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd)
set.seed(54321)
Seas_aov <- aovp(Spat ~ Quarter, data = TBrecruit, perm = "", nperm = 10000)
Seas_Tuk <- TukeyHSD(Seas_aov)
SeasLett <- data.frame(Letters = multcompView::multcompLetters4(Seas_aov, comp = Seas_Tuk)$Quarte$Letters) %>%
  tibble::rownames_to_column("Quarter")
Seasonmeans <- merge(Seasons, SeasLett, by = "Quarter")
Seasonmeans$Quarter <- plyr::revalue(as.factor(Seasonmeans$Quarter), c("1" = "Winter", "2" = "Spring", "3" = "Summer", "4" = "Fall")) %>%
  factor(levels = c("Winter", "Spring", "Summer", "Fall"))
#Use letters to differentiate groups
seas_letters <- ggplot(Seasonmeans, aes(Quarter, mean, fill = Letters))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Quarter, ymin = mean, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.93, 0.90))+ axistheme+
  ylab("Average Settlement (spat/collector/month)")+
  scale_y_continuous(expand = c(0,0), limits = c(0,15))
#
###Bay
Bays <- TBrecruit %>% group_by(Bay) %>% rstatix::get_summary_stats(Spat, type = "mean_sd") %>% 
  dplyr::select(-c("variable")) %>% transform(lower = mean-sd, upper = mean+sd)
#TBrecruit$Bay <- as.factor(TBrecruit$Bay)
set.seed(54321)
Bays_aov <- aovp(Spat ~ Bay, data = TBrecruit, perm = "", nperm = 10000)
Bays_Tuk <- TukeyHSD(Bays_aov)
BaysLett <- data.frame(Letters = multcompView::multcompLetters4(Bays_aov, comp = Bays_Tuk)$Bay$Letters) %>%
  tibble::rownames_to_column("Bay")
Baymeans <- merge(Bays, BaysLett, by = "Bay")
#
#Use letters to differentiate groups
bays_letters <- ggplot(Baymeans, aes(Bay, mean, fill = Letters))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Bay, ymin = mean, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.93, 0.933))+ axistheme +
  ylab("")+
  scale_x_discrete(labels = Bay_names)+ 
  scale_y_continuous(expand = c(0,0), limits = c(0,20))
#
###Bay and season figure
BandS <- ggpubr::ggarrange(seas_letters, bays_letters, labels = c("A", "B"))
BandS
#
#
####Fig 4 - Annual settlement####
####Years
#load files
Bothdf <- read.csv("../CSV/Output/Summary/Both_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
OTBdf <- read.csv("../CSV/Output/Summary/OTB_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

#Both
Bothbar <- ggplot(Bothdf, aes(Year, mean, fill = Letters))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.963, 0.805))+ axistheme+
  ylab("Average Settlement (spat/collector/month)")+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) 

#OTB
OTBbar <- ggplot(OTBdf, aes(Year, mean, fill = Letters))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.963, 0.81))+ axistheme+
  ylab("Average Settlement (spat/collector/month)")+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2006), breaks = seq(2002, 2005, by = 1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) 

#
##Combine shaded bars into 1 figure
Years_combined <- ggarrange(Bothbar, OTBbar, labels = c("A", "B"), nrow = 2)
Years_combined
#Combine with 1 axis title
Years_nolab <- ggarrange(Bothbar + rremove("y.title"), OTBbar + rremove("y.title"), 
                         labels = c("A", "B"), hjust = -3, vjust = 1.75, nrow = 2)
annotate_figure(Years_nolab, left = text_grob("Average Settlement (spat/collector/month)", face = "bold", rot = 90))
#
####Fig 5 - Crab CPUE####
Stones <- read.csv("../CSV/Summary/StoneCPUE.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Stone_mean <- Stones %>% group_by(Year) %>% summarise(meanCPUE = mean(CPUE, na.rm = T),
                                                      sd = sd(CPUE, na.rm = T),
                                                      median = median(CPUE, na.rm = T),
                                                      IQR = IQR(CPUE, na.rm = T),
                                                      lower = (meanCPUE - sd), 
                                                      upper = (meanCPUE + sd))

Stone_CPUE<- Stone_mean %>% ggplot(aes(Year, meanCPUE))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = meanCPUE, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  basetheme + legends + ylab("Catch per unit effort (crabs/trap/month)")+ axistheme+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2005, 2018), breaks = seq(2006, 2017, by = 2)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10))

Blues <- read.csv("../CSV/Summary/BlueCPUE.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Blue_mean <- Blues %>% group_by(Year) %>% summarise(meanCPUE = mean(CPUE, na.rm = T),
                                                    sd = sd(CPUE, na.rm = T),
                                                    median = median(CPUE, na.rm = T),
                                                    IQR = IQR(CPUE, na.rm = T),
                                                    lower = (meanCPUE - sd), 
                                                    upper = (meanCPUE + sd))

Blue_CPUE <- Blue_mean %>% ggplot(aes(Year, meanCPUE))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = meanCPUE, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  basetheme + legends + ylab("Catch per unit effort (crabs/trap/month)")+ axistheme+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2006), breaks = seq(2002, 2005, by = 1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10))

####Blue and Stone as A and B
Both_crabs <- ggarrange(Blue_CPUE, Stone_CPUE, labels = c("A", "B"), ncol = 2)
Both_crabs
###Blue and stone by color on 1 fig
Stone_mean$Species <- "Stone"
Blue_mean$Species  <- "Blue"
AllCrabs <- rbind(Stone_mean,Blue_mean)

All_crabs <- AllCrabs %>% ggplot(aes(Year, meanCPUE, fill = Species))+
  geom_bar(stat = "identity", size = 2)+
  scale_fill_grey(start = 0, end = 0.4, labels = CrabSp)+
  geom_errorbar(aes(Year, ymin = meanCPUE, ymax = upper), width = 0.2)+
  basetheme + legends + ylab("Catch per unit effort (crabs/trap/month)")+ axistheme+
  #theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  theme(legend.position = "none")+ 
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10))

All_crabs
#

####Fig 6 - Crab FDM DATA####
SFDM <- read.csv("../CSV/Summary/SCLanding.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
SFDM_total <- SFDM %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
SFDM_total$Spp <- "Stone"

Stone_FDM <- SFDM_total %>% ggplot(aes(Year, Total), fill = "#666666")+
  geom_bar(stat = "identity", size = 2)+
  scale_fill_grey(start = 0, end = 0.4, labels = CrabSp)+
  basetheme + legends + ylab("Total Pounds")+ axistheme+
  #theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  theme(legend.position = "none")+ 
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,500000), labels = scales::comma)
  
BFDM <- read.csv("../CSV/Summary/BCLanding.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
BFDM_total <- BFDM %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
BFDM_total$Spp <- "Blue"

Blue_FDM <- BFDM_total %>% ggplot(aes(Year, Total, fill = "#000000"))+
  geom_bar(stat = "identity", size = 2)+
  scale_fill_grey(start = 0, end = 0.4, labels = CrabSp)+
  basetheme + legends + ylab("Total Pounds")+ axistheme+
  #theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  theme(legend.position = "none")+ 
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1250000), labels = scales::comma)

Both_FDM <- ggarrange(Blue_FDM, Stone_FDM, labels = c("A", "B"), nrow = 2, align = "v")
Both_FDM
#1 y-axis
FDM_nolab <- ggarrange(Blue_FDM + rremove("y.title"), Stone_FDM + rremove("y.title"), 
                         labels = c("A", "B"), hjust = -6.25, vjust = 1.75, nrow = 2, align = "v")
annotate_figure(FDM_nolab, left = text_grob("Total Pounds", face = "bold", rot = 90))
#
AllFDM <- rbind(SFDM_total, BFDM_total)

AllFDM %>% ggplot(aes(Year, Total, fill = Spp))+
  geom_bar(position = "dodge", stat = "identity", size = 2)+
  scale_fill_grey(start = 0, end = 0.4, labels = CrabSp)+
  basetheme + legends + ylab("Total Pounds")+ 
  theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  #theme(legend.position = "none")+ 
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1010000))
#

#Letters
BFDM <- read.csv("../CSV/Output/Summary/BFDM_yearly_letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
SFDM <- read.csv("../CSV/Output/Summary/SFDM_yearly_letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

Blue_FDM <- BFDM %>% ggplot(aes(Year, Total, fill = Letters))+
  geom_bar(stat = "identity", size = 2)+
  scale_fill_grey(labels = CrabSp)+
  basetheme + legends + ylab("Total Pounds")+ 
  #theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1250000), labels = scales::comma)
Stone_FDM <- SFDM %>% ggplot(aes(Year, Total, fill = Letters))+
  geom_bar(stat = "identity", size = 2)+
  scale_fill_grey(labels = CrabSp)+
  basetheme + legends + ylab("Total Pounds")+ 
  #theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,500000), labels = scales::comma)
#
####Fig 7 - TB WQ final MLR####
load("../CSV/Output/Models/fullBayWQ8.rda")
Bays <- read.csv("../CSV/Output/Bays_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
BaysW_fill <- Bays[complete.cases(Bays), ] 
Bay_means <- read.csv("../CSV/Output/Summary/DBays_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) 

predicted_BayWQ <- data.frame(predW = predict(fullBayWQ8, BaysW_fill), Year = BaysW_fill$Year)
BaysWQCI <- predict(fullBayWQ8, interval = "confidence")
modelBaysWQ <- cbind(predicted_BayWQ, BaysWQCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predW, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predW = mean(predW, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
BayWQwCI  <- ggplot(Bay_means)+
  geom_point(aes(Year, AveBay, color = "Mean"))+
  geom_line(data = modelBaysWQ, aes(Year, predW, color = "Predict"))+
  geom_line(data = modelBaysWQ, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelBaysWQ, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.91))+ axistheme +
  ylab("log(Mean spat + 1)")+ 
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.25,1)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
BayWQwCI
#
####Fig 8 - OTB WQ final MLR####
load("../CSV/Output/Models/fullOTBWQ8.rda")
OTB <- read.csv("../CSV/Output/OTB_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
OTBW_fill <- OTB[complete.cases(OTB),]
OTB_means <- read.csv("../CSV/Output/Summary/DOTB_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

#
#Get CI values for MLR
predicted_OTBWQ <- data.frame(predW = predict(fullOTBWQ8, OTBW_fill), Year = OTBW_fill$Year)
OTBWQCI <- stats::predict(fullOTBWQ8, interval = "confidence")
modelOTBWQ <- cbind(predicted_OTBWQ, OTBWQCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predW, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predW = mean(predW, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
OTBwCI <- ggplot(filter(OTB_means, Year <= 2005))+
  geom_point(aes(Year, AveOTB, color = "Mean"))+
  geom_line(data = modelOTBWQ, aes(Year, predW, color = "Predict"))+
  geom_line(data = modelOTBWQ, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelOTBWQ, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.91))+ axistheme+
  ylab("log(Mean spat + 1)")+
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2006), breaks = seq(2002, 2005, by = 1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.25,0.75)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
####Fig 9 - TB WQ + Crabs MLR####
load("../CSV/Output/Models/fullBayB7.rda")
load("../CSV/Output/Models/fullBayS9.rda")

Bay_Blue <- read.csv("../CSV/Output/Bays_Blue.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Bay_Blue_fill <- Bay_Blue[complete.cases(Bay_Blue), ]
Bay_means <- read.csv("../CSV/Output/Summary/DBays_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#BayBluefit <- lm(formula = DlogBoth ~ Year + TempAll + SalAll + PerSecAll + DOpadj, data = Bay_Blue_fill)


Bay_Stone <- read.csv("../CSV/Output/Bays_Stone.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Bay_Stone_fill <- Bay_Stone[complete.cases(Bay_Stone), ]
Bay_means <- read.csv("../CSV/Output/Summary/DBays_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#BayStonefit <- lm(formula = DlogBoth ~ Year + SalAll + Phosadj, data = Bay_Stone_fill)
#Get CI values for MLR - 2002-2005
predicted_Bays_Blue <- data.frame(predB = predict(fullBayB7, Bay_Blue_fill), Year = Bay_Blue_fill$Year)
BaysBCI <- predict(fullBayB7, interval = "confidence")
modelBaysBlue <- cbind(predicted_Bays_Blue, BaysBCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predB, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predB = mean(predB, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Get CI values for MLR - 2006-2017
predicted_Bays_Stone <- data.frame(predS = predict(fullBayS9, Bay_Stone_fill), Year = Bay_Stone_fill$Year)
BaysSCI <- predict(fullBayS9, interval = "confidence")
modelBaysStone <- cbind(predicted_Bays_Stone, BaysSCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predS, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predS = mean(predS, na.rm = T), lwr = mean(lwr), upr = mean(upr))


#Data with selected regression - Years
Bay_Blue_model <- ggplot(filter(Bay_means, Year <= 2005))+
  geom_point(aes(Year, AveBay, color = "Mean"))+
  #geom_errorbar(aes(Year, ymin = AveBay-sdBay, ymax = AveBay+sdBay), color = "#999999", width = 0.2)+
  geom_line(data = modelBaysBlue, aes(Year, predB, color = "Predict"))+
  geom_line(data = modelBaysBlue, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelBaysBlue, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + ylab("log(Mean spat + 1)")+ axistheme+
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2006), breaks = seq(2002, 2005, by = 1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.5,1.75)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#annotate("text", x = 0.85, y = 0.75, label = equation(fullBay6), parse = T)
Bay_Stone_Model <- ggplot(filter(Bay_means, Year >= 2006))+
  geom_point(aes(Year, AveBay, color = "Mean"))+
  #geom_errorbar(aes(Year, ymin = AveBay-sdBay, ymax = AveBay+sdBay), color = "#999999", width = 0.2)+
  geom_line(data = modelBaysStone, aes(Year, predS, color = "Predict"))+
  geom_line(data = modelBaysStone, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelBaysStone, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + ylab("log(Mean spat + 1)")+ axistheme+
  scale_x_continuous(expand = c(0,0), limits = c(2005, 2018), breaks = seq(2006, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.05,0.32)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
##One figure: A and B
#
#One figure: All data
Bays_all_Model <- ggplot(Bay_means)+
  geom_point(aes(Year, AveBay, color = "Mean"))+
  #Blue
  geom_line(data = modelBaysBlue, aes(Year, predB, color = "Predict"))+
  geom_line(data = modelBaysBlue, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelBaysBlue, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  geom_text(geom = "text", label = "A", x = 2001.3, y = 1.7, size = 6)+
  #Stone 
  geom_line(data = modelBaysStone, aes(Year, predS, color = "Predict"))+
  geom_line(data = modelBaysStone, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelBaysStone, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  geom_text(geom = "text", label = "B", x = 2005.8, y = 1.7, size = 6)+
  #Formatting
  basetheme + legendON + theme(legend.position = c(0.899, 0.91))+ axistheme+
  ylab("log(Mean spat + 1)")+
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.5,1.75)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_vline(xintercept = 2005.5)+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
#
#
####Fig 10 - OTB WQ + Crabs MLR####
#Load model and data file
load("../CSV/Output/Models/fullOTBB11.rda")
OTB <- read.csv("../CSV/Output/OTB_Blue.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
OTB_fill <- OTB[complete.cases(OTB),]
OTB_means <- read.csv("../CSV/Output/Summary/DOTB_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

#
#Get CI values for MLR
predicted_OTB <- data.frame(fit = predict(fullOTBB11, OTB_fill), Year = OTB_fill$Year)
OTBCI <- stats::predict(fullOTBB11, OTB_fill, interval = "confidence")
modelOTB <- merge(predicted_OTB, OTBCI, by = "fit", all = T) %>% dplyr::select(Year, fit, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predO = mean(fit, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
OTBwCI <- ggplot(filter(OTB_means, Year <= 2005))+
  geom_point(aes(Year, AveOTB, color = "Mean"))+
  #geom_errorbar(aes(Year, ymin = AveOTB-sdOTB, ymax = AveOTB+sdOTB), color = "#999999", width = 0.1)+
  geom_line(data = modelOTB, aes(Year, predO, color = "Predict"))+
  geom_line(data = modelOTB, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelOTB, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.91))+ axistheme+
  ylab("log(Mean spat + 1)")+
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2006), breaks = seq(2002, 2005, by = 1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.25,0.75)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))

####Appendix 1 - WQ Station Map####
#Stations
perna_spat <- read.csv("../CSV/TB_recruit.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
perna_spat <- perna_spat %>% mutate(MonYr = paste(Month, Year, sep = "/"))  #Create MonYr column
stationsIvO <- perna_spat %>% select(Station, IvO, Quarter, Lat, Long, Bay, Year, MonYr)
#WQ stations - pull from all combined file, remove dulpicates, and filter as needed
WQsites <- read.csv("../CSV/WQ_site/WQ_all_MBS.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% 
  dplyr::select(Month:MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>%
  distinct(MonitoringLocationIdentifier, .keep_all = TRUE)
WQ_settlement <- WQsites %>% filter(Bay != "HB") %>% filter(LatitudeMeasure <= 27.859866)

#Use if creating map for WQ over settlement data
WQsites <- read.csv("../CSV/WQ_site/TB_stations_bays_2.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
###List of stations to exclude
excluding <- read.csv("../CSV/WQ_site/TB_excluded_WQ.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
removelist <- unlist(excluding)
#removelist <- "WQX-BAY4"
water_site <- WQsites[!grepl(paste0('*',removelist, "$", collapse = "|"), WQsites$MonitoringLocationIdentifier),]

bounding <- function(dflatcol, dflongcol, source, type, color){
  boundaries <<- data.frame(
    North <- max(dflatcol+0.03),
    South <- min(dflatcol-0.03),
    East <- max(dflongcol+0.03),
    West <- min(dflongcol-0.03))
  
  maparea <- get_map(location = c(West, South, East, North), source = source, maptype = type, color = color)
  return(maparea)
}

#map_area <- bounding(perna_spat$Lat, perna_spat$Long, "google", "terrain", "bw")
station_area <- bounding(WQsites$LatitudeMeasure, WQsites$LongitudeMeasure, "google", "satellite", "bw")
names(boundaries) <- c("North", "South", "East", "West")

#Add WQ stations to recruit stations for full WQ station map - APpendix
allWQ <- ggmap(station_area) +
  geom_point(data = WQsites, aes(x = LongitudeMeasure, y = LatitudeMeasure, color = Bay, shape = Bay), size = 3, alpha = 0.3)+
  geom_point(data = stationsIvO, aes(x = Long, y = Lat, color = Bay, shape = Bay), size = 3, alpha = 0.9)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_blank(), axis.text = element_text(color = "black"),
        legend.position = c(0.85, 0.33), #legend.title = element_blank(),
        legend.background = element_rect(fill = "#CCCCCC"),
        legend.key = element_rect(color = "transparent", fill = "transparent"),
        legend.text.align = 0, legend.box.just = "right")+
  scale_x_continuous(expand = c(0,0), breaks = c(-82.75, -82.65, -82.55, -82.45, -82.35), 
                     labels = c("-82.75°", "-82.65°", "-82.55°", "-85.45°", "-82.35°"))+
  scale_y_continuous(expand = c(0,0), breaks = c(27.55,27.65,27.75,27.85, 27.95), 
                     labels = c("27.55°","27.65°","27.75°","27.85°", "27.95°"))+
  scale_shape_manual(values = shapes)+
  scale_color_manual(values = BayColorH)+
  scalebar(dist = 2, dist_unit = "mi", transform = TRUE, model = "WGS84",  #Transform = true:data in decimals so needs transformed 
           st.size = 3, st.bottom = FALSE, st.dist = 0.06, #size = bar size, bottom = text location, dist = btw text and bar
           x.min = -82.38, x.max = -82.46,
           y.min = 27.53, y.max = 27.67)

#Add north arrow
allWQ %>% north2(x= 0.654, y = 0.16, scale = 0.06, symbol = 12)
#

