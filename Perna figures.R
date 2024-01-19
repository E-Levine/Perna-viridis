##Figures for Perna paper, final for publication

library(ggplot2)
library(ggmap)
library(ggsn)
library(dplyr)
library(ggpubr)
library(gridExtra)
#
#####
Sampling <- read.csv("../CSV/Sample_stations_Baywide.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#
####Themes and formatting####
basetheme <- theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12, face = "bold", color = "black"), axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), axis.text.y = element_text(size = 11, face = "bold"),
        panel.grid = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "black"))

axistheme <- theme(axis.ticks.length = unit(-0.15, "cm"), 
                   axis.text.x = element_text(color = "black", margin = unit(c(0.5, 0.5, 0, 0.5), "cm")), 
                   axis.text.y = element_text(color = "black", margin = unit(c(0, 0.5, 0, 0), "cm")))
TNR <- theme(text = element_text(family="Times New Roman"))

legends <- theme(legend.position = "right", legend.key = element_rect(color = "transparent", fill = "transparent"))
legendNone <- theme(legend.position = "none")
legendON <- theme(legend.position = c(0.897, 0.899), legend.title = element_blank(),
                  legend.background = element_blank(),#rect(linetype = 1, size = 0.5, color = "black"),
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
Season_names <- c("1" = "Winter", "2" = "Spring", "3" = "Summer", "4" = "Fall")
#Crab names
CrabSp <- c("Blue" = "C. sapidus", "Stone" = "M. mercenaria")
Crabcolor <- c("#000000", "#999999")
names(Crabcolor) <- c("Blue", "Stone")
#Shapes to station type (helper) color to IvO
shapesAll <- c(16, 22, 17, 15)
names(shapesAll) <- levels(as.factor(Sampling$Helper))
#Color to IvO
IvOColor <- c("#000000", "#666666", "#000000", "#333333")
names(IvOColor) <- c("I", "O", "N", "A")
#
Helpers <- c("Bycatch" = "Bycatch", "Crabs" = expression(italic("C. sapidus")*" Bycatch"), 
             "S-I" = "Settlement - C", "S-O" = "Settlement - L")
InandOut <- c("I" = "C", "O" = "L")
#
#remove top and left box lines
#
#

####Fig 1 - Station Maps####
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

#df of stations - settlement v adults
Sampling <- read.csv("../CSV/Sample_stations_Baywide.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Sampling$Helper <- as.factor(Sampling$Helper)
str(Sampling)
#
Sample_area <- bounding(Sampling$Lat, Sampling$Long, "google", "terrain", "bw")

#Perna
Perna <- ggmap(Sample_area)+
  geom_point(data = (Sampling %>% filter(Species != "BC")), aes(x = Long, y = Lat, shape = Helper), size = 3, alpha = 0.4)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_blank(), axis.text = element_text(color = "black", face = "bold"),
        #legend.position = "none")+
        legend.position = c(0.79, 0.30), #legend.title = element_blank(),
        legend.background = element_rect(fill = "#CCCCCC"),
        legend.key = element_rect(color = "transparent", fill = "transparent"),
        legend.text.align = 0, legend.box.just = "right", legend.title = element_blank())+
  scale_x_continuous(expand = c(0,0), breaks = c(-82.75, -82.65, -82.55, -82.45, -82.35), 
                     labels = c("-82.75°", "-82.65°", "-82.55°", "-85.45°", "-82.35°"))+
  scale_y_continuous(expand = c(0,0), breaks = c(27.50, 27.60,27.70,27.80,27.90, 28.00), 
                     labels = c("27.50°", "27.60°","27.70°","27.80°","27.90°", "28.00°"))+
  #scale_color_manual(name = "Type", values = IvOColor,breaks = c("I", "O"), labels = InandOut, guide = FALSE)+
  scale_shape_manual(name = "", values = shapesAll, labels = Helpers)+
  scalebar(dist = 2, dist_unit = "mi", transform = TRUE, model = "WGS84",  #Transform = true:data in decimals so needs transformed 
           st.size = 4, st.bottom = FALSE, st.dist = 0.06, #size = bar size, bottom = text location, dist = btw text and bar
           x.min = -82.33, x.max = -82.55,
           y.min = 27.45, y.max = 27.65)+
  axistheme + theme(axis.text = element_text(size = 10))+ TNR

#Crabs
Crabs <- ggmap(Sample_area)+
  geom_point(data = (Sampling %>% filter(Species == "BC")), aes(x = Long, y = Lat, shape = Helper), color = "#666666", size = 2, alpha = 0.6)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_blank(), axis.text = element_text(color = "black", face= "bold"),
        #legend.position = "none")+
        legend.position = c(0.77, 0.30), #legend.title = element_blank(),
        legend.background = element_rect(fill = "#CCCCCC"),
        legend.key = element_rect(color = "transparent", fill = "transparent"),
        legend.text.align = 0, legend.box.just = "right", legend.title = element_blank())+
  scale_x_continuous(expand = c(0,0), breaks = c(-82.75, -82.65, -82.55, -82.45, -82.35), 
                     labels = c("-82.75°", "-82.65°", "-82.55°", "-85.45°", "-82.35°"))+
  scale_y_continuous(expand = c(0,0), breaks = c(27.50, 27.60,27.70,27.80,27.90, 28.00), 
                     labels = c("27.50°", "27.60°","27.70°","27.80°","27.90°", "28.00°"))+
  scale_shape_manual(name = "", values = shapesAll, labels = Helpers)+
  scalebar(dist = 2, dist_unit = "mi", transform = TRUE, model = "WGS84",  #Transform = true:data in decimals so needs transformed 
           st.size = 4, st.bottom = FALSE, st.dist = 0.06, #size = bar size, bottom = text location, dist = btw text and bar
           x.min = -82.33, x.max = -82.55,
           y.min = 27.45, y.max = 27.65)+
  axistheme + theme(axis.text = element_text(size = 10))+ TNR+
  ggspatial::annotation_north_arrow(which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"))


Combined <- ggarrange(Perna, Crabs, nrow = 1, labels = c("A", "B"), hjust = -5, vjust = 1.9) 
Combined
#
tiff(file = "../Figures/Paper/TIFF/Fig1_Stations.tiff", res = 300, compression = "lzw")
ggsave(path = "../Figures/Paper", filename = "Fig1_Stations map_2021 09.tiff",dpi=500)
#

####Fig 2 - WQ Station Map####
#Stations for WQ samples
Sampling_WQ <- read.csv("../CSV/Sample_stations_2.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% filter(Species == "GM")
Sampling_WQ$Bay <- as.factor(Sampling_WQ$Bay)
WQ_stations <- read.csv("../CSV/WQ_site/WQ_all_MBS.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% 
  dplyr::select(Bay, MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>%
  distinct(MonitoringLocationIdentifier, .keep_all = TRUE) %>%
  rename(Station = MonitoringLocationIdentifier, Lat = LatitudeMeasure, Long = LongitudeMeasure)
All_WQ_Stations <- rbind(dplyr::select(Sampling_WQ, c("Bay", "Station", "Lat", "Long")), WQ_stations)

bounding <- function(dflatcol, dflongcol, source, type, color){
  boundaries <<- data.frame(
    North <- max(dflatcol+0.03),
    South <- min(dflatcol-0.03),
    East <- max(dflongcol+0.03),
    West <- min(dflongcol-0.03))
  
  maparea <- get_map(location = c(West, South, East, North), source = source, maptype = type, color = color)
  return(maparea)
}

station_area <- bounding(All_WQ_Stations$Lat, All_WQ_Stations$Long, "google", "satellite", "bw")
names(boundaries) <- c("North", "South", "East", "West")
#
##All WQ stations, no Bays
allWQ <- ggmap(station_area) +
  geom_point(data = All_WQ_Stations, aes(x = Long, y = Lat), size = 3, alpha = 0.3)+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.title = element_blank(), axis.text = element_text(color = "black", face = "bold"),
        legend.position = c(0.85, 0.33), #legend.title = element_blank(),
        legend.background = element_rect(fill = "#CCCCCC"),
        legend.key = element_rect(color = "transparent", fill = "transparent"),
        legend.text.align = 0, legend.box.just = "right")+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))+
  scale_x_continuous(expand = c(0,0), breaks = c(-82.75, -82.65, -82.55, -82.45, -82.35), 
                     labels = c("-82.75°", "-82.65°", "-82.55°", "-85.45°", "-82.35°"))+
  scale_y_continuous(expand = c(0,0), breaks = c(27.55,27.65,27.75,27.85, 27.95), 
                     labels = c("27.55°","27.65°","27.75°","27.85°", "27.95°"))+
  scalebar(dist = 2, dist_unit = "mi", transform = TRUE, model = "WGS84",  #Transform = true:data in decimals so needs transformed 
           st.size = 3, st.bottom = FALSE, st.dist = 0.06, #size = bar size, bottom = text location, dist = btw text and bar
           x.min = -82.38, x.max = -82.46,
           y.min = 27.53, y.max = 27.67)+
  axistheme + theme(axis.text = element_text(size = 10))+ TNR+
  ggspatial::annotation_north_arrow(which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), 
                                    location = "br", pad_x = unit(1.38, "cm"), pad_y = unit(1.45, "cm"))
#
allWQ
#
ggsave(path = "../Figures/Paper", filename = "Fig2_WQ stations_2021 09.tiff",dpi=500)
#
####Fig 3 - Temp and Sal####
SetWQ <- read.csv("../CSV/Output/Summary/Baywide/Sett_WQsummary.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
ByWQ <- read.csv("../CSV/Output/Summary/Baywide/By_WQsummary.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
#MonYr
SetWQ$MonYr <- zoo::as.yearmon(SetWQ$MonYr, format = "%m/%Y")
ByWQ$MonYr <- zoo::as.yearmon(ByWQ$MonYr, format = "%m/%Y")
#
###Separate by areas
#
SetTemp <- SetWQ %>% ggplot(aes(x = MonYr, y = T_mean))+
  geom_line(size = 1)+
  basetheme+ axistheme+
  ylab("Temperature (°C)")+
  scale_x_continuous(limits = c(2001.5, 2017.5), breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0, 40), expand = c(0,0))+
  geom_hline(yintercept = 26, linetype = "dashed")+
  geom_hline(yintercept = 32, linetype = "dashed")+
  geom_rect(aes(ymin = 26, ymax = 32, xmin = -Inf, xmax = Inf),
            fill = "#999999", alpha = 0.006)+
  geom_line(data = SetWQ, aes(x = MonYr, y = T_min), linetype = "dashed")+
  geom_line(data = SetWQ, aes(x = MonYr, y = T_max), linetype = "dashed")+
  TNR
SetSal <- SetWQ %>% ggplot(aes(x = MonYr, y = S_mean))+
  geom_line(size = 1)+
  basetheme+ axistheme+
  ylab("Salinity")+
  scale_x_continuous(limits = c(2001.5, 2017.5), breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0, 45), expand = c(0,0))+
  geom_hline(yintercept = 26, linetype = "dashed")+
  geom_hline(yintercept = 32, linetype = "dashed")+
  geom_rect(aes(ymin = 26, ymax = 32, xmin = -Inf, xmax = Inf),
            fill = "#999999", alpha = 0.006)+
  geom_line(data = SetWQ, aes(x = MonYr, y = S_min), linetype = "dashed")+
  geom_line(data = SetWQ, aes(x = MonYr, y = S_max), linetype = "dashed")+
  TNR
#
ByTemp <- ByWQ %>% ggplot(aes(x = MonYr, y = T_mean))+
  geom_line(size = 1)+
  basetheme+ axistheme+
  ylab("Temperature (°C)")+
  scale_x_continuous(limits = c(2001.5, 2017.5), breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0, 40), expand = c(0,0))+
  geom_hline(yintercept = 26, linetype = "dashed")+
  geom_hline(yintercept = 32, linetype = "dashed")+
  geom_rect(aes(ymin = 26, ymax = 32, xmin = -Inf, xmax = Inf),
            fill = "#999999", alpha = 0.006)+
  geom_line(data = ByWQ, aes(x = MonYr, y = T_min), linetype = "dashed")+
  geom_line(data = ByWQ, aes(x = MonYr, y = T_max), linetype = "dashed")+
  TNR
BySal <- ByWQ %>% ggplot(aes(x = MonYr, y = S_mean))+
  geom_line(size = 1)+
  basetheme+ axistheme+
  ylab("Salinity")+
  scale_x_continuous(limits = c(2001.5, 2017.5), breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0, 45), expand = c(0,0))+
  geom_hline(yintercept = 26, linetype = "dashed")+
  geom_hline(yintercept = 32, linetype = "dashed")+
  geom_rect(aes(ymin = 26, ymax = 32, xmin = -Inf, xmax = Inf),
            fill = "#999999", alpha = 0.006)+
  geom_line(data = ByWQ, aes(x = MonYr, y = S_min), linetype = "dashed")+
  geom_line(data = ByWQ, aes(x = MonYr, y = S_max), linetype = "dashed")+
  TNR
#
#Combine with 1 axis title both x and y
WQ_nolab <- ggarrange(SetTemp + rremove("y.title"), ByTemp + rremove("y.title"),
                      SetSal + rremove("y.title"), BySal + rremove("y.title"),
                      labels = c("A", "B", "C", "D"), hjust = -3.3, vjust = 1.65, nrow = 4, align = "v")
WQ_no <- annotate_figure(WQ_nolab, left = text_grob("Temperature (°C)", face = "bold", rot = 90, hjust = -0.70, size = 11))
annotate_figure(WQ_no, left = text_grob("Salinity", face = "bold", rot = 90, vjust = 2.4, hjust = 3, size = 11))
#
ggsave(path = "../Figures/Paper", filename = "Fig3_Temp_Sal_2021 09.tiff",dpi=1000)

#
####Fig 4 - Season Averages####
#Settlement
Sets <- read.csv("../CSV/Output/Summary/Baywide/Spat_seasons_letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Sets$Quarter <- as.factor(Sets$Quarter)
Bys <- read.csv("../CSV/Output/Summary/Baywide/Bycatch_seasons.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Bys$Quarter <- as.factor(Bys$Quarter)

###Settlement
#Use letters to differentiate groups
Sett_letters <- ggplot(Sets, aes(Quarter, mean, fill = "black"))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Quarter, ymin = mean, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  geom_text(aes(label = Letters, y = upper), size = 5, vjust = -0.35)+
  basetheme + legendNone + axistheme+
  theme(axis.title.y = element_text(size = 11))+
  ylab("Average Settlement (spat/collector/month)")+
  scale_x_discrete(labels = Season_names)+ 
  scale_y_continuous(expand = c(0,0), limits = c(0,15)) + TNR
#
###Bycatch
By_seasons <- ggplot(Bys, aes(Quarter, mean, fill = "black"))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Quarter, ymin = mean, ymax = upper), width = 0.2)+
  scale_fill_grey()+
  basetheme + legendNone+ axistheme +
  theme(axis.title.y = element_text(size = 11))+
  ylab("Average Bycatch (pounds/trawl/month)")+
  scale_x_discrete(labels = Season_names)+ 
  scale_y_continuous(expand = c(0,0), limits = c(0,10))+ 
  axistheme + TNR
#

###Combined season figure
BandS <- ggpubr::ggarrange(Sett_letters, By_seasons, labels = c("A", "B"), nrow = 1, ncol = 2, hjust = -5.3, vjust = 1.55, align = "v")
BandS
#
ggsave(path = "../Figures/Paper", filename = "Fig4_Seasons_2021 09.tiff",dpi=500)
#
####Fig 5 - Annual settlement####
####Years
#load files
Settlement <- read.csv("../CSV/Output/Summary/Baywide/Spat_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Bycatch <- read.csv("../CSV/Output/Summary/Baywide/Adults_ori_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

#Settlement
Set_yr <- ggplot(Settlement, aes(Year, mean))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = (mean + sd)), width = 0.2)+
  geom_text(aes(label = Letters, y = (mean + sd)), size = 4, vjust = -0.35)+
  scale_fill_grey()+
  basetheme + legendNone + axistheme+  theme(axis.title.y = element_text(size = 11))+ 
  ylab(expression(atop(bold("Average Settlement"), paste(bold("(spat/collector/month)")))))+
  scale_x_continuous(expand = c(0.0,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) + axistheme + TNR

#Bycatch
By_yr <- ggplot(Bycatch, aes(Year, mean))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = (mean+sd)), width = 0.2)+
  geom_text(aes(label = Letters, y = (mean + sd)), size = 4, vjust = -0.35)+
  scale_fill_grey()+
  basetheme + legendNone+ axistheme+ theme(axis.title.y = element_text(size = 11, margin = margin(r = 8)))+
  ylab(expression(atop(bold("Average Bycatch"),paste(bold("(pounds/trawl/month)")))))+
  scale_x_continuous(expand = c(0.0,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) + axistheme + TNR  

##Combine shaded bars into 1 figure
Years_combined <- ggarrange(Set_yr, By_yr, labels = c("A", "B"), hjust = -6.75, nrow = 2)
Years_combined
#
ggsave(path = "../Figures/Paper", filename = "Fig5_Annual_2021 09.tiff",dpi=500)
#

#Combine with 1 axis title both x and y
Years_nolab <- ggarrange(Bothbar + rremove("y.title") + theme(axis.text.x = element_text(size = 0.5, color = "white")), OTBbar + rremove("y.title") + theme(axis.text.x = element_text(size = 0.5, color = "white")),
                         Lowerbar + rremove("y.title") + theme(axis.text.x = element_text(size = 0.5, color = "white")), Upperbar + rremove("y.title"),
                         labels = c("A", "B", "C", "D"), hjust = -3.6, vjust = 1.65, nrow = 4, align = "v")
Years_no <- annotate_figure(Years_nolab, left = text_grob("Average Settlement (spat/collector/month)", face = "bold", rot = 90, hjust = -0.1, size = 11))
annotate_figure(Years_no, left = text_grob("Average Bycatch (pounds/trawl/month)", face = "bold", rot = 90, vjust = 2.1, hjust = 1.05, size = 11))
#
####Fig 6 - Crab FDM DATA####
SFDM <- read.csv("../CSV/Summary/SCLanding.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
SFDM_total <- SFDM %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
SFDM_total$Spp <- "Stone"

Stone_FDM <- SFDM_total %>% ggplot(aes(Year, Total))+
  geom_bar(stat = "identity", size = 2, fill = "#999999")+
  #scale_fill_manual(values = c("#999999"))+
  basetheme + legends + ylab("Total Pounds")+ axistheme+
  #theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  theme(legend.position = "none")+ 
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,500000), labels = scales::comma) + axistheme + TNR

BFDM <- read.csv("../CSV/Summary/BCLanding.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
BFDM_total <- BFDM %>% group_by(Year) %>% summarise(Total = sum(Total, na.rm = T))
BFDM_total$Spp <- "Blue"

Blue_FDM <- BFDM_total %>% ggplot(aes(Year, Total))+
  geom_bar(stat = "identity", size = 2, fill = "#000000")+
  #scale_fill_grey(start = 0, end = 0.6, labels = CrabSp)+
  basetheme + legends + ylab("Total Pounds")+ axistheme+
  #theme(legend.title = element_blank(), legend.text = element_text(face = "italic"))+
  theme(legend.position = "none")+ 
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1250000), labels = scales::comma) + axistheme + TNR

Both_FDM <- ggarrange(Blue_FDM, Stone_FDM, labels = c("A", "B"), nrow = 2, align = "v")
Both_FDM
#1 y-axis
FDM_nolab <- ggarrange(Blue_FDM + rremove("y.title"), Stone_FDM + rremove("y.title"), 
                       labels = c("A", "B"), hjust = -6.75, vjust = 1.75, nrow = 2, align = "v")
annotate_figure(FDM_nolab, left = text_grob("Total Pounds", face = "bold", rot = 90))
#
ggsave(path = "../Figures/Paper", filename = "Fig6_Landings_2021 09.tiff",dpi=500)
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
####Fig 7 - Crab CPUE####
#Trawl crabs
FIM <- read.csv("../CSV/Output/Summary/Baywide/FIM_crab_letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
FIM_yrs <- FIM %>% ggplot(aes(Year, mean))+ 
  geom_bar(stat = "identity", size = 2, fill = "black")+
  geom_errorbar(aes(Year, ymin = mean, ymax = upper), width = 0.2)+
  basetheme + legends + ylab("Catch per unit effort (crabs/trawl/month)")+ axistheme+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,35))
FIM_yrs
#
ggsave(path = "../Figures/Paper", filename = "Fig7_TrawlCPUE_2021 09.tiff",dpi=500)
#
####Fig 8 - Settlement final MLR####
load(file = "../CSV/Output/Baywide/Models/fullSett8_sep.rda")
Settle <- read.csv("../CSV/Output/Baywide/Settlement_data.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Settle_fill <- Settle[complete.cases(Settle), ] 
Settle_means <- Settle %>% group_by(Year) %>% summarise(ave = mean(SpatAdj, na.rm = T))
  
predicted_Settle <- data.frame(predS = predict(fullSett8, Settle_fill), Year = Settle_fill$Year)
SettleCI <- predict(fullSett8, interval = "confidence")
modelSettle <- cbind(predicted_Settle, SettleCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predS, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predS = mean(predS, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
SettlewCI  <- ggplot(Settle_means)+
  geom_point(aes(Year, ave, color = "Mean"))+
  geom_line(data = modelSettle, aes(Year, predS, color = "Predict"))+
  geom_line(data = modelSettle, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelSettle, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.8), legend.text = element_text(size = 11), legend.margin = margin(c(2,3,2,2)))+ axistheme +
  ylab("log(Mean spat + 1)")+ 
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.25,0.8)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
SettlewCI
#
ggsave(path = "../Figures/Paper", filename = "Fig8_Settlement model_2021 09.tiff",dpi=1000)
#
####Fig 9 - Bycatch final MLR####
load("../CSV/Output/Baywide/Models/fullBy8_sep.rda")
Bycatch <- read.csv("../CSV/Output/Baywide/Bycatch_data.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Bycatch_fill <- Bycatch[complete.cases(Bycatch),]
Bycatch_means <- Bycatch %>% group_by(Year) %>% summarise(ave = mean(PV, na.rm = T))

#
#Get CI values for MLR
predicted_Bycatch <- data.frame(predB = predict(fullBy8, Bycatch_fill), Year = Bycatch_fill$Year)
BycatchCI <- stats::predict(fullBy8, interval = "confidence")
modelBycatch <- cbind(predicted_Bycatch, BycatchCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predB, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predB = mean(predB, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
BycatchwCI <- ggplot(Bycatch_means)+
  geom_point(aes(Year, ave, color = "Mean"))+
  geom_line(data = modelBycatch, aes(Year, predB, color = "Predict"))+
  geom_line(data = modelBycatch, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelBycatch, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.8), legend.text = element_text(size = 11), legend.margin = margin(c(2,3,2,2)))+ axistheme +
  ylab("log(Mean Bycatch + 1)")+ 
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.25,0.75)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
BycatchwCI
#
ggsave(path = "../Figures/Paper", filename = "Fig9_Bycatch model_2021 09.tiff",dpi=1200)
#
####Appendix 1 - Regional Figures####
###Fig 1 - Annual amounts
#load files
Bothdf <- read.csv("../CSV/Output/Summary/Both_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
OTBdf <- read.csv("../CSV/Output/Summary/OTB_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Lowerdf <- read.csv("../CSV/Output/Summary/Lower_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% mutate(upper = mean + sd)
Upperdf <- read.csv("../CSV/Output/Summary/Upper_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% mutate(upper = mean + sd)

#Both
Bothbar <- ggplot(Bothdf, aes(Year, mean))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = upper), width = 0.2)+
  geom_text(aes(label = Letters, y = upper), size = 4, vjust = -0.35)+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.963, 0.8), legend.text = element_text(size = 8), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"))+ axistheme+
  ylab("Average Settlement (spat/collector/month)")+
  scale_x_continuous(expand = c(0.0,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) 

#OTB
OTBbar <- ggplot(OTBdf, aes(Year, mean))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = upper), width = 0.2)+
  geom_text(aes(label = Letters, y = upper), size = 4, vjust = -0.35)+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.963, 0.8), legend.text = element_text(size = 8), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"))+ axistheme+
  ylab("Average Settlement (spat/collector/month)")+
  scale_x_continuous(expand = c(0.0,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,30)) 

#Lower
Lowerbar <- ggplot(Lowerdf, aes(Year, mean))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = upper), width = 0.2)+
  geom_text(aes(label = Letters, y = upper), size = 4, vjust = -0.35)+
  scale_fill_grey()+
  basetheme + legendNone+ axistheme+ theme(axis.title.y = element_text(margin = margin(r = 8)))+
  ylab("Average Bycatch (pounds/trawl/month)")+
  scale_x_continuous(expand = c(0.0,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1.5)) 
#scale_y_continuous(expand = c(0,0), limits = c(0,25)) 

#Upper
Upperbar <- ggplot(Upperdf, aes(Year, mean))+
  geom_bar(stat = "identity", size = 2)+
  geom_errorbar(aes(Year, ymin = mean, ymax = upper), width = 0.2)+
  geom_text(aes(label = Letters, y = upper), size = 4, vjust = -0.35)+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.963, 0.75), legend.text = element_text(size = 8), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"))+ axistheme+
  ylab("Average Bycatch (pounds/trawl/month)")+
  scale_x_continuous(expand = c(0.0,0.01), limits = c(2001, 2018), breaks = seq(2002, 20017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,15)) 
#
##Combine shaded bars into 1 figure
Years_combined <- ggarrange(Bothbar + rremove("y.title"), OTBbar + rremove("y.title"), 
                            Lowerbar + rremove("y.title"), Upperbar + rremove("y.title"), 
                            labels = c("A", "B", "C", "D"), hjust = -3.6, vjust = 1.65, nrow = 4, align = "v")
Years_combined
#
annotate_figure(Years_combined, left = text_grob(
  expression(atop(bold("             Average Bycatch                                         Average Settlement  "),
                  paste(bold("       (pounds/trawl/month)                                    (spat/collector/month)")))), rot = 90, size = 10))

#
ggsave(path = "../Figures/Paper", filename = "A1_Annual_2021 09.tiff",dpi=800)
#
#
#
###Fig 2 - MTB + LTB model
load(file = "../CSV/Output/Models/fullBayC7_sep.rda")
Bays <- read.csv("../CSV/Output/Bays_crabs_sep.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Bays_fill <- Bays[complete.cases(Bays), ] 
Bay_means <- read.csv("../CSV/Output/Summary/DBays_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) 

predicted_Bay <- data.frame(predC = predict(fullBayC7, Bays_fill), Year = Bays_fill$Year)
BaysCI <- predict(fullBayC7, interval = "confidence")
modelBays <- cbind(predicted_Bay, BaysCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predC, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predC = mean(predC, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
BaywCI  <- ggplot(Bay_means)+
  geom_point(aes(Year, AveBay, color = "Mean"))+
  geom_line(data = modelBays, aes(Year, predC, color = "Predict"))+
  geom_line(data = modelBays, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelBays, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.8), legend.text = element_text(size = 11), legend.margin = margin(c(2,3,2,2)))+ axistheme +
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
BaywCI
#
ggsave(path = "../Figures/Paper", filename = "A2_TB settlement_2021 09.tiff",dpi=1000)
#
#
###Fig 3 - OTB model
load("../CSV/Output/Models/fullOTBC10_sep.rda")
OTB <- read.csv("../CSV/Output/OTB_crabs_sep.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
OTB_fill <- OTB[complete.cases(OTB),]
OTB_means <- read.csv("../CSV/Output/Summary/DOTB_means.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

#
#Get CI values for MLR
predicted_OTB <- data.frame(predC = predict(fullOTBC10, OTB_fill), Year = OTB_fill$Year)
OTBCI <- stats::predict(fullOTBC10, interval = "confidence")
modelOTB <- cbind(predicted_OTB, OTBCI) %>% dplyr::select(-fit) %>% dplyr::select(Year, predC, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predC = mean(predC, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
OTBwCI <- ggplot(filter(OTB_means, Year <= 2005))+
  geom_point(aes(Year, AveOTB, color = "Mean"))+
  geom_line(data = modelOTB, aes(Year, predC, color = "Predict"))+
  geom_line(data = modelOTB, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelOTB, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.8), legend.text = element_text(size = 11), legend.margin = margin(c(2,3,2,2)))+ axistheme+
  ylab("log(Mean spat + 1)")+
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2006), breaks = seq(2002, 2005, by = 1)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.35,0.75)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
OTBwCI
#
ggsave(path = "../Figures/Paper", filename = "A3_OTB settlement_2021 09.tiff",dpi=1000)
#
#
###Fig 4 - Lower model
#Load model and data file
load("../CSV/Output/Models/fullLowC10_sep.rda")
LowC <- read.csv("../CSV/Output/Lower_Crabs_sep.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
LowC_fill <- LowC[complete.cases(LowC),]
Low_means <- read.csv("../CSV/Output/Summary/Lower_Logoriginal_meansSD.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

#
#Get CI values for MLR
predicted_LowC <- data.frame(fit = predict(fullLowC10, LowC_fill), Year = LowC_fill$Year)
LowCCI <- stats::predict(fullLowC10, LowC_fill, interval = "confidence")
modelLowC <- merge(predicted_LowC, LowCCI, by = "fit", all = T) %>% dplyr::select(Year, fit, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predC = mean(fit, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
LowCwCI <- ggplot(Low_means)+
  geom_point(aes(Year, mean, color = "Mean"))+
  geom_line(data = modelLowC, aes(Year, predC, color = "Predict"))+
  geom_line(data = modelLowC, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelLowC, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.8), legend.text = element_text(size = 11), legend.margin = margin(c(2,3,2,2)))+ axistheme+
  ylab("log(Mean bycatch + 1)")+
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.030,0.08)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
LowCwCI
#
ggsave(path = "../Figures/Paper", filename = "A4_Lower bycatch_2021 09.tiff",dpi=1000)
#
#
###Fig 5 - Upper model
#Load model and data file
load("../CSV/Output/Models/fullUppC7_sep.rda")
UppC <- read.csv("../CSV/Output/Upper_Crabs_sep.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
UppC_fill <- UppC[complete.cases(UppC),]
Upp_means <- read.csv("../CSV/Output/Summary/Upper_Logoriginal_meansSD.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))

#
#Get CI values for MLR
predicted_UppC <- data.frame(fit = predict(fullUppC7, UppC_fill), Year = UppC_fill$Year)
UppCCI <- stats::predict(fullUppC7, UppC_fill, interval = "confidence")
modelUppC <- merge(predicted_UppC, UppCCI, by = "fit", all = T) %>% dplyr::select(Year, fit, everything()) %>%
  group_by(Year) %>% dplyr::summarise(predC = mean(fit, na.rm = T), lwr = mean(lwr), upr = mean(upr))

#Data with selected regression - Years
UppCwCI <- ggplot(Upp_means)+
  geom_point(aes(Year, mean, color = "Mean"))+
  geom_line(data = modelUppC, aes(Year, predC, color = "Predict"))+
  geom_line(data = modelUppC, aes(Year, lwr, color = "95% CI"), linetype = "dashed")+
  geom_line(data = modelUppC, aes(Year, upr, color = "95% CI"), linetype = "dashed")+
  basetheme + legendON + theme(legend.position = c(0.899, 0.8), legend.text = element_text(size = 11), legend.margin = margin(c(2,3,2,2)))+ axistheme+
  ylab("log(Mean bycatch + 1)")+
  scale_x_continuous(expand = c(0,0), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.35,1.2)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_manual(name = "",
                     breaks = c("Mean", "Predict", "95% CI"),
                     values = c("#000000", "#FF0000", "#999999"),
                     labels = c("Observed Mean", "Predicted Mean", "95% confidence limit"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "dashed"),
                       shape = c(19, NA, NA))))
UppCwCI
#
ggsave(path = "../Figures/Paper", filename = "A5_Upper bycatch_2021 09.tiff",dpi=1000)
#

#####WQ parameters####
WQ <- read.csv("../CSV/WQ_site/WQ_all_MBS.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% 
  rename(Station = MonitoringLocationIdentifier, Lat = LatitudeMeasure, Long = LongitudeMeasure) %>% 
  dplyr::select(Month:Type, ResultMeasureValue) %>%
  mutate(MonYr = zoo::as.yearmon(paste(Month, Year, sep = "/"), format = "%m/%Y"),
         Quarter = ifelse(Month <= 3, 1, 
                          ifelse((Month >= 4)&(Month <= 6), 2,
                                 ifelse(Month >= 10, 3, 4))))
params <- WQ %>% group_by(Month, Year, MonYr, Bay, Type) %>% summarise(count = n(), 
                                                                Ave = mean(ResultMeasureValue, na.rm = T),
                                                                Min = min(ResultMeasureValue, na.rm = T),
                                                                Max = max(ResultMeasureValue, na.rm = T))
temp <- params %>% filter(Type == "N") 
temp %>%
  ggplot(aes(MonYr, Ave))+
  geom_point()+
  facet_grid(Bay~.)+
  geom_smooth(se = FALSE)
WQ %>% filter(Type == "N") %>% group_by(Year, Bay, Type) %>% summarise(count = n(), 
                                                             Ave = mean(ResultMeasureValue, na.rm = T),
                                                             Min = min(ResultMeasureValue, na.rm = T),
                                                             Max = max(ResultMeasureValue, na.rm = T)) %>%
  ggplot(aes(Year, Ave))+
  geom_point()+
  facet_grid(Bay~.)#+
  #geom_smooth(se = FALSE)
WQ %>% filter(Type == "N") %>% group_by(Year, Type) %>% summarise(count = n(), 
                                                                       Ave = mean(ResultMeasureValue, na.rm = T),
                                                                       Min = min(ResultMeasureValue, na.rm = T),
                                                                       Max = max(ResultMeasureValue, na.rm = T)) %>%
  ggplot(aes(Year, Ave))+
  geom_point()+basetheme
#
Settlement <- read.csv("../CSV/Output/Summary/Baywide/Spat_original_meansSD_Letters.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z"))
Settlement <- Settlement %>% mutate(Year1 = Year +1)
#annual settlment
ggplot(Settlement, aes(Year, mean, fill = Letters))+
  #geom_bar(stat = "identity", size = 2)+
  #geom_errorbar(aes(Year, ymin = mean, ymax = (mean + sd)), width = 0.2)+
  geom_point()+
  scale_fill_grey()+
  basetheme + legendON + theme(legend.position = c(0.963, 0.8), legend.text = element_text(size = 10), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"), axis.title.y = element_text(size = 10))+ axistheme+
  ylab("Average Settlement (spat/collector/month)")+
  scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) 
#annual settlement and N
ggarrange(WQ %>% filter(Type == "N") %>% group_by(Year, Type) %>% summarise(count = n(), 
                                                                            Ave = mean(ResultMeasureValue, na.rm = T),
                                                                            Min = min(ResultMeasureValue, na.rm = T),
                                                                            Max = max(ResultMeasureValue, na.rm = T)) %>%
            ggplot(aes(Year, Ave))+
            geom_point()+
            scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)),
          ggplot(Settlement, aes(Year, mean, fill = Letters))+
            #geom_bar(stat = "identity", size = 2)+
            #geom_errorbar(aes(Year, ymin = mean, ymax = (mean + sd)), width = 0.2)+
            geom_point()+
            scale_fill_grey()+ #basetheme + 
            legendON + theme(legend.position = c(0.963, 0.8), legend.text = element_text(size = 10), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"), axis.title.y = element_text(size = 10))+ axistheme+
            ylab("Average Settlement (spat/collector/month)")+
            scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
            scale_y_continuous(expand = c(0,0), limits = c(0,20)),
          ggplot(Settlement, aes(Year+1, mean, fill = Letters))+
            #geom_bar(stat = "identity", size = 2)+
            #geom_errorbar(aes(Year, ymin = mean, ymax = (mean + sd)), width = 0.2)+
            geom_point()+
            scale_fill_grey()+ #basetheme + 
            legendON + theme(legend.position = c(0.963, 0.8), legend.text = element_text(size = 10), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"), axis.title.y = element_text(size = 10))+ axistheme+
            ylab("Average Settlement (spat/collector/month)")+
            scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
            scale_y_continuous(expand = c(0,0), limits = c(0,20)), nrow = 3)


perna_spat <- read.csv("../CSV/TB_recruit.csv", stringsAsFactors = FALSE, na.strings = c("NA", " ", "", "Z")) %>% dplyr::select(Month, Year, Spat) %>%
  mutate(MonYr = zoo::as.yearmon(paste(Month, Year, sep = "/"), format = "%m/%Y"),
         Quarter = ifelse(Month <= 3, 1, 
                          ifelse((Month >= 4)&(Month <= 6), 2,
                                 ifelse(Month >= 10, 3, 4)))) %>% group_by(Year, Quarter, MonYr) %>%
  summarise(Ave = mean(Spat, na.rm = T))
  

ggarrange(WQ %>% filter(Type == "N") %>% group_by(MonYr) %>% summarise(Ave = mean(ResultMeasureValue, na.rm = T)) %>%
            ggplot(aes(MonYr, Ave))+
            geom_point()+
            scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)),
          ggplot(perna_spat, aes(MonYr, Ave))+
            #geom_bar(stat = "identity", size = 2)+
            #geom_errorbar(aes(Year, ymin = mean, ymax = (mean + sd)), width = 0.2)+
            geom_point()+
            #basetheme + 
            legendON + theme(legend.position = c(0.963, 0.8), legend.text = element_text(size = 10), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"), axis.title.y = element_text(size = 10))+ axistheme+
            ylab("Average Settlement (spat/collector/month)")+
            scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)), nrow = 2)

ggarrange(perna_spat %>% group_by(Year) %>% summarise(Ave = mean(Ave, na.rm = T)) %>%
            ggplot(aes(Year, Ave))+
            geom_point()+
            scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)),
          ggplot(Settlement, aes(Year, mean, fill = Letters))+
            #geom_bar(stat = "identity", size = 2)+
            #geom_errorbar(aes(Year, ymin = mean, ymax = (mean + sd)), width = 0.2)+
            geom_point()+
            scale_fill_grey()+ #basetheme + 
            legendON + theme(legend.position = c(0.963, 0.8), legend.text = element_text(size = 10), legend.margin = margin(c(2,3,2,2)), legend.key.size = unit(0.75, "lines"), axis.title.y = element_text(size = 10))+ axistheme+
            ylab("Average Settlement (spat/collector/month)")+
            scale_x_continuous(expand = c(0.05,0.01), limits = c(2001, 2018), breaks = seq(2002, 2017, by = 3)) + 
            scale_y_continuous(expand = c(0,0), limits = c(0,20)), nrow = 2)
