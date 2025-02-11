# Pull big table from Repo to here. Some Tiers are not following all the way through.
# Maybe bind big table just before graphs or just before Elem3 defs

library(ggplot2)
library(lubridate)
library(svDialogs)
library(dplyr)
library(tidyr)
library(logr)
library(stringr)
library(zoo)
library(tidyverse)
library(readxl)
library(RAQSAPI)
library(tcltk)

# Choose xls file for analysis. Defaults to working directory. This is specifically for xls format.
# fileIn <- file.choose(new = FALSE)

# Pull from excel data and make dataframe (df) named, in this case,  Elem2. You can change this 
# to read csv or xlsx format
# Elem2 <- read_xls(paste0(fileIn), col_names = TRUE, na = c("void", "VOID"), col_types = NULL)

#########################################
#########################################
#Need loop for getting AQS data from AQS#
#########################################
#########################################

# source("S:/RATSorganizing/computer_scripts_tools/R/in_development/aqs_envista_query/pass/pass2.r")
# aqs_credentials(FocusEmail, FocusPass)

 # toxlist <- vector("list", 2)
 # m <- 20160101
 # n <- 20161231
 # o <- 1
 # test <- unique(big_table$EPA_Parameter_Code)
 # 
 # Elem <- for (t in test) {
 #   if (m > 20170100 | n < 20171232) {
 #     test <- test[!grepl(17141, test)]
 #     Elem2 <- aqs_sampledata_by_state(t, m, n, "41", return_header = FALSE)
 #   }
 #   else {
 #       test <- unique(big_table$EPA_Parameter_Code)
 #       Elem2 <- aqs_sampledata_by_state(t, m, n, "41", return_header = FALSE)
 # }
 # 
 #   toxlist[[o]] <- Elem2
 #   m <- m+10000
 #   n <- n+10000
 #   o <- o+1
 # 
 # }
 # 
 # Elem2 <- dplyr::bind_rows(toxlist)

# Choose single xls file from Element. Defaults to working directory. This is specifically for xls format.
# fileIn <- file.choose(new = FALSE)

# Elem2 <- read_xls(paste0(fileIn), col_names = TRUE, na = c("void", "VOID"), col_types = NULL)
 
# Choose all csv files from Repository DB located in the Data folder of the working directory
# lfiles <- list.files("./Data/", pattern = "*.csv", full.names = TRUE)
# atlist <- vector("list", length(lfiles))
# n <- 1
# for (f in lfiles) {
#     df <- read.csv(f, stringsAsFactors = FALSE)[,-1]
# 
#     atlist[[n]] <- df
#     n <- n+1
# }
# 
#  Elem2 <- bind_rows(atlist)
#  Elem2 <- Elem2 %>% select(!(...36))

source("//deqlab1/bayres/R/Projects/R Training/RepositoryPull.R")
# source("//deqlab1/bayres/R/Projects/R Training/RepositoryUpdate.R")

save.image("E:/R/Projects/R Training/all_data.RData")

 # Sets Working Directory
setwd("S:/Air Toxics/Air Toxics 12-24/")

# Make a copy and work from. Start here if there is a mistake in the below coding.
Elem <- Repo

# class(Elem$tResult)
# Filters to remove duplicate rows, Voided samples and comments
Elem <- Elem %>% distinct()
Elem <- Elem[!grepl("Void", Elem$Result),]
Elem <- Elem[!grepl("ZZZ", Elem$Project),]
Elem <- Elem[!grepl("Blank", Elem$SampleType),]
Elem <- Elem[!grepl("Predeployment", Elem$Project),]
Elem <- Elem[!grepl("Special", Elem$Project),]
Elem <- Elem[!grepl("McMinnville", Elem$Project),]
Elem <- Elem[!grepl("LRAPA", Elem$Project),]
Elem <- Elem[!grepl("Study", Elem$Project),]
Elem <- Elem[!grepl("Acetone", Elem$Analyte),]
Elem <- Elem[!grepl("Isopropanol", Elem$Analyte),]
Elem <- Elem[!grepl("n-Hexane", Elem$Analyte),]
Elem <- Elem[!grepl("pptv", Elem$Units),]
Elem <- Elem[!grepl("Speciation", Elem$Matrix),]
Elem <- Elem[!grepl("XRF", Elem$Analysis),]
Elem <- Elem[!grepl("Southeast Lafayette", Elem$Project),]

Elem$Yr <- year(Elem$Sampled)

Elem <- Elem %>% 
          mutate(qtr = quarter(Sampled, with_year = T)) %>%
          arrange(Sampled, Project, SpecificMethod, Analyte)

# Remove data below B grade
Elem <- Elem[!grepl("C|D|F", Elem$DQL),]


# Remove rows with specific criteria, ex. Blanks
 Elem <- Elem[!(Elem$SampleType=="Pre-deployment Check" | Elem$Project=="Special Projects" | Elem$Project=="Sample Media Lot Blanks"),]
 Elem <- Elem[!(Elem$Project=="Predeployment Equipment Check" | Elem$Project=="Project" | Elem$SampleType=="Blank - Equipment"),]
 Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]
 
 Elem2 <- Elem
 Elem <- Elem2

# Make new value named "Location" from the column named "Project" and choose one or multiple sites (with CTRL or Shift)
# Assign integer values to the locations (sites) you chose
YrRange <- as.character(unique(Elem$Yr))
v <- select.list(YrRange, preselect = NULL, multiple = T, title = "Date Range? Hold Shift or Ctrl for multiple", graphics = TRUE)

Elem <- Elem[Elem$Yr %in% v,]
Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]
 
Location <- unique(Elem$Project)
x <- select.list(Location, preselect = NULL, multiple = T, title = "Locations? Hold SHIFT or CTRL for multiple",  graphics = TRUE)

#ByDate <- unique(Elem$Sampled)
#g <- select.list("ByDate", preselect = NULL, multiple = T, title = "Testing Dates", graphics = TRUE)

# Remove all rows of data that are NOT at a site you chose
Elem <- Elem[Elem$Project %in% x,]
Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]

# Make new value named "Method" from the column named "SpecificMethod" and choose multiple Methods (with CTRL or Shift)
Method <- unique(Elem$SpecificMethod)
y <- select.list(Method, preselect = NULL, multiple = T, title = "Method? Hold SHIFT or CTRL for multiple",  graphics = TRUE)

# Remove all data that are NOT at an analysis method you chose
Elem <- Elem[Elem$SpecificMethod %in% y,]
Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]

# Pick Tiers of data to be used
HAPTier <- unique(Elem$Tier)
w <- select.list(HAPTier, preselect = NULL, multiple = T, title = "Tier? Hold SHIFT or CTRL for multiple", graphics = TRUE)

# Remove all data that are NOT in the Tier class chosen
Elem <- Elem[Elem$Tier %in% w,]
Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]

# Make new value named "Pollutant" from the column named "Analyte" and choose multiple pollutants (with CTRL or Shift)
Pollutant <- unique(Elem$Analyte)
z <- select.list(Pollutant, preselect = NULL, multiple = T, title = "Pollutant? Hold SHIFT or CTRL for multiple",  graphics = TRUE)

# Remove all data that are NOT a pollutant you chose
Elem <- Elem[Elem$Analyte  %in% z,]
Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]

# Making a list to choose how to color the scatter plot points
Legend <- colnames(Elem)
i <- menu(Legend, graphics = TRUE, title = "Reference Color?")

legend <- Legend[i]
Test2 <- unique(Elem[[i]])

Elem3 <- Elem

Elem3$CancerRisk = Elem3$uResult/Elem3$Cancer_Risk_ABC*100
Elem3$NonCancerRisk = Elem3$uResult/Elem3$NonCancer_Risk_ABC*100
Elem3$AcuteNonCancerRisk = Elem3$uResult/Elem3$Acute_NonCancer_Risk_ABC*100



Elem %>%
  group_by(SampleType) %>%
  summarize(n = n())
# legend <- Legend[i]
# Test2 <- unique(Elem[[i]])

yrsamp <- (unique(year(Elem3$Sampled)))
yrfirst <- as.character(yrsamp[1])
yrfinal <- as.character(yrsamp[length(yrsamp)])
tiern <- as.character(Elem3$Tier[1])

# Calculate Annual average
Elem4 <- Elem3 %>%
  group_by(Project, Yr, Analyte, Cancer_Risk_ABC, NonCancer_Risk_ABC) %>%
  summarize(mean = mean(uResult, na.rm=T))

# Take month/year and expected number of samples

ggplot(Elem4, aes(Yr, mean, colour = interaction(Project, Analyte))) + geom_point(size = 2)

# Uncomment for box/whisker graph
#location <- ggplot(Elem, aes(x = Analyte, y = uResult, fill = Elem[[i]])) +
#  ggtitle("KPM Determination")
whisplot1 <- ggplot(Elem3, aes(x = Analyte, y = CancerRisk, fill = Elem3[[i]]))
whisplot1 +
  ggtitle(paste("Tier", tiern, "HAP \nCancer Risk"), subtitle = paste(yrfirst,"-",yrfinal)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "16"),
        axis.text.y=element_text(size = "16", face = "bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.position="right") + 
  guides(fill=guide_legend(title=legend)) +
  xlab("Analyte") +
  ylab(bquote('% risk of'~10^6)) +
  #ylab(expression(paste(mu,g/m^3))) +
  # Create boxplot chart in ggplot2
  geom_boxplot(outlier.colour = "black", outlier.fill = "black", outlier.size = 4) + 
  facet_wrap(~Project)
  
# drnm <- tclvalue(tkchooseDirectory())
# flnm <- paste("Tier", tiern, "CR")
# savefile = paste(drnm, flnm, sep = "/")
# fltp <- paste(savefile, "pdf", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='pdf', dpi=700)

# geom_hline(yintercept=c(0.00023), linetype='solid', color=c('orange'), size = 2) + 
  # geom_hline(yintercept=c(0.00008), linetype='solid', color=c('red'), size = 2) 
#location +
  # geom_hline(yintercept=Elem$NonCancer_Risk_ABC, colour = 'red', size = 2)
whisplot2 <- ggplot(Elem3, aes(x = Analyte, y = NonCancerRisk, fill = Elem3[[i]]))
whisplot2 +
  ggtitle(paste("Tier", tiern, "HAP \nNon-Cancer Risk"), subtitle = paste(yrfirst,"-",yrfinal)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "16"),
        axis.text.y=element_text(size = "16", face = "bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.position="right") + 
  guides(fill=guide_legend(title=legend)) +
  xlab("Analyte") +
  ylab(bquote('% risk of'~10^6)) +
  #ylab(expression(paste(mu,g/m^3))) +
  # Create boxplot chart in ggplot2
  geom_boxplot(outlier.colour = "black", outlier.fill = "black", outlier.size = 4) 

# flnm <- paste("Tier", tiern, "NC")
# savefile = paste(drnm, flnm, sep = "/")
# fltp <- paste(savefile, "pdf", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='pdf', dpi=700)

whisplot3 <- ggplot(Elem3, aes(x = Analyte, y = AcuteNonCancerRisk, fill = Elem3[[i]]))
whisplot3 +
  ggtitle(paste("Tier", tiern, "HAP \nAcute Non-Cancer Risk"), subtitle = paste(yrfirst,"-",yrfinal)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "16"),
        axis.text.y=element_text(size = "16", face = "bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.position="right") + 
  guides(fill=guide_legend(title=legend)) +
  xlab("Analyte") +
  ylab(bquote('% risk of'~10^6)) +
  #ylab(expression(paste(mu,g/m^3))) +
  # Create boxplot chart in ggplot2
  geom_boxplot(outlier.colour = "black", outlier.fill = "black", outlier.size = 4)

# flnm <- paste("Tier", tiern, "Ac")
# savefile = paste(drnm, flnm, sep = "/")
# fltp <- paste(savefile, "pdf", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='pdf', dpi=700)

  # Add horizontal lines for TRVs See \\deqlab1\AQM\Air Toxics\Benchmarks\340-245-8010.pdf for those TRV
# location <- ggplot(Elem, aes(x = Analyte, y = uResult, fill = Elem[[i]])) +
#  location #+
#  guides(fill=guide_legend(title="Analyte"))

  # geom_hline(yintercept=c(100), linetype='solid', color=c('orange'), size = 2) +
  # geom_hline(yintercept=c(180), linetype='solid', color=c('purple'), size = 2) +
  # geom_hline(yintercept=c(46), linetype='solid', color=c('yellow'), size = 2) +
  # geom_hline(yintercept=c(180), linetype='solid', color=c('green'), size = 2) +
  # geom_hline(yintercept=c(41), linetype='solid', color=c('black'), size = 2)

# print(w)

# setwd("E:/R")

# Uncomment below line if you want to save graphs
#flnm <- dlgInput("SaveAs...", Sys.info()["filename"])$res
#savefile = paste(drnm, flnm, sep = "\\")
#fltp <- paste(savefile, "pdf", sep = ".")
#ggsave(fltp, units = c("cm"), width = 90, height = 48, device='pdf', dpi=700)
# imported from previous version and will work on next:
#############################################################################
# STOP HERE FOR BOX-WHISKER PLOT

# Uncomment for simple time vs concentration graph. uResult is all in micrograms. Result is in the reportable units like ppbv

pointplot <- ggplot(Elem3, aes(Sampled, uResult, colour = Analyte))
pointplot + geom_point(size = 2) + facet_wrap(~Elem3[[i]]) +
  ggtitle(paste("Tier", tiern, "HAP \nTime Series"), subtitle = paste(yrfirst,"-",yrfinal)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "16"),
        axis.text.y=element_text(size = "16", face = "bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.position="right") +
  xlab("Sampling Day") +
  ylab(expression(paste(mu,g/m^3))) + 
  guides(fill=guide_legend(title=legend))

# flnm <- paste("Tier", tiern, "TimeSeries")
# savefile = paste(drnm, flnm, sep = "/")
# fltp <- paste(savefile, "pdf", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='pdf', dpi=700)

Annual <- Elem3 %>% group_by(Project, Analyte, Yr, Cancer_Risk_ABC, NonCancer_Risk_ABC, Acute_NonCancer_Risk_ABC) %>% summarise(mean(uResult))

AnCR <- ggplot(Annual, aes(Yr, `mean(uResult)`, color = Analyte)) + geom_point(size = 2) + facet_wrap(~Project) +
  geom_hline(aes(yintercept=Cancer_Risk_ABC, colour=Analyte), linewidth = 1) +
  scale_x_continuous(limits = c(2013, 2025), breaks = c(2013, 2015, 2017, 2019, 2021, 2023, 2025)) +
  ggtitle(paste("Tier", tiern, "Annual Cancer Risk"), subtitle = paste(yrfirst,"-",yrfinal)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "16"),
        axis.text.y=element_text(size = "16", face = "bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.position="right") +
  ylim(0,10) +
  xlab("Sampling Day") +
  ylab(expression(paste(mu,g/m^3))) + 
  guides(fill=guide_legend(title=legend))
AnCR

# flnm <- paste("Tier", tiern, "Annual CR")
# savefile = paste(drnm, flnm, sep = "/")
# fltp <- paste(savefile, "pdf", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='pdf', dpi=700)

AnNC <- ggplot(Annual, aes(Yr, `mean(uResult)`, color = Analyte)) + geom_point(size = 2) + facet_wrap(~Project) +
  geom_hline(aes(yintercept=NonCancer_Risk_ABC, colour=Analyte), linewidth = 1) +
  scale_x_continuous(limits = c(2013, 2025), breaks = c(2013, 2015, 2017, 2019, 2021, 2023, 2025)) +
  ggtitle(paste("Tier", tiern, "Annual Non-Cancer Risk"), subtitle = paste(yrfirst,"-",yrfinal)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "16"),
        axis.text.y=element_text(size = "16", face = "bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.position="right") +
  ylim(0,10) +
  xlab("Sampling Day") +
  ylab(expression(paste(mu,g/m^3))) + 
  guides(fill=guide_legend(title=legend))
AnNC

# flnm <- paste("Tier", tiern, "Annual NC")
# savefile = paste(drnm, flnm, sep = "/")
# fltp <- paste(savefile, "pdf", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='pdf', dpi=700)

AnAc <- ggplot(Annual, aes(Yr, `mean(uResult)`, color = Analyte)) + geom_point(size = 2) + facet_wrap(~Project) +
  geom_hline(aes(yintercept=Acute_NonCancer_Risk_ABC, colour=Analyte), linewidth = 1) +
  scale_x_continuous(limits = c(2010, 2025), breaks = c(2010, 2015, 2020, 2025)) +
  ggtitle(paste("Tier", tiern, "Annual Acute Non-Cancer Risk"), subtitle = paste(yrfirst,"-",yrfinal)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 16),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "16"),
        axis.text.y=element_text(size = "16", face = "bold"),
        axis.title=element_text(size=16,face="bold"),
        legend.position="right") +
  ylim(0,10) +
  xlab("Sampling Day") +
  ylab(expression(paste(mu,g/m^3))) + 
  guides(fill=guide_legend(title=legend))
AnAc

# Uncomment below line if you want to  save graphs
# flnm <- paste("Tier", tiern, "Annual Ac")
# savefile = paste(drnm, flnm, sep = "/")
# fltp <- paste(savefile, "pdf", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='pdf', dpi=700)

# For data completeness, add capability for multiple years, multiple sites and multiple specific methods
setwd("E:/R/Projects/R Training")
