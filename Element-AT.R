# Pull big table from Repo to here. Some Tiers are not following all the way through.
# Maybe bind big table just before graphs or just before DEQdf3 defs

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

DLData <- c("REPO", "AQS", "XLS")
d <- select.list(DLData, graphics = TRUE, multiple = FALSE)

setwd("E:/R/Projects/R Training/")

ifelse(d == "REPO", source("./PullREPOData.R"),
       ifelse(d == "AQS", source("./PullAQSData.R"),
              ifelse(d == "XLS", source("./PullXLSData.R"))))

save.image("E:/R/Projects/R Training/all_data.RData")

# Make a copy and work from. Start here if there is a mistake in the below coding.
DEQdf <- Elem

# class(DEQdf$tResult)
# Filters to remove duplicate rows, Voided samples and comments
DEQdf <- DEQdf %>% distinct()
DEQdf <- DEQdf[!grepl("Void", DEQdf$Result),]
DEQdf <- DEQdf[!grepl("ZZZ", DEQdf$Project),]
DEQdf <- DEQdf[!grepl("Blank", DEQdf$SampleType),]
DEQdf <- DEQdf[!grepl("Predeployment", DEQdf$Project),]
DEQdf <- DEQdf[!grepl("Special", DEQdf$Project),]
DEQdf <- DEQdf[!grepl("McMinnville", DEQdf$Project),]
DEQdf <- DEQdf[!grepl("LRAPA", DEQdf$Project),]
DEQdf <- DEQdf[!grepl("Study", DEQdf$Project),]
DEQdf <- DEQdf[!grepl("Acetone", DEQdf$Analyte),]
DEQdf <- DEQdf[!grepl("Isopropanol", DEQdf$Analyte),]
DEQdf <- DEQdf[!grepl("n-Hexane", DEQdf$Analyte),]
DEQdf <- DEQdf[!grepl("pptv", DEQdf$Units),]
DEQdf <- DEQdf[!grepl("Speciation", DEQdf$Matrix),]
DEQdf <- DEQdf[!grepl("XRF", DEQdf$Analysis),]
DEQdf <- DEQdf[!grepl("Southeast Lafayette", DEQdf$Project),]

DEQdf$Yr <- year(DEQdf$Sampled)

DEQdf <- DEQdf %>% 
          mutate(qtr = quarter(Sampled, with_year = T)) %>%
          arrange(Sampled, Project, SpecificMethod, Analyte)

# Remove data below B grade
DEQdf <- DEQdf[!grepl("C|D|F", DEQdf$DQL),]


# Remove rows with specific criteria, ex. Blanks
 DEQdf <- DEQdf[!(DEQdf$SampleType=="Pre-deployment Check" | DEQdf$Project=="Special Projects" | DEQdf$Project=="Sample Media Lot Blanks"),]
 DEQdf <- DEQdf[!(DEQdf$Project=="Predeployment Equipment Check" | DEQdf$Project=="Project" | DEQdf$SampleType=="Blank - Equipment"),]
 DEQdf <- DEQdf[rowSums(is.na(DEQdf)) != ncol(DEQdf),]
 
 DEQdf2 <- DEQdf
 DEQdf <- DEQdf2

# Make new value named "Location" from the column named "Project" and choose one or multiple sites (with CTRL or Shift)
# Assign integer values to the locations (sites) you chose
YrRange <- as.character(unique(DEQdf$Yr))
v <- select.list(YrRange, preselect = NULL, multiple = T, title = "Date Range? Hold Shift or Ctrl for multiple", graphics = TRUE)

DEQdf <- DEQdf[DEQdf$Yr %in% v,]
DEQdf <- DEQdf[rowSums(is.na(DEQdf)) != ncol(DEQdf),]
 
Location <- unique(DEQdf$Project)
x <- select.list(Location, preselect = NULL, multiple = T, title = "Locations? Hold SHIFT or CTRL for multiple",  graphics = TRUE)

#ByDate <- unique(DEQdf$Sampled)
#g <- select.list("ByDate", preselect = NULL, multiple = T, title = "Testing Dates", graphics = TRUE)

# Remove all rows of data that are NOT at a site you chose
DEQdf <- DEQdf[DEQdf$Project %in% x,]
DEQdf <- DEQdf[rowSums(is.na(DEQdf)) != ncol(DEQdf),]

# Make new value named "Method" from the column named "SpecificMethod" and choose multiple Methods (with CTRL or Shift)
Method <- unique(DEQdf$SpecificMethod)
y <- select.list(Method, preselect = NULL, multiple = T, title = "Method? Hold SHIFT or CTRL for multiple",  graphics = TRUE)

# Remove all data that are NOT at an analysis method you chose
DEQdf <- DEQdf[DEQdf$SpecificMethod %in% y,]
DEQdf <- DEQdf[rowSums(is.na(DEQdf)) != ncol(DEQdf),]

# Pick Tiers of data to be used
HAPTier <- unique(DEQdf$Tier)
w <- select.list(HAPTier, preselect = NULL, multiple = T, title = "Tier? Hold SHIFT or CTRL for multiple", graphics = TRUE)

# Remove all data that are NOT in the Tier class chosen
DEQdf <- DEQdf[DEQdf$Tier %in% w,]
DEQdf <- DEQdf[rowSums(is.na(DEQdf)) != ncol(DEQdf),]

# Make new value named "Pollutant" from the column named "Analyte" and choose multiple pollutants (with CTRL or Shift)
Pollutant <- unique(DEQdf$Analyte)
z <- select.list(Pollutant, preselect = NULL, multiple = T, title = "Pollutant? Hold SHIFT or CTRL for multiple",  graphics = TRUE)

# Remove all data that are NOT a pollutant you chose
DEQdf <- DEQdf[DEQdf$Analyte  %in% z,]
DEQdf <- DEQdf[rowSums(is.na(DEQdf)) != ncol(DEQdf),]

# Making a list to choose how to color the scatter plot points
Legend <- colnames(DEQdf)
i <- menu(Legend, graphics = TRUE, title = "Reference Color?")

legend <- Legend[i]

DEQdf3 <- DEQdf

DEQdf3$CancerRisk = DEQdf3$uResult/DEQdf3$Cancer_Risk_ABC*100
DEQdf3$NonCancerRisk = DEQdf3$uResult/DEQdf3$NonCancer_Risk_ABC*100
DEQdf3$AcuteNonCancerRisk = DEQdf3$uResult/DEQdf3$Acute_NonCancer_Risk_ABC*100

DEQdf %>%
  group_by(SampleType) %>%
  summarize(n = n())

yrsamp <- (unique(year(DEQdf3$Sampled)))
yrfirst <- as.character(yrsamp[1])
yrfinal <- as.character(yrsamp[length(yrsamp)])
tiern <- as.character(DEQdf3$Tier[1])

# Calculate Annual average
DEQdf4 <- DEQdf3 %>%
  group_by(Project, Yr, Analyte, Cancer_Risk_ABC, NonCancer_Risk_ABC) %>%
  summarize(mean = mean(uResult, na.rm=T))

# Take month/year and expected number of samples

ggplot(DEQdf4, aes(Yr, mean, colour = interaction(Project, Analyte))) + geom_point(size = 2)

# Uncomment for box/whisker graph

whisplot1 <- ggplot(DEQdf3, aes(x = Analyte, y = CancerRisk, fill = DEQdf3[[i]]))
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
  # geom_hline(yintercept=DEQdf$NonCancer_Risk_ABC, colour = 'red', size = 2)
whisplot2 <- ggplot(DEQdf3, aes(x = Analyte, y = NonCancerRisk, fill = DEQdf3[[i]]))
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

whisplot3 <- ggplot(DEQdf3, aes(x = Analyte, y = AcuteNonCancerRisk, fill = DEQdf3[[i]]))
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
# location <- ggplot(DEQdf, aes(x = Analyte, y = uResult, fill = DEQdf[[i]])) +
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

pointplot <- ggplot(DEQdf3, aes(Sampled, uResult, colour = Analyte))
pointplot + geom_point(size = 2) + facet_wrap(~DEQdf3[[i]]) +
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

Annual <- DEQdf3 %>% group_by(Project, Analyte, Yr, Cancer_Risk_ABC, NonCancer_Risk_ABC, Acute_NonCancer_Risk_ABC) %>% summarise(mean(uResult))

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
