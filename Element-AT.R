# Data Analysis tool for data pulled from the Element EDD "BLDR:Excel-Simple" format. 
# This could easily be adjusted to fit another EDD format.
# Change the working directory to fit your computer's R-workflow
# A lot of the data is in character format, which you can check using "class()"
# Ex. class(Elem$Sampled) --- See below for converting this to the proper format

library(readxl)
library(ggplot2)
library(lubridate)
library(svDialogs)
library(dplyr)
library(logr)
library(stringr)
library(openxlsx)

# Sets Working Directory
setwd("E:/Data DL/Air Toxics 18-23")

# Choose xls file for analysis. Defaults to working directory. This is specifically for xls format.
fileIn <- file.choose(new = FALSE)

# Pull from excel data and make dataframe (df) named, in this case,  Elem2. You can change this 
# to read csv or xlsx format
Elem2 <- read_xls(paste0(fileIn), col_names = TRUE, na = c("void", "VOID"), col_types = NULL)

# Make a copy and work from. Start here if there is a mistake in the below coding.
Elem <- Elem2

#class(Elem$tResult)
#class(Elem$Sampled)

# tResult is a character. This makes it a number
Elem$tResult <- as.numeric(Elem$tResult)

# Element spits out the column "sampled," which is a character. This makes it a date in local time (POSIXct 
# in this case)
Elem$Sampled <- format(mdy(Elem$Sampled),
                       format = "%m/%d/%Y")
Elem$Sampled <- as.POSIXct(Elem$Sampled, tz = "GMT", format = "%m/%d/%Y")

#class(Elem$tResult)
#class(Elem$Sampled)

# Make a duplicate df to work with for conversions to ug/m3
Elem$uResult <- Elem$tResult

# Pull in .csv files. For the 2022 air toxics report, these are the 2022 Air Toxics.csv file and the 
# Big_table.csv (cross tables).
big_table <- read.csv("//deqlab1/Assessment/Air Toxics/Annual Air Toxics Summaries/2022 Air Toxics Monitoring Summary/R Scripts/AT/Supporting Tables/Big_Table_Ben.csv")

# Joins the table to the element table matching the Analyte column in the Element doc table to the 
# analyte_name_deq column in the Big Table
Elem <- left_join(Elem, big_table, by = c("Analyte" = "analyte_name_deq"))

#Convert the units to ug_m3.  Apply to Units column.  
Elem$uResult <- ifelse(
  Elem$Units %in% c("ng/m³ STP", "ng/m³ LTP"),
  Elem$tResult / 1000,
  ifelse(
    Elem$Units == "ppbv",
    Elem$tResult * (Elem$mol_weight_g_mol) / 24.45,
    Elem$tResult
  )
)

#############################################
#############################################
#Use the above loop to make seasonal entries#
#############################################
#############################################

# Sort the data 
Elem[with(Elem, order("Project", "Sampled", "SpecificMethod", "Analyte")), ]

# Remove rows with specific criteria, ex. Blanks
# Elem <- Elem[!(Elem$ClientMatrix=="PM 10 - HV" | Elem$sampletype=="Blank - Equipment"),]
 Elem <- Elem[!(Elem$sampletype=="Pre-deployment Check" | Elem$Project=="Special Projects" | Elem$Project=="Sample Media Lot Blanks"),]
 Elem <- Elem[!(Elem$Project=="Predeployment Equipment Check" | Elem$Project=="Project"),]
 Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]

# Make new value named "Location" from the column named "Project" and choose one or multiple sites (with CTRL or Shift)
# Assign integer values to the locations (sites) you chose
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

# Uncomment next line if you want to save graphs
# drnm <- choose.dir(default = "", caption = "Select folder")

# Uncomment for box/whisker graph
location <- ggplot(Elem, aes(x = Analyte, y = uResult, fill = Elem[[i]])) +
  ggtitle("KPM Determination") +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        title = element_text(size = 24),
        axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "24"),
        axis.text.y=element_text(size = "24", face = "bold"),
        axis.title=element_text(size=24,face="bold"),
        legend.position="right") +
  xlab("Analyte") +
  #ylab(location$data$Units) +
  ylab(expression(paste(mu,g/m^3))) +
  # Create boxplot chart in ggplot2
  geom_boxplot(outlier.colour = "black", outlier.fill = "black", outlier.size = 4) #+
  # geom_hline(yintercept=Elem$NonCancer_Risk_ABC, colour = 'red', size = 2)

# Add horizontal lines for TRVs See \\deqlab1\AQM\Air Toxics\Benchmarks\340-245-8010.pdf for those TRV
location #+
  # guides(fill=guide_legend(title="Analyte"))

  # geom_hline(yintercept=Elem$NonCancer_Risk_ABC, colour='blue', size = 2) #+
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
#fltp <- paste(savefile, "jpg", sep = ".")
#ggsave(fltp, units = c("cm"), width = 90, height = 48, device='jpg', dpi=700)
# imported from previous version and will work on next:
#############################################################################
# STOP HERE FOR BOX-WHISKER PLOT

# Uncomment for simple time vs concentration graph. uResult is all in micrograms. tResult is in the reportable units like ppbv
  location <- ggplot(Elem, aes(Sampled, uResult))
# # location <- ggplot(Elem, aes(Sampled, tResult))

location +
  ggtitle(x) +
  #geom_point(data = Elem, aes(Sampled, tResult, colour = Elem[[i]]), size = 4) +
  geom_point(data = Elem, aes(Sampled, uResult, group = Elem[[i]], colour = Elem[[i]]), size = 4) +
  scale_color_discrete(name = legend) +
  theme(axis.text.x=element_text(angle=45, hjust=1, face = "bold", size = "12"), axis.text.y=element_text(size = "12", face = "bold"), axis.title=element_text(size=14,face="bold")) +
  xlab("Sampling Day") +
  ylab(location$data$Units) #+
# ylab(expression(paste(mu,g/m^3)))
#    geom_hline(yintercept=Elem$NonCancer_Risk_ABC, colour = Elem[[i]], size = 2) #+
  # geom_hline(yintercept=Elem$NonCancer_Risk_ABC, colour='blue', size = 2) +
  # geom_hline(yintercept=c(100), linetype='solid', color=c('orange'), size = 2) +
  # geom_hline(yintercept=c(180), linetype='solid', color=c('purple'), size = 2) +
  # geom_hline(yintercept=c(46), linetype='solid', color=c('yellow'), size = 2) +
  # geom_hline(yintercept=c(180), linetype='solid', color=c('green'), size = 2) +
  # geom_hline(yintercept=c(41), linetype='solid', color=c('black'), size = 2)
# 

# Uncomment below line if you want to save graphs
# flnm <- dlgInput("SaveAs...", Sys.info()["filename"])$res
# savefile = paste(drnm, flnm, sep = "\\")
# fltp <- paste(savefile, "jpg", sep = ".")
# ggsave(fltp, units = c("cm"), width = 30, height = 16, device='jpg', dpi=700)