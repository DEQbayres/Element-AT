library(readxl)
library(ggplot2)
library(lubridate)
library(svDialogs)

fileIn <- file.choose(new = FALSE)

# Pull data from excel data and make dataframe (df) named, in this case,  Elem
Elem <- read_xls(paste0(fileIn), col_names = TRUE, na = c("void", "VOID"), col_types = NULL)

# tResult is a character. This makes it a number
Elem$tResult <- as.numeric(Elem$tResult)

# Sampled is a character. This makes it a date in local time
Elem$Sampled <- parse_date_time(Elem$Sampled, '%m/%d/%Y', tz = 'Etc/GMT-8')

# Sort the data 
Elem[with(Elem, order("Project", "Sampled", "SpecificMethod", "Analyte")), ]

# Remove rows with specific criteria, ex. Blanks
Elem <- Elem[!(Elem$ClientMatrix=="PM 10 - HV" | Elem$sampletype=="Blank - Equipment"),]
Elem <- Elem[!(Elem$sampletype=="Pre-deployment Check" | Elem$Project=="Special Projects" | Elem$Project=="Sample Media Lot Blanks"),]
Elem <- Elem[!(Elem$Project=="Predeployment Equipment Check" | Elem$Project=="Project"),]
Elem <- Elem[rowSums(is.na(Elem)) != ncol(Elem),]

# Make new value named "Location" from the column named "Project" and choose one or multiple sites (with CTRL or Shift)
# Assign integer values to the locations (sites) you chose
Location <- unique(Elem$Project)
x <- select.list(Location, preselect = NULL, multiple = T, title = "Locations? Hold SHIFT or CTRL for multiple",  graphics = TRUE)

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

Legend <- colnames(Elem)

i <- menu(Legend, graphics = TRUE, title = "Reference Color?")

legend <- Legend[i]

# imported from previous version and will work on next:

location <- ggplot(Elem, aes(Sampled, tResult))

location +
  ggtitle(y) +
  geom_point(data = Elem, aes(Sampled, tResult, colour = Elem[[i]]), size = 3) +
  scale_color_discrete(name = legend) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  xlab("Sampling Day") +
  ylab(location$data$Units)

drnm <- choose.dir(default = "", caption = "Select folder")
flnm <- dlgInput("SaveAs...", Sys.info()["filename"])$res
savefile = paste(drnm, flnm, sep = "\\")
fltp <- paste(savefile, "jpg", sep = ".")
ggsave(fltp, units = c("cm"), width = 30, height = 16, device='jpg', dpi=700)
