## Question 1. 

## Loading data into R

zipname <- "project2.zip"
if (!file.exists(zipname)) {
        zipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(zipURL, zipname, method = "auto")
}

zip.data = "exdata_data_NEI_data.zip"

if (!file.exists("exdata_data_NEI_data.zip")) {
        unzip(zipname)
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Grouping data per year

library(dplyr)
total_peryear <- summarize(group_by(NEI,year), Emissions=sum(Emissions))

## Constructing plot

barcolors <- 2:(length(total_peryear$year)+1)
totalplot <- with(total_peryear, barplot(height = Emissions/1000, names.arg = year, 
                            col = barcolors, 
                            main = expression("Total PM"[2.5]*" Emissions 1999-2008"), 
                            ylab = expression("PM"[2.5]*" in Kilotons"), 
                            xlab = "Year", ylim = c(0,8000)))
text(x = totalplot, y = round(total_peryear$Emissions/1000,2), 
     label = round(total_peryear$Emissions/1000,2), pos = 3, cex = 0.8,
     col = "black")

