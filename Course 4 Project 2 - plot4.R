## Question 4.

library(dplyr)

## Subsetting coal-related sources from SCC

coal_logical <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
coal_SCC <- SCC[coal_logical,]

## Subsetting coal-related sources from NEI

coal_NEI <- subset(NEI, SCC %in% coal_SCC$SCC)

## Grouping and obtaining total by year

coal_peryear <- summarize(group_by(coal_NEI, year), Emissions = sum(Emissions))

## Building plot

barcolors2 <- 2:(length(coal_peryear$year)+1)
coalplot <- with(coal_peryear, barplot(height = Emissions/1000, names.arg = year,
                           col = barcolors2,
                           main = "Coal Combustion Emissions 1999-2008",
                           ylab = expression("PM"[2.5]*" Emissions in Kilotons"),
                           xlab = "Year", ylim = c(0,700)))
text(x = coalplot, y = round(coal_peryear$Emissions/1000,2), 
     label = round(coal_peryear$Emissions/1000,2), pos = 3, cex = 0.8,
     col = "black")

