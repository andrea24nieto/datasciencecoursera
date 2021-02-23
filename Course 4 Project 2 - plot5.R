## Question 5.

library(dplyr)

## Subsetting vehicle-related sources in Baltimore

vehicle_balt <- subset(NEI, fips == "24510" & type == "ON-ROAD")

## Grouping and obtaining total per year

vehicle_peryear <- summarize(group_by(vehicle_balt,year), 
                             Emissions = sum(Emissions))

## Building plot

barcolors3 <- 2:(length(vehicle_peryear$year)+1)
vehicleplot <- with(vehicle_peryear, barplot(height = Emissions, names.arg = year,
                                       col = barcolors3,
                                       main = "Motor Vehicle Emissions in Baltimore 1999-2008",
                                       ylab = expression("PM"[2.5]*" Emissions in Tons"),
                                       xlab = "Year", ylim = c(0,400)))
text(x = vehicleplot, y = round(vehicle_peryear$Emissions,2), 
     label = round(vehicle_peryear$Emissions,2), pos = 3, cex = 0.8,
     col = "black")