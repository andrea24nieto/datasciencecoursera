## Question 6.

library(dplyr)
library(ggplot2)

## Subsetting vehicle-related sources

vehicle_balt <- subset(NEI, fips == "24510" & type == "ON-ROAD")
vehicle_la <- subset(NEI, fips == "06037" & type == "ON-ROAD")

## Grouping and obtaining total per year

vehiclebalt_peryear <- summarize(group_by(vehicle_balt,year), 
                             Emissions = sum(Emissions))
vehiclela_peryear <- summarize(group_by(vehicle_la,year), 
                             Emissions = sum(Emissions))

## Adding county variable

vehiclebalt_peryear$County <- "Baltimore City"
vehiclela_peryear$County <- "Los Angeles County"

## Merging data frames

labalt <- rbind(vehiclebalt_peryear, vehiclela_peryear)

## Constructing plot

labaltplot <- ggplot(data = labalt, aes(x = factor(year), y = Emissions, 
                                        fill = County,
                                        label = round(Emissions/1000,2))) + 
        geom_bar(stat = "identity") + facet_grid(. ~ County) + xlab("Year") + 
        ylab(expression("PM"[2.5]*" Emissions in Kilotons")) +
        ggtitle("Los Angeles vs. Baltimore Motor Vehicle Emissions") +
        geom_label(aes(fill = County), color = "white", fontface = "bold")
print(labaltplot)
        
        
        