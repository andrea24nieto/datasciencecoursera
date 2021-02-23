## Question 2.
 
## Subsetting Baltimore emissions per year

baltimore <- subset(NEI, fips == 24510)
balt_peryear <- summarize(group_by(baltimore, year), Emissions=sum(Emissions))

## Constructing plot

barcolors <- 2:(length(total_peryear$year)+1)
baltplot <- with(balt_peryear, barplot(height = Emissions/1000, names.arg = year, 
                            col = barcolors, 
                            main = expression("Baltimore PM"[2.5]*" Emissions 1999-2008"), 
                            ylab = expression("PM"[2.5]*" in Kilotons"), 
                            xlab = "Year", ylim = c(0,4)))
text(x = baltplot, y = round(balt_peryear$Emissions/1000,2), 
     label = round(balt_peryear$Emissions/1000,2), pos = 3, cex = 0.8,
     col = "black")