## Question 3.

## Grouping by year and type

balt_peryrtp <- summarize(group_by(baltimore, year, type), 
                          Emissions=sum(Emissions))

## Plotting with ggplot2

library(ggplot2)

typeplot <- ggplot(data = balt_peryrtp, aes(x = factor(year), y = Emissions, 
                                         fill = type, 
                                         label = round(Emissions,2))) + 
        geom_bar(stat = "identity") + 
        facet_grid(. ~ type) + xlab("Year") + 
        ylab(expression("PM"[2.5]*" Emissions in Tons")) + 
        ggtitle(expression("Baltimore Emissions 1999-2008 by Source Type")) + 
        geom_label(aes(fill = type), color = "white", fontface = "bold",
                   size = 3)
print(typeplot)
