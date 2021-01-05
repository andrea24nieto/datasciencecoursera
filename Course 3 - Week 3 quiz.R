## Getting and Cleaning Data - Week 3 quiz

## 1. Creating logical vector that ids households on greater than 10 acres
##    who sold more than $10,000 worth of agricultural products

        ## Downloading and reading data into R
        
        url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        download.file(url1, destfile = "/acsfile.csv")
        acs <- read.csv("/acsfile.csv")
        
        ## Subsetting data according to specifications and id first 3 true rows
        
        agricultureLogical <- acs$ACR == 3 & acs$AGS == 6
        which(agricultureLogical) [1:3]
        
        
## 2. Obtaining 30th and 80th quantiles of jpg data
        
        ## Accessing jpeg package
        
        install.packages("jpeg")
        library(jpeg)
        
        ## Downloading and reading data into R
        
        url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
        download.file(url2, destfile = "/image.jpg")
        pic <- readJPEG("/image.jpg", native = TRUE)
        
        ## Getting 30th and 80th quantiles
        
        quantile(pic, probs = c(0.3, 0.8))
        
        
## 3. Matching and sorting GDP data (13th country)
        
        ## Downloading and reading data into R
        
        url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        url4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
        download.file(url3, destfile = "/GDP.csv")
        download.file(url4, destfile = "/edu.csv")
        pregdp <- read.csv("/GDP.csv", skip = 5, header = FALSE, nrows = 190)
        gdp <- pregdp[,c(1,2,4,5)]
        edu <- read.csv("/edu.csv")
        
        ## Accessing dplyr package
        
        install.packages("dplyr")
        library(dplyr)
        
        ## Renaming GDP data columns
        
        colnames(gdp) <- c("CountryCode", "Ranking", "Economy", "GDP")
        
        ## Merging data by CountryCode
        
        mergedDF <- merge(gdp, edu, by = "CountryCode", all = FALSE)
        
        ## Obtaining number of country code matches
        
        nrow(mergedDF)
        
        ## Sorting data in descending order by GDP
        
        sortedDF <- arrange(mergedDF, desc(Ranking))
        
        ## Getting 13th country 
        
        sortedDF[13,"Economy"]
        
## 4. Getting the average GDP ranking for the "High income: OECD" and 
##    "High income: nonOECD" group
        
        ## Filtering data according to income group
        hiOECD <- filter(mergedDF, Income.Group == "High income: OECD")
        hinonOECD <- filter(mergedDF, Income.Group == "High income: nonOECD")
        
        ## Calculating averages
        mean(hiOECD$Ranking)
        mean(hinonOECD$Ranking)
       
## 5. GDP vs. Income.Group: How many countries are Lower middle income but among 
##    the 38 nations with highest GDP?
       
        ## Creating new variable by cutting data into quantiles
                
        sortedDF$quants <- cut(sortedDF$Ranking, 
                               breaks = quantile(sortedDF$Ranking,
                                                 probs = c(0,0.2,0.4,0.6,0.8,1)))
        
        ## Creating table
        
        table(sortedDF$quants, sortedDF$Income.Group)