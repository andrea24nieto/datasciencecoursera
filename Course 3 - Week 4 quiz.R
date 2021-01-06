## Getting and Cleaning Data - Week 4 Quiz

## 1. Apply strsplit() to split all the names on the characters "wgtp"

        ## Downloading and reading data
        
        url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        download.file(url1, destfile = "/acs.csv")
        acsdata <- read.csv("/acs.csv")
        
        ## Splitting names on "wgtp"
        
        acsvars <- names(acsdata)
        splitnames <- strsplit(acsvars, "wgtp")
        
## 2. Remove commas from the GDP numbers in millions of dlls and average them.
        
        ## Downloading and reading data
        
        url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        download.file(url2, destfile = "/gdp.csv")
        gdpdata0 <- read.csv("/gdp.csv", skip = 4, nrows = 190)
        
        ## Cleaning data
        
        gdpdata <- gdpdata0[,c(1,2,4,5)]
        gdpvars <- c("CountryCode", "Ranking", "Economy", "GDP")
        colnames(gdpdata) <- gdpvars
        
        ## Removing commas from GDP data
        
        gdpdata$GDPfinal <- as.integer(gsub(",", "", gdpdata$GDP))
        
        
        ## Obtaining average
        
        mean(gdpdata$GDPfinal, na.rm = TRUE)
        
## 3. Regular Expression to count countries that begin with "United"
        
        united <- grep("^United", gdpdata$Economy)
        length(united)
        
## 4. How many fiscal years end in June?
        
        ## Downloading and reading second set of data
        
        url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
        download.file(url3, destfile = "/edu.csv")
        edudata <- read.csv("/edu.csv")
        
        ## Matching data based on country short code
        
        mergedDF <- merge(gdpdata, edudata, by = "CountryCode", all = FALSE)
        
        ## Searching countries whose fiscal year ends in june
        
        table(grepl("Fiscal year end: June", mergedDF$Special.Notes))
        
## 5. How many values were collected in 2012? On Mondays in 2012?
        
        ## Installing quantmod and lubridate packages
        
        install.packages("quantmod")
        install.packages("lubridate")
        
        ## Downloading data on Amazon's stock price
        
        library(quantmod)
        amzn = getSymbols("AMZN",auto.assign=FALSE)
        sampleTimes = index(amzn)
        
        ## Obtaining values collected in 2012
        
        library(lubridate)
        years <- lapply(sampleTimes, year)
        yrs12 <- grep("2012", years)
        length(yrs12)
        
        ## Obtaining values collected on Mondays 2012
        
        dates12 <- as.Date(grep("2012", sampleTimes, value = TRUE))
        wkd12 <- lapply(dates12, weekdays)
        mon <- grep("Monday", wkd12, value = TRUE)
        length(mon)
        
        
        
        
        
        
        
        
        
        
        
        
        