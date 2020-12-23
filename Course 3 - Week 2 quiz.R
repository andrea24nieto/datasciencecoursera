## Getting and Cleaning Data - Week 2 quiz

## 1. Github API: Creation Time

## Installing packages
install.packages("httr")
install.packages("jasonlite")
install.packages("httpuv")
library(httr)
library(jsonlite)
library(httpuv)

## Finding OAuth settings for github
oauth_endpoints("github")

## Accessing API
myapp <- oauth_app("github", key = "7545a41a3d7145989d09",
                   secret = "5b6080dd88332ee1c04bd56ace2e851b7f873b1f")

## Getting OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

## Using API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

## Taking action on http error
stop_for_status(req)

## Extracting content from request
json1 = content(req)

## Converting to data frame
gitDF = jsonlite::fromJSON(toJSON(json1))

## Subsetting data frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]


## 2. sqldf package: data selection

## Install sqldf package
install.packages("sqldf")
library(sqldf)

## Downloading and loading ACS data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL, destfile = "/acsfile.csv")
acs <- read.csv("/acsfile.csv")

## Selecting  data for the probability weights pwgtp1 with ages less than 50
sqldf("select pwgtp1 from acs where AGEP < 50")

## 3. Equivalent to unique(acs$AGEP)
sqldf("select distinct AGEP from acs")


## 4. Characters in HTML lines

## Reading HTML data
htmlURL <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlInfo <- readLines(htmlURL)

## Closing connection
close(htmlURL)

## Finding number of characters in given line
c(nchar(htmlInfo[10]), nchar(htmlInfo[20]), nchar(htmlInfo[30]),
  nchar(htmlInfo[100]))


## 5. Sum of numbers in fourth (of nine) column (fixed-width file)
forURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
SST <- read.fwf(forURL, skip = 4, widths = c(12, 7, 4, 9, 4, 9, 4, 9, 4))

## Obtaining answer
sum(SST[,4])