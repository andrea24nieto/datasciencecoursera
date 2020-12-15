## 1. Plot the 30-day mortality rates for heart attack

outcomeplot <- read.csv("outcome-of-care-measures.csv",
                    colClasses = "character")

outcomeplot[, 11] <- as.numeric(outcomeplot[, 11])

hist(outcomeplot[, 11])


## 2. Finding the best hospital in a state

best <- function(state, outcome) {

        ## Read outcome data

        outcomes <- read.csv("outcome-of-care-measures.csv",
                     colClasses = "character")

        ## Cleaning up data according to task

        rates <- as.data.frame(cbind(outcomes[, 2],   ## hospital
                                     outcomes[, 7],   ## state
                                     outcomes[, 11],  ## heart attack
                                     outcomes[, 17],  ## heart failure
                                     outcomes[, 23]), ## pneumonia
                               stringsAsFactors = FALSE)

        ## Renaming columns

        colnames(rates) <- c("hospital", "state", "heart attack", 
                     "heart failure", "pneumonia")

        ## Checking that arguments are valid (state & outcome)

        if(!state %in% rates[,"state"]) {
                stop("invalid state")   
                
                }

        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
                
                }

        ## Returning hospital name with lowest 30-day death rate

        ## Getting only hospitals in the specified state
        dRates <- rates[(rates[, "state"] == state), ]
        
        ## Converting outcome rates to numeric
        dRates[, outcome] <- suppressWarnings(as.numeric(dRates[, outcome]))
        
        ## Removing NA values
        dRates <- dRates[!is.na(dRates[, outcome]), ]
        
        ## Ordering by outcome rate
        dRates <- dRates[order(dRates[, outcome]), ]
        
        ## Getting name of hospital with lowest rate
        dNames <- dRates[dRates[, outcome] == min(dRates[,outcome]), 1]
        
        ## Sorting names in case of a tie
        sort(dNames) [1]

}

## 3. Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num="best") {
        
        ## Read outcome data
        
        outcomes <- read.csv("outcome-of-care-measures.csv",
                             colClasses = "character")
        
        ## Cleaning up data according to task
        
        rates <- as.data.frame(cbind(outcomes[, 2],   ## hospital
                                     outcomes[, 7],   ## state
                                     outcomes[, 11],  ## heart attack
                                     outcomes[, 17],  ## heart failure
                                     outcomes[, 23]), ## pneumonia
                               stringsAsFactors = FALSE)
        
        ## Renaming columns
        
        colnames(rates) <- c("hospital", "state", "heart attack", 
                             "heart failure", "pneumonia")
        
        ## Checking that arguments are valid (state & outcome)
        
        if(!state %in% rates[,"state"]) {
                stop("invalid state")   
                
        }
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
                
        }
        
        ## Returning name of hospital with chosen ranking
        
        ## Getting only hospitals in specified state
        dRates <- rates[(rates[,"state"]) == state, ]
        
        ## Converting outcome rates to numeric
        dRates[ ,outcome] <- suppressWarnings(as.numeric(dRates[ ,outcome]))
        
        ## Removing NA values from data
        dRates <- dRates[!is.na(dRates[ ,outcome]), ]
        
        ## Converting text entries to numeric value
        if(num == "best") {
                num <- 1
        }
                
        if(num == "worst") {
                num <- nrow(dRates)
        }
        
        ## Ordering by outcome rate & hospital name
        dRates <- dRates[order(dRates[,outcome], dRates[,"hospital"]), ]
        
        ## Returning hospital name
        dRates[num,1]
}
        


## 4. Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        
        outcomes <- read.csv("outcome-of-care-measures.csv",
                             colClasses = "character")
        
        ## Cleaning up data according to task
        
        rates <- as.data.frame(cbind(outcomes[, 2],   ## hospital
                                     outcomes[, 7],   ## state
                                     outcomes[, 11],  ## heart attack
                                     outcomes[, 17],  ## heart failure
                                     outcomes[, 23]), ## pneumonia
                               stringsAsFactors = FALSE)
        
        ## Renaming columns
        
        colnames(rates) <- c("hospital", "state", "heart attack", 
                             "heart failure", "pneumonia")
        
        ## Checking that arguments are valid (outcome)
        
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
                
        }
        
        ## Returning data frame with hospitals per state with chosen rate 
        
        dRank <- data.frame()
        
        for(state in sort(unique(rates[,"state"]))) {
                
                ## Getting only hospitals in a state
                dRates <- rates[(rates[,"state"] == state), ]
                
                ## Converting outcomes to numeric
                dRates[ ,outcome] <- suppressWarnings(as.numeric
                                                     (dRates[ ,outcome]))
                
                ## Removing NA values
                dRates <- dRates[!is.na(dRates[,outcome]), ]
                
                ## Converting text entries to numeric value
                if(num == "best") {
                        rnum <- 1
                }
                else if (num == "worst") {
                        rnum <- nrow(dRates)
                }
                else {
                        rnum = num
                }
                
                ## Ordering by outcome rate & hospital name
                dRates <- dRates[order(dRates[,outcome], dRates[,"hospital"]), ]
                
                ## Getting hospital name
                dName <- dRates[rnum,1]
                
                ## Compiling data from every state
                dRank <- rbind(dRank, data.frame(hospital = dName, state=state))
                
        }
        
        ## Returning data frame
        dRank
        
}