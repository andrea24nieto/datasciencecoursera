##Part 1 (solution 1)
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #path <- file.path(getwd(),directory)
  #setwd(file.path(getwd(),directory))
  
  path <- paste0(getwd(),"/", directory) #this one is more general
  
  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    
  }
  return(mean(data[,pollutant], na.rm = T))
}

##Part 1 (solution 2)
pollutantmean("specdata", "sulfate", 1:10)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  directory <- paste(getwd(), "/", directory, "/", sep="")
  
  file_list <- list.files(directory)
  data <- NA
  
  for (i in id) {
    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
    
    data <- rbind(data,file_data)
  }

  mean(data[[pollutant]],na.rm=TRUE)
}

##Part 2
complete <- function(directory, id = 1:332) {
  path <- paste0(getwd(),"/", directory)
  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      
    }
    nobs <- sum(complete.cases(dat))
    data9 <- data.frame(i, nobs)
    data <- rbind(data,data9)
    
  }
  return(data)
}
  
##Part 3
corr <- function(directory,threshold = 0) {
  
  path <- paste0(getwd(),"/",directory)
  corr_vect <- NULL
  
  for (i in 1:332) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    
    data <- dat[complete.cases(dat),]
    if (nrow(data)>threshold) {
      corr_vect <- c(corr_vect, cor(data[,"sulfate"], data[,"nitrate"]))
      
    }
  }
  
    return(corr_vect)
}
  
  
