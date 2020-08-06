library(tidyverse)
dir <- getwd()


# FUNCTION 1 --------------------------------------------------------------

pollutantmean <- function(directory, pollutant, id) {
    
    dir <- setwd(directory)
    list <- list.files(dir)
    data_list <- vector(mode = "list", length = length(id))
    
    for (i in id) {
        data_list[[i]] <- read_csv(list[i])
        comb <- do.call(rbind, data_list)
    }
    
    comb[,3] <- as.numeric(comb[,3])
    comb[,4] <- as.numeric(comb[,4])
    
    
    if (pollutant == "sulfate") {
        mean(comb$sulfate, na.rm = T)
    } else if (pollutant == "nitrate") {
        mean(comb$nitrate, na.rm = T)
    } else {
        "Not a valid pollutant"
    }
    
}

pollutantmean(dir, "nitrate", 1:50)


# FUNCTION 2 --------------------------------------------------------------

complete <- function(directory, id = 1:332) {
    
    dir <- setwd(directory)
    list <- list.files(dir)
    list2 <- lapply(list, read_csv)
    data <- list()
    
    for (i in 1:length(list2)) {
        data[[i]] <- summarise(list2[[i]],
                               ID = mean(ID),
                               cases = sum(complete.cases(list2[[i]]))
                               )
    }
    
    data <- do.call(rbind, data)
    return(data[id,])
    
}


complete(dir, c(2,4,8,10,12))

RNGversion("3.5.1")  
set.seed(42)
cc <- complete(dir, 332:1)
use <- sample(332, 10)
print(cc[use, "cases"])

# FUNCTION 3 --------------------------------------------------------------

corr <- function(directory, threshold = 0) {
    
    dir <- setwd(directory)
    list <- list.files(dir)
    list2 <- lapply(list, read_csv, na = "NA")
    data <- list()
    
    for (i in 1:length(list2)) {
        
        if (is.numeric(list2[[i]][,2]) && is.atomic(list2[[i]][,2])) {next}
        if (is.numeric(list2[[i]][,3]) && is.atomic(list2[[i]][,3])) {next}

        if (sum(complete.cases(list2[[i]])) > threshold) {
        
            data[i] <- cor(list2[[i]][,2], list2[[i]][,3], use = "complete.obs")
            
        }
    
    }
    
    corr <- do.call(rbind, data)
    return(corr)
    
    
}

calc <- corr(dir, threshold = 0)
summary(calc)

cr <- corr(dir, 2000)                
n <- length(cr)                
cr <- corr(dir, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

       