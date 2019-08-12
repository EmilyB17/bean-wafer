## FUNCTION to make a reclassification matrix given Albeke model output data
# parameters: df = dataframe of "is" data
# parameters: classColumn: text string of identifying column where "is" data is

# OUTPUT: reclassification matrix with two columns: "is" and "becomes"


reclassFunction <- function(df, classColumn = "TimeSeriesClass") {
  
  
  # split all integers 
  strMat <- str_extract_all(df[, classColumn], "", simplify = TRUE)
  
  ## FUNCTION to define the counts of each code 
  countVec <- function(vec, dfID) {
    
    # fill dataframe with the count of unique codes
    countDF <- data.frame(row.names = dfID,
                          count1 = length(which(vec == "1")),
                          count2 = length(which(vec == "2")),
                          count3 = length(which(vec == "3")),
                          count4 = length(which(vec == "4")),
                          count5 = length(which(vec == "5")),
                          count7 = length(which(vec == "7")))
    return(countDF)
    
  }
  
  # define output matrix
  finMat <- matrix(0, nrow = nrow(df), ncol = 2,
                   dimnames = list(NULL, c("is", "becomes")))
  
  ## iterate every row of df and determine what that row becomes
  
  for(j in 1:nrow(df)) {
    
    
    # get the unique set of codes
    vec <- strMat[j, ]
    vec <- vec[!vec == ""]
    
    # get the counts of each integer
    cv <- countVec(vec, dfID = df[j, "id"])
    
    ## take everything through ONE "if" statement
    if(all(sapply(vec, FUN = identical, vec[1])) == TRUE) { # TEST FOR UNIFORMITY
      
      # finalize
      finMat[j, "is"] <- as.numeric(df[j, "TimeSeriesClass"])
      finMat[j, "becomes"] <- as.numeric(vec[1])
      
    } else if(any(cv > (length(vec) * .65 ))) { # TEST FOR 65$ MAJORITY
      
      # finalize by majority
      finMat[j, "is"] <- as.numeric(df[j, "TimeSeriesClass"])
      # get column value of the vector majority
      finMat[j, "becomes"] <- as.numeric(
        strsplit(names(cv[which(cv > (length(vec) * 0.65))]), "t")[[1]][2])
      
      
    } else if(all(sapply(vec, FUN = identical, vec[1])) == FALSE & 
              any(cv > (length(vec) * .65 )) == FALSE) {
      
      # check for absence of bare/fallow/crop
      if(!any(vec %in% c(1, 2, 3))) { # TEST FOR ABSENCE OF BARE/FALLOW/CROP
        
        # if absent: finalize by majority
        cv.sort <- sort(colSums(cv), decreasing = TRUE)
        finMat[j, "is"] <- df[j, "TimeSeriesClass"]
        finMat[j, "becomes"] <- as.numeric(
          strsplit(names(cv.sort[1]), "t")[[1]][2]
        )
        
      } else {
        
        # get the end of the vector
        endVec <- vec[length(vec) - 0]
        
        # test for presence of crop or fallow
        if(any(vec %in% c(2, 3))) {
          
          if(endVec == 2 | vec[1] == 2) { # TEST FOR ENDING IN CROP OR STARTNG IN CROP
            
            # If the last integer or first integer is CROP, finalize as crop
            finMat[j, "is"] <- as.numeric(df[j, "TimeSeriesClass"])
            finMat[j, "becomes"] <- 2
            
            
          } else if(length(which(vec == 2)) > 2) { # TEST FOR MORE THAN ONE CROP
            
            # If there are more than 2 CROP integers, finalize as crop
            finMat[j, "is"] <- as.numeric(df[j, "TimeSeriesClass"])
            finMat[j, "becomes"] <- 2
            
          } else if(vec[1] %in% c(1, 3, 7) & any(vec == 2)) { # TEST FOR STARTING AS FALLOW, BARE, OR WATER FOLLOWED BY CROP
            
            # If the first integer is fallow or water followed by crop, finalize as crop
            finMat[j, "is"] <- as.numeric(df[j, "TimeSeriesClass"])
            finMat[j, "becomes"] <- 2
            
          } else if(all(vec != 2) & any(vec == 3)) { # TEST FOR NO CROP BUT FALLOW EXISTS
            
            # if crop does not exist but fallow does, finalize as fallow
            finMat[j, "is"] <- as.numeric(df[j, "TimeSeriesClass"])
            finMat[j, "becomes"] <- 3
            
          } else {
            
            # if none of the above, finalize by majority
            cv.sort <- sort(colSums(cv), decreasing = TRUE)
            finMat[j, "is"] <- df[j, "TimeSeriesClass"]
            finMat[j, "becomes"] <- as.numeric(
              strsplit(names(cv.sort[1]), "t")[[1]][2]
            )
            
          }
        } else {
          # if there is NO crop or fallow
          if(length(which(cv >= 1)) > 2)  {
            
            # if there are more than 2 seasonal changes and no
            # finalize as "confused"
            finMat[j, "is"] <- as.numeric(df[j, "TimeSeriesClass"])
            finMat[j, "becomes"] <- 99
            
          } else {
            
            # if none of the above, finalize by majority
            cv.sort <- sort(colSums(cv), decreasing = TRUE)
            finMat[j, "is"] <- df[j, "TimeSeriesClass"]
            finMat[j, "becomes"] <- as.numeric(
              strsplit(names(cv.sort[1]), "t")[[1]][2]
            )
          }
          
        }
        
        
      }
    } 
    
    # quality check: is there are NAs, stop the function
    
    if(any(is.na(finMat[j,]))) {
      stop(paste0("NAs induced at iteration ", j))
    }
    
    
    # print every 500 iterations
    iteration.count <- 1000
    if(j %% iteration.count == 0) cat("loop 1: iteration ", j, "complete\n")
    
  } # close the loop
  
  # print status check: how many are confused?
  cat("\ntotal percent of confused tiles:" ,
      round(
        length(which(finMat[, "becomes"] == 99)) / nrow(df) * 100, 2))
  
  # print status check: how many are reclassed
  cat("\npercent of data reclassed:" ,
      round(
        length(which(finMat[, "is"] != 0)) / nrow(df) * 100, 2))
  
  # return the finalized reclassification matrix
  return(finMat)
  
  
} # close the function

### --- 1984 data ----
require(tidyverse)
# read file of model output
input.df <- read.table(
  file = "C:/Users/emily/OneDrive - University of Wyoming/Summer2019/MajorityMergedClasses1994.txt",
  header = TRUE,
  stringsAsFactors = FALSE)
input.df <- input.df %>% mutate(id = row.names(input.df))

# make lookup table of definitions

luClasses <- data.frame(
  cover = c("bareSoil", "crop", "fallow", "forest", "grassShrub", "water"),
  code = c(1, 2, 3, 4, 5, 7)
)
reclass1984 <- reclassFunction(df = input.df, classColumn = "TimeSeriesClass")
