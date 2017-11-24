## script that convert WAIS t-scores to IQ scores
#####README! ####
# requires: tidyverse  
# input:
    # IN:       array of T-scores data 
    # table:    conversion table:
                        #WASI.Full.2tests --> IQ Equivalents of Sums of T scores: Full Scale-2 subtests (pp. 192-193, WASI manual)
                        #WASI.Full.4tests --> IQ Equivalents of Sums of T scores: Full Scale-4 subtests (pp. 188-191, WASI manual)
#Output: 
    #array with converted IQ scores. Wrong data is denoted with a "-1"



##### start script ####
#install.packages("tidyverse")                # uncomment if necessary
#library(tidyverse)                           # uncomment if necessary  
wdir="M:/MOAS/WASI_conversion"                #change accordingly
setwd(wdir)

## function definition ##
wasi.conversion <- function(IN,type) {
  Int <- IN
  
  wasi.tb <- as.matrix(read_excel("conversion_tables.xlsx", sheet =type, col_names = FALSE))
  Int[IN < min(wasi.tb[,1])] <-  999
  Int[IN > max(wasi.tb[-length(wasi.tb[,1]),1])] <-  999
  inds <- match(Int, wasi.tb[,1])
  OUT <- wasi.tb[inds,2]
  return(OUT)
  }
  

## invoking function - input here your T-scores arrays and your desired conversion table##
Tscores <- c(35,100,82,130, 180, 111)           # array with t-scores
tb <- "WASI.Full.4tests"                        # WASI.Full.2tests or WASI.Full.4tests

IQ <- wasi.conversion(IN=Tscores, type=tb)


