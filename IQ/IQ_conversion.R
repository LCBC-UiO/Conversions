wasi.conversion = function(IN, nSubtests=2) {
  require(tidyverse); require(readxl)
  
  # Get correct conversion table
  type = paste0("WASI.Full.", nSubtests ,"tests")
  wasi.tb = as.matrix(read_excel("conversion_tables.xlsx", sheet =type, col_names = FALSE))
  
  #set t-values outside allowed range to 999
  Int = IN %>% 
    ifelse(. < min(wasi.tb[,1]) | . > max(wasi.tb[-length(wasi.tb[,1]),1]), 
           999, .) 
  
  #Index the t-scores to the wasi table
  inds = match(Int, wasi.tb[,1])
  
  #Get the values from the wasi table at the extrected indeces
  OUT = wasi.tb[match(Int, wasi.tb[,1]),2]
  
  return(OUT)
}


