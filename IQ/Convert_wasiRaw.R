Convert_wasiRaw = function(DATA, Type = "Vocab", TabFile){
  require(rio); require(tidyverse)
  
  TABLE = import(TabFile) %>% filter(Subtest %in% Type)
  TABLE$DEC = sapply(TABLE$Age,function(x) strsplit(x,split = ":")[[1]][2]) %>% as.numeric()/12
  TABLE$Age = sapply(TABLE$Age,function(x) strsplit(x,split = ":")[[1]][1]) %>% as.numeric()
  TABLE$Age = rowSums(TABLE[,c("Age","DEC")], na.rm=T)
  
  tmp = DATA %>% select(Age, contains(paste0("WASI_", Type))) %>% 
    mutate(ROW = NA,NEW=NA, TYPE=Type)
  names(tmp)[2:3] = c("RAW","T")

  for(i in 1:nrow(tmp)){
    k = tmp$Age[i]-TABLE$Age
    tmp$ROW[i] = ifelse(is_empty(k), NA, which.min(k[k>0]))
  } 

  for(i in 1:nrow(tmp)){
    k = tmp[i,"RAW"]-TABLE[tmp$ROW[i],3:ncol(TABLE)]
    k = ifelse(is_empty(k), NA, which.min(k[k>=0]))
    tmp$NEW[i] = ifelse(is.na(k), NA, names(TABLE)[k+2] %>% as.numeric())
  }

  #Merge this with existing data, the TABLE does not have scores for below 16years
  tmp[,"T"] = ifelse(is.na(tmp$NEW), tmp[,"T"], tmp$NEW)
  
  DATA2 = DATA
  DATA2[,paste0("WASI_", Type,"_T")] = tmp[,"T"]
  
  return(DATA2)
}

