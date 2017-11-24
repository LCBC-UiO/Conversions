Calc_MapMean = function(DATA){
  require(tidyverse)
  # Compute blood pressure composite measure (MAP mean)
  
  #Calculate means for the two times blood pressure was taken
  DATA = DATA %>% 
    group_by(CrossProject_ID, Subject_Timepoint) %>% 
    mutate(DELETE1=mean(c(BloodPress_Diastolic_1,BloodPress_Diastolic_2), na.rm=T),
           DELETE2=mean(c(BloodPress_Systolic_1,BloodPress_Systolic_2), na.rm=T)) %>% 
    as.data.frame()
  
  # Some participants have means punched, not raw, this if does so that if the calculated exist use that instead of punched.
  DATA$BloodPress_Diastolic_Mean = ifelse(!is.na(DATA$BloodPress_Diastolic_Mean)
                                          ,DATA$BloodPress_Diastolic_Mean,
                                          DATA$DELETE1)
  
  # Some participants have means punched, not raw, this if does so that if the calculated exist use that instead of punched.
  DATA$BloodPress_Systolic_Mean = ifelse(!is.na(DATA$BloodPress_Systolic_Mean)
                                         ,DATA$BloodPress_Systolic_Mean,
                                         DATA$DELETE2)
  
  #Delete uncecessary columns, and calculate the MAP mean
  DATA = DATA %>% 
    select(-contains("DELETE")) %>%
    mutate(BloodPress_MapMean = ((DATA$BloodPress_Diastolic_Mean*2)+DATA$BloodPress_Systolic_Mean)/3)
  
  return(DATA)
}