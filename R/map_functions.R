#' Calculate mean arterial pressure
#' 
#' @param DATA data frame containing necessary columns
#'
#' @importFrom dplyr group_by mutate select contains
#' @importFrom magrittr '%>%'
calc_map_mean = function(DATA){
  # Compute blood pressure composite measure (MAP mean)
  
  #Calculate means for the two times blood pressure was taken
  DATA = DATA %>% 
    dplyr::group_by(CrossProject_ID, Subject_Timepoint) %>% 
    dplyr::mutate(DELETE1=mean(c(BloodPress_Diastolic_1,BloodPress_Diastolic_2), na.rm=T),
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
    dplyr::select(-dplyr::contains("DELETE")) %>%
    dplyr::mutate(BloodPress_MapMean = ((DATA$BloodPress_Diastolic_Mean*2)+DATA$BloodPress_Systolic_Mean)/3)
  
  return(DATA)
}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("BloodPress_Diastolic_Mean", "BloodPress_Systolic_Mean",
                                                        "BloodPress_MapMean", 
                                                        "BloodPress_Systolic_1", "BloodPress_Systolic_2",
                                                        "BloodPress_Diastolic_1","BloodPress_Diastolic_2",
                                                        "CrossProject_ID", "Subject_Timepoint"))
