
ds_calc_total <- function(forward, backward){
  forward + backward
}

ds_calc_ratio <- function(forward, backward){
  backward/forward
}

cvlt_a_calc <- function(data, cols = dplyr::matches("CVLT_A[1-5]")){

  mutate(data,
         tmp = ifelse(!is.na(CVLT_A1), 
                      dplyr::select(., {{cols}}) %>% rowSums(.,na.rm=T),
                      CVLT_A_Total)
  )
  data$tmp
}

delay_days_calc <- function(data, test_date){

  dplyr::mutate(data,
         tmp =  ifelse(!is.na( {{test_date}} ), 
                       {{test_date}}-Test_Date, 
                       DelayedMem_Days)
  )
  data$tmp
}


bids_spec <- function(CrossProject_ID, Subject_Timepoint, Site_Name){
  ifelse(Site_Name != "noMRI", 
         paste0("sub-",CrossProject_ID,"/ses-", 
                str_pad(Subject_Timepoint, width = 2, side = "left", pad="0"), 
                Site_Name),
         NA)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("CVLT_A1", "CVLT_A_Total"))
}

