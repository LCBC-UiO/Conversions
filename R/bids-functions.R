
#' Create BIDS type structure
#' 
#' Function to return the equivalent
#' BIDS-type reference for a specific 
#' set of data.
#'
#' @param ID CrossProject_ID
#' @param session Subject_Timepoint
#' @param site Site_Name
#' @param type "file" or "folder"
#'
#' @return character in BIDS compliant format
#' @export
#' @examples
#' bidsify(1100300, 2)
#' bidsify(1100300, 2, "ousAvanto")
#' bidsify(1100300, 2, "ousAvanto", type = "folder")
bidsify <- function(ID, session, site=NULL, type="file"){
  
  type <- match.arg(type, c("file", "folder"))
  
  seep <- switch(type,
                 file = "_",
                 folder = "/")
  
  tmp <- paste(
    paste0("sub-", ID), 
    paste0("ses-", sprintf("%02d",session)),
         sep=seep)
  
  if(!is.null(site)){
    
    k <- match.arg(site, 
                   choices = c("ousAvanto",
                               "ousSkyra",
                               "ousPrisma",
                               "ntnuAvanto", 
                               "curatoAvanto"))

    tmp <- paste0(tmp, k)
  }
  
  tmp
}