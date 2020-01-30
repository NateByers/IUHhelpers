#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @export
connect_Azure_EDW <- function(DSN = "Azure") {
  con <- try(DBI::dbConnect(odbc::odbc(), DSN))
  
  if(class(con) == "try-error") {
    DBI_packages <- installed.packages() %>%
      as.data.frame() %>%
      dplyr::filter(Package == "DBI")
    
    for(i in row.names(DBI_packages)) {
      
      library_ <- as.character(DBI_packages[i, "LibPath"])
      
      remove.packages("DBI", lib = library_)
      
      install.packages("DBI", lib = library_)
    }
  }
  
  DBI::dbConnect(odbc::odbc(), DSN)
  
}