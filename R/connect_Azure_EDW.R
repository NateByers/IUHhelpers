#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @export
connect_Azure_EDW <- function(DSN = "Azure") {
  
  con <- try(DBI::dbConnect(odbc::odbc(), DSN), silent = TRUE)
  cnt <- 0
  
  while(inherits(con, 'try-error') && cnt < 3) {
    cnt <- cnt + 1
    reload_dbi()
    con <- try(DBI::dbConnect(odbc::odbc(), DSN), silent = TRUE)
  }
  
  if(inherits(con, 'try-error')) {
    stop("Unable to reinstall DBI. Aborting", call. = FALSE)
  } else {
    return(con)
  }

}

reload_dbi <- function() {
  local({
    library(IUHhelpers)
    library(odbc)
    library(DBI)
    
    detach("package:IUHhelpers", unload = TRUE, character.only = TRUE)
    detach("package:odbc", unload = TRUE, character.only = TRUE)
    detach("package:DBI", unload = TRUE, character.only = TRUE)
    install.packages("DBI")
  }, envir = .GlobalEnv)
  
}
