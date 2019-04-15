#' Transform vector into strings that can be headers in a data.frame
#' @export
#' @param x Character vector of column names
#' @examples 
#' column_names <- c("MRN ID", "First & Middle Name", "Name: Last")
#' 
#' cleanup_headers(column_names)
cleanup_headers <- function(x) {

  x <- trimws(x)
  x <- gsub("&", "and", x)
  x <- gsub("\\s", "_", x)
  x <- gsub("\\/", "_", x)
  x <- gsub("-|\\.", "_", x)
  x <- gsub(";|:", "_", x)
  x <- gsub("\\(|\\)", "_", x)
  x[grepl("^\\d", x)] <- paste0("X", x[grepl("^\\d", x)])
  x <- gsub("_{2,}", "_", x)

  x
}
