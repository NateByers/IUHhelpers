#' Transform vector into strings that can be headers in a data.frame
#' @export
cleanup_for_headers <- function(x) {

  x <- trimws(x)
  x <- gsub("&", "and", x)
  x <- gsub("\\s", "_", x)
  x <- gsub("\\/", "_", x)
  x <- gsub("-", "_", x)
  x <- gsub(";|:", "_", x)
  x <- gsub("\\(|\\)", "_", x)
  x[grepl("^\\d", x)] <- paste0("X", x[grepl("^\\d", x)])

  x
}
