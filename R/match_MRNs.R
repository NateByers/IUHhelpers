#' Match MRNs from two EMRs
#' 
#' Takes two data.frame objects with patient information and matches them
#' 
#' @import stringdist
#' @export
#' @param x,y Two data frames with columns `mrn`, `first_name`, `last_name`, and 
#' `birth_date` (yyyy-mm-dd)
#' @return A data.frame with column names with suffixes '_x' and '_y' corresponding
#' to the x and y inputs. The dist column gives the string distance between the 
#' two first names.
#' @details The function cleans up the last names slightly and then matches exactly
#' on the cleaned up last names and the birth dates. Then fuzzy matching is done
#' on the first name, using the \code{stringdist} package.
#' @examples 
#' data(y_mrn)
#' data(x_mrn)
#' 
#' match_MRNs(x_mrn, y_mrn)
match_MRNs <- function(x, y){
  
  column_names <- c("mrn", "first_name", "last_name", "birth_date")
  
  x_names <- tolower(names(x))
  y_names <- tolower(names(y))
  
  if(sum(column_names %in% x_names) != length(column_names) |
     sum(column_names %in% x_names) != length(column_names)) {
    stop(paste("must have the following column names in both data frames:",
               paste(column_names, collapse = ", ")))
  }
  
  names(x) <- x_names
  names(y) <- y_names
  
  x <- x %>%
    dplyr::select(!!column_names)
  y <- y %>%
    dplyr::select(!!column_names)
  
  names(x) <- paste0(names(x), "_x")
  names(y) <- paste0(names(y), "_y")
  
  x <- x %>%
    dplyr::mutate(birth_date = birth_date_x,
                  last = tolower(last_name_x),
                  last = remove_suffix(last),
                  last = remove_space(last),
                  first_x = tolower(first_name_x),
                  first_x = remove_suffix(first_x),
                  first_x = remove_space(first_x))
  
  y <- y %>%
    dplyr::mutate(birth_date = birth_date_y,
                  last = tolower(last_name_y),
                  last = remove_suffix(last),
                  last = remove_space(last),
                  first_y = tolower(first_name_y),
                  first_y = remove_suffix(first_y),
                  first_y = remove_space(first_y))
  
  matches <- x %>%
    dplyr::inner_join(y, c("last", "birth_date")) %>%
    dplyr::mutate(dist = stringdist::stringdist(first_x, first_y)) %>%
    dplyr::filter(dist <= 3) %>%
    dplyr::select(!!c(paste0(column_names, "_x"),
                      paste0(column_names, "_y"),
                      "dist"))
  
  
  matches
  
}

remove_suffix <- function(x) {
  sub("[Jj]r\\.?$|[Ss]r\\.?$", "", x)
}

remove_space <- function(x) {
  gsub("\\s|\\.|-", "", x)
}
