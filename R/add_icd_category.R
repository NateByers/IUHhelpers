#' Add a column of ICD categories to a data.frame
#' @export
#' @importFrom dplyr %>% mutate select select_at distinct filter inner_join left_join arrange row_number 
#' @importFrom parallel makeCluster parSapply stopCluster
#' @param icd A data.frame with ICD 9 or 10 diagnoses.
#' @param icd_column The name of the column of ICD codes.
#' @param icd_type Are these diagnosis or procedure codes (diagnosis default).
#' @param icd_lookup_column Which column from \code{icd_lookup} should be returned. You
#'                          can see this data frame by running \code{data(icd_lookup)}.
#' @param parallel Use parallel processing?
#' @param cluster_num Number of clusters to use if \code{parallel} is TRUE.
#' @examples 
#' x <- data.frame(icd = c("43852", "M93929", "W002XXA"))
#' 
#' add_icd_category(icd = x, icd_column = "icd")
add_icd_category <- function(icd, icd_column, icd_type = c("diagnosis", "procedure"),
                             icd_lookup_column = "global_short_description",
                             parallel = FALSE,
                             cluster_num = parallel::detectCores() - 1) {
  
  original_names <- names(icd)

  icd <- icd %>%
    dplyr::mutate(row_num = row_number())

  icd_distinct <- icd %>%
    dplyr::select_at(icd_column) %>%
    dplyr::distinct()

  icd_cleaned <- gsub("\\.", "", icd_distinct[[icd_column]])

  lookup <- IUHhelpers::icd_lookup
  
  lookup <-  lookup %>%
    dplyr::filter(type == icd_type[1])

  if(parallel) {

    cl <- parallel::makeCluster(cluster_num)

    icd_distinct[["matched_code"]] <- parallel::parSapply(cl, icd_cleaned,
                                                          match_icd, lookup = lookup)

    parallel::stopCluster(cl)

  } else {

    icd_distinct[["matched_code"]] <- sapply(icd_cleaned, match_icd, lookup = lookup)

  }
  
  icd <- icd %>%
    dplyr::inner_join(icd_distinct, icd_column) %>%
    dplyr::left_join(select_at(lookup, c("icd_code", icd_lookup_column)),
                     c("matched_code" = "icd_code")) %>%
    dplyr::arrange(row_num) %>%
    dplyr::select_at(c(original_names, icd_lookup_column))

  icd
}

match_icd <- function(code, lookup) {

  matched_icd <- c()

  while(length(matched_icd) == 0 & nchar(code) > 1) {

    matched_icd <- lookup[grepl(paste0("^", code), lookup[["icd_code"]]), "icd_code"]

    code <- substr(code, 1, nchar(code) - 1)
  }

  if(length(matched_icd) == 0) {
    return(NA)
  }

  head(matched_icd[order(matched_icd)], 1)
}
