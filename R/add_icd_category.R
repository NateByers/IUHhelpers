
#' @export
#' @import parallel
add_icd_category <- function(icd, icd_column, type = c("disease", "diagnosis"),
                           icd_lookup_column = "global_short_description",
                           parallel = FALSE,
                           cluster_num = parallel::detectCores() - 1) {
  # icd = diagnoses; icd_column = "diagnosis"; type = c("disease", "diagnosis"); icd_lookup_column =  "global_short_description"
  # parallel = TRUE; clusters = parallel::detectCores() - 1

  original_names <- names(icd)

  icd <- icd %>%
    dplyr::mutate(row_num = row_number())

  icd_distinct <- icd %>%
    dplyr::select_at(icd_column) %>%
    dplyr::distinct()

  icd_cleaned <- gsub("\\.", "", icd_distinct[[icd_column]])

  # filter_vector <- 1:20; icd_distinct <- icd_distinct %>% filter(row_number() %in% filter_vector); icd_cleaned <- icd_cleaned[filter_vector]

  if(parallel) {

    cl <- parallel::makeCluster(cluster_num)

    parallel::clusterEvalQ(cl, {
      icd_lookup <- IUHhelpers::icd_lookup
      NULL
    })

    icd_distinct[["matched_code"]] <- parallel::parSapply(cl, icd_cleaned, match_icd)

    parallel::stopCluster(cl)

  } else {

    icd_distinct[["matched_code"]] <- sapply(icd_cleaned, match_icd)

  }

  icd <- icd %>%
    dplyr::inner_join(icd_distinct, icd_column) %>%
    dplyr::left_join(icd_lookup, c("matched_code" = "icd_code")) %>%
    dplyr::arrange(row_num) %>%
    dplyr::select_at(c(original_names, icd_lookup_column))

  icd
}

match_icd <- function(code) {
  # icd <- icd_lookup; code <- "Z98890"
  matched_icd <- c()

  while(length(matched_icd) == 0 & nchar(code) > 1) {

    matched_icd <- icd_lookup[grepl(paste0("^", code), icd_lookup[["icd_code"]]), "icd_code"]

    code <- substr(code, 1, nchar(code) - 1)
  }

  if(length(matched_icd) == 0) {
    return(NA)
  }

  head(matched_icd[order(matched_icd)], 1)
}
