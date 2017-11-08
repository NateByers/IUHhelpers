#' Select features to use in a model
#' @export
#' @details The point to this function is to eliminate features in your data frame
#' based on the information given to the function. This information can be
#' expertly generated, automatically generated, or both. The fundamental assumption
#' is that you will want to remove highly correlated information. The rank_table
#' supplies the information needed to choose the best member of each highly
#' correlated group. The features data frame is returned with the chosen features.
#' The columns will be ordered from highest ranked (left) to lowest rank,
#' along with an attribute giving the rank information.
#' @param features A data frame of numeric features
#' @param corr_cutoff Minimum correlation used to make groups
#' @param rank_table A data frame of information for selecting features. The
#' first column should be named "variable" and have the name of each column in
#' the feature data frame. Each column after "variable" should be numeric column
#' for ranking the features. The order for each column should be increasing, i.e.
#' a rank of 1 will beat a rank of 2.
select_model_features <- function(features, corr_cutoff = .9, rank_table) {
  features <- mtcars; corr_cutoff = .75
  rank_table <- data.frame(variable = names(mtcars),
                           priority = c(rep(1, 5), rep(2, 6)),
                           rank = c(1, rep(NA, 3), 2, 1:3, rep(NA, 3)),
                           stringsAsFactors = FALSE)

  groups <- find_corr_groups(features, cutoff = corr_cutoff)

  if(length(groups) != 0) {

    for(i in groups) {
      # i = groups[[1]]

      rank_columns <- names(rank_table)[names(rank_table) != "variable"]

      group <- rank_table %>%
        dplyr::filter(variable %in% i)

      while(length(rank_columns) > 0 & nrow(group) > 1) {

        if(sum(!is.na(group[[rank_columns[1]]])) == 0) {

          rank_columns <- rank_columns[-1]

          next

        } else {

          group <- group %>%
            dplyr::filter_at(rank_columns[1], any_vars(. == min(., na.rm = TRUE)))

          rank_columns <- rank_columns[-1]

        }
      }

      if(nrow(group) > 1) {

        corr <- features %>%
          dplyr::select_at(group$variable) %>%
          cor(use = "na.or.complete") %>%
          as.table() %>%
          as.data.frame() %>%
          filter(Var1 != Var2)

        sum_cor <- sapply(group$variable, function(variable, corr) {
          corr_var <- corr %>%
            dplyr::filter(Var1 == variable | Var2 == variable)
          sum(corr_var$Freq, na.rm = TRUE)
        }, corr = corr)

        min_corr_variable <- group$variable[sum_cor == min(sum_cor, na.rm = TRUE)]

        group <- group %>%
          dplyr::filter(variable == min_corr_variable[1])

      }
    }
  } else {

    warning("no correlation groups found")

  }
}
