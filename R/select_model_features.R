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

  corr_groups <- find_corr_groups(features, cutoff = corr_cutoff)
  removed_features <- c()

  if(length(corr_groups) != 0) {

    for(i in corr_groups) {

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

        min_corr_feature <- features %>%
          find_min_correlated_feature(group$variable)

        group <- group %>%
          dplyr::filter(variable == min_corr_feature[1])

      }

      removed_features <- c(removed_features, i[i != group$variable])

    }
  } else {

    warning("no correlated groups found, so no features removed")

    corr_groups <- NA
    removed_features <- NA
  }

  rank_table <- rank_table %>%
    dplyr::filter(!variable %in% removed_features) %>%
    dplyr::arrange_at(names(rank_table)[names(rank_table) != "variable"])

  features <- features %>%
    dplyr::select_at(rank_table$variable) %>%
    as.data.frame()

  attr(features, "correlated_groups") <- corr_groups
  attr(features, "removed_features") <- removed_features

  features
}

find_min_correlated_feature <- function(features, group) {
  # features <- features %>% dplyr::select_at(group$variable)
  corr <- features %>%
    cor(use = "na.or.complete") %>%
    as.table() %>%
    as.data.frame() %>%
    filter(Var1 != Var2)

  sum_cor <- sapply(group, function(variable, corr) {
    corr_var <- corr %>%
      dplyr::filter(Var1 == variable | Var2 == variable)
    sum(corr_var$Freq, na.rm = TRUE)
  }, corr = corr)

  min_corr_feature <- group[sum_cor == min(sum_cor, na.rm = TRUE)]

  min_corr_feature
}
