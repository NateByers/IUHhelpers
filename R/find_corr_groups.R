#' Find highly correlated groups for dimension reduction
#' @import dplyr igraph
#' @export
#' @param data A data.frame or matrix of features for modeling
#' @param cutoff Minimum correlation used to make groups
#' @param cor_mat If TRUE, a list of correlation matrices are returned
#' @examples
#' find_corr_groups(mtcars)
#' find_corr_groups(mtcars, cor_mat = TRUE)
find_corr_groups <- function(data, cutoff = .9, cor_mat = FALSE) {
  corr <- cor(data, use = "na.or.complete")
  corr <- corr*lower.tri(corr)
  check_corr <- which(corr >= cutoff, arr.ind = TRUE)

  graph_cor <- igraph::graph.data.frame(check_corr, directed = FALSE)
  groups_corr <- split(unique(as.vector(check_corr)),
                       igraph::clusters(graph_cor)$membership)
  redundant_groups <- lapply(groups_corr, function(list_corr){
    rownames(corr)[list_corr]
  })

  if(cor_mat) {
    redundant_groups <- lapply(redundant_groups, function(group_names) {
      dplyr::select_at(data, .vars = group_names) %>%
        cor(use = "na.or.complete")
    })
  }

  redundant_groups
}
