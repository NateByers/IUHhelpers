#' Process a data.frame for a Sankey network graph
#' @import dplyr rlang
#' @export
#' @param dat A \code{data.frame}.
#' @param lineage A vector of column names in descending order.
#' @param value Name of the value column.
#' @param drop_NAs Logical indicating what should be done with NA sources and targets.
#' @return A list of two \code{data.frame}s: links and nodes.
#' @examples
#'library(EDW)
#'library(networkD3)
#'
#'edw <- connect_edw() %>%
#'  tbl_(c(e = "facilitybillline"), schema = "finance") %>%
#'  inner_join_(c(en = "encounter"), c("e.[encounter id]" = "en.[encounter id]"),
#'              schema = "population") %>%
#'  filter_("e.[revenue group category] == 'LAB'") %>%
#'  select_(c("en.[encounter type]", "en.[GL company]", "e.[charge line total]")) %>%
#'  group_by_(c("en.[encounter type]", "en.[GL company]")) %>%
#'  summarise_(c("charge_total" = "sum(e.[charge line total])")) %>%
#'  run_query()
#'
#'edw <- process_sankey(dat = edw, c("encounter type", "GL company"),
#'                      value = "charge_total")
#'
#'sankeyNetwork(Links = edw$links, Nodes = edw$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              units = "dollas")
process_sankey <- function(dat, lineage, value, drop_NAs = TRUE) {
  # lineage <- c("kind", "dept", "type"); value = "total"; drop_NAs = TRUE
  links <- dat[, ] %>%
    group_by_at(.vars = c(lineage[1:2])) %>%
    summarize_at(.vars = value, sum) %>%
    as.data.frame()

  names(links) <- c("parent", "child", "value")

  if(length(lineage) > 2) {
    for(i in 3:length(lineage)) {
      dat_ <- dat %>%
        group_by_at(.vars = c(lineage[(i -1):i])) %>%
        summarize_at(.vars = value, sum) %>%
        as.data.frame()
      names(dat_) <- c("parent", "child", "value")
      links <- rbind(links, dat_)
    }
  }

  links <- links %>%
    filter(value > 0)

  if(drop_NAs) {
    links <- links %>%
      filter_all(all_vars(!is.na(.)))
  } else {
    for(i in 1:2) {
      if(class(links[, i]) == "character") {
        links[is.na(links[, i]), i] <- "NA"
      }
    }
  }

  nodes <- data.frame(name = unique(c(links$parent, links$child)),
                      index = seq_along(unique(c(links$parent, links$child))) - 1,
                      stringsAsFactors = FALSE)

  links <- links %>%
    left_join(nodes, c("parent" = "name")) %>%
    rename(source = index) %>%
    left_join(nodes, c("child" = "name")) %>%
    rename(target = index) %>%
    select(source, target, value) %>%
    as.data.frame()

  nodes <- nodes %>%
    select(name) %>%
    as.data.frame()

  list(nodes = nodes, links = links)
}


