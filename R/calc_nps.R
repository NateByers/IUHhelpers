#' Calculate NPS
#' 
#' @export
#' @param scores Numeric vector of promoter scores
#' @param detractors Scores that are categorized as detractors
#' @param neutrals Scores that are categorized as neutrals
#' @param promoters Scores that are categorized as promoters
#' @examples 
#' promoter_scores <- c(1, 8, 8, 10, 5, NA, 9)
#' 
#' calc_nps(promoter_scores)
calc_nps <- function(scores, detractors = 0:6, neutrals = 7:8, promoters = 9:10) {
  
  detractors <- sum(scores %in% detractors)
  neutrals <- sum(scores %in% neutrals)
  promoters <- sum(scores %in% promoters)
  
  total <- detractors + neutrals + promoters
  
  NPS <- (promoters - detractors)/total
  
  NPS*100
}
