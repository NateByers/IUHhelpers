#' Calculate NPS Standard Error
#' 
#' @export
#' @param scores Numeric vector of promoter scores
#' @param detractors Scores that are categorized as detractors
#' @param neutrals Scores that are categorized as neutrals
#' @param promoters Scores that are categorized as promoters
#' @examples 
#' promoter_scores <- c(1, 8, 8, 10, 5, NA, 9)
#' 
#' calc_nps_std_error(promoter_scores) 
calc_nps_std_error <- function(scores, detractors = 0:6, neutrals = 7:8,
                               promoters = 9:10) {
  
  detractors <- sum(scores %in% detractors)
  neutrals <- sum(scores %in% neutrals)
  promoters <- sum(scores %in% promoters)
  
  total <- detractors + neutrals + promoters
  
  NPS <- (promoters - detractors)/total
  
  variance <- ((1 - NPS)^2)*(promoters/total) + ((0 - NPS)^2)*(neutrals/total) + ((-1 - NPS)^2)*(detractors/total)
  
  marginal_error <- sqrt(variance)/sqrt(total)
  
  marginal_error*100
}