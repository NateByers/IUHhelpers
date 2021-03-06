% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_nps.R
\name{calc_nps}
\alias{calc_nps}
\title{Calculate NPS}
\usage{
calc_nps(scores, detractors = 0:6, neutrals = 7:8, promoters = 9:10)
}
\arguments{
\item{scores}{Numeric vector of promoter scores}

\item{detractors}{Scores that are categorized as detractors}

\item{neutrals}{Scores that are categorized as neutrals}

\item{promoters}{Scores that are categorized as promoters}
}
\description{
Calculate NPS
}
\examples{
promoter_scores <- c(1, 8, 8, 10, 5, NA, 9)

calc_nps(promoter_scores)
}
