% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc.byset.byhoriz.plot.R
\name{roc.byset.byhoriz.plot}
\alias{roc.byset.byhoriz.plot}
\title{Frequency vs accuracy of prediction for all 3 sets and all horizons}
\usage{
roc.byset.byhoriz.plot(spec.sens, stock, model)
}
\arguments{
\item{spec.sens}{data frame with columns Specificity, Sensitivity, Set, Horizon}

\item{stock}{stock name for title}

\item{model}{model name for title}
}
\value{
4 by 3 facets ggplot
}
\description{
Frequency vs accuracy of prediction for all 3 sets and all horizons
}
