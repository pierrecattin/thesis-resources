% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq.vs.accuracy.plot.R
\name{freq.vs.accuracy.plot}
\alias{freq.vs.accuracy.plot}
\title{Plot frequency vs accuracy of prediction. Single plot}
\usage{
freq.vs.accuracy.plot(y, probs, conf, set, ytitle = T, legend = T,
  granularity)
}
\arguments{
\item{y}{numeric -1, +1}

\item{probs}{numeric indicating probability of up}

\item{conf}{numeric indicating if confidence intervall to be plotted (if missing, no intervall is plotted)}

\item{set}{character (used for title)}

\item{ytitle}{logical indicating if y title should be diplayed}

\item{legend}{logical indicating if y legend should be displayed}

\item{granularity}{numeric indicating granularity for the sequence of min support}
}
\value{
ggplot
}
\description{
Plot frequency vs accuracy of prediction. Single plot
}
