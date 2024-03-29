% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{gsub_ic}
\alias{gsub_ic}
\title{A function to do gsub() but ignoring case as default & pasting a vec instead of just a string}
\usage{
gsub_ic(
  pattern,
  replacement,
  x,
  ignore.case = T,
  perl = F,
  fixed = F,
  useBytes = F,
  exact = F
)
}
\description{
A function to do gsub() but ignoring case as default & pasting a vec instead of just a string
}
\examples{
gsubic(pattern, to, v, exact=F, ignore.case=T)
}
