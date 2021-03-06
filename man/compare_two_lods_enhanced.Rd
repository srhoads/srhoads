% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{compare_two_lods_enhanced}
\alias{compare_two_lods_enhanced}
\title{Samantha Rhoads's function to compare the columns of two lists of dataframes to see their differences based on their `sumry()`s and if the column `sumry()`s are `identical()`}
\usage{
compare_two_lods_enhanced(
  list1 = list(df = dfsampler()),
  list2 = list(df = dfsampler(which = "short")),
  exampleOfColumnName = "^name$",
  print_only_differences = T
)
}
\description{
Samantha Rhoads's function to compare the columns of two lists of dataframes to see their differences based on their `sumry()`s and if the column `sumry()`s are `identical()`
}
\examples{
list1=list(nvc=NursesVC); list2=list(nvc=NursesVC2); exampleOfColumnName="EEID"; print_only_differences=T
compare_two_lods_enhanced(list1 = list(df1 = dfsampler()), list2 =  list(df2 = dfsampler(which="short")), exampleOfColumnName="^name$")
}
