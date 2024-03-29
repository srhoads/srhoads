% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{writexl_open}
\alias{writexl_open}
\title{Samantha Rhoads's function to write to Excel just like writexl::write_xlsx (same args) but you tell the shell to open the file (in Excel); fxn returns the dataframe object!}
\usage{
writexl_open(
  x,
  path = tempfile(fileext = ".xlsx"),
  col_names = T,
  format_headers = T,
  use_zip64 = F,
  open_file = T
)
}
\description{
Samantha Rhoads's function to write to Excel just like writexl::write_xlsx (same args) but you tell the shell to open the file (in Excel); fxn returns the dataframe object!
}
\examples{
writexl_open(x, path=tempfile(fileext=".xlsx"), col_names=T, format_headers=T, use_zip64=F)
}
