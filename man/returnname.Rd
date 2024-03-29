% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{returnname}
\alias{returnname}
\title{A function to get the name of an object to use for other stuff. Ie: get the names of the object/data and make it a variable in the data. 
This function allows you to get the name of an object to use for other stuff. Ie: get the names of the object/data and make it a variable in the data. 
Idea derived from: https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function about: In R, how to get an object's name after it is sent to a function?}
\usage{
returnname(z)
}
\description{
A function to get the name of an object to use for other stuff. Ie: get the names of the object/data and make it a variable in the data. 
This function allows you to get the name of an object to use for other stuff. Ie: get the names of the object/data and make it a variable in the data. 
Idea derived from: https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function about: In R, how to get an object's name after it is sent to a function?
}
\examples{
Using `returnname()` to get name of dataframe, then making a column in the df the name of the dataframe
returnname(mtcars) # returns "mtcars"
returnname(z)
}
