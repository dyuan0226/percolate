% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_boards}
\alias{read_boards}
\title{read_boards() function for reading from a txt file to a list of boards}
\usage{
read_boards(file)
}
\arguments{
\item{file}{specifies the path of the input file}
}
\value{
a list of boards as specified inside the file
}
\description{
read_boards() function for reading from a txt file to a list of boards
}
\examples{
read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")
}
