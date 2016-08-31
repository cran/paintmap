\name{paintmap-package}
\alias{paintmap-package}
\docType{package}
\title{
Plotting paintmaps
}
\description{
Plots matrices of colours as grids of coloured squares - aka heatmaps, 
guaranteeing legible row and column names, 
without transformation of values, 
without re-ordering rows or columns,
and without dendrograms.
}
\details{
The function `bhm' takes a matrix of colours (i.e. a character matrix of descriptions like red or hex-codes), and creates a plot using `grid' graphics.
}
\author{
Daniel Greene
Maintainer: Daniel Greene <dg333@cam.ac.uk>
}
\keyword{ heatmap }
\examples{
paintmap(matrix(heat.colors(9), 3, 3, dimnames=list(letters[1:3], letters[4:6])))
}
