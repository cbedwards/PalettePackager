% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_colorvec.R
\name{make_colorvec}
\alias{make_colorvec}
\title{Returns a color vector based on name}
\usage{
make_colorvec(palette.name = "wolfe2014")
}
\arguments{
\item{palette.name}{Name of the color palette to use. Current options include: "wolfe2014"}
}
\value{
A named character vector, with each entry giving a color,
and the corresponding name giving the associated species or other identifier.
}
\description{
This is the workhorse of the package, storing vectors of names and colors based
on the publications of the wolfe lab. Can be used to get the color vectors directly,
also used internally in more user-friendly functions.
}
\details{
This function comes pre-populated with an example palette, "wolfe2014", based on the colors
and species shown in \href{https://doi.org/10.1016/j.cell.2014.05.041}{Wolfe et al. 2014}.

To develop color palettes for your own package, see \code{palette_augment_helper()}. To add the palette
to this function, see \code{augment2palette()}.
}
\examples{
make_colorvec(palette.name = "wolfe2014")
}
