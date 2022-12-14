% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/severance_palettes.R
\name{severance_palette}
\alias{severance_palette}
\title{Severance palette generator}
\usage{
severance_palette(name, n, type = c("discrete", "continuous"))
}
\arguments{
\item{name}{Name of desired palette. Choices are:
\code{Dinner}, \code{Half}, \code{Hell}, \code{HideAndSeek}, \code{Jazz01}, \code{Jazz02}, \code{TheYouYouAre}}

\item{n}{Number of colors desired. If omitted, uses all colours.}

\item{type}{Either "continuous" or "discrete". Use continuous if you want
to automatically interpolate between colors.}
}
\value{
A vector of colors.
}
\description{
These are a handful of color palettes from the show Severance
}
\examples{
severance_palette("Jazz02")
severance_palette("Hell")
severance_palette("TheYouYouAre")
severance_palette("Jazz01", 3)

# If you need more colors than normally found in a palette, you
# can use a continuous palette to interpolate between existing
# colors
pal <- severance_palette(name = "Jazz02", n = 2, type = "continuous")
}
\keyword{colors}
