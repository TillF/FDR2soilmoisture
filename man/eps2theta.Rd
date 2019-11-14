\name{eps2theta}
\alias{eps2theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert epsilon (permittivity) to theta (volumetric water content).
}
\description{
Convert epsilon (permittivity) to theta (volumetric water content) using different equations.
}
\usage{
eps2theta(epsdata, equation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{epsdata}{
dataframe holding a column \code{epsilon} and potentially others, depending on the choice of \code{equation}.
}
  \item{equation}{
Equation to be used. Must be one of "deltaT_minorg", "FerschEtal2017", "ToppEtal1980", "MalickiEtal1996", "Jacobsen_Schjonning1993".
}
}
\details{
Applies various equations, most of which are listed in Mohamed & Paleologos, 2018: 

    

}
\value{
A vector of reals, i.e. theta (volumetric water content) [-].

For \code{equation=="list"}, a character vector of all supported equations is returned.
}
\references{
Mohamed, A.-M.O., Paleologos, E.K., 2018. Dielectric Permittivity and Moisture Content, in: Fundamentals of Geoenvironmental Engineering. Elsevier, pp. 581–637. https://doi.org/10.1016/B978-0-12-804830-6.00016-8

}
\author{
Till Francke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{correct_sensor_voltage}}
}
\examples{
#return list of supported equations
eps2theta(epsdata = NULL, equation = "list")
}
