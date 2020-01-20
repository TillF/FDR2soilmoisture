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
dataframe holding at least a column \code{epsilon} and potentially others, depending on the choice of \code{equation}. See details.
}
  \item{equation}{
Equation to be used. Must be one of "deltaT_minorg", "FerschEtal2017", "ToppEtal1980", "MalickiEtal1996", "Jacobsen_Schjonning1993".
}
}
\details{
Applies various equations, most of which are listed in Mohamed & Paleologos, 2018.
The following columns in the data frame \code{epsdata} are supported and may be required, depending on the choice of the equation:
\itemize{
\item{epsilon} { Electric permittivity. [-]} 
\item{soil} { Soil type indicator. \code{"mineral", "organic"}} 
\item{n} { Soil porosity. [-]}
\item{BD} { dry Bulk density. [g/cm³]}
\item{clay_perc} { Percent clay. [\%]}
\item{om_perc} { Percent organic matter. [\%]}
}
   
The respective equations used are


Malicki et al., 1996:
\deqn{\theta =  \frac{\sqrt{\epsilon}-0.819-0.168\rho-0.159\rho^2}{7.17+1.18\rho}
}{%
\theta =  (\sqrt{\epsilon}-0.819-0.168\rho-0.159\rho^2)/(7.17+1.18\rho).
}

}
\value{
A vector of reals, i.e. theta (volumetric water content) [-].

For \code{equation=="list"}, a named list of all supported equations is returned. The elements in the list are character strings, denoting the necessary input fields for the equations.
}

\references{
Delta-T Devices Ltd, 1998. ThetaProbe SOIL MOISTURE SENSOR TYPE ML2 USER MANUAL (ML2-UM-1). Delta-T Devices Ltd, Cambridge, UK.

Fersch, B., Jagdhuber, T., Schroen, M., Voelksch, I., Jaeger, M., 2018. Synergies for Soil Moisture Retrieval Across Scales From Airborne Polarimetric SAR, Cosmic Ray Neutron Roving, and an In Situ Sensor Network. Water Resour. Res. 54, 9364–9383. https://doi.org/10.1029/2018WR023337

Jacobsen, O., Schjonning, P., 1993. A Laboratory Calibration of Time-Domain Reflectometry for Soil-Water Measurement Including Effects of Bulk-Density and Texture. J. Hydrol. 151, 147–157. https://doi.org/10.1016/0022-1694(93)90233-Y

Malicki, M.A., Plagge, R., Roth, C.H., 1996. Improving the calibration of dielectric TDR soil moisture determination taking into account the solid soil. Eur. J. Soil Sci. 47, 357–366. https://doi.org/10.1111/j.1365-2389.1996.tb01409.x

Mohamed, A.-M.O., Paleologos, E.K., 2018. Dielectric Permittivity and Moisture Content, in: Fundamentals of Geoenvironmental Engineering. Elsevier, pp. 581–637. https://doi.org/10.1016/B978-0-12-804830-6.00016-8

Topp, G., Davis, J., Annan, A., 1980. Electromagnetic Determination of Soil-Water Content - Measurements in Coaxial Transmission-Lines. Water Resour. Res. 16, 574–582. https://doi.org/10.1029/WR016i003p00574


}
\author{
Till Francke
}

\note{
The different equations can automatically tested and compared with \code{\link{compare_eps2theta_equations}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{correct_sensor_voltage}}
\code{\link{compare_eps2theta_equations}}
}
\examples{
#return list of supported equations
eps2theta(epsdata = NULL, equation = "list")
}
