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
Equation to be used. Must be one of 
"deltaT_minorg", 
"DrnevichEtal2005",
"FerschEtal2017", 
"Jacobsen_Schjonning1993",
"JuEtal2010",
"MalickiEtal1996",
"RothEtal1992",
"ToppEtal1980", 
"SchaapEtal1997",
"ZhaoEtal2016".

}
}
\details{
Applies various equations, most of which are listed in Mohamed & Paleologos, 2018.
The following columns in the data frame \code{epsdata} are supported and may be required, depending on the choice of the equation:
\itemize{
\item{epsilon} { Electric permittivity. [-]} 
\item{soil} { Soil type indicator. \code{"mineral", "organic"}} 
\item{n} { Soil porosity. [-]}
\item{BD} { dry bulk density. [g/cm³]}
\item{clay_perc} { Percent clay. [\%]}
\item{om_perc} { Percent organic matter. [\%]}
\item{cohesive} { is the soil cohesive? [0/1]}
}
   
The respective equations used are:


Delta-T Devices Ltd, 1998:
\deqn{\theta =  \frac{\sqrt{\epsilon}-\a_subscript{0}}{\a_1}
}{%
\theta =  (\sqrt{\epsilon}-\a~0~)/\a_1.
} with a~0~=1.6 and a_1=8.4 for mineral soils, a_0=1.3 and a_1=7.7 for organic soils, 

Malicki et al., 1996:
\deqn{\theta =  \frac{\sqrt{\epsilon}-0.819-0.168\BD-0.159\BD^2}{7.17+1.18\BD}
}{%
\theta =  (\sqrt(\epsilon)-0.819-0.168 BD-0.159 BD^2)/(7.17+1.18 BD).
}

Zhao et al., 2016:
\deqn{\theta =  \frac{0.3039 * \BD - 2.1851 + \sqrt{\epsilon}}{(18.0283 * BD - 17.9531) + (-0.6806 * BD + 1.8351) * \sqrt{\epsilon}}
}{%
\theta = (0.3039 * BD - 2.1851 + sqrt(\epsilon))/((18.0283 * BD - 17.9531) + (-0.6806 * BD + 1.8351) * sqrt(\epsilon)).
}

[to be completed]

}
\value{
A vector of reals, i.e. theta (volumetric water content) [-].

For \code{equation=="list"}, a named list of all supported equations is returned. The elements in the list are character strings, denoting the necessary input fields for the equations.
}

\references{
Delta-T Devices Ltd, 1998. ThetaProbe SOIL MOISTURE SENSOR TYPE ML2 USER MANUAL (ML2-UM-1). Delta-T Devices Ltd, Cambridge, UK.

Drnevich, V.P., Ashmawy, A.K., Yu, X., Sallam, A.M., 2005. Time domain reflectometry for water content and density of soils: study of soil-dependent calibration constants. Can. Geotech. J. 42, 1053–1065. https://doi.org/10.1139/T05-047

Fersch, B., Jagdhuber, T., Schroen, M., Voelksch, I., Jaeger, M., 2018. Synergies for Soil Moisture Retrieval Across Scales From Airborne Polarimetric SAR, Cosmic Ray Neutron Roving, and an In Situ Sensor Network. Water Resour. Res. 54, 9364–9383. https://doi.org/10.1029/2018WR023337

Jacobsen, O., Schjonning, P., 1993. A Laboratory Calibration of Time-Domain Reflectometry for Soil-Water Measurement Including Effects of Bulk-Density and Texture. J. Hydrol. 151, 147–157. https://doi.org/10.1016/0022-1694(93)90233-Y

Ju, Z., Liu, X., Ren, T., & Hu, C., 2010. Measuring Soil Water Content With Time Domain Reflectometry. Soil Science, 175(10), 469–473. doi:10.1097/ss.0b013e3181f55aa3 

Malicki, M.A., Plagge, R., Roth, C.H., 1996. Improving the calibration of dielectric TDR soil moisture determination taking into account the solid soil. Eur. J. Soil Sci. 47, 357–366. https://doi.org/10.1111/j.1365-2389.1996.tb01409.x

Mohamed, A.-M.O., Paleologos, E.K., 2018. Dielectric Permittivity and Moisture Content, in: Fundamentals of Geoenvironmental Engineering. Elsevier, pp. 581–637. https://doi.org/10.1016/B978-0-12-804830-6.00016-8

Topp, G., Davis, J., Annan, A., 1980. Electromagnetic Determination of Soil-Water Content - Measurements in Coaxial Transmission-Lines. Water Resour. Res. 16, 574–582. https://doi.org/10.1029/WR016i003p00574

Zhao, Y., Ling, D., Wang, Y., Huang, B., Wang, H., 2016. Study on a calibration equation for soil water content in field tests using time domain reflectometry. J. Zhejiang Univ.-SCI A 17, 240–252. https://doi.org/10.1631/jzus.A1500065

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
