\name{V2eps}
\alias{V2eps}
\alias{eps2V}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert sensor voltage to epsilon and v.v. using manufacturer's specifications.
}
\description{
Convert sensor voltage to epsilon and v.v. using manufacturer's specifications. Defined for Theta Probe and PR2 (DeltaT).
}
\usage{
V2eps(V, type)
eps2V(eps, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
(corrected) sensor voltage [V]. Raw sensor output should be corrected first using \code{\link{correct_sensor_voltage}}.
}
  \item{eps}{
permittivity epsilon [-].
}
  \item{type}{
Type of sensor. Must be "Theta Probe" OR "PR2".
}
}
\details{
Applies the manufacturer's conversion functions or its inverse. V: voltage in [V] 

PR2 default equation according to manual (PR2_user_manual_version_5.0.pdf, eq. 2), (PR2_SDI-12-_User_Manual_version_4_1.pdf, eq. 2):
\deqn{\sqrt{\epsilon} = 1.125 - 5.53*\V + 67.17*\V^2 - 234.42*\V^3 + 413.56*\V^4 - 356.68*\V^5 + 121.53*\V^6
}{%
\sqrt\epsilon = 1.125 - 5.53*V + 67.17*V^2 - 234.42*V^3 + 413.56*V^4 - 356.68*V^5 + 121.53*V^6
}
    
Theta Probe (ThetaProbe user manual, eq. 3):
\deqn{\epsilon = 1.07 + 6.4*\V-6.4*\V^2+4.7*\V^3 
}{%
\epsilon = 1.07 + 6.4*V-6.4*V^2+4.7*V^3 
}


}
\value{
Permittivity value (epsilon) [-] or sensor voltage [V].
}
\references{
Delta-T Devices Ltd, 1998. ThetaProbe SOIL MOISTURE SENSOR TYPE ML2 USER MANUAL (ML2-UM-1). Delta-T Devices Ltd, Cambridge, UK.

Delta-T Devices Ltd, 2016. User Manual for the Profile Probe type PR2 (PR2-UM-5.0). Delta-T Devices Ltd, Cambridge, UK.


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
V2eps(V= 1.1, type = "Theta Probe") #convert to epsilon, eq. 1 of Theta-Probe user manual

  eps_air =1.00059
  eps_h2o =81 #at 20 °C
  eps2V(eps_air, type = "Theta Probe") 
  eps2V(eps_h2o, type = "Theta Probe") 


# reproducing the figure on p.11,  Theta-Probe user manual
  V = seq(from=0.0, to=1., by=0.1)
  eps = V2eps(V, type = "Theta Probe")
  plot(V, sqrt(eps))
  abline(lm(sqrt(eps)~V))
  
  # add values from inverse function
  eps_inv = seq(from=1, to=80, by=2)
  V_inv = eps2V(eps_inv, type = "Theta Probe")  
  points(V_inv, sqrt(eps_inv), col="red", pch="+")  
}
