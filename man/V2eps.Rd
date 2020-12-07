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
%% ~put references to the literature/web site here ~
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
V2eps(V= 1.3, type = "Theta Probe") #convert to epsilon, eq. 1 of ThetaProbe user manual
}
