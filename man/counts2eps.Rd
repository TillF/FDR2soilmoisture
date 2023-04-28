\name{counts2eps}
\alias{counts2eps}
\alias{eps2counts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert oscillation frequency output from SMT100 sensors into permittivity and v.v using an empirical model.

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
counts2eps(counts, alpha=-0.1305, beta=0.2549, gamma=1.8342)
eps2counts(perm, alpha=-0.1305, beta=0.2549, gamma=1.8342)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{counts}{counts/oscillation frequency measured by a SMT100 sensor [-]}
  \item{perm}{apparent dielectric permittivity [-]}
  \item{alpha, beta, gamma}{Calibration parameters, see Details}
}
\details{

The following empirical model is used from Bogena et all, 2017:
\deqn{\epsilon= \gamma + \frac{1}{\alpha+\frac{\beta}{(18000-\code{counts})/5000}}
}{%
  \epsilon= \gamma + 1/(\alpha+(\beta/((18000-\code{counts})/5000))) 
  }
  
The function uses as default average parameters for \code{alpha}, \code{beta} and \code{gamma} (table 2, Bogena et all, 2017). Nevertheless, since each sensor is individually calibrated those should be set individually too.
}
\value{
Permittivity value (epsilon) [-] or sensor counts [-].
}
\references{
Bogena, H.R.; Huisman, J.A.; Schilling, B.; Weuthen, A.; Vereecken, H. Effective Calibration of Low-Cost Soil Water Content Sensors. Sensors 2017, 17, 208. \link{https://doi.org/10.3390/s17010208}

}
\author{
Jakob Terschlüsen
}

\note{
%%  ~~further notes~~
}


\seealso{
%
}

\examples{

#random counts
counts2eps(10000)

#eps for air
eps2counts(1.00059)

#individual set of parameters:
counts2eps(10000, alpha=-0.1306256,beta=0.254624, gamma= 1.834865)
eps2counts(1.00059, alpha=-0.1306256,beta=0.254624, gamma= 1.834865)
}
