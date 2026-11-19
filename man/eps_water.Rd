\name{eps_water}
\alias{eps_water}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Temperature-Dependent Permittivity of Water
}
\description{
Using one of the equations from Weast (1986), Uematsu & Franck (1980) or the IAPWS formulation for electrostatic properties of water, the function calculates the permittivity/epsilon [-] of water for a given temperature at normal pressure. The non-default equations require (\code{"iapws"}) or benefit from (\code{"Uematsu1980"}) the installation of the iapws package (see Details).
}
\usage{
eps_water(T, equ = "Weast86")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{
Temperature of water [degree Celsius]
}
  \item{equ}{
Equation to be used. Must be one of \code{"Weast86"} (default), \code{"Uematsu1980"} or \code{"iapws"}
}

}
\details{
Three equations are implemented:

  \itemize{
  
    \item \code{"Weast86"} (Weast, R.C., 1986): 
    \deqn{\epsilon =  78.54 * (1 - (4.5791 * 10^{-3})*(T-25) + 1.19*10^{-5} * (T-25)^2 - 2.8 * 10^{-8} * (T-25)^3)
    }{%
    \epsilon =  78.54 * (1 - (4.5791 * 10^-3)*(T-25) + 1.19*10^-5 * (T-25)^2 - 2.8 * 10^-8 * (T-25)^3)
    }
    
    \item \code{"Uematsu1980"} (Uematsu & Franck, 1980): 
    \deqn{\epsilon = 1 + \frac{A}{T_2} * \rho_2 +
      (\frac{B}{T_2} + C + D * T_2) * \rho_2^2 +
      (\frac{E}{T_2} + F * T_2 + G * T_2^2) * \rho_2^3 +
      (\frac{H}{T_2^2} + \frac{I}{T_2} + J) * \rho_2^4
    }{%
    \epsilon = 1 + (A/T_2) * \rho_2 +
      ((B/T_2) + C + D * T_2) * \rho_2^2 +
      ((E/T_2) + F * T_2 + G * T_2^2) * \rho_2^3 +
      ((H/T_2^2) + (I/T_2) + J) * \rho_2^4
    } with \eqn{T_2 = \frac{T + 273.15}{298.15}}{T_2 = (T + 273.15)/298.15} \eqn{[\frac{K}{K}]}{[K/K]}, \eqn{\rho_2=\rho/1000}{rho_2 = rho/1000} \eqn{[\frac{kg*m^{-3}}{kg*m^{-3}}]}{[(kg*m^-3)/(kg*m^-3)]} and \eqn{A=7.62571}, \eqn{B=244.003}, \eqn{C=-140.569}, \eqn{D=27.7841}, \eqn{E=-96.2805}, \eqn{F=41.7909}, \eqn{G=-10.2099}, \eqn{H=-45.2059}, \eqn{I=84.6395}, \eqn{J=-35.8644}.
    
    The temperature-dependent density of water \eqn{\rho}{rho} is calculated using the \link[iapws:iapws95]{iapws} package by Jonathan Debove (\code{install.packages("iapws")}). If this package is not installed, a constant density of 998.2072 \eqn{kg*m^{-3}}{kg*m^-3} (water at 20 degree Celsius) is used and a warning is issued.
    
\item \code{"iapws"} (Debove J, 2022): Formulations for the thermodynamic and transport properties released by the International Association for the Properties of Water and Steam (IAPWS) and implemented in the \link[iapws:iapws_epsilon]{iapws} package by Jonathan Debove (\code{install.packages("iapws")}).
  

  }

}
\value{
Permittivity of water [-]
}
\references{
Debove J (2022). \emph{iapws: Formulations of the International Association for the
  Properties of Water and Steam}. R package version 1.1,
  \url{https://CRAN.R-project.org/package=iapws}.

Uematsu, M., Franck, E.U. (1980): Static Dielectric Constant of Water and Steam. Journal of Physical and Chemical Reference Data 9, 1291. \url{https://doi.org/10.1063/1.555632}

Weast, R.C. CRC Handbook of Chemistry and Physics, 67th ed.; CRC Press: Boca Raton, FL, USA, 1986.
}

\author{
Jakob Terschluesen
}

\seealso{
\url{https://cran.r-project.org/package=iapws}
}

\examples{

#calculate permittivity of water at 20.5 degree Celsius
T=20.5
eps_water(T,equ = "Weast86")
eps_water(T,equ = "Uematsu1980") #uses constant density of water if iapws is not installed

#only if package iapws is installed
if (requireNamespace("iapws", quietly = TRUE)) {
  eps_water(T, equ = "iapws")
}

T=seq(5,40,5)
eps_water(T,equ = "Weast86")
eps_water(T,equ = "Uematsu1980")

#only if package iapws is installed
if (requireNamespace("iapws", quietly = TRUE)) {
  eps_water(T, equ = "iapws")
}

}

