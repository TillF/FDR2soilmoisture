\encoding{UTF-8}
\name{V2eps}
\alias{V2eps}
\alias{eps2V}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert sensor voltage to epsilon and vice versa using manufacturer's specifications
}
\description{
Convert sensor voltage to epsilon and vice versa using manufacturer's specifications. Defined for Theta Probe and PR2 (Delta-T Devices) and picoSMS.
}
\usage{
V2eps(V, type, temp = NULL)
eps2V(eps, type, temp = NULL)
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
Type of sensor. Must be \code{"Theta Probe"}, \code{"Theta Probe polynomial"}, \code{"PR2"}, \code{"picoSMS_linear"} or \code{"picoSMS_poly"}. See details for explanation.
}

  \item{temp}{
Temperature during measurements [degree Celsius], needed for picoSMS.}
}
\details{
Applies the manufacturer's conversion functions or their inverse. \code{V}: voltage in [V]; \code{eps}: epsilon [-]

Values for \code{type}:

\itemize{

  \item \code{"PR2"}: PR2 default equation according to manual (PR2_user_manual_version_5.0.pdf, eq. 2), (PR2_SDI-12-_User_Manual_version_4_1.pdf, eq. 2):
  \deqn{
    \sqrt{\epsilon} = 1.125 - 5.53*V + 67.17*V^2 - 234.42*V^3 + 413.56*V^4 - 356.68*V^5 + 121.53*V^6
  }{%
    sqrt(\epsilon) = 1.125 - 5.53*V + 67.17*V^2 - 234.42*V^3 + 413.56*V^4 - 356.68*V^5 + 121.53*V^6
  }

  \item \code{"Theta Probe polynomial"}: polynomial according to ThetaProbe user manual (eq. 1):
  \deqn{
    \sqrt{\epsilon} = 1.07 + 6.4*V - 6.4*V^2 + 4.7*V^3
  }{%
    sqrt(\epsilon) = 1.07 + 6.4*V - 6.4*V^2 + 4.7*V^3
  }
  Don't use for voltage > 1 V or theta > 0.5, see section \sQuote{Warning} below!

  \item \code{"Theta Probe"}: interpolated lookup data table from ThetaProbe user manual table (p. 14), see section \sQuote{Warning} below.

  \item \code{"picoSMS_linear"} (Sheshadrivasan et al., 2025):
  \deqn{
    \epsilon = 2.3811 + 70.0811 * V - 0.0337 * temp
  }{%
    \epsilon = 2.3811 + 70.0811 * V - 0.0337 * temp
  }

  \item \code{"picoSMS_poly"} (Sheshadrivasan et al., 2025):
  \deqn{
    \epsilon = 1.9284 + 43.9328 * V + 0.0181 * temp + 26.4293 * V^2 - 0.0942 * V * temp - 0.0005 * temp^2
  }{%
    \epsilon = 1.9284 + 43.9328 * V + 0.0181 * temp + 26.4293 * V^2 - 0.0942 * V * temp - 0.0005 * temp^2
  }
}
}

\value{
Permittivity value (epsilon) [-] or sensor voltage [V].
}
\section{Warning}{
Regarding \code{"Theta Probe polynomial"}, the ThetaProbe user manual states:
\itemize{
\item p. 10: \dQuote{In the range 0 to 1 Volt (corresponding to a soil moisture range 0 to ~0.55 by volume),
this relationship can be fitted very precisely by a 3rd order polynomial}

\item p. 11: \dQuote{For very high moisture contents (\eqn{\theta \,>\,0.5\, m^3 \cdot \, m^{-3}}{theta > 0.5 m^3*m^-3}), the polynomial equation should be used. This is usually only necessary for organic soils.}
}
\bold{The statement on p. 11 is obviously wrong} (see 'example 1', below): The polynomial differs considerably from the points in the linearization table, especially for voltage > 1 V or theta > 0.5, so only the statement on p. 10 seems correct.

\bold{-> For V > 1 V, use \code{"Theta Probe"} (i.e. based on the table) rather than \code{"Theta Probe polynomial"}.}
}


\references{
Delta-T Devices Ltd, 1998. ThetaProbe SOIL MOISTURE SENSOR TYPE ML2 USER MANUAL (ML2-UM-1). Delta-T Devices Ltd, Cambridge, UK.

Delta-T Devices Ltd, 2016. User Manual for the Profile Probe type PR2 (PR2-UM-5.0). Delta-T Devices Ltd, Cambridge, UK.

Sheshadrivasan V.K., Langhammer J., Scheiffele L., Terschlüsen J., Francke T. (2025): picoSMS: Development of a Low-Cost, Open-Source, Off-Grid, Soil Moisture monitoring Station, and the Calibration and Validation of commercially available Capacitive Soil Moisture Sensor for Hydrological Applications. Sensors 2025, \url{https://zenodo.org/records/17072390}



}
\author{
Till Francke, Jakob Terschlüsen
}
%\note{
%%  ~~further notes~~
%}


\seealso{
\code{\link{correct_sensor_voltage}}
}
\examples{
V2eps(V= 1.1, type = "Theta Probe") #convert to epsilon, lookup table of Theta-Probe user manual

  eps_air =1.00059     #permittivity of air
  eps_h2o =81          #permittivity of water at 20 degree C
  #display voltages that would be expected when measuring in air and water
  eps2V(eps_air, type = "Theta Probe")  
  eps2V(eps_h2o, type = "Theta Probe") 


# example 1: reproducing the figure on p.11,  Theta-Probe user manual (voltage vs. sqrt(eps)) ####
  V = seq(from=0.0, to=1.3, by=0.02)
  eps = V2eps(V, type = "Theta Probe polynomial") #polynomial equation (eq. 1)
  
  plot(1, xlim=c(0, 1.4), ylim=c(0,9), type="n", 
  xlab="voltage [V]", ylab="sqrt(epsilon) [-]", 
  main = "Theta Probe, voltage-to-epsilon conversion")
  lines(V, sqrt(eps), ylim=c(0,9), type="l", lty=2, col="red")
  abline(a=1.1, b=4.44, col="green") #linearized version (eq. 2)
  eps = V2eps(V, type = "Theta Probe") #lookup table, linear interpolation
  lines(V, sqrt(eps), ylim=c(0,9), type="l", lty=1, col="red")
  
 
  abline(v=1, lty=2) #show extent of the original figure
  abline(h=6, lty=2)
  
  legend("topleft", legend=c("polynomial", "linearized", "lookup, linear interpolation",
  "extent of orig. figure", "calibration data"),
         lty=c(2,1,1,2,0), pch=c(NA,NA, NA, NA, 21),
         col=c("red", "green", "red", "black", "blue"))
  


# example 1b: add examples from device-specific calibration to plot ####
  #replace this by your own calibration file
  calib_file = system.file("example", "sensor_calibration.txt", package = "FDR2soilmoisture")
  #load the file
  calib_data = read.table(calib_file, nrow=-1, sep="\t",
  stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  
  
  calib_data = calib_data[calib_data$type=="Theta Probe" & calib_data$var_type=="Voltage",]
  
  points(calib_data$air_measurement/1000, rep(1, nrow(calib_data)), col="blue")
  points(calib_data$water_measurement/1000, sqrt(rep(81, nrow(calib_data))), col="blue")
  
  
}
