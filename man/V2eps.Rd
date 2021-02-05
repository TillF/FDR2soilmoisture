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
Type of sensor. Must be "Theta Probe", "Theta Probe polynomial" OR "PR2".
}
}
\details{
Applies the manufacturer's conversion functions or its inverse. V: voltage in [V] 

PR2 default equation according to manual (PR2_user_manual_version_5.0.pdf, eq. 2), (PR2_SDI-12-_User_Manual_version_4_1.pdf, eq. 2):
\deqn{\sqrt{\epsilon} = 1.125 - 5.53*\V + 67.17*\V^2 - 234.42*\V^3 + 413.56*\V^4 - 356.68*\V^5 + 121.53*\V^6
}{%
\sqrt\epsilon = 1.125 - 5.53*V + 67.17*V^2 - 234.42*V^3 + 413.56*V^4 - 356.68*V^5 + 121.53*V^6
}
    
Theta Probe polynomial* (ThetaProbe user manual, eq. 3. ):
\deqn{\epsilon = 1.07 + 6.4*\V-6.4*\V^2+4.7*\V^3 
}{%
\epsilon = 1.07 + 6.4*V-6.4*V^2+4.7*V^3 
}
Don't use for U > 1 V or theta > 0.5, see below!

Theta Probe (ThetaProbe user manual, linearisation table p. 14):
derived from data table, see details below


}
\value{
Permittivity value (epsilon) [-] or sensor voltage [V].
}
\section{Warning}{
"Theta Probe polynomial":

*ThetaProbe user manual, p. 10 states:
"In the range 0 to 1 Volt (corresponding to a soil moisture range 0 to ~ 0.55 by volume),
this relationship can be fitted very precisely by a 3rd order polynomial"

p. 11 states:
"For very high moisture contents (θ > 0.5 m3.m-3), the polynomial equation should be used. This is usually only necessary for organic soils."
This is obviously wrong (see 'example 1', below): The polynom obviously differs considerably from the points in the linearization table, especially for U > 1 V or theta > 0.5, so only the statement on page 10 seems correct. For U > 1 V, use "Theta Probe" (i.e. based on the table) rather than "Theta Probe polynomial".
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


\seealso{
\code{\link{correct_sensor_voltage}}
}
\examples{
V2eps(V= 1.1, type = "Theta Probe") #convert to epsilon, eq. 1 of Theta-Probe user manual

  eps_air =1.00059     #permittivity of air
  eps_h2o =81          #permittivity of water at 20 °C
  eps2V(eps_air, type = "Theta Probe")  #display voltages that would be expected when measuring in air and water
  eps2V(eps_h2o, type = "Theta Probe") 


# example 1: reproducing the figure on p.11,  Theta-Probe user manual (voltage vs. sqrt(eps)) ####
  V = seq(from=0.0, to=1.3, by=0.02)
  eps = V2eps(V, type = "Theta Probe polynomial") #polynomial equation (eq. 1)
  plot(1, xlim=c(0, 1.4), ylim=c(0,9), type="n", xlab="voltage [V]", ylab="sqrt(epsilon) [-]", main = "Theta Probe, voltage-to-epsilon conversion")
  lines(V, sqrt(eps), ylim=c(0,9), type="l", lty=2, col="red")
  abline(a=1.1, b=4.44, col="green") #linearized version (eq. 2)
  eps = V2eps(V, type = "Theta Probe") #polynomial equation (eq. 1)
  lines(V, sqrt(eps), ylim=c(0,9), type="l", lty=1, col="red")
  
 
  abline(v=1, lty=2) #show  extent of the original figure
  abline(h=6, lty=2)
  
  legend("topleft", legend=c("polynomial", "linearized", "lookup, linear interpolation", "extent of orig. figure", "calibration data"),
         lty=c(2,1,1,2,0), pch=c(NA,NA, NA, NA, 21), col=c("red", "green", "red", "black", "blue"))
  


# example 1b: add examples from device-specific calibration to plot ####
  calib_file = system.file("example", "sensor_calibration.txt", package = "FDR2soilmoisture") #replace this by your own calibration file
  calib_data = read.table(calib_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file
  
  calib_data = calib_data[calib_data$type=="Theta Probe",]
  
  points(calib_data$voltage_air_mV/1000, rep(1, nrow(calib_data)), col="blue")
  points(calib_data$voltage_water_mV/1000, sqrt(rep(81, nrow(calib_data))), col="blue")
  
  
}
