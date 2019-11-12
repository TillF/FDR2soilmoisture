\name{correct_sensor_voltage}
\alias{correct_sensor_voltage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
correct_sensor_voltage
}
\description{
Correct the sensor voltage output to values stated by the manufacturer using calibration values
}
\usage{
correct_sensor_voltage(V, serial_no=NULL, probe_id=NULL, ring_no=1, calib_data)
}
\arguments{
  \item{V}{
	raw voltage from sensor [V] to be corrected
}
  \item{serial_no}{
	Serial number of probe. The number is serves for finding the respective records in \code{calib_data}. Either \code{serial_no} OR \code{probe_id} must be specified.
}
  \item{probe_id}{
	Serial number of probe. The number is serves for finding the respective records in \code{calib_data}. Either \code{serial_no} OR \code{probe_id} must be specified.
}
  \item{ring_no}{
	Ring number (only relevant for PR2-probes, i.e. with multiple sensor rings). The number is serves for finding the respective records in \code{calib_data}. 
}
  \item{calib_data}{
	Data frame with calibration data. See details. 
}
}
\details{
In spite of manufacture specifications, Theta Probe and PR2-instruments usually yield output deviating from the specifications. This deviation seems especially pronounced for the analogue PR2 probes, with varying offsets along the length of the rod. We assume this mismatch to be linear. Thus, it can be corrected with to reference points. These are taken from the supplied calibration data, which contain two measurements (air, water) for each sensor. Thus, the given raw input \code{V} is linearly transformed in a way, that the manufacture's conversion of Voltage to permittivity (epsilon) holds for air and water.

\itemize{
\code{calib_data}: Data frame with calibration data. The following columns must be present:
\item{probe_id} {User-ID of probe. Can be blank if \code{serial_no} is given.} 
\item{ring_no}{User-ID of probe. Can be blank if \code{probe_id} is given.} 
\item{voltage_air_mV}{Sensor output when sensor is in the air [mV].} 
\item{voltage_water_mV}{Sensor output when sensor is in the water [mV].} 
\item{remarks}{(optional)} 
\item{type}{must contain the substrings "Theta Probe" OR "PR2". Used for selecting the correct manufacturer's conversion functions.} 
\item{sensor_no}{Manufacturer serial number of probe. Can be blank if \code{probe_id} is given.} 
\item{date}{Date of calibration.} 
}
The directory \code{example} of the package holds an example text file.
}

\value{
 Corrected voltage value [V]
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

