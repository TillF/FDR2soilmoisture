\name{correct_sensor_voltage}
\alias{correct_sensor_voltage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
correct_sensor_voltage
}
\description{
Convert measured the sensor voltage output to "should-be" values stated by the manufacturer using user-supplied calibration values
}
\usage{
correct_sensor_voltage(V, serial_no=NULL, probe_id=NULL, ring_no=1, calib_data)
}
\arguments{
  \item{V}{
	raw voltage from sensor [V] to be corrected
}
  \item{serial_no}{
	Serial number of probe. The number is serves for finding the respective records in \code{calib_data}. Either \code{serial_no} OR \code{probe_id} must be specified. With the example file (see Details), the placeholders \code{"Theta Probe", "PR2, analogue", "PR2, analogue grounded", "PR2, SDI-12"} can be used for unknown devices. In this case, mean values of the same sensor type and ring number are used.
}
  \item{probe_id}{
	Serial number of probe. The number is serves for finding the respective records in \code{calib_data}. Either \code{serial_no} OR \code{probe_id} must be specified.
}
  \item{ring_no}{
	Ring number (only relevant for PR2-probes, i.e. with multiple sensor rings). The number is serves for finding the respective records in \code{calib_data}. 
}
  \item{calib_data}{
	Data frame with calibration data. See details. }
	
  \item{warnOnly=FALSE}{
  If no data for the specified probe are found, an error is raised. With \code{warnOnly=TRUE}, instead, a warning is issued and medians of the same probe type are used.
}

  \item{adjust_range=TRUE}{
  When this option is enabled, the 99-percentile of voltages supplied in \code{V} that is above the the calibration voltages \code{voltage_water_mV}, \code{voltage_water_mV} is enlarged accordingly, i.e. assuming there is a higher maximum value to be used. If disabled, voltages outside the interval are converted to NA.
}
}
\details{
In spite of manufacture specifications, Theta Probe and PR2-instruments usually yield output voltage deviating from the specifications. This deviation seems especially pronounced for higher water contents (see 'example 2b adn 2c' in examples of \link{eps2theta}) and the analogue PR2 probes, with varying offsets along the length of the rod. We assume this mismatch in voltage to be linear. Thus, it can be corrected with two reference points. These are taken from the supplied user calibration data (\code{calib_data}), which contain two measurements (in air and water, respectively) for each sensor. Thus, the given raw input \code{V} is linearly transformed in a way, that the manufacture's conversion of Voltage to Permittivity (epsilon) holds for air and water.

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

calib_file = system.file("example", "sensor_calibration.txt", package = "FDR2soilmoisture") #replace this by your own calibration file
calib_data = read.table(calib_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file
V_corr = correct_sensor_voltage(V=0.8, serial_no = "114/035", calib_data = calib_data) #apply sensor-specific voltage calibration

}

