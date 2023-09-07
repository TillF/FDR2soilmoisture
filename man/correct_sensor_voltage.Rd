\name{correct_sensor_voltage}
\alias{correct_sensor_voltage}
\alias{correct_sensor_values}
\alias{correct_sensor_values_all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correct sensor outputs (Voltage, Permittivity)
}
\description{
\code{correct_sensor_values} converts the measured sensor output (voltage, permittivity, counts) to "should-be" values stated by the manufacturer using user-supplied calibration values.

\code{correct_sensor_values_all} does correct also values outside the in calibdata provided reference values (see \code{adjust_range}).

}
\usage{
correct_sensor_values(values, serial_no=NULL, probe_id=NULL,var_type=NULL, ring_no=1, calib_data, warnOnly=FALSE, adjust_range=TRUE, ...)

correct_sensor_values_all(values, serial_no=NULL, probe_id=NULL,var_type=NULL, ring_no=1, calib_data, warnOnly=FALSE, adjust_range=TRUE, ...)

correct_sensor_voltage(V, serial_no=NULL, probe_id=NULL, ring_no=1, calib_data, warnOnly=FALSE, adjust_range=TRUE)

}
\arguments{
  \item{values}{
  raw values from sensor to be corrected. Type of values must be specified in \code{var_type} - either Voltage [V], Permittivity [-] or Counts [-].
  }
  \item{V}{
	raw voltage from sensor [V] to be corrected
}
  \item{serial_no}{
	Serial number of probe. The number is serves for finding the respective records in \code{calib_data}. Either \code{serial_no} OR \code{probe_id} must be specified. With the example file (see Details), the placeholders \code{"Theta Probe", "PR2, analogue", "PR2, analogue grounded", "PR2, SDI-12"} can be used for uncalibrated devices (see argument \code{warnOnly}).
}
  \item{probe_id}{
	Serial number of probe. The number is serves for finding the respective records in \code{calib_data}. Either \code{serial_no} OR \code{probe_id} must be specified.
}
  \item{var_type}{
  Type of meassured input variable from sensor. Must be one of \code{"Voltage","Permittivity","Counts"} and also be part of the used \code{calib_data}.
  }
  \item{ring_no}{
	Ring number (only relevant for PR2-probes, i.e. with multiple sensor rings). The number is serves for finding the respective records in \code{calib_data}. 
}
  \item{calib_data}{
	Data frame with calibration data. See details. }
	
  \item{warnOnly}{
  \code{=FALSE} If no data for the specified probe are found, an error is raised. With \code{warnOnly=TRUE}, instead, a warning is issued and median values are used. See details.
}

  \item{adjust_range}{
  \code{=True}: When this option is enabled, the 99-percentile of voltages supplied in \code{V} that is above the the calibration voltages \code{voltage_water_mV}, \code{voltage_water_mV} is enlarged accordingly, i.e. assuming that this is a higher maximum value that should rather be used. Analoguously for values below \code{voltage_air_mV}.
  If disabled, voltages outside the in the \code{calib_data} given interval are converted to NA.
}
\item{...}{
  Further individual callibraion parameters for SMT100 \code{alpha}, \code{beta}, \code{gamma} to pass to \code{\link{counts2eps}} and not default permittivity reference values for the two callibration points "air" \code{epsilon_0} and "water" \code{epsilon_1}}
}

\details{
In spite of the manufacturer's specifications, Theta Probe and PR2-instruments usually yield output voltage deviating from the specifications. This deviation seems especially pronounced for higher water contents (see 'example 2b and 2c' in examples of \link{eps2theta}) and the analogue PR2 probes, with varying offsets along the length of the rod. We assume this mismatch in voltage to be linear. Thus, it can be corrected with two reference points. The function takes these from the user calibration data (\code{calib_data}), which must contain two measurements (in air and water, respectively) for each sensor. Thus, the given raw input \code{V} is linearly transformed in a way, that the manufacture's conversion of Voltage to Permittivity (epsilon) holds for air and water.

UPDATE:
With \code{correct_sensor_values()} corrections with two refference points can also be applied on directly measured "Permittivity" ("Theta Probe", "PR2", "TDR", "SMT100") or "Counts" (SMT100). Therfore \code{var_type} must be specified in the function and \code{calib_data}.

\itemize{
\code{calib_data}: Data frame with calibration data. The following columns must be present:
\item{\code{probe_id}: User-ID of probe corresponding to argument \code{probe_id}. Can be blank if \code{serial_no} is given.} 
\item{\code{serial_no}: Manufacturer serial number of probe corresponding to argument \code{serial_no}. Can be blank if \code{probe_id} is given.} 
\item{\code{ring_no}: Sensor/Ring number of probe for multi-sensor probes (e.g. 1-6 for PR2). Set to \code{1} for all single-sensor probes (e.g. for ThetaProbes).}
\item{\code{voltage_air_mV}: Sensor output when sensor is in the air [mV].} 
\item{\code{voltage_water_mV}: Sensor output when sensor is in the water [mV].} 
\item{\code{remarks}: (optional)} 
\item{\code{type}: must contain the substrings "Theta Probe" OR "PR2". Used for selecting the corresponding manufacturer's conversion functions.} 
\item{\code{date}: Date of calibration.}
\item{\code{var_type}: Type of meassured variable ("Voltage"," Permittivity" or "Counts"). Necessary for \code{correct_sensor_values()}}
}
The directory \code{example} of the package holds an example text file.

With \code{warnOnly=TRUE} if coefficients are missing for SOME rings, the median of all rings of *this* probe wil be used. If coefficients are missing for ALL rings, the median values of all probes of this type is used. This can be used for 
uncalibrated devices by supplying placeholders for \code{serial_num}, which refernece to a dummy records with all coefficients being NA.

\code{correct_sensor_values_all()} can be used if a correction of values outside the in the calib_data given interval interval is desired and an adjustment of highest and lowest values to the 99--percentile (\code{adjust_range=TRUE}) is not useful.

}


\value{
 \code{correct_sensor_value()}: Respective corrected input values [V or -]
 \code{correct_sensor_value_all()}: Respective corrected input values [V or -]
 \code{correct_sensor_voltage()}: Corrected voltage value [V]
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Till Francke, Jakob Terschluesen
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
V_corr = correct_sensor_values(values=0.8, serial_no = "114/035", calib_data = calib_data, var_type = "Voltage") #apply sensor-specific voltage calibration

}

