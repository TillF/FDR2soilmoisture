\name{correct_sensor_voltage}
\alias{correct_sensor_voltage}
\alias{correct_sensor_values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correct sensor outputs (Voltage, Permittivity)
}
\description{
\code{correct_sensor_values} converts the measured sensor output (voltage, permittivity, counts) to "should-be" values stated by the manufacturer using user-supplied calibration values.

}
\usage{
correct_sensor_values(values, serial_no=NULL, probe_id=NULL,var_type=NULL, ring_no=1, calib_data, warnOnly=FALSE, adjust_range=TRUE, discard_outlier=TRUE, ...)

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
  \code{=True}: If the option is enabled and the 99th percentile of the in \code{values} given values is outside the interval [\code{air_measurement}, \code{water_measurement}] (calib_data), \code{air_measurement} and \code{water_measurement} are extended to the 99th percentile accordingly, i.e. assuming that this is a higher maximum or lower minimum value that should rather be used. When disabled values outside the interval are treated as defined in \code{discard_outlier}.
}
  \item{discard_outlier}{
  \code{=True}: If enabled, all values outside the possible adjusted interval between \code{air_measurement} and \code{water_measurement} (see \code{adjust_range}), will be set to \code{NA}.
  If disabled all \code{values} are corrected.
}
  \item{...}{
  Further individual calibraion parameters for SMT100 \code{alpha}, \code{beta}, \code{gamma} to pass to \code{\link{counts2eps}} and not default permittivity reference values for the two calibration points "air" \code{epsilon_0} and "water" \code{epsilon_1}}
}

\details{
In spite of the manufacturer's specifications, Theta Probe and PR2-instruments usually yield output voltage deviating from the specifications. This deviation seems especially pronounced for higher water contents (see 'example 2b and 2c' in examples of \link{eps2theta}) and the analogue PR2 probes, with varying offsets along the length of the rod. We assume this mismatch in voltage to be linear. Thus, it can be corrected with two reference points. The function takes these from the user calibration data (\code{calib_data}), which must contain two measurements (in air and water, respectively) for each sensor. Thus, the given raw input \code{V} is linearly transformed in a way, that the manufacture's conversion of Voltage to Permittivity (epsilon) holds for air and water.

UPDATE:
With \code{correct_sensor_values()} corrections with two reference points can also be applied on directly measured "Permittivity" ("Theta Probe", "PR2", "TDR", "SMT100") or "Counts" (SMT100). Therfore \code{var_type} must be specified in the function and \code{calib_data}.

\itemize{
\code{calib_data}: Data frame with calibration data. The following columns must be present:
\item{\code{probe_id}: User-ID of probe corresponding to argument \code{probe_id}. Can be blank if \code{serial_no} is given.} 
\item{\code{serial_no}: Manufacturer serial number of probe corresponding to argument \code{serial_no}. Can be blank if \code{probe_id} is given.} 
\item{\code{ring_no}: Sensor/Ring number of probe for multi-sensor probes (e.g. 1-6 for PR2). Set to \code{1} for all single-sensor probes (e.g. for ThetaProbes).}
\item{\code{air_measurement}: Sensor output when sensor is in the air [mV].} 
\item{\code{water_measurement}: Sensor output when sensor is in the water [mV].} 
\item{\code{remarks}: (optional)} 
\item{\code{type}: must contain the substrings "Theta Probe" OR "PR2". Used for selecting the corresponding manufacturer's conversion functions.} 
\item{\code{date}: Date of calibration.}
\item{\code{var_type}: Type of meassured variable ("Voltage"," Permittivity" or "Counts"). Necessary for \code{correct_sensor_values()}}
\item{\code{temp}: Temperature of water}

The directory \code{example} of the package holds an example text file
}

With \code{warnOnly=TRUE} if coefficients are missing for SOME rings, the median of all rings of *this* probe wil be used. If coefficients are missing for ALL rings, the median values of all probes of this type is used. This can be used for uncalibrated devices by supplying placeholders for \code{serial_num}, which refernece to a dummy records with all coefficients being NA.

With the additional arguments of \code{epsilon_0 =} and \code{epsilon_1 = } it is possbile to set own permittivity values for the two calibration points. This provides the possiblility to use the two point correction also for other calibration mediums, changing also the calibdata columns (\code{air_measurement} and \code{water_measurement} to the used calibration mediums accordingly.


}


\value{
 \code{correct_sensor_value()}: Respective corrected input values [V or -]
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

\section{Warnings}{
If you are using the correction for mutiple sensors in a loop, it is possible that you get multiple warnings of the same type, especially using the outdated function \code{correct_sensor_voltage()}. It can help to set \code{options(nwarnings = 10000)} before you apply the function and use \code{summary(warnings())} afterwards to get an overview.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

calib_file = system.file("example", "sensor_calibration.txt", package = "FDR2soilmoisture") #replace this by your own calibration file
calib_data = read.table(calib_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file
V_corr = correct_sensor_values(values=0.8, serial_no = "114/035", calib_data = calib_data, var_type = "Voltage", warnOnly=FALSE, adjust_range=TRUE, discard_outlier=TRUE) #apply sensor-specific voltage calibration

}

