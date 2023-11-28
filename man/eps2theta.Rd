\name{eps2theta}
\alias{eps2theta}
\alias{theta2eps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert between epsilon (permittivity) and theta (volumetric water content) .
}
\description{
Convert between epsilon (permittivity) and theta (volumetric water content) using different equations.
}
\usage{
eps2theta(epsdata,   equation)
theta2eps(thetadata, equation, check_range = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{epsdata}{
dataframe holding at least a column \code{epsilon} and potentially others, depending on the choice of \code{equation}. See details.
}
  \item{thetadata}{
dataframe holding at least a column \code{theta} (volumetric water content, [-]) and potentially others, depending on the choice of \code{equation}. See details.
}
  \item{equation}{
Equation to be used. Must be one of 
\code{"deltaT_minorg"}, 
\code{"DrnevichEtal2005"},
\code{"FerschEtal2017"}, 
\code{"Jacobsen_Schjonning1993"},
\code{"JuEtal2010"},
\code{"MalickiEtal1996"},
\code{"RothEtal1992"},
\code{"SchaapEtal1997"},
\code{"SinghEtal2017"},
\code{"ToppEtal1980"}, 
\code{"ZhaoEtal2016"}.

}

\item{check_range}{= TRUE. Raise error if theta is outside [0,1]
}
}
\details{
Applies various equations, most of which are listed in Mohamed & Paleologos, 2018.
The following columns in the data frame \code{epsdata} are supported and may be required, depending on the choice of the equation:
\itemize{
\item{\var{epsilon}:} { Electric permittivity. [-]} 
\item{\var{soil}:} { Soil type indicator. \code{"mineral", "organic"}} 
\item{\var{n}:} { Soil porosity. [-]}
\item{\var{BD}:} { dry bulk density. [\eqn{g/\text{cm}^3}{g/cm^3}]}
\item{\var{clay_perc}:} { Percent clay. [\%]}
\item{\var{om_perc}:} { Percent organic matter. [\%]}
\item{\var{cohesive}:} { is the soil cohesive? [0/1]}
}
   
\code{theta2eps} is the inverse of \code{eps2theta}. However, currently only implemented for \code{equation=c("deltaT_minorg", "SchaapEtal1997", "ToppEtal1980"} by piecewise linear interpolation.
  
\subsection{Equations}{   
The respective equations used are:

\itemize{

  \item \code{"deltaT_minorg"} (Delta-T Devices Ltd, 1998):
  \deqn{\theta =  \frac{\sqrt{\epsilon}-a_0}{a_1}}{%
  \theta =  (\sqrt(\epsilon)-a_0)/a_1.
  } with \eqn{a_0=1.6} and \eqn{a_1=8.4} for mineral soils, \eqn{a_0=1.3} and \eqn{a_1=7.7} for organic soils 
  
  
  \item \code{"DrnevichEtal2005"} (Drnevich et al., 2005):
  \deqn{\theta =  \frac{\sqrt{\epsilon}-\frac{a}{BD}}{b}
  }{%
  \theta = (\sqrt(\epsilon) - a * BD) / b
  } with \eqn{a=0.95} and \eqn{b=8.8} for cohesive soils, \eqn{a=1} and \eqn{b=8.5} for non-cohesive soils, 
  
  
  \item \code{"FerschEtal2017"} (Fersch et al. 2017):
  \deqn{\theta =  \frac{(\sqrt(\epsilon) - (1 - n* \sqrt(3.29)) - n * 1)}{
        (\sqrt(80) - \sqrt(1))}
  }{%
  \theta = (\sqrt(\epsilon) - (1 - n* \sqrt(3.29)) - n * 1) /
        (\sqrt(80) - \sqrt(1))
  } 
  
  \item \code{"Jacobsen_Schjonning1993"} (Jacobsen & Schjonning, 1993):
  \deqn{\theta =  -3.41*1e-2 +3.45*1e-2*\epsilon-11.4*1e-4*\epsilon^2 + 17.1*1e-6*\epsilon^3- 3.7*1e-2*BD + 7.36*1e-4*clay\_perc + 47.7*1e-4 * om\_perc
  }{%
  \theta = -3.41*1e-2 +3.45*1e-2*\epsilon-11.4*1e-4*\epsilon^2 + 17.1*1e-6*\epsilon^3-
        3.7*1e-2*BD + 7.36*1e-4*clay_perc + 47.7*1e-4 * om_perc
  } 
  
  \item \code{"JuEtal2010"} (Ju et al., 2010):
  \deqn{\theta =  0.1228*\sqrt{\epsilon} - 0.1322*BD -0.0152
  }{%
  \theta = 0.1228*\sqrt(\epsilon) - 0.1322*BD -0.0152
  } 
  
  
  \item \code{"MalickiEtal1996"} (Malicki et al., 1996):
  \deqn{\theta =  \frac{\sqrt{\epsilon}-0.819-0.168*BD-0.159*BD^2}{7.17+1.18*BD}
  }{%
  \theta =  (\sqrt(\epsilon)-0.819-0.168 BD-0.159 BD^2)/(7.17+1.18 BD)
  }
  
  \item \code{"RothEtal1992"} (Roth et al., 1992):
  \deqn{\theta =  (a_1 + a_2*{\epsilon} +a_3 *{\epsilon}^2 + a_4*{\epsilon}^3)
  }{%
  \theta =  (a_1 + a_2*\epsilon +a_3 *\epsilon^2 + a_4*\epsilon^3)
  }
   with \eqn{a_1 = -0.0728, a_2 = 0.0448, a_3 = -0.00195, a_4= 0.0000361} for mineral soils and \eqn{a_1 =  0.0233, a_2 = 0.0285, a_3 = -0.000431, a_4 = 0.00000304} for organic soils 
  
  \item \code{"SchaapEtal1997"} (Schaap et al. 1997 [eq. 2b in Vaz et al. 2013]):
  \deqn{\theta =  (0.133*\sqrt{\epsilon} -0.146)^{0.885}s
  }{%
  \theta = (0.133*\sqrt(\epsilon) -0.146)^{0.885}
  }  
    
  \item \code{"SinghEtal2017"} (Singh et al. 2019 (version for CS655)):
  \deqn{\theta = a*\sqrt{\epsilon} + b
  }{%
  \theta = a*\sqrt(\epsilon) + b
  }
  with 
  \deqn{a = a_1*clay\_perc^2 + a_2*clay\_perc + a_3
  }{%
  	a = a1*clay_perc^2 + a_2*clay_perc + a_3
  }
  \deqn{b = b_1*clay\_perc^2 + b_2*clay\_perc + b_3
  }{%
  	b = b_1*clay_perc^2 + b_2*clay_perc + b3
  }  
   and the coefficients for CS655 (\eqn{a_1=-3.33e-5, a_2=1.14e-3, a_3=0.108, b_1=6.52e-5, b_2=-2.48e-3, b_3=-0.16})
  	
     
  \item \code{"ToppEtal1980"} (Topp et al., 1980):
  \deqn{\theta =  -0.053+ 2.92e-2*\epsilon -5.5e-4*\epsilon^2 + 4.3e-6*\epsilon^3
  }{%
  \theta = -0.053+ 2.92e-2*\epsilon -5.5e-4*\epsilon^2 + 4.3e-6*\epsilon^3
  }
  
  \item \code{"ZhaoEtal2016"} (Zhao et al., 2016):
  \deqn{\theta =  \frac{0.3039 * BD - 2.1851 + \sqrt{\epsilon}}{(18.0283 * BD - 17.9531) + (-0.6806 * BD + 1.8351) * \sqrt{\epsilon}}
  }{%
  \theta = (0.3039 * BD - 2.1851 + \sqrt(\epsilon))/((18.0283 * BD - 17.9531) + (-0.6806 * BD + 1.8351) * \sqrt(\epsilon))
  }
}

}

}
\value{
A vector of reals, i.e. theta (volumetric water content) [-].

For \code{equation=="list"}, a named list of all supported equations is returned. The elements in the list are character strings, denoting the necessary input fields for the equations.
}

\references{
Delta-T Devices Ltd, 1998. ThetaProbe SOIL MOISTURE SENSOR TYPE ML2 USER MANUAL (ML2-UM-1). Delta-T Devices Ltd, Cambridge, UK.

Drnevich, V.P., Ashmawy, A.K., Yu, X., Sallam, A.M., 2005. Time domain reflectometry for water content and density of soils: study of soil-dependent calibration constants. Can. Geotech. J. 42, 1053-1065. \url{https://doi.org/10.1139/T05-047}

Fersch, B., Jagdhuber, T., Schroen, M., Voelksch, I., Jaeger, M., 2018. Synergies for Soil Moisture Retrieval Across Scales From Airborne Polarimetric SAR, Cosmic Ray Neutron Roving, and an In Situ Sensor Network. Water Resour. Res. 54, 9364-9383. \url{https://doi.org/10.1029/2018WR023337}

Jacobsen, O., Schjonning, P., 1993. A Laboratory Calibration of Time-Domain Reflectometry for Soil-Water Measurement Including Effects of Bulk-Density and Texture. J. Hydrol. 151, 147-157. \url{https://doi.org/10.1016/0022-1694(93)90233-Y}

Ju, Z., Liu, X., Ren, T., & Hu, C., 2010. Measuring Soil Water Content With Time Domain Reflectometry. Soil Science, 175(10), 469-473. \url{doi:10.1097/ss.0b013e3181f55aa3} 

Malicki, M.A., Plagge, R., Roth, C.H., 1996. Improving the calibration of dielectric TDR soil moisture determination taking into account the solid soil. Eur. J. Soil Sci. 47, 357-366. \url{https://doi.org/10.1111/j.1365-2389.1996.tb01409.x}

Mohamed, A.-M.O., Paleologos, E.K., 2018. Dielectric Permittivity and Moisture Content, in: Fundamentals of Geoenvironmental Engineering. Elsevier, pp. 581-637. \url{https://doi.org/10.1016/B978-0-12-804830-6.00016-8}

Singh, J., Lo, T., Rudnick, D.R., Irmak, S., Blanco-Canqui, H., 2019. Quantifying and correcting for clay content effects on soil water measurement by reflectometers. Agric. Water Manage. 216, 390-399. \url{https://doi.org/10.1016/j.agwat.2019.02.024}

Topp, G., Davis, J., Annan, A., 1980. Electromagnetic Determination of Soil-Water Content - Measurements in Coaxial Transmission-Lines. Water Resour. Res. 16, 574-582. \url{https://doi.org/10.1029/WR016i003p00574}

Vaz, C. & Jones, Scott & Meding, Stephen & Tuller, Markus. (2013). Evaluation of Standard Calibration Functions for Eight Electromagnetic Soil Moisture Sensors. Vadose Zone Journal. 12. 10.2136/vzj2012.0160. \url{https://doi.org/10.2136/vzj2012.0160}

Zhao, Y., Ling, D., Wang, Y., Huang, B., Wang, H., 2016. Study on a calibration equation for soil water content in field tests using time domain reflectometry. J. Zhejiang Univ.-SCI A 17, 240-252. \url{https://doi.org/10.1631/jzus.A1500065}

}
\author{
Till Francke
}

\note{
The different equations can automatically tested and compared with \code{\link{compare_eps2theta_equations}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{correct_sensor_voltage}}
\code{\link{compare_eps2theta_equations}}
}
\examples{
#return list of supported equations
eps2theta(epsdata = NULL, equation = "list")

\dontrun{
# example 2: reproduce figure on page 10, DeltaT manual (voltage vs. theta) ####
  # load linearisation table
  lin_file = system.file("example", "linearization_Theta_Probe.txt", package = "FDR2soilmoisture") #Linearisation table from DeltaT ThetaProbe manual. p. 14
  lin_data = read.table(lin_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file

  #plot data from linearization table
  plot(1, xlim=c(0, 1.2), ylim= c(0,1), xlab="ThetaProbe voltage [V]", ylab="theta", type="n")
  points(lin_data$V_min, lin_data$theta, col="blue",pch=0)
  points(lin_data$V_org, lin_data$theta, col="green", pch=0)

  V = seq(from=0.0, to=1.3, length.out=100) #voltage values to plot along
  
  #add graphs produced with polynoms (eq. 1)
  #mineral
  theta = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = "Theta Probe polynomial"), soil="mineral"), equation="deltaT_minorg")
  lines(V, theta, col="blue", lty="dashed")
  #organic
  theta = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = "Theta Probe polynomial"), soil="organic"), equation="deltaT_minorg")
  lines(V, theta, col="green", lty="dashed")
  
  #add graphs produced with lookup-function (table page 11)
  #mineral
  theta_min = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = "Theta Probe"), soil="mineral"), equation="deltaT_minorg")
  lines(V, theta_min, col="blue", lty=1)
  #organic
  theta_org = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = "Theta Probe"), soil="organic"), equation="deltaT_minorg")
  lines(V, theta_org, col="green", lty=1)
  
  
  legend("topleft", legend=c("mineral", "organic", "lookup table", "polynomial", "lookup, linear interpolation"),
              lty=c(1,1,0,2,1), pch=c(NA,NA, 0,NA, NA), col=c("blue", "green", "black", "black", "black"))
  #manual, p. 10 states:
  # "In the range 0 to 1 Volt (corresponding to a soil moisture range 0 to ~ 0.55 by volume),
  # this relationship can be fitted very precisely by a 3rd order polynomial"
  #manual, p. 11 states:
  #"For very high moisture contents (theta > 0.5 m3*m-3), the polynomial equation should be used.  This is usually only necessary for organic soils."
  # However, the polynom obviously differs considerably from the points in the linearization table
  # especially for U > 1 V or theta > 0.5, so only the statement on page 10 seems correct.
  
  
  
# example 2b) add curves from device-specific calibration ####
  calib_file = system.file("example", "sensor_calibration.txt", package = "FDR2soilmoisture") #replace this by your own calibration file
  calib_data = read.table(calib_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file
  
  calib_data = calib_data[calib_data$type=="Theta Probe",] #only use calibration data from Theta Probes
  
  #plot the point obtained from the calibration records
  points(calib_data$air_measurement/1000, rep(0, nrow(calib_data)), col="red")
  points(calib_data$water_measurement/1000, sqrt(rep(1, nrow(calib_data))), col="red")

  #compute corresponding corrected voltage for all sensors in calibration list
  V_corr = sapply(FUN = correct_sensor_values, X = calib_data$serial_no,  V=V, calib_data=calib_data, adjust_range = FALSE,var_type = "Voltage")
  #...and the respective epsilon
  eps_corr = apply(FUN = V2eps, X =  V_corr,  MARGIN = 1, type="Theta Probe")
  #...and the respective theta
  eps2theta_wrapper = function(eps, soil)
  {
    eps2theta(epsdata = data.frame(epsilon=eps, soil=soil), equation="deltaT_minorg")
  }
  theta_corr_min = apply(FUN = eps2theta_wrapper, X =  eps_corr,  MARGIN = 1, soil="mineral")
  theta_corr_org = apply(FUN = eps2theta_wrapper, X =  eps_corr,  MARGIN = 1, soil="organic")
  
  #plot lines for all sensors
  for (i in 1:ncol(theta_corr_min))
    lines(V, theta_corr_min[,i], col="lightblue", lty="dotted")

  for (i in 1:ncol(theta_corr_org))
    lines(V, theta_corr_org[,i], col="lightgreen", lty="dotted")
  
  #add original lines again on top
  lines(V, theta_min, col="blue", lty=1)  #mineral
  lines(V, theta_org, col="green", lty=1) #organic

#example 2b: add plot showing range of theta values
    range_theta_corr_min = apply(X=theta_corr_min, MAR=1, FUN=function(x){diff(range(x, na.rm=TRUE))})
    lines(V, range_theta_corr_min, col="blue", lwd=2)
    median(range_theta_corr_min) 
    
    range_theta_corr_org = apply(X=theta_corr_org, MAR=1, FUN=function(x){diff(range(x, na.rm=TRUE))})
    lines(V, range_theta_corr_org, col="green", lwd=2)
    median(range_theta_corr_org) 
    
    legend("topleft", legend=c("mineral", "organic", "lookup table", "polynomial", "lookup, linear interpolation", "..., uncorrected sensors", "range of theta"),
           lty=c(1,1,0,2,1,3,1), pch=c(NA,NA, 0,NA, NA, NA), col=c("blue", "green", "black", "black", "black", "black", "black"),
           lwd=c(rep(1,6),2))

}
}
