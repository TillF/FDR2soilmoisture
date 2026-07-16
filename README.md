# FDR2soilmoisture
R-package for processing data of FDR-measurements (Frequency Domain Reflectrometry) to obtain soil moisture. 
Obtain sensor specific calibrations, choose or adjust suitable conversion functions between voltage, permittivity and soil moisture. 

Customized for PR2 profile probes and Theta Probes (both produced by DeltaT):
Assuming that the deviation in the measured variable of the sensor is linear, it can be effectively corrected using two reference points. By taking measurements in both air and water for each sensor, the measured variable can be linearly transformed, ensuring that the manufacturer's conversion from the primary measured variable to permittivity remains valid for both mediums. This allows a correction of voltage for the ThetaProbes and PR2/6 ProfileProbes. For the SMT100 sensors, permittivity can be treated as the primary variable and adjusted accordingly, since the conversion from the raw variable to permittivity provided by the manufacturer is device specific and should be preserved.

The following equations for converting epsilon (permittivity) to theta (volumetric water content) are included:
deltaT, manufacturer's equation, 
Drnevich et al. (2005), 
Fersch et al. (2017), 
Jacobsen_Schjonning (1993), 
Ju et al. (2010), 
Malicki et al. (1996), 
Roth et al. (1992), 
Topp et al. (1980),  
Schaap et al. (1997), 
Zhao et al. (2016).

## INSTALLATION

* command line installation:

```R
install.packages("devtools") 
library(devtools)
install_github("TillF/FDR2soilmoisture")
```

* from zip/tar:
	* download zip/tar from github: [>LINK<](https://github.com/TillF/FDR2soilmoisture/releases)
	* install via command line or R-GUI

## FEEDBACK and BUGS

Feel free to comment via github issues: [>LINK<](https://github.com/TillF/FDR2soilmoisture/issues)
