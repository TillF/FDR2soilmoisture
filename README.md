# FDR2soilmoisture
R-package for processing data of FDR-measurements (Frequency Domain Reflectrometry) to obtain soil moisture. 
Obtain sensor specific calibrations, choose or create suitable conversion functions between voltage, permittivity and soil moisture. 
Customized for PR2 profile probes and Theta Probes (both produced by DeltaT).
The following equations for converting epsilon (permittivity) to theta (volumetric water content) are included:
deltaT, manufacturer's equation, 
Drnevich et al. (2005), 
Fersch et al. (2017),  
Jacobsen_Schjonning1993), 
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
	* install via R-GUI



## FEEDBACK and BUGS

Feel free to comment via github issues: [>LINK<](https://github.com/TillF/FDR2soilmoisture/issues)


