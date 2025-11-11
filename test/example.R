#library(FDR2soilmoisture)



# example 2: reproduce figures of manuals ####
  probe="Theta Probe"
  #probe="PR2"

  if (probe=="Theta Probe")
  {
    #for Theta Probe: DeltaT manual (voltage vs. theta), page 10 ####  
    lin_file = system.file("example", "linearization_Theta_Probe.txt", package = "FDR2soilmoisture") #Linearisation table from DeltaT ThetaProbe manual. p. 14
  }  else
  {  
    #for PR2: [no corresponding figure] 
    lin_file = system.file("example", "linearization_PR2.txt", package = "FDR2soilmoisture") #Linearisation table from DeltaT ThetaProbe manual. p. 14
  }
  
  # load linearisation table
  lin_data = read.table(lin_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file

  #plot data from linearization table
  plot(1, xlim=c(-0.1, 1.0), ylim= c(-0.1, 1.5), xlab=paste0(probe, "voltage [V]"), ylab="theta", type="n")
  plot(1, xlim=c(0.7, 1.1), ylim= c(-0.1, 1.5), xlab=paste0(probe, "voltage [V]"), ylab="theta", type="n")
  points(lin_data$V_min, lin_data$theta, col="blue",pch=0)
  points(lin_data$V_org, lin_data$theta, col="green", pch=0)

  V = seq(from=0.0, to=1.3, length.out=100) #voltage values to plot along
  
  suffix="" 
  if (probe == "Theta Probe")
    suffix = " polynomial" #for "Theta Probe", "polynomial" is not the default
  
  #add graphs produced with polynoms (eq. 1)
  #mineral
  theta = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = paste0(probe, suffix)), soil="mineral"), equation="deltaT_minorg")
  lines(V, theta, col="blue", lty="dashed")
  #organic
  theta = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = paste0(probe, suffix)), soil="organic"), equation="deltaT_minorg")
  lines(V, theta, col="green", lty="dashed")
  
  if (probe == "Theta Probe")
  {
    #add graphs produced with lookup-function (table page 11)
    #mineral
    theta_min = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = probe), soil="mineral"), equation="deltaT_minorg")
    lines(V, theta_min, col="blue", lty=1, lwd=2)
    #organic
    theta_org = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = probe), soil="organic"), equation="deltaT_minorg")
    lines(V, theta_org, col="green", lty=1, lwd=2)
  }  
  
  legend("topleft", legend=c("mineral", "organic", "lookup table", "polynomial", "lookup, linear interpolation"),
         lty=c(1,1,0,2,1), pch=c(NA,NA, 0,NA, NA), col=c("blue", "green", "black", "black", "black"))
  
  
  #######
  #reconstruct epsilon values
  epsilon_org = theta2eps(thetadata = data.frame(theta=lin_data$theta, 
                                                 soil=rep("organic", nrow(lin_data))), equation = "deltaT_minorg")
  epsilon_min = theta2eps(thetadata = data.frame(theta=lin_data$theta, 
                                                 soil=rep("mineral", nrow(lin_data))), equation = "deltaT_minorg")
  
  #put into data frames
  lin_data_min = data.frame(epsilon=epsilon_min, voltage = lin_data$V_min)
  lin_data_org = data.frame(epsilon=epsilon_org, voltage = lin_data$V_org)
  
  # generate mean data (between mineral and organic) that will be used as a general lookup table
  # because "mineral" and "organic" differ slightly for high moisture values
  V = lin_data_org$voltage
  
  eps_org = lin_data_org$epsilon
  #get epsilon values for the same voltages as in "organic"
  eps_min = approx(x=lin_data_min$voltage, y=lin_data_min$epsilon, xout = V)$y
  
  #do averaging between "mineral" and "organic", disregarding NAs
  eps_mean = apply(X = cbind(eps_min, eps_org), MAR=1, FUN=mean, na.rm=TRUE)
  
  #extend data table to epsilon=eps_air using manufacturer's equation
  eps_mean = c(eps_air, eps_mean)
  V_mean = c(eps2V(eps = eps_mean[1], type = paste0(probe, " polynomial")), V)  
  
  
  V = seq(from=0, to=1, length.out=50)
  epsmean1 = V2eps(V, type=probe)
  epsmean2 = V2eps(V, type="PR2 polynomial")
  epsmean3 = (1.125 - 5.53*V + 67.17*V^2 - 234.42*V^3 + 413.56*V^4 - 356.68*V^5 + 121.53*V^6)^2 
  epsmean4 =  .V2eps(V)
  
  plot(V, epsmean1, type="l")
  lines(V, epsmean2, type="l", col="green") #ok 
  lines(V, epsmean3, type="l", col="red") #ok
  lines(V, epsmean4, type="l", col="orange")
  lines(V_mean, eps_mean)
  
  
  theta_org = eps2theta(epsdata = data.frame(epsilon =eps_mean, soil="organic"), equation="deltaT_minorg")
  #points(V_mean, theta_org, col="red")
  #lines(V_mean, theta_org, col="red")
  
  assign(".V2eps", value = approxfun(x=V_mean, y = eps_mean)) 
  
  theta_org2 = eps2theta(epsdata = data.frame(epsilon =.V2eps(V), soil="organic"), equation="deltaT_minorg")
  lines(V, theta_org2, col="red", lty=1)
  
  
  
  
  legend("topleft", legend=c("mineral", "organic", "lookup table", "polynomial", "lookup, linear interpolation"),
              lty=c(1,1,0,2,1), pch=c(NA,NA, 0,NA, NA), col=c("blue", "green", "black", "black", "black"))
  #manual, p. 10 states:
  # "In the range 0 to 1 Volt (corresponding to a soil moisture range 0 to ~ 0.55 by volume),
  # this relationship can be fitted very precisely by a 3rd order polynomial"
  #manual, p. 11 states:
  #"For very high moisture contents (θ > 0.5 m3.m-3), the polynomial equation should be used.  This is usually only necessary for organic soils."
  # However, the polynom obviously differs considerably from the points in the linearization table
  # especially for U > 1 V or theta > 0.5, so only the statement on page 10 seems correct.
  
  
  
# example 2b) add curves from device-specific calibration ####
  calib_file = system.file("example", "sensor_calibration.txt", package = "FDR2soilmoisture") #replace this by your own calibration file
  calib_data = read.table(calib_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file
  
  calib_data = calib_data[grepl(x = calib_data$type, probe),] #only use calibration data from Theta Probes
  
  #plot the point obtained from the calibration records
  points(calib_data$voltage_air_mV/1000, rep(0, nrow(calib_data)), col="red")
  points(calib_data$voltage_water_mV/1000, sqrt(rep(1, nrow(calib_data))), col="red")

  #compute corresponding corrected voltage for all sensors in calibration list
  V_corr = sapply(FUN = correct_sensor_voltage, X = calib_data$serial_no,  V=V, calib_data=calib_data, adjust_range = FALSE)
  #...and the respective epsilon
  eps_corr = apply(FUN = V2eps, X =  V_corr,  MARGIN = 1, type=probe)
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

#example 2c: add plot showing range of theta values
    range_theta_corr_min = apply(X=theta_corr_min, MAR=1, FUN=function(x){diff(range(x, na.rm=TRUE))})
    lines(V, range_theta_corr_min, col="blue", lwd=2)
    median(range_theta_corr_min) 
    
    range_theta_corr_org = apply(X=theta_corr_org, MAR=1, FUN=function(x){diff(range(x, na.rm=TRUE))})
    lines(V, range_theta_corr_org, col="green", lwd=2)
    median(range_theta_corr_org) 
    
    legend("topleft", legend=c("mineral", "organic", "lookup table", "polynomial", "lookup, linear interpolation", "..., uncorrected sensors", "range of theta"),
           lty=c(1,1,0,2,1,3,1), pch=c(NA,NA, 0,NA, NA, NA), col=c("blue", "green", "black", "black", "black", "black", "black"),
           lwd=c(rep(1,6),2))
    


# example 3: mismatch between using linearization and polynomial ####
    V = seq(from=0.0, to=1.3, length.out=100) #voltage values to plot along
    
    #add graphs produced with polynoms (eq. 1)
    #mineral
    theta_poly_min = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = paste0(probe, " polynomial")), soil="mineral"), equation="deltaT_minorg")
    #organic
    theta_poly_org = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = paste0(probe, " polynomial")), soil="organic"), equation="deltaT_minorg")

    #add graphs produced with lookup-function (table page 11)
    #mineral
    theta_lookup_min = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = paste0(probe)), soil="mineral"), equation="deltaT_minorg")
    #organic
    theta_lookup_org = eps2theta(epsdata = data.frame(epsilon =V2eps(V, type = paste0(probe)), soil="organic"), equation="deltaT_minorg")

    plot (1, xlim=c(0,1), ylim=c(0,1), xlab="theta (lookup / corrected)", ylab = "theta (polynome / old)", type="n")
    lines(theta_lookup_min, theta_poly_min, col="blue")
    lines(theta_lookup_org, theta_poly_org, col="green")
    abline(b=1, a=0)
    
    legend("topleft", legend=c("mineral", "organic"),
           lty=c(1,1), col=c("blue", "green"))
    

    
# example 4: comparison between linearization and polynomial ####
    probe = "Theta Probe"
    
    V = seq(from=0.0, to=1.3, length.out=100) #voltage values to plot along
    
    #add graphs produced with polynomes (eq. 1)
    eps_poly = V2eps(V, type = paste0(probe, " polynomial"))

    #add graphs produced with lookup-function (table page 11)
    #mineral
    eps_lookup = V2eps(V, type = paste0(probe))
    
    #plot (1, xlim=c(0,1.3), ylim=c(0,80), xlab="V [(lookup / corrected)]V]", ylab = "eps [-]", type="n")
    plot (1, xlim=c(-0.1,0.1), ylim=c(0,5), xlab="V [(lookup / corrected)]V]", ylab = "eps [-]", type="n")
    lines(V, eps_poly, col="blue")
    lines(V, eps_lookup, col="green")

    
    legend("topleft", legend=c("poly", "organic"),
           lty=c(1,1), col=c("blue", "green"))
    
    eps2V(eps_air,type="Theta Probe")
    
    V2eps(eps2V(eps_air,type="Theta Probe"),type="Theta Probe")
    V2eps(eps2V(eps_air,type="Theta Probe polynomial"),type="Theta Probe polynomial")
    
        eps_air
            