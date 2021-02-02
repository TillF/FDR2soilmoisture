# functions for converting sensor permittivity [-] to voltage  [Volts] and back


#.onLoad <- function(libname, pkgname) {
print("hello from onload")
  # replace polynom (inappropriate over 0.55 V) with converted lookup table ####
  lin_file = system.file("example", "ThetaProbe.txt", package = "FDR2soilmoisture") #Linearisation table from DeltaT ThetaProbe manual. p. 14
  lin_data = read.table(lin_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file

  lin_data = lin_data[-c(1, nrow(lin_data)),] #remove first and last entry that served for extrapolation only

  #reconstruct epsilon values
  epsilon_org = theta2eps(thetadata = data.frame(theta=lin_data$theta, 
                                               soil=rep("organic", nrow(lin_data))), equation = "deltaT_minorg")
  epsilon_min = theta2eps(thetadata = data.frame(theta=lin_data$theta, 
                                               soil=rep("mineral", nrow(lin_data))), equation = "deltaT_minorg")

  #put into data frames
  lin_data_min = data.frame(epsilon=epsilon_min, voltage = lin_data$V_min)
  lin_data_org = data.frame(epsilon=epsilon_org, voltage = lin_data$V_org)

  #generate mean data (between mineral and organic) that will be used as a general lookup table
  # because "mineral" and "organic" differ slightly for high moisture values
  V = lin_data_org$voltage

  eps_org = lin_data_org$epsilon
  #get epsilon values for the same voltages as in "organic"
  eps_min = approx(x=lin_data_min$voltage, y=lin_data_min$epsilon, xout = V)$y

  #do averaging between "mineral" and "organic", disregarding NAs
  eps_mean = apply(X = cbind(eps_min, eps_org), MAR=1, FUN=mean, na.rm=TRUE)

  #extend to epsilon=1 using manufactures equation
  eps_mean = c(1, eps_mean)
  V_mean = c(eps2V(eps = eps_mean[1], type = "Theta Probe"), V)  

  V2eps_theta_probe_table = approxfun(x=V_mean, y = eps_mean)

#}

print("hello outside onload")

# voltage to permittivity ####
  V2eps = function(V, type)
  {  

    if (grepl(type, pattern="PR2"))
    #PR2
    #default equation according to manual (PR2_user_manual_version_5.0.pdf, eq. 2)
    # (PR2_SDI-12-_User_Manual_version_4_1.pdf, eq. 2)
    return(c(eps =(1.125 - 5.53*V + 67.17*V^2 - 234.42*V^3 + 413.56*V^4 - 356.68*V^5 + 121.53*V^6)^2)) 
    
    #theta-probe, polynomial
    #convert to epsilon, eq. 1 of Theta Probe user manual, p.12
    if (grepl(type, pattern="Theta Probe polynomial"))
      return(c(eps = (1.07 + 6.4*V-6.4*V^2+4.7*V^3 )^2))
    
    #theta-probe, lookup values reconstructed from table p. 14
    #converted to epsilon, eq. 1 of Theta Probe user manual, p.12
    #this is more general than "Theta Probe polynomial", as it also fits above 0.55 V
    if (grepl(type, pattern="Theta Probe"))
      return(c(eps =V2eps_theta_probe_table(V)))
    
    stop("type must be 'PR2', 'Theta Probe' or 'Theta Probe polynomial'.")
  }

#permittivity to voltage ####
  i_eps2V = list() #internal list containing functions for each sensor type
  #find inverse functions: since there are no easy analytical inverse, we use piecewise linear inverse functions 
  for (probe_type in c("PR2", "Theta Probe"))
  {  
    minV = optimize(V2eps, interval=c(-0.2, 0.1), type = probe_type)$minimum  #find minimum of regression, i.e. start of monotonically increasing parabola
  
    V_synth = seq(from=minV, to=1.35, length.out=500) #range of Voltage values [V] used for lookup table
    eps_synth = V2eps(V_synth, type=probe_type) #range of epsilon values
    i_eps2V[[probe_type]] = approxfun(x=eps_synth, y=V_synth) #use piecewise linear approximation

    # # illustration ####
    # V2 =i_eps2V[[probe_type]](eps_synth) #approximate inverse of default regression
    # plot(V_synth, eps_synth, type="l", lwd=2, main=probe_type) #, xlim=c(0,0.1), ylim=c(0,))
    # lines(V2, eps_synth, col="red", lty="dashed")
  }
  
  eps2V = function(eps, type) #wrapper for internal function created above
  {  
    #replace e.g. "PR2, analogue" by "PR2"
    type = sub(x = type, pattern = paste0("^(", paste0(names(i_eps2V), collapse="|" ), ").*"), repl="\\1") 
    if (!(type %in% names(i_eps2V)))
      stop("Unknown probe type. Must be one of ('", paste0(names(i_eps2V), collapse="', '"),"').")
    return(i_eps2V[[type]](eps))
  }

  