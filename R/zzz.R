.onLoad <- function(libname, pkgname) {
  # we construct the functions V2eps and eps2V for type="Theta Probe" here, because we use some
  # functions that need to be defined before
  
  # replace polynom (inappropriate over 0.55 V(Theta Probe) ) with converted lookup table ####
  for (probe in c("Theta Probe")) #for PR2, this problem does not exist
  {
    lin_file = system.file("example", paste0("linearization_", sub(probe, pattern = " ", replacement="_"), ".txt"), package = "FDR2soilmoisture") #Linearisation table from DeltaT ThetaProbe manual. p. 14
    if (!file.exists(lin_file))
      next #dirty workaround to stop crashes when installing the package and the file is not yet available
    lin_data = utils::read.table(lin_file, nrow=-1, sep="\t", stringsAsFactors = FALSE, header=TRUE, na.strings = c("NA",""))  #load the file
    
    #if (probe =="Theta Probe")
    #  lin_data = lin_data[-c(1, nrow(lin_data)),] #remove first and last entry that served for extrapolation only
    
    #reconstruct epsilon values
    epsilon_org = theta2eps(thetadata = data.frame(theta=lin_data$theta, 
                                                   soil=rep("organic", nrow(lin_data))), equation = "deltaT_minorg",check_range = FALSE)
    epsilon_min = theta2eps(thetadata = data.frame(theta=lin_data$theta, 
                                                   soil=rep("mineral", nrow(lin_data))), equation = "deltaT_minorg",check_range = FALSE)
    
    #put into data frames
    lin_data_min = data.frame(epsilon=epsilon_min, voltage = lin_data$V_min)
    lin_data_org = data.frame(epsilon=epsilon_org, voltage = lin_data$V_org)
    
    # generate mean data (between mineral and organic) that will be used as a general lookup table
    # because "mineral" and "organic" differ slightly for high moisture values
    V = lin_data_org$voltage
    
    eps_org = lin_data_org$epsilon
    #get epsilon values for the same voltages as in "organic"
    eps_min = stats::approx(x=lin_data_min$voltage, y=lin_data_min$epsilon, xout = V)$y
    
    #do averaging between "mineral" and "organic", disregarding NAs
    eps_mean = apply(X = cbind(eps_min, eps_org), MARGIN =1, FUN=mean, na.rm=TRUE)
    
    #extend data table to epsilon=eps_air using manufacturer's equation
      eps_mean = c(eps_air, eps_mean)
      V_mean = c(eps2V(eps = eps_mean[1], type = paste0(probe, " polynomial")), V)  
      
    #update V2eps-function
    #globvars$V2eps_Theta_Probe_table = stats::approxfun(x=V_mean, y = eps_mean)
    #assign(".V2eps_Theta_Probe_table", value = stats::approxfun(x=V_mean, y = eps_mean), envir = parent.env(environment())) 
    assign(paste0(".V2eps_", sub(probe, pattern = " ", replacement="_"), "_table"), value = stats::approxfun(x=V_mean, y = eps_mean), envir = parent.env(environment())) 
    #.V2eps_PR2_table(v = 1.067)
    #update eps2V-function
    tt = .eps2V #get current values of list of conversion functions
    tt[[probe]] = stats::approxfun(y=V_mean, x = eps_mean) #update inverse function (so far, this was a dummy)
    #tt[[probe]](v=81)
    
    assign(".eps2V", value = tt, envir = parent.env(environment())) 
  } 
}