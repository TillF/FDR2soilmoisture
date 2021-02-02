# functions for converting sensor permittivity [-] to voltage  [Volts] and back

# voltage to permittivity ####
  V2eps = function(V, type)
  {  

    if (grepl(type, pattern="PR2"))
    #PR2
    #default equation according to manual (PR2_user_manual_version_5.0.pdf, eq. 2)
    # (PR2_SDI-12-_User_Manual_version_4_1.pdf, eq. 2)
    return(c(eps =(1.125 - 5.53*V + 67.17*V^2 - 234.42*V^3 + 413.56*V^4 - 356.68*V^5 + 121.53*V^6)^2)) 
    
    #theta-probe
    #convert to epsilon, eq. 1 of ThetaProbe user manual, p.12
    if (grepl(type, pattern="Theta Probe"))
      return(c(eps = (1.07 + 6.4*V-6.4*V^2+4.7*V^3 )^2))
    
    stop("type must be 'PR2' or 'Theta Probe'.")
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

  