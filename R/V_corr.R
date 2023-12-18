#linear correction of sensor values using measured voltages in air and water

#function depend on the input variable, e.g. counts/perm/etc
var_corr = function(x, var_air_meas, var_h2o_meas, type, var_type, temp, ...)
{
  # processing additional arguments for SMT100
  alpha_set <- -0.1305
  beta_set <- 0.2549
  gamma_set <- 1.8342
  if (!is.null(list(...)$alpha)) {
    alpha_set <- list(...)$alpha
  }
  if (!is.null(list(...)$beta)) {
    beta_set <- list(...)$beta
  }
  if (!is.null(list(...)$gamma)) {
    gamma_set <- list(...)$gamma
  }
  
  
  #processing reference values for  permittivity with temperature correction for water
  eps_0 <- eps_air
  eps_1 <- eps_h2o
  
  if (is.na(temp)) {
    # temp = 20
    warning("Since no temperature \"temp\" is given in calib_data, a permittivity of 81 (ca. 18 degree Celsius) is used for water as default\n")
  } else {
    #use iapws equation if package is installed
    if (!requireNamespace("iapws", quietly = TRUE)) {
      eps_1 <- eps_water(T = temp, equ = "Weast86")
      warning("eps_water(equ =\"Weast86\") is used to calculate the temperature dependece of epsilon.
            Consider the installation of the iapws package (see ?eps_water) or the usage of individual permittivity reference values (see ?correct_sensor_values).")
    } else {
      eps_1 <- eps_water(T = temp, equ = "iapws")
    }
  }
  
  # in case individual permittivity values were given
  if (!is.null(list(...)$epsilon_0)) {
    eps_0 <- list(...)$epsilon_0
    #print("You are using your own reference values")
    }
  if (!is.null(list(...)$epsilon_1)) {
    eps_1 <- list(...)$epsilon_1
    #print("You are using your own reference values")
    }
  

  
  #here the function depend on the input variable, e.g. counts/perm/etc
  if (var_type=="Voltage"){
    var_air_nominal = eps2V(eps_0, type) #theoretical value for voltage in air [V]
    var_h2o_nominal = eps2V(eps_1, type) #theoretical value for voltage in water [V]
    
  } else if(var_type=="Permittivity"){
    
    var_air_nominal = eps_0 #theoretical value for epsilon in air
    var_h2o_nominal = eps_1 #theoretical value for epsilon in water
    
  } else if(var_type=="Counts"){
    var_air_nominal = eps2counts(perm = eps_0, alpha = alpha_set, beta = beta_set, gamma = gamma_set) #theoretical value for epsilon in air
    var_h2o_nominal = eps2counts(perm = eps_1, alpha = alpha_set, beta = beta_set, gamma = gamma_set) #theoretical value for epsilon in water
    
  } else{
    var_air_nominal = NA #theoretical value for epsilon in air
    var_h2o_nominal = NA #theoretical value for epsilon in water
    warning("Only Voltage, Permittivity and Counts are yet implemented, giving NA for other input variables.\n")
  }
  
  # Apply a correction function to correct the measured voltage var_raw of the air (var_air_meas) and water (var_h2o_meas)
  # to the expected "should-be" ones (var_air_exp , V_h2o_exp ). Using the linear interpolation graph between those points
  
  var_corr = (var_h2o_nominal - var_air_nominal) / (var_h2o_meas - var_air_meas) * (x - var_air_meas) + var_air_nominal
  #return corrected values
  return(var_corr)
}

#implementing older function:

V_corr = function(V, V_air_meas, V_h2o_meas, type)
{
  V_corr = var_corr(x = V, var_air_meas = V_air_meas, var_h2o_meas = V_h2o_meas, type = type, var_type = "Voltage")
  return(V_corr)
}

