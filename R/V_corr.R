#linear correction of sensor voltage using measured voltages in air and water

#V_corr = function(V, V_air_meas, V_h2o_meas, type)
#{
#  V_air_nominal = eps2V(eps_air, type) #theoretical value for voltage in air [V]
#  V_h2o_nominal = eps2V(eps_h2o, type) #theoretical value for voltage in water  [V]
#  
#  V_corr = (V_h2o_nominal - V_air_nominal) / (V_h2o_meas - V_air_meas) * (V - V_air_meas) + V_air_nominal
#  return(V_corr)
#}

# add function for different inputs
#here the function should depend on the input variable, e.g. counts/perm/etc
var_corr = function(x, var_air_meas, var_h2o_meas, type, var_type)
{
  #here the function should depend on the input variable, e.g. counts/perm/etc
  if (var_type=="Voltage"){
    var_air_nominal = eps2V(eps_air, type) #theoretical value for voltage in air [V]
    var_h2o_nominal = eps2V(eps_h2o, type) #theoretical value for voltage in water [V]
  } else if(var_type=="Permittivity"){
    var_air_nominal = eps_air #theoretical value for epsilon in air
    var_h2o_nominal = eps_h2o #theoretical value for epsilon in water
  } else{
    var_air_nominal = NA #theoretical value for epsilon in air
    var_h2o_nominal = NA #theoretical value for epsilon in water
    warning("Only Voltage and Permittivity are yet implemented, giving NA for other input variables")
  }
    
  var_corr = (var_h2o_nominal - var_air_nominal) / (var_h2o_meas - var_air_meas) * (x - var_air_meas) + var_air_nominal
  return(var_corr)
}

#implementing older function:

V_corr = function(V, V_air_meas, V_h2o_meas, type)
{
  V_corr = var_corr(x = V, var_air_meas = V_air_meas, var_h2o_meas = V_h2o_meas, type = type, var_type = "Voltage")
  return(V_corr)
}

