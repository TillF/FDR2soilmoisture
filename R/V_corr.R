#linear correction of sensor voltage using measured voltages in air and water
V_corr = function(V, V_air_meas, V_h2o_meas, type)
{
  eps_air =1.00059
  eps_h2o =81
  
  V_air_nominal = eps2V(eps_air, type) #theoretical value for voltage in air [V]
  V_h2o_nominal = eps2V(eps_h2o, type) #theoretical value for voltage in water  [V]
  
  V_corr = (V_h2o_nominal - V_air_nominal) / (V_h2o_meas - V_air_meas) * (V - V_air_meas) + V_air_nominal
  return(V_corr)
}