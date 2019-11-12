eps_air =1.00059
eps_h2o =81

#linear correction of sensor voltage using measured voltages in air and water
V_corr = function(V, V_air_meas, V_h2o_meas, type)
{
  V_air_nominal = eps2V(eps_air, type) #theoretical value for voltage in air [V]
  V_h2o_nominal = eps2V(eps_h2o, type) #theoretical value for voltage in water  [V]
  
  V_corr = (V_h2o_nominal - V_air_nominal) / (V_h2o_meas - V_air_meas) * (V - V_air_meas) + V_air_nominal
  return(V_corr)
}

# for (i in 1:nrow(calib_data))
#   print(V_corr(V = calib_data$voltage_water_mV[i] / 1000, V_air_meas = calib_data$voltage_air_mV[i]/ 1000, V_h2o_meas = calib_data$voltage_water_mV[i]/ 1000))

get_reference_voltage = function(serial_no=NULL, probe_id=NULL, ring_no, calib_data)
{
  if (!is.null(serial_no) & !is.null(probe_id)) stop("Either serial_no OR probe_id must be specified")
  if (!is.null(serial_no) & !is.null(probe_id)) stop("Either serial_no OR probe_id must be specified")
  if (is.null(calib_data$voltage_air_mV)   | any(!is.numeric(calib_data$voltage_air_mV  ))) stop("'calib_data' must have a numeric column 'voltage_air_mV'")
  if (is.null(calib_data$voltage_water_mV) | any(!is.numeric(calib_data$voltage_water_mV))) stop("'calib_data' must have a numeric column 'voltage_air_mV'")
  
  if (!is.null(serial_no))
    arg = "serial_no"    else
    arg = "probe_id"
  
  cur_row = which(calib_data[, arg] == get(arg) & calib_data$ring_no == ring_no ) 
  
  
  if (length(cur_row)< 1) stop(paste0("Probe with ", arg, " ", get(arg), " and ring-no ", ring_no, " not found in calibration data."))
  if (length(cur_row)> 1) 
  {  
    warning(paste0("Multiple probes with ", arg, " ", get(arg), " and ring-no ", ring_no, " found in calibration data (lines ", paste(cur_row, collapse=","), ") using last entry."))
    cur_row = max(cur_row)
  }  
  
  V_air_meas = calib_data$voltage_air_mV  [cur_row] / 1000
  V_h2o_meas = calib_data$voltage_water_mV[cur_row] / 1000
  type = calib_data$type[cur_row]
    
  if (is.na(V_air_meas + V_h2o_meas))
    warning(paste0("NA-coefficients for ", arg, " ", get(arg), " and ring-no ", ring_no, ". Using medians of same type and ring."))
  

  #if coefficients are missing for SOME rings, use the median of all rings of this probe
  cur_row = calib_data[, arg] == get(arg)
  if (is.na(V_air_meas))
    V_air_meas = median(calib_data$voltage_air_mV  [cur_row], na.rm=TRUE) / 1000
  if (is.na(V_h2o_meas))
    V_h2o_meas = median(calib_data$voltage_water_mV[cur_row], na.rm=TRUE) / 1000
  
  #if coefficients are missing for ALL rings, use the median of all probes of this type
  cur_row = calib_data$ring_no == ring_no & calib_data$type[cur_row] == type
  if (is.na(V_air_meas))
    V_air_meas = median(calib_data$voltage_air_mV  [cur_row], na.rm=TRUE) / 1000
  if (is.na(V_h2o_meas))
    V_h2o_meas = median(calib_data$voltage_water_mV[cur_row], na.rm=TRUE) / 1000
  
  return(list(V_air_meas=V_air_meas, V_h2o_meas=V_h2o_meas, type=type))
}

correct_sensor_voltage = function(V, serial_no=NULL, probe_id=NULL, ring_no=1, calib_data)
# converts sensor voltage [Volts] according to calibration data in calib_data for the specified sensor and ring   
{
  #tt = get_reference_voltage(serial_no = serial_no, probe_id = probe_id, ring_no = ring_no, calib_data = calib_data)
  
  if (!is.null(serial_no) & !is.null(probe_id)) stop ("Either serial_no OR probe_id must be specified.")
  if (!is.null(serial_no))
      unique_settings = data.frame(serial_no=serial_no) else
      unique_settings = data.frame(probe_id=probe_id) 
  
  unique_settings$ring_no=ring_no
  unique_settings=unique(unique_settings) #reduce to unique combinations of identifier and ring_no

  #ref_voltages = sapply(FUN=get_reference_voltage, X = serial_no, probe_id = probe_id, ring_no = ring_no, calib_data = calib_data)
  
  V_corrected = NA*V
  unique_settings[, c("V_air_meas", "V_h2o_meas")] = NA #create empty cols
  for (ss in 1:nrow(unique_settings))
  {
    if (!is.null(unique_settings$serial_no))
    {
      tt = get_reference_voltage(serial_no = unique_settings$serial_no[ss], ring_no = unique_settings$ring_no[ss], calib_data = calib_data) 
      cur_rows = serial_no == unique_settings$serial_no[ss] & ring_no == unique_settings$ring_no[ss] 
    }    else
    {  
      tt= get_reference_voltage(probe_id  = unique_settings$probe_id [ss], ring_no = unique_settings$ring_no[ss], calib_data = calib_data) 
      cur_rows = probe_id == unique_settings$probe_id[ss] & ring_no == unique_settings$ring_no[ss] 
    }
    unique_settings[ss, c("V_air_meas", "V_h2o_meas")] = c(tt$V_air_meas, tt$V_h2o_meas)
    unique_settings[ss, "type"] = tt$type
  
    #check consistency between maximum recorded voltages and calibration voltage for water
    Vmax_measured = quantile(V[cur_rows], probs = 0.99, na.rm=T) #get maximum voltage measured, discarding outliers
    if (Vmax_measured > unique_settings[ss, "V_h2o_meas"])
    {
      warning(paste0("Max V in time series (", Vmax_measured, ") larger than reference V for water (", unique_settings[ss, "V_h2o_meas"],"). Consider updating calibration data for ",
                     ifelse(is.null(unique_settings$probe_id), paste("ser_no",unique_settings$serial_no[ss]), paste("probe-id", unique_settings$probe_id[ss])),", ring ", unique_settings$ring_no[ss], ". Using new max."))
      unique_settings[ss, "V_h2o_meas"] = Vmax_measured
    }
    V_corrected[cur_rows] = V_corr(V = V[cur_rows], V_air_meas = unique_settings[ss, "V_air_meas"], V_h2o_meas = unique_settings[ss, "V_h2o_meas"], type=unique_settings[ss, "type"])
    
  }
  return(V_corrected)
}

# # test function
# for (i in 1:nrow(calib_data))
#   print(correct_sensor_voltage(V = calib_data$voltage_air_mV[i]/1000, serial_no = calib_data$serial_no[i], ring_no = calib_data$ring_no[i], calib_data = calib_data))

