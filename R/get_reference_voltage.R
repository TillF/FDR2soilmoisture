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
      warning(paste0("NA-coefficients for ", arg, " '", get(arg), "' and ring-no '", ring_no, "'. Using medians of same type and ring."))
    
    
    #if coefficients are missing for SOME rings, use the median of all rings of this probe
    cur_row = calib_data[, arg] == get(arg)
    if (is.na(V_air_meas))
      V_air_meas = median(calib_data$voltage_air_mV  [cur_row], na.rm=TRUE) / 1000
    if (is.na(V_h2o_meas))
      V_h2o_meas = median(calib_data$voltage_water_mV[cur_row], na.rm=TRUE) / 1000
    
    #if coefficients are missing for ALL rings, use the median of all probes of this type
    cur_row = calib_data$ring_no == ring_no & calib_data$type == type
    if (is.na(V_air_meas))
      V_air_meas = median(calib_data$voltage_air_mV  [cur_row], na.rm=TRUE) / 1000
    if (is.na(V_h2o_meas))
      V_h2o_meas = median(calib_data$voltage_water_mV[cur_row], na.rm=TRUE) / 1000
    
    return(list(V_air_meas=V_air_meas, V_h2o_meas=V_h2o_meas, type=type))
}
