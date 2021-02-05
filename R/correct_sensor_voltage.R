correct_sensor_voltage = function(V, serial_no=NULL, probe_id=NULL, ring_no=1, calib_data, warnOnly=FALSE, adjust_range=TRUE)
# converts sensor voltage [Volts] according to calibration data in calib_data for the specified sensor and ring   
{
  #tt = get_reference_voltage(serial_no = serial_no, probe_id = probe_id, ring_no = ring_no, calib_data = calib_data)
  
  if ( (!is.null(serial_no) & !is.null(probe_id)) |
       ( is.null(serial_no) &  is.null(probe_id)) ) 
    stop ("Either serial_no OR probe_id must be specified.")
  if (!is.null(serial_no) & !is.null(probe_id)) stop ("Either serial_no OR probe_id must be specified.")
  
  if (!is.null(serial_no)) #which option is used for identification? serial_no or probe_id? 
    id_argument = "serial_no" else
    id_argument = "probe_id" 
    
  if (any(is.na(get(id_argument))))      
    stop(paste(id_argument, " contains NAs, please fix this. Use dummy serial_no, if needed (see help)."))
    
    unique_settings = data.frame(get(id_argument), ring_no=ring_no)   
    names(unique_settings)[1] = id_argument
  
    unique_settings=unique(unique_settings) #reduce to unique combinations of identifier and ring_no

  #ref_voltages = sapply(FUN=get_reference_voltage, X = serial_no, probe_id = probe_id, ring_no = ring_no, calib_data = calib_data)
  
  V_corrected = NA*V
  unique_settings[, c("V_air_meas", "V_h2o_meas")] = NA #create empty cols
  for (ss in 1:nrow(unique_settings))
  {
    args = list(dummy = unique_settings[ss, id_argument], ring_no = unique_settings$ring_no[ss], calib_data = calib_data, warnOnly=warnOnly)
    names(args)[1] = id_argument
    tt = do.call(get_reference_voltage, args = args) #get the reference voltage
    cur_rows = get(id_argument) == unique_settings[ss, id_argument] & ring_no == unique_settings$ring_no[ss] 
    
    unique_settings[ss, c("V_air_meas", "V_h2o_meas")] = c(tt$V_air_meas, tt$V_h2o_meas)
    unique_settings[ss, "type"] = tt$type
  
    #check consistency between maximum recorded voltages and calibration voltage for water
    Vmax_measured = quantile(V[cur_rows], probs = 0.99, na.rm=TRUE) #get maximum voltage measured, discarding outliers
    Vmin_measured = quantile(V[cur_rows], probs = 0.01, na.rm=TRUE) #get minimum voltage measured, discarding outliers
    
    if (adjust_range) #update calibration values with measurements
    {  
      if (Vmax_measured > unique_settings[ss, "V_h2o_meas"])
      {
        warning(paste0("Max V in time series (", Vmax_measured, " V) larger than reference V for water (", unique_settings[ss, "V_h2o_meas"]," V). Consider updating calibration data for ",
                       ifelse(is.null(unique_settings$probe_id), paste0("ser_no '",unique_settings$serial_no[ss]), paste0("probe-id '", unique_settings$probe_id[ss])),"', ring '", unique_settings$ring_no[ss], "'. Using new max."))
        unique_settings[ss, "V_h2o_meas"] = Vmax_measured
      }
    } else #discard measurements outside calibration range
    {
       V[(V > unique_settings[ss, "V_h2o_meas"]) | (V < unique_settings[ss, "V_air_meas"]) ] = NA
    }  
    V_corrected[cur_rows] = V_corr(V = V[cur_rows], V_air_meas = unique_settings[ss, "V_air_meas"], V_h2o_meas = unique_settings[ss, "V_h2o_meas"], type=unique_settings[ss, "type"])
    
  }
  return(V_corrected)
}

# # test function
# for (i in 1:nrow(calib_data))
#   print(correct_sensor_voltage(V = calib_data$voltage_air_mV[i]/1000, serial_no = calib_data$serial_no[i], ring_no = calib_data$ring_no[i], calib_data = calib_data))

