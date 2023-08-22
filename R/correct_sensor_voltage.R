# new function, implementing next to Voltage also other variables

correct_sensor_values = function(values, serial_no=NULL, probe_id=NULL,var_type=NULL, ring_no=1, calib_data, warnOnly=FALSE, adjust_range=TRUE, ...)
  # converts sensor values of var_type (Voltage [V], Permittivity [-], Counts [-] ) according to calibration data in calib_data for the specified sensor and ring   
{
  #tt = get_reference_voltage(serial_no = serial_no, probe_id = probe_id, ring_no = ring_no, calib_data = calib_data)
  
  #check if all necessary inputs are given
  if ( (!is.null(serial_no) & !is.null(probe_id)) |
       ( is.null(serial_no) &  is.null(probe_id)) ) 
    stop ("Either serial_no OR probe_id must be specified.")
  if (!is.null(serial_no) & !is.null(probe_id)) stop ("Either serial_no OR probe_id must be specified.")
  
  if (is.null(var_type) &  !is.null(serial_no)) stop ("var_type must be specified for serial_no.")
  
  if (!is.null(serial_no)) #which option is used for identification? serial_no or probe_id? 
    id_argument = "serial_no" else
      id_argument = "probe_id" 
    
    if (any(is.na(get(id_argument)))){      
      stop(paste(id_argument, " contains NAs, please fix this. Use dummy serial_no, if needed (see help)."))
    }
    unique_settings = data.frame(get(id_argument), ring_no=ring_no,var_type=var_type)   #adding var_type as add. variable
    names(unique_settings)[1] = id_argument
    
    unique_settings=unique(unique_settings) #reduce to unique combinations of identifier and ring_no
    
    #ref_voltages = sapply(FUN=get_reference_voltage, X = serial_no, probe_id = probe_id, ring_no = ring_no, calib_data = calib_data)
    
    #create dummy
    V_corrected = NA*values
    
    unique_settings[, c("var_air_meas", "var_h2o_meas")] = NA #create empty cols
    
    #for each unique 
    for (ss in 1:nrow(unique_settings))
    {
      #set unit for warnings
      unit=ifelse(unique_settings$var_type[ss]=="Voltage","V","")
      
      #add cols
      args = list(dummy = unique_settings[ss, id_argument], ring_no = unique_settings$ring_no[ss], var_type = unique_settings$var_type[ss], calib_data = calib_data, warnOnly=warnOnly)
      names(args)[1] = id_argument
      
      #get the reference values
      tt = do.call(get_reference_values, args = args)
      cur_rows = get(id_argument) == unique_settings[ss, id_argument] & ring_no == unique_settings$ring_no[ss] & var_type==unique_settings$var_type[ss]
      if (all(is.na(values[cur_rows]))) #only NA-values, nothing to do here
      {
        V_corrected[cur_rows] = NA
        next
      }    
      
      unique_settings[ss, c("var_air_meas", "var_h2o_meas")] = c(tt$var_air_meas, tt$var_h2o_meas)
      unique_settings[ss, "type"] = tt$type
      
      #check consistency between maximum recorded voltages and calibration voltage for water
      V_range_measured = quantile(values[cur_rows], probs = c(0.01, 0.99), na.rm=TRUE) #get range of voltage measured, discarding outliers
      Vmax_measured = V_range_measured[2] #get maximum voltage measured, discarding outliers
      Vmin_measured = V_range_measured[1] #get minimum voltage measured, discarding outliers
      
      #prepare warning message
      probe_id_str = paste0(ifelse(is.null(unique_settings$probe_id), paste0("serial_no '",unique_settings$serial_no[ss]), paste0("probe-id '", unique_settings$probe_id[ss])),"', ring '", unique_settings$ring_no[ss])
      
      sign = ifelse(unique_settings[ss, "var_type"] == "Counts", -1, 1) #for SMT100, "voltage" (actually "counts") are negatively correalted to epsilon
      
      if (sign == 1) #for PR2 and ThetaProbe: raw value (voltages) increases with permittivity
      {
        beyond_air   = Vmin_measured < unique_settings[ss, "var_air_meas"]
        beyond_water = Vmax_measured > unique_settings[ss, "var_h2o_meas"]
        
        if (beyond_air)
        {  
          warn_str = paste0("Min ",unique_settings$var_type[ss]," in time series (", Vmin_measured, unit, ") smaller than reference ",
                            unique_settings$var_type[ss] ," for air (", unique_settings[ss, "var_air_meas"], unit,
                            "). Consider updating calibration data for ", probe_id_str, "'.\n")
          if (adjust_range)
          { 
            warn_str = paste0(probe_id_str, ": Min ",unique_settings$var_type[ss]," in time series (", Vmin_measured, unit, ") smaller than reference ",
                              unique_settings$var_type[ss] ," for air (", unique_settings[ss, "var_air_meas"], unit, "). New min is used instead.\n")
            unique_settings[ss, "var_air_meas"] = Vmin_measured #update calibration values with measurements
          }
          warning(warn_str)
        }
        if (beyond_water)
        {  
          warn_str = paste0("Max ",unique_settings$var_type[ss]," in time series (", Vmax_measured, unit, ") larger than reference ",
                            unique_settings$var_type[ss] ," for water (", unique_settings[ss, "var_h2o_meas"], unit,
                            "). Consider updating calibration data for ", probe_id_str, "'.\n")
          if (adjust_range)
          {
            warn_str = paste0(probe_id_str, ": Max ",unique_settings$var_type[ss]," in time series (", Vmax_measured, unit, ") larger than reference ",
                              unique_settings$var_type[ss] ," for water (", unique_settings[ss, "var_h2o_meas"],
                              unit, "). New max is used instead.\n")
            unique_settings[ss, "var_h2o_meas"] = Vmax_measured #update calibration values with measuremen ",unique_settings$var_type[ss],"
          }
          warning(warn_str)
        }  
      } else
      {
        #(sign ==--1: for Counts: raw value (counts) decrease with permittivity unit,")
        beyond_air   = Vmax_measured >  unique_settings[ss, "var_air_meas"]
        beyond_water = Vmin_measured <  unique_settings[ss, "var_h2o_meas"]
        
        if (beyond_air)
        {  
          warn_str = paste0("Max ",unique_settings$var_type[ss]," in time series (", Vmax_measured, unit, ") larger than reference ",
                            unique_settings$var_type[ss] ," for air (", unique_settings[ss, "var_air_meas"],
                            unit, "). Consider updating calibration data for ",
                            probe_id_str, "'.\n")
          if (adjust_range)
          {  
            warn_str = paste0(probe_id_str, ": Max ",unique_settings$var_type[ss]," in time series (", Vmax_measured, unit, ") larger than reference ",
                              unique_settings$var_type[ss] ," for air (", unique_settings[ss, "var_air_meas"],unit, 
                              "). New max is used instead.\n")
            unique_settings[ss, "var_air_meas"] = Vmax_measured #update calibration values with measurements
          }
          warning(warn_str)
        }
        if (beyond_water)
        {  
          warn_str = paste0("Min ",unique_settings$var_type[ss], " in time series (", Vmin_measured, unit ,") smaller than reference ",
                            unique_settings$var_type[ss], " for water (", unique_settings[ss, "var_h2o_meas"],unit ,
                            "). Consider updating calibration data for ",
                            probe_id_str, "'.\n")
          if (adjust_range)
          {  
            warn_str = paste0(probe_id_str, ": Min ",unique_settings$var_type[ss], " in time series (", Vmin_measured, unit ,") smaller than reference ",
                              unique_settings$var_type[ss], " for water (", unique_settings[ss, "var_h2o_meas"],unit ,
                              "). New min is used instead.\n")
            unique_settings[ss, "var_h2o_meas"] = Vmin_measured #update calibration values with measurements
          }
          warning(warn_str)
        }  
      } 
      
      #discard measurements outside calibration range but not overwrite because of different possible var_types
      if (!adjust_range){
        V_adj=values
        V_adj[(values < min(unique_settings[ss, c("var_air_meas", "var_h2o_meas")])) |
                (values > max(unique_settings[ss, c("var_air_meas", "var_h2o_meas")])) ] = NA
        
        #use adjusted for corrections
        V_corrected[cur_rows] = var_corr(x = V_adj[cur_rows], 
                                         var_air_meas = unique_settings[ss, "var_air_meas"],
                                         var_h2o_meas = unique_settings[ss, "var_h2o_meas"],
                                         type=unique_settings[ss, "type"],
                                         var_type=unique_settings[ss, "var_type"], ...)
      } else{
        #use normal values
        V_corrected[cur_rows] = var_corr(x = values[cur_rows], 
                                         var_air_meas = unique_settings[ss, "var_air_meas"],
                                         var_h2o_meas = unique_settings[ss, "var_h2o_meas"],
                                         type=unique_settings[ss, "type"],
                                         var_type=unique_settings[ss, "var_type"], ...)
      }
      
    }
  return(V_corrected)
}


# Ensure that older function is still working
correct_sensor_voltage = function(V, serial_no=NULL, probe_id=NULL, ring_no=1, calib_data, warnOnly=FALSE, adjust_range=TRUE)
  # converts sensor voltage [Volts] according to calibration data in calib_data for the specified sensor and ring   
{
  warning("You are using an outdated function. Please use get_reference_values().")
  
  if("vartype" %in% colnames(calib_data)){
    calib_data$vartype = rep("Voltage",nrow(calib_data))
  }
  
  if(colnames(calib_data) != c("probe_id", "ring_no", "air_measurement", "water_measurement", "remarks", "type", "serial_no", "date","var_type")){
    colnames(calib_data) = c("probe_id", "ring_no", "air_measurement", "water_measurement", "remarks", "type", "serial_no", "date","var_type")
  }
  
  
  V_corrected <- correct_sensor_values(values=V, serial_no=serial_no, probe_id=probe_id,var_type="Voltage", ring_no=1, calib_data=calib_data, warnOnly=warnOnly, adjust_range=adjust_range)
  warning("Voltage is set as default var_type")
  return(V_corrected)
}

# # test function
# for (i in 1:nrow(calib_data))
#   print(correct_sensor_voltage(V = calib_data$water_measurement[i]/1000, serial_no = calib_data$serial_no[i], ring_no = calib_data$ring_no[i], calib_data = calib_data))
