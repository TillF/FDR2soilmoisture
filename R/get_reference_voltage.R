# new function, implementing also the other variables
## adding therefore an additional input variable "var_type":

get_reference_values<-function(serial_no=NULL, probe_id=NULL,var_type = NULL, ring_no=1, calib_data, warnOnly=FALSE)
  #retrieve the values of calibration voltage from the supplied data frame
{
  # prevent colname error from older scripts
  if(("voltage_air_mV" %in% colnames(calib_data))){
    colnames(calib_data)[colnames(calib_data)=="voltage_air_mV"] = "air_measurement"
  }
  if(("voltage_water_mV" %in% colnames(calib_data))){
    colnames(calib_data)[colnames(calib_data)=="voltage_water_mV"] = "water_measurement"
  }
  
  # check if everything is given correctly
  if (!is.null(serial_no) & !is.null(probe_id)) stop("Either serial_no OR probe_id must be specified")
  if (!is.null(serial_no) & !is.null(probe_id)) stop("Either serial_no OR probe_id must be specified")
  if (is.null(calib_data$air_measurement)   | any(!is.numeric(calib_data$air_measurement  ))) stop("'calib_data' must have a numeric column 'air_measurement'")
  if (is.null(calib_data$water_measurement) | any(!is.numeric(calib_data$water_measurement))) stop("'calib_data' must have a numeric column 'water_measurement'")
  
  if (is.null(calib_data$var_type) | any(!is.character(calib_data$var_type))) stop("'calib_data' must have a character column 'var_type'")
  
  if(is.null(calib_data$temp)){
    calib_data$temp = rep(NA,nrow(calib_data))
  }
  
  #last check
  if(!all(c("probe_id", "ring_no", "air_measurement", "water_measurement", "remarks", "type", "serial_no", "date", "var_type", "temp") %in% colnames(calib_data))){
    stop("'calib_data' is not correctly given, please see help")
  }
  
  # check if calibration for specific probe or with serial  number
  if (!is.null(serial_no))
    arg = "serial_no"    else
      arg = "probe_id"
  
  # select rows for probe in calib data    
  cur_row = which(calib_data[, arg] == get(arg) & calib_data$ring_no == ring_no & calib_data$var_type == var_type ) 
  
  
  if (length(cur_row)== 0) #no entry found?
    stop(paste0("Probe with ", arg, "='", get(arg), "', ring-no='", ring_no, "' and var_type='", var_type, "' not found in calibration data. Please add a dummy record with sensor type and NAs, if you want to use  medians of same type and ring number.")) 
  
  if (length(cur_row)> 1) 
  {  
    warning(paste0("Multiple records with ", arg, "='", get(arg), "', ring-no='", ring_no, "' and var_type='", var_type, "' found in calibration data (lines ", paste(cur_row, collapse=", "), "), using last record."))
    cur_row = max(cur_row)
  }  
  
  var_air_meas = calib_data$air_measurement[cur_row]
  var_h2o_meas = calib_data$water_measurement[cur_row]
  temp_meas = calib_data$temp[cur_row]
  
  type = calib_data$type[cur_row]
  
  use_medians=FALSE
  if (is.na(var_air_meas + var_h2o_meas))
    if (warnOnly)
    {  
      use_medians=TRUE 
      fill_var_h2o="found"
      fill_var_air="found"
    }else
      stop(paste0("NA-coefficients for ", arg, "='", get(arg), "' and ring-no='", ring_no, "'. Use 'warnOnly=TRUE' to use medians instead.")) 
  
  
  #if coefficients are missing for SOME rings, use the median of all rings of *this* probe
  cur_row = calib_data[, arg] == get(arg) & calib_data$var_type == var_type
  if (is.na(var_air_meas))
  {  
    var_air_meas = median(calib_data$air_measurement[cur_row], na.rm=TRUE)
    fill_var_air = "median(this probe)"
  }  
  if (is.na(var_h2o_meas))
  {  
    var_h2o_meas = median(calib_data$water_measurement[cur_row], na.rm=TRUE)
    fill_var_h2o = "median(this probe, all rings)"
  }
  #if (is.na(temp_meas))
  #{
  #  temp_meas = median(calib_data$temp[cur_row], na.rm=TRUE)
  #}
  
  #if coefficients are missing for ALL rings, use the median of all probes of this type
  cur_row = calib_data$ring_no == ring_no & calib_data$type == type & calib_data$var_type == var_type
  if (is.na(var_air_meas))
  {
    var_air_meas = median(calib_data$air_measurement  [cur_row], na.rm=TRUE)
    fill_var_air = paste0("median(type='", type,"', ring_no='", ring_no)
  }
  if (is.na(var_h2o_meas))
  {
    var_h2o_meas = median(calib_data$water_measurement[cur_row], na.rm=TRUE)
    fill_var_h2o = paste0("median(type='", type,"', ring_no='", ring_no)
  }
  #if (is.na(temp_meas))
  #{
  #  temp_meas = median(calib_data$temp[cur_row], na.rm=TRUE)
  #}
  
    # Since calibdata for voltage are in mV
  if(var_type=="Voltage"){
    var_air_meas = var_air_meas / 1000
    var_h2o_meas = var_h2o_meas / 1000
    }
  
  if (use_medians)
    warning(paste0("Found NA-coefficient(s) for ", arg, "='", get(arg), "' and ring-no='", ring_no, 
                   "', using mean values for this combination:\nvar_air_", fill_var_air, ")=", var_air_meas, 
                   "\nvar_h2o_", fill_var_h2o, ")=", var_h2o_meas))
  
  return(list(var_air_meas=var_air_meas, var_h2o_meas=var_h2o_meas, type=type, temp_meas=temp_meas))
}


# should not be called (just in case)
get_reference_voltage = function(serial_no=NULL, probe_id=NULL, ring_no=1, calib_data, warnOnly=FALSE)

{ warning("You are using an outdated function. Please use get_reference_values().")
  
  #check for var_type in calibdata
  if(!("var_type" %in% colnames(calib_data))){
    calib_data$var_type = rep("Voltage",nrow(calib_data))
  }
  
  #check for Temperature column in calibdata
  if(!("temp" %in% colnames(calib_data))){
    calib_data$temp = rep(NA,nrow(calib_data))
  }
  
  #rename columns if not the same
  #if(sum(colnames(calib_data) != c("probe_id", "ring_no", "air_measurement", "water_measurement", "remarks", "type", "serial_no", "date", "var_type", "temp")) != 0){
  #  colnames(calib_data) = c("probe_id", "ring_no", "air_measurement", "water_measurement", "remarks", "type", "serial_no", "date", "var_type", "temp")
  #}
  
  #call new function
  output=get_reference_values(serial_no=serial_no, probe_id=probe_id,var_type = "Voltage", ring_no = ring_no, calib_data=calib_data, warnOnly=warnOnly)
    
  return(list(output))
}



