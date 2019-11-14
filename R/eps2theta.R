# functions for converting sensor permittivity [-] volumetric water content [%]

eps2theta = function(epsdata, equation)
{
  required_fields = list(
    deltaT_minorg = c("epsilon", "soil"),
    FerschEtal2017 = c("epsilon", "n"),
    ToppEtal1980 = c("epsilon"),
    MalickiEtal1996 = c("epsilon", "BD"),
    Jacobsen_Schjonning1993= c("epsilon", "BD", "clay_perc", "om_perc")    
  )
  
  if (equation == "list") return (names(required_fields)) #only return names of supported equations
  
  if (!is.data.frame(epsdata)) stop("epsdata must be a dataframe.")
  if (!is.character(equation)) stop("equation must be a character string.")
  
  
  if (!(all(equation %in% names(required_fields)))) stop("equation must be one of ", paste0(names(required_fields), collapse = ", "))
  for (eq in equation)
  {
    if (!(all(required_fields[[eq]] %in% names(epsdata)))) stop(paste0("Equation '", eq, "' needs the columns ", paste0(required_fields[[eq]], collapse = ", "), " in epsdata."))    
  }  
  
  
  theta=NA
  if (equation=="deltaT_minorg")
  {
    if (any (!(unique(epsdata$soil, na.rm=TRUE) %in% c("mineral", "organic"))))
      stop("Field 'soil' must be 'mineral' or 'organic'")
    theta = rep(NA, nrow(epsdata))
    min_ix = epsdata$soil == "mineral" 
    a0 = 1.6  #ThetaProbe manual, page 14
    a1 = 8.4   
    theta[min_ix] = (sqrt(epsdata$epsilon[min_ix]) - a0) / a1 #ThetaProbe manual, eq. 6
    a0 = 1.3  #ThetaProbe manual, page 14
    a1 = 7.7   
    theta[!min_ix] = (sqrt(epsdata$epsilon[!min_ix]) - a0) / a1 #ThetaProbe manual, eq. 6
    
  }
  
  if (equation=="FerschEtal2017")
  {  
    theta = (sqrt(epsdata$epsilon) - (1 - epsdata$n* sqrt(3.29)) - epsdata$n * 1) /
      (sqrt(80) - sqrt(1))
  }
  
  # Topp et al 1980 (eq. 16.62 in Mohamed & Paleologos, 2018)
  if (equation=="ToppEtal1980")
  { 
    theta = -0.053+ 2.93e-2*epsdata$epsilon -5.5e-4*epsdata$epsilon^2 + 4.3e-6*epsdata$epsilon^3
  }
  
  #Malicki et al 1996(eq. 16.63 in Mohamed & Paleologos, 2018)
  if (equation=="MalickiEtal1996")
  {
    theta = (sqrt(epsdata$epsilon)-0.819 - 0.168*epsdata$BD-0.168*epsdata$BD^2)/(7.17+1.18*epsdata$BD)
  }
  
  #Jacobsen & Schjonning (1993) from Mohamed & Paleologos, 2018
  if (equation=="Jacobsen_Schjonning1993")
  {
    theta = -3.41*1e-2 +3.45*1e-2*epsdata$epsilon-11.4*1e-4*epsdata$epsilon^2 + 17.1*1e-6*epsdata$epsilon^3-
      3.7*1e-2*epsdata$BD + 7.36*1e-4*epsdata$clay_perc + 47.7*1e-4 * epsdata$om_perc
  }
  


  
  return(theta)
}