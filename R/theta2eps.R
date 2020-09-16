# functions for converting volumetric water content [%] to sensor permittivity [-] 

theta2eps = function(thetadata, equation)
{
  required_fields = list(
    deltaT_minorg = c("theta", "soil") #,
    # FerschEtal2017 = c("theta", "n"),
    # ToppEtal1980 = c("theta"),
    # RothEtal1992 = c("theta", "soil"),
    # SchaapEtal1997 = c("theta"),
    # MalickiEtal1996 = c("theta", "BD"),
    # Jacobsen_Schjonning1993= c("theta", "BD", "clay_perc", "om_perc"),    
    # DrnevichEtal2005= c("theta", "BD", "cohesive"),
    # JuEtal2010 = c("theta", "BD"),
    # ZhaoEtal2016 = c("theta", "BD")
  )
  
  if (equation == "list") return (required_fields) #only return names of supported equations
  
  if (!is.data.frame(thetadata)) stop("thetadata must be a dataframe.")
  if (!is.character(equation)) stop("equation must be a character string.")
  
  
  if (!(all(equation %in% names(required_fields)))) stop("equation must be one of: '", paste0(names(required_fields), collapse = "', '"), "'")
  for (eq in equation)
  {
    if (!(all(required_fields[[eq]] %in% names(thetadata)))) stop(paste0("Equation '", eq, "' needs the column(s) '", paste0(required_fields[[eq]], collapse = "', '"), "' in thetadata."))    
  }  
  
	if (any(thetadata$theta > 1) |  any(thetadata$theta < 0)) stop("theta must be within [0..1] (volumetric water content).")
  
 
  eps=NA
  if (equation=="deltaT_minorg")
  {
    if (any (!(unique(thetadata$soil, na.rm=TRUE) %in% c("mineral", "organic", "clay"))))
      warning("Field 'soil' must be 'mineral' or 'organic'")
    eps = rep(NA, nrow(thetadata))
    min_ix = (thetadata$soil == "mineral") |(thetadata$soil == "clay")
    a0 = 1.6  #ThetaProbe manual, page 14; Profile Probe User Manual 5.0, page 21
    a1 = 8.4   
    
    eps[min_ix] = (thetadata$theta[min_ix] * a1 + a0)^2  #ThetaProbe manual, eq. 6
    
    a0 = 1.3  #ThetaProbe manual, page 14
    a1 = 7.7   
    eps[!min_ix] = (thetadata$theta[!min_ix] * a1 + a0)^2  #ThetaProbe manual, eq. 6
    
  }

  #   #Kargas & Kerkides 2008seem to give a similar eq.
  # 
  # if (equation=="FerschEtal2017")
  # {  
  #   theta = (sqrt(thetadata$epsilon) - (1 - thetadata$n* sqrt(3.29)) - thetadata$n * 1) /
  #     (sqrt(80) - sqrt(1))
  # }
  # 
  # # Topp et al 1980 (eq. 16.62 in Mohamed & Paleologos, 2018)
  # if (equation=="ToppEtal1980")
  # { 
  #   theta = -0.053+ 2.93e-2*thetadata$epsilon -5.5e-4*thetadata$epsilon^2 + 4.3e-6*thetadata$epsilon^3
  # }
  # 
  # # Roth et al 1992 (eq. 6 in Kargas & Kerkides, 2006)
  # if (equation=="RothEtal1992")
  # { 
  #   if (any (!(unique(thetadata$soil, na.rm=TRUE) %in% c("mineral", "organic"))))
  #     warning("Field 'soil' must be 'mineral' or 'organic'")
  #   theta = rep(NA, nrow(thetadata))
  #   min_ix = thetadata$soil == "mineral" 
  #   #mineral
  #   theta[ min_ix] = -0.0728 + 0.0448*thetadata$epsilon[ min_ix] -0.00195 *thetadata$epsilon[ min_ix]^2 + 0.0000361*thetadata$epsilon[min_ix]^3
  #   #organic
  #   theta[!min_ix] =  0.0233 + 0.0285*thetadata$epsilon[!min_ix] -0.000431*thetadata$epsilon[!min_ix]^2 + 0.00000304*thetadata$epsilon[!min_ix]^3
  # }
  # #Yoshikawa et al. (2004), Pepin et al. (1992) give further polyniomial equations
  # 
  # # Schaap et al 1997 (eq. 2b in Vaz et al 2013)
  # if (equation=="SchaapEtal1997")
  # { 
  #   theta = (0.133*sqrt(thetadata$epsilon) -0.146)^0.885
  # }
  # 
  # 
  # #Malicki et al 1996(eq. 16.63 in Mohamed & Paleologos, 2018)
  # if (equation=="MalickiEtal1996")
  # {
  #   theta = (sqrt(thetadata$epsilon)-0.819 - 0.168*thetadata$BD-0.168*thetadata$BD^2)/(7.17+1.18*thetadata$BD)
  # }
  # 
  # #Jacobsen & Schjonning (1993) from Mohamed & Paleologos, 2018
  # if (equation=="Jacobsen_Schjonning1993")
  # {
  #   theta = -3.41*1e-2 +3.45*1e-2*thetadata$epsilon-11.4*1e-4*thetadata$epsilon^2 + 17.1*1e-6*thetadata$epsilon^3-
  #     3.7*1e-2*thetadata$BD + 7.36*1e-4*thetadata$clay_perc + 47.7*1e-4 * thetadata$om_perc
  # }
  # 
  # #Drnevich et al (2005) from Zhao et al., 2016
  # if (equation=="DrnevichEtal2005")
  # {
  #   a = ifelse(thetadata$cohesive, 0.95, 1)    
  #   b = ifelse(thetadata$cohesive, 8.8, 8.5)    
  #   theta = (sqrt(thetadata$epsilon) / thetadata$BD - a) / b
  # }
  # 
  # #Zhao et al., 2016
  # if (equation=="ZhaoEtal2016")
  # {
  #   theta = (0.3039*thetadata$BD - 2.1851 + sqrt(thetadata$epsilon)) /
  #           ((18.0283*thetadata$BD-17.9531) +(-0.6806*thetadata$BD+1.8351)*sqrt(thetadata$epsilon))
  # }
  # 
  # #Ju et al, 2010
  # if (equation=="JuEtal2010")
  # {
  #   theta = 0.1228*sqrt(thetadata$epsilon) - 0.1322*thetadata$BD -0.0152
  # }
  # 
  # 
  # 
  # 
  # 
  # #Schwartz et al 2008 (10.2136/sssaj2007.0208) provide multilinear regression using partcle size fractions and Ca-content etc.


  
  return(eps)
}