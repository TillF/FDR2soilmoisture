# functions for converting sensor permittivity [-] volumetric water content [%]

eps2theta = function(epsdata, equation)
{
  required_fields = list(
    deltaT_minorg = c("epsilon", "soil"),
    FerschEtal2017 = c("epsilon", "n"),
    ToppEtal1980 = c("epsilon"),
    RothEtal1992 = c("epsilon", "soil"),
    SchaapEtal1997 = c("epsilon"),
    MalickiEtal1996 = c("epsilon", "BD"),
    Jacobsen_Schjonning1993= c("epsilon", "BD", "clay_perc", "om_perc"),    
    DrnevichEtal2005= c("epsilon", "BD", "cohesive"),
    JuEtal2010 = c("epsilon", "BD"),
    ZhaoEtal2016 = c("epsilon", "BD")
  )
  
  if (equation == "list") return (required_fields) #only return names of supported equations
  
  if (!is.data.frame(epsdata)) stop("epsdata must be a dataframe.")
  if (!is.character(equation)) stop("equation must be a character string.")
  
  
  if (!(all(equation %in% names(required_fields)))) stop("equation must be one of ", paste0(names(required_fields), collapse = ", "))
  for (eq in equation)
  {
    if (!(all(required_fields[[eq]] %in% names(epsdata)))) stop(paste0("Equation '", eq, "' needs the column(s) '", paste0(required_fields[[eq]], collapse = "', '"), "' in epsdata."))    
  }  
  
  
  theta=NA
  if (equation=="deltaT_minorg")
  {
    if (any (!(unique(epsdata$soil, na.rm=TRUE) %in% c("mineral", "organic", "clay"))))
      warning("Field 'soil' must be 'mineral' or 'organic'")
    theta = rep(NA, nrow(epsdata))
    min_ix = (epsdata$soil == "mineral") |(epsdata$soil == "clay")
    a0 = 1.6  #ThetaProbe manual, page 14; Profile Probe User Manual 5.0, page 21
    a1 = 8.4   
    theta[min_ix] = (sqrt(epsdata$epsilon[min_ix]) - a0) / a1 #ThetaProbe manual, eq. 6
    a0 = 1.3  #ThetaProbe manual, page 14
    a1 = 7.7   
    theta[!min_ix] = (sqrt(epsdata$epsilon[!min_ix]) - a0) / a1 #ThetaProbe manual, eq. 6
    
  }
  #Kargas & Kerkides 2008seem to give a similar eq.
  
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
  
  # Roth et al 1992 (eq. 6 in Kargas & Kerkides, 2006)
  if (equation=="RothEtal1992")
  { 
    if (any (!(unique(epsdata$soil, na.rm=TRUE) %in% c("mineral", "organic"))))
      warning("Field 'soil' must be 'mineral' or 'organic'")
    theta = rep(NA, nrow(epsdata))
    min_ix = epsdata$soil == "mineral" 
    #mineral
    theta[ min_ix] = -0.0728 + 0.0448*epsdata$epsilon[ min_ix] -0.00195 *epsdata$epsilon[ min_ix]^2 + 0.0000361*epsdata$epsilon[min_ix]^3
    #organic
    theta[!min_ix] =  0.0233 + 0.0285*epsdata$epsilon[!min_ix] -0.000431*epsdata$epsilon[!min_ix]^2 + 0.00000304*epsdata$epsilon[!min_ix]^3
  }
  #Yoshikawa et al. (2004), Pepin et al. (1992) give further polyniomial equations
  
  # Schaap et al 1997 (eq. 2b in Vaz et al 2013)
  if (equation=="SchaapEtal1997")
  { 
    theta = (0.133*sqrt(epsdata$epsilon) -0.146)^0.885
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
  
  #Drnevich et al (2005) from Zhao et al., 2016
  if (equation=="DrnevichEtal2005")
  {
    a = ifelse(epsdata$cohesive, 0.95, 1)    
    b = ifelse(epsdata$cohesive, 8.8, 8.5)    
    theta = (sqrt(epsdata$epsilon) / epsdata$BD - a) / b
  }
  
  #Zhao et al., 2016
  if (equation=="ZhaoEtal2016")
  {
    theta = (0.3039*epsdata$BD - 2.1851 + sqrt(epsdata$epsilon)) /
            ((18.0283*epsdata$BD-17.9531) +(-0.6806*epsdata$BD+1.8351)*sqrt(epsdata$epsilon))
  }
  
  #Ju et al, 2010
  if (equation=="JuEtal2010")
  {
    theta = 0.1228*sqrt(epsdata$epsilon) - 0.1322*epsdata$BD -0.0152
  }
  
  
  
  
  
  #Schwartz et al 2008 (10.2136/sssaj2007.0208) provide multilinear regression using partcle size fractions and Ca-content etc.


  
  return(theta)
}