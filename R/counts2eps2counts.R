# functions for converting sensor permittivity [-] to counts [-] of SMT100 and back

counts2eps <- function(counts, alpha=-0.1305, beta=0.2549, gamma=1.8342){
  eps=gamma + 1/(alpha+(beta/((18000-counts)/5000)))
  if(alpha==-0.1305 & beta==0.2549 & gamma==1.8342){
    warning("Default calibration parameters were used to convert counts to epsilon. Since each SMT100 is calibrated individually, you should consider to set individually parameters.")
  }
  return(eps)
}
#Bogena, H.R.; Huisman, J.A.; Schilling, B.; Weuthen, A.; Vereecken, H. Effective Calibration of Low-Cost Soil Water Content Sensors. Sensors 2017, 17, 208. https://doi.org/10.3390/s17010208


eps2counts <- function(perm, alpha=-0.1305, beta=0.2549, gamma=1.8342){
  counts=18000-(beta*5000/((1/(perm - gamma))-alpha))
  if(alpha==-0.1305 & beta==0.2549 & gamma==1.8342){
    warning("Default calibration parameters were used to convert epsilon to counts. Since each SMT100 is calibrated individually, you should consider to set individually parameters.")
  }
  return(counts)
}