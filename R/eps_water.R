# Function to calculate the temperature corrected permittivity of water

eps_water <- function(T, equ = "Weast86") {
  if (equ == "Weast86") {
    # Weast, R.C. CRC Handbook of Physics and Chemistry, 67th ed.; CRC Press: Boca Raton, FL, USA, 1986.
    eps_w <- 78.54 * (1 - (4.5791 * 10**(-3)) * (T - 25) + 1.19 * 10**(-5) * (T - 25)**2 - 2.8 * 10**(-8) * (T - 25)**3)
  }

  if (equ == "iapws") {
    # need iapws package
    if (!requireNamespace("iapws", quietly = TRUE)) {
      stop(paste("Error: Package iapws is not installed. Please install it before running or choose another equation see help."))
    }

    rho_w <- iapws::iapws95("rho", t = 273.15 + T, p = 1013.25 / 10000)
    eps_w <- iapws::iapws_epsilon(rho = rho_w, t = 273.15 + T)
  }

  if (equ == "Uematsu1980") {
    # Journal of Physical and Chemical Reference Data 9, 1291 (1980); https://doi.org/10.1063/1.555632
    T0 <- 298.15
    T2 <- (T + 273.15) / T0

    # density of water
    if (requireNamespace("iapws", quietly = TRUE)) {
      rho <- iapws::iapws95("rho", t = 273.15 + T, p = 1013.25 / 10000)[1, ]
    } else {
      rho <- 998.2072 # standard for atmospheric pressure and 20 degree C without package
      warning("A constant density of water at 20 degree C of 998.2072 kg/m**2 is used, see help and install package iapws for temperature dependent usage.")
    }

    rho0 <- 1000
    rho2 <- rho / rho0

    pA <- 7.62571
    pB <- 244.003
    pC <- -140.569
    pD <- 27.7841
    pE <- -96.2805
    pF <- 41.7909
    pG <- -10.2099
    pH <- -45.2059
    pI <- 84.6395
    pJ <- -35.8644

    eps_w <- 1 + (pA / T2) * rho2 +
      (pB / T2 + pC + pD * T2) * rho2**2 +
      (pE / T2 + pF * T2 + pG * T2**2) * rho2**3 +
      (pH / T2**2 + pI / T2 + pJ) * rho2**4
  }
  return(eps_w)
}
