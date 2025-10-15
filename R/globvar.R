#package-global variables
eps_air = 1.00059     #permittivity of air
eps_h2o = 81          #permittivity of water at around 18 °C, to keep consistence with older package version

globvars <- new.env()
# globvars2 <- new.env(parent = emptyenv())
# 
# package_env = environment() #get parent environment, so we can assign variables to it from within functions
# package_frame = parent.frame() #get parent environment, so we can assign variables to it from within functions
# 
# globvars$a =1
# globvars2$a =1
# 
# globvars$V2eps_Theta_Probe_table = function(x){x} #just a dummy function, so the code initializes correctly. 

.V2eps_Theta_Probe_table = function(x){x} #just a dummy function, so the code initializes correctly. 
#.V2eps_PR2_table = function(x){x} #just a dummy function, so the code initializes correctly. 
#Will be set in .onload() with meaningful values (we cannot do it here as we require some functions first)

#print(globvars$a)
#print(globvars2$a)

# x=3
# tt = function ()
# {
#   return(x)
# }
# tt()
# x=4
# tt()
# 
# environment()
# tt2 = function ()
# {
#   x=5
#   print(environment())
#   parent.env(tt) <- environment()
#   return(tt())
# }
# 
# tt2()
