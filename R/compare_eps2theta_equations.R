#compare various equations for eps2theta conversion
compare_eps2theta_equations = function(common_set)
{  
    if (is.null(common_set$training)) common_set$training   = TRUE #default: use all as training, none as test
    
    
    #for assessing goodness of fit
    r2 = function(mod, obs) 
    {
      complete = is.finite(mod+obs)
      return(1 - sum((mod[complete]-obs[complete])^2, na.rm=TRUE  ) / sum((mean(obs[complete]) - obs[complete])^2, na.rm=TRUE  )) 
    }
    
    r2_train=NULL #for collecting r2 values
    r2_test =NULL #for collecting r2 values (test data set)
    eps2theta_function_list = list() #collect conversion functions
    
    supported_eqs = eps2theta(epsdata = NULL, equation = "list") #get list of all supported equations
    for (i in 1:length(supported_eqs))
    {
      eq = names(supported_eqs)[i]
      missing_fields = setdiff (supported_eqs[[i]], names(common_set))
      if (length(missing_fields) > 0)
      {
        print(paste0("Skipped ", eq, "; missing fields: ", paste0(missing_fields, collapse = ", ")))
        next
      }
      common_set[, paste0("theta_", eq)] = eps2theta(common_set, equation = eq) #apply equation
      r2_train   [paste0("theta_", eq)] = r2(common_set[ common_set$training, paste0("theta_", eq)], common_set$theta[ common_set$training]) 
      r2_test    [paste0("theta_", eq)] = r2(common_set[!common_set$training, paste0("theta_", eq)], common_set$theta[!common_set$training]) 
      ftemp = eval(parse(text=paste0("function(epsdata){eps2theta(epsdata = epsdata, equation = \"",eq, "\")}")))
      eps2theta_function_list [[paste0("theta_", eq)]] = ftemp #store conversion function
    }
    
  
    #custom fits:  
    
    #simple regression on epsilon_sqrt
    {
      eq="theta_simple_linear"
      lm_all = lm(theta ~ sqrt(epsilon), data=common_set[common_set$training,])
      
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        #valid_rows = is.finite(common_set$theta + common_set$epsilon)
        #theta_pred = rep(NA, nrow(common_set)) #create empty array for result
        
        theta_pred = predict(get(x = "lm_theta_simple_linear",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
  
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      }
    
    
    # Ledieu et al. (1986). adjusted (eq. 16.62 in Mohamed, 2018)
    if (length(setdiff ("BD", names(common_set))) ==0)
    {  
      eq="theta_Ledieu_adj"
      lm_all = lm(theta ~ epsilon+BD, data=common_set[common_set$training,])
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use

      ftemp = function(common_set)
      {  
        #valid_rows = is.finite(common_set$theta + common_set$epsilon+common_set$BD)
        #theta_pred = rep(NA, nrow(common_set)) #create empty array for result
        
        theta_pred = predict(get(x = "lm_theta_Ledieu_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
    }
    
    #Malicki, adjusted
    if (length(setdiff ("BD", names(common_set))) ==0)
    {
      eq="theta_malicki_adj"
      lm_all = nls(formula = theta ~ (sqrt(epsilon) -a - b*BD- c*BD^2)/(d+e*BD), data = common_set[common_set$training,], start = c(a=0.819, b=0.168, c=0.168, d=7.17, e=2.18))
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use

      ftemp = function(common_set)
      {  
         theta_pred = predict(get(x = "lm_theta_malicki_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])       
    }
    
    #Jacobsen & Schjonning (1993), adjusted
    if (length(setdiff (c("BD", "clay_perc", "om_perc"), names(common_set))) ==0)
    {
      eq="theta_jacsch_adj"
      lm_all = lm(formula = theta ~  epsilon+epsilon^2+epsilon^3+ BD + clay_perc + om_perc, data = common_set[common_set$training,])
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      

      ftemp = function(common_set)
      {  
        theta_pred = predict(get(x = "lm_theta_jacsch_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
  
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])       
    }
  
   
    #own glm
    if (length(setdiff (c("BD"), names(common_set))) ==0)
    {
      eq="theta_glm"
      lm_all = glm(formula = theta ~  epsilon+I(epsilon^2)+ BD + I(BD^2), data = common_set[common_set$training,], family=quasipoisson)
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      

      ftemp = function(common_set)
      {  
        theta_pred = predict(get(x = "lm_theta_glm",  envir = globvars), newdata = common_set, type="response")
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])     
    }
    rm(lm_all)
    
    
  # compare results  
    models = names(common_set)
    models = models[grepl(models, pattern = "theta_")]
    
  windows(width = 40, height = 30) #open largest possible window in 4:3 format
    par(mfrow=c(length(models) %/% 4 +1, 4))
  for (mm in models)    
  {
    plot(1, 1, main=paste0(mm, "\n R2 = ", format(r2_train[mm], digits = 3), " / ", format(r2_test[mm], digits = 3)), xlim=c(0.05,0.95), ylim=c(0.05,0.95), type="n", xlab="theta_obs", ylab="theta_mod")
    points(common_set$theta[!common_set$training], common_set[!common_set$training, mm],  col="black", pch=20)
    points(common_set$theta[ common_set$training], common_set[ common_set$training, mm],  col=ifelse(common_set$soil=="mineral",1,2) , pch=20)
           
    abline(b=1, a=0)
    #r2_ = r2(common_set[, mm], common_set$theta)
    #text(x = 0.22, y=0.6, labels = paste0("R2 = ", format(r2_, digits = 3)))
  } 
  plot(1, 1, main="", type="n", axes=FALSE, xlab="", ylab="")    
  legend("topleft", legend=c("mineral", "organic", "testdata", "1:1"), col=c(palette()[1:2], "black", "black"), pch=c(20, 20, 20, NA), lty=c(0,0,0,1))
  

  return(list(r2_train=r2_train, r2_test=r2_test, eps2theta_function=eps2theta_function_list))
}  