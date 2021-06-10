#compare various equations for eps2theta conversion
compare_eps2theta_equations = function(common_set, legend_args=NULL)
{  
    if (is.null(common_set$theta)) stop("common_set must contain a column 'theta'")
  
    common_set  = common_set[!is.na(common_set$theta),] #discard NAs in theta
  
  
    if (any(common_set$theta > 1 | common_set$theta < 0)) stop("theta must be within [0,1].")
  
    if (is.null(common_set$training)) common_set$training   = TRUE #default: use all as training, none as test
    
    f_all_equal = function(x){all(x[1]==x)} #test if all values in a vector are equal
    all_equal = apply(FUN = f_all_equal, X = common_set, MAR=2)
    
    if (all_equal["epsilon"]) stop("epsilon-values mustn't be all equal.")
    
    legend_args2=list(x="topleft", legend="1:1", lty=1, pch=NA, col="black")
    
    if (is.null(common_set$pch))
    {  
      common_set$pch = 20  #default symbol for plotting
      legend_args2$pch = c(legend_args2$pch, 20, 20)
    }
    if (is.null(common_set$col))
    {  
      common_set$col = ifelse(common_set$training, "black", "grey")  #default colour for plotting
      legend_args2$col = c(legend_args2$col, "black", "red")
      legend_args2$lty = c(legend_args2$lty, 0, 0)
      legend_args2$legend = c(legend_args2$legend, "training", "test")
    }
   
    if (is.null(legend_args)) 
      legend_args=legend_args2

    #for assessing goodness of fit
    r2 = function(mod, obs) 
    {
      complete = is.finite(mod+obs)
      return(1 - sum((mod[complete]-obs[complete])^2, na.rm=TRUE  ) / sum((mean(obs[complete]) - obs[complete])^2, na.rm=TRUE  )) 
    }
    
    
    #generate "average" data for plotting
    numeric_cols = sapply(common_set, class) == "numeric" #index to numeric columns
    modus = function(x)
    { return (names(sort(-table(x)))[1])}
    eps_range = seq(from=1.1, to=80, by=1)
    
    
    #prepare arrays for plotting curves later using mean/modus properties
    common_set_plot_train = sapply(common_set[common_set$training, numeric_cols], median, na.rm=TRUE)
    common_set_plot_train = data.frame(t(common_set_plot_train), t(sapply(common_set[common_set$training, !numeric_cols], modus)))
    common_set_plot_train$epsilon = NULL #discard original epsilon column
    common_set_plot_train = cbind(common_set_plot_train, epsilon=eps_range)
    
    
    if (all(common_set$training))
      common_set_plot_test=common_set_plot_train[1,][-1,] else #empty dataframe
    {    
      common_set_plot_test = sapply(common_set[!common_set$training, numeric_cols], median, na.rm=TRUE)
      common_set_plot_test = data.frame(t(common_set_plot_test), t(sapply(common_set[!common_set$training, !numeric_cols], modus)))
      common_set_plot_test$epsilon = NULL
      common_set_plot_test = cbind(common_set_plot_test, epsilon=eps_range)
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
      
      #collect equation output for plotting
      common_set_plot_train[, paste0("theta_", eq)] = eps2theta(common_set_plot_train, equation = eq) #apply equation
      common_set_plot_test [, paste0("theta_", eq)] = eps2theta(common_set_plot_test , equation = eq) #apply equation
    }
    
  
    #custom fits:  ####

    
    
    # #simple regression on epsilon_sqrt
    # {
    #   eq="theta_simple_linear"
    #   lm_all = lm(theta ~ sqrt(epsilon), data=common_set[common_set$training,])
    #   
    #   mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
    #   
    #   ftemp = function(common_set)
    #   {  
    #     #valid_rows = is.finite(common_set$theta + common_set$epsilon)
    #     #theta_pred = rep(NA, nrow(common_set)) #create empty array for result
    #     
    #     theta_pred = predict(get(x = "lm_theta_simple_linear",  envir = globvars), newdata = common_set)
    #     return(theta_pred)
    #   }
    # 
    #   eps2theta_function_list [[eq]] = ftemp #store conversion function
    #   common_set             [, eq] = ftemp(common_set) #apply conversion function
    #   r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
    #   r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
    #   
    #   #collect equation output for plotting
    #   common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
    #   common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
    # }
    # 
    #adjusted Delta_T with fixed intercept, regression on sqrt eps ####
    if (length(setdiff (c("soil"), names(common_set))) ==0)
    {
      eq="theta_deltaT_adj"
      if (any (!(unique(common_set$soil, na.rm=TRUE) %in% c("mineral", "organic", "clay"))))
        stop("Field 'soil' must be 'mineral' or 'organic'")
      
      common_set$a0 = NA #auxiliary column to fix intercept (aProfile Probe User Manual 5.0, p. 47)
      common_set$a0[common_set$soil == "organic"] = 1.4
      common_set$a0[common_set$soil == "mineral"] = 1.6
      common_set$a0[common_set$soil == "clay"]    = 1.8
      
      lm_all = lm(sqrt(epsilon)-a0 ~ theta -1, data=common_set[common_set$training,])
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        common_set$a0[common_set$soil == "organic"] = 1.4
        common_set$a0[common_set$soil == "mineral"] = 1.6
        common_set$a0[common_set$soil == "clay"]    = 1.8
        
        lm_t = get("lm_theta_deltaT_adj", envir = globvars) 
        #a1 = lm_t$coefficients["theta"]
        theta_pred =  1/lm_t$coefficients["theta"]*sqrt(common_set$epsilon) - common_set$a0/lm_t$coefficients["theta"] 
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      common_set$a0 = NULL #remove auxiliary col
      
    }
    
    #adjusted Delta_T with fixed intercept, direct regression ####
    if (length(setdiff (c("soil"), names(common_set))) == 0)
    {
      eq="theta_deltaT_adj2"
      if (any (!(unique(common_set$soil, na.rm=TRUE) %in% c("mineral", "organic", "clay"))))
        stop("Field 'soil' must be 'mineral' or 'organic'")
      
      common_set$a0 = NA #auxiliary column to fix intercept (aProfile Probe User Manual 5.0, p. 47)
      common_set$a0[common_set$soil == "organic"] = 1.4
      common_set$a0[common_set$soil == "mineral"] = 1.6
      common_set$a0[common_set$soil == "clay"]    = 1.8
      
      lm_all = nls(formula = theta ~ 1/a1 * sqrt(epsilon) -a0/a1, data = common_set[common_set$training,], start = c(a1=0.12),
                   nls.control(maxiter = 100, warnOnly = FALSE) )
      
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        common_set$a0[common_set$soil == "organic"] = 1.4
        common_set$a0[common_set$soil == "mineral"] = 1.6
        common_set$a0[common_set$soil == "clay"]    = 1.8
        
        theta_pred = predict(get(x = "lm_theta_deltaT_adj2",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      common_set$a0 = NULL #remove auxiliary col
      
    }
    
    
    # Ledieu et al. (1986). adjusted (eq. 16.62 in Mohamed, 2018) ####
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
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
    
    #Malicki, adjusted ####
    if (length(setdiff ("BD", names(common_set))) ==0)
    {
      eq="theta_malicki_adj"
      lm_all = nls(formula = theta ~ (sqrt(epsilon) -a - b*BD- c*BD^2)/(d+e*BD), data = common_set[common_set$training,], start = c(a=0.819, b=0.168, c=0.168, d=7.17, e=2.18),
                   nls.control(maxiter = 100, warnOnly = FALSE)             )
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
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
    
    # #Malicki, sqrt ####
    # if (length(setdiff ("BD", names(common_set))) ==0)
    # {
    #   eq="theta_malicki_adj_sqrt"
    #   lm_all = nls(formula = theta ~ sqrt((sqrt(epsilon) -a - b*BD- c*BD^2)/(d+e*BD)), data = common_set[common_set$training,], start = c(a=0.819, b=0.168, c=0.168, d=7.17, e=2.18),
    #                nls.control(maxiter = 100, warnOnly = FALSE)             )
    #   mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
    #   
    #   ftemp = function(common_set)
    #   {  
    #     theta_pred = predict(get(x = "lm_theta_malicki_adj_sqrt",  envir = globvars), newdata = common_set)
    #     return(theta_pred)
    #   }
    #   
    #   eps2theta_function_list [[eq]] = ftemp #store conversion function
    #   common_set             [, eq] = ftemp(common_set) #apply conversion function
    #   r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
    #   r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])       
    # }
    
    #Jacobsen & Schjonning (1993), adjusted ####
    required_fields = eps2theta(equation = "list")$"Jacobsen_Schjonning1993"
    if (all(required_fields %in% names(common_set)))
    {
      eq="theta_jacsch_adj"
      #coefs_org = c(BD =- 3.7*1e-2,  clay_perc = 7.36*1e-4, om_perc= 47.7*1e-4) #coefficients of original equation
      fmla = "theta ~  epsilon+epsilon^2+epsilon^3+ BD + clay_perc + om_perc"
      #replace coefficients with original values, for those where there is no variation in the column (otherwise, the fitting gets problematic)
      #exclude "all-equal" predictors from regression formula
      for (cc in names(which(all_equal)))
        fmla = gsub(x = fmla, pattern = paste0("\\+ *",cc), replacement = "")
      #    fmla = gsub(x = fmla, pattern = cc, replacement = paste0("I(",coefs_org[cc],")"))
      
      fmla = formula(fmla) #convert to formula object
      lm_all = lm(formula = fmla, data = common_set[common_set$training,])
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
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
  
    #Drnevich et al (2005) adjusted ####
    required_fields = eps2theta(equation = "list")$DrnevichEtal2005
    if (all(required_fields %in% names(common_set)) &
        length(unique(common_set$cohesive))>1 ) #there must be at least two different classes, otherwise, the fitting fails
    {
      eq="theta_Drnevich_adj"
      fmla = "theta ~ (sqrt(epsilon) / BD - (cohesive*a_coh + (1-cohesive)*a_ncoh)) /
                     (cohesive*b_coh + (1-cohesive)*b_ncoh)"
    
      fmla = formula(fmla) #convert to formula object
      lm_all = nls(formula = fmla, 
                   data = common_set[common_set$training,], 
                   #start = c(a_coh=0.95, a_ncoh=1, b_coh=8.8, b_ncoh=8.5),
                   start = c(a_coh=1, a_ncoh=1, b_coh=1, b_ncoh=1),
                   lower = c(a_coh=0.01, a_ncoh=0.01, b_coh=0.01, b_ncoh=0.01), 
                   upper = c(a_coh=100, a_ncoh=100, b_coh=100, b_ncoh=100), 
                   algorithm =  "port",
                   nls.control(maxiter = 100, warnOnly = FALSE))  
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        theta_pred = predict(get(x = "lm_theta_Drnevich_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])       
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
    
    #Zhao et al., 2016, adjusted ####
    required_fields = eps2theta(equation = "list")$ZhaoEtal2016
    if (all(required_fields %in% names(common_set)))
    {
      eq="theta_Zhao_adj"
      
      coefs_org=c(a=0.3039, b= -2.1851, c= 18.0283, d=-17.9531, e=-0.6806, f=1.8351) #coefficients of original equation/Zhao's values
      fmla = "theta ~ ( a *BD + b + sqrt(epsilon)) /
                       (( c *BD+d) +(e*BD+f)*sqrt(epsilon))"
      #replace coefficients with original values, for those where there is no variation in the column (otherwise, the fitting gets problematic)
      if ("BD" %in% names(which(all_equal)))
      {
        fmla = gsub(x = fmla, pattern = " a ", replacement =  coefs_org["a"])
        fmla = gsub(x = fmla, pattern = " b ", replacement =  coefs_org["b"])
      }
      fmla = formula(fmla) #convert to formula object

      
      #robust optimization using optim
      obj_fun = function(parms, data1)
      {  
        theta_mod = (parms[1]*data1$BD +parms[2] + sqrt(data1$epsilon)) /
          ((parms[3]*data1$BD+parms[4]) +(parms[5]*data1$BD+parms[6])*sqrt(data1$epsilon))
        sse = sum((theta_mod - data1$theta)^2, na.rm=TRUE)
        return(sse)             
      }
      res=optim(par = c(a=0, b=0, c=0, d=0, e=1, f=1), fn = obj_fun, data1 = common_set[common_set$training,])
      
      ignore = is.na(common_set$BD + common_set$theta + common_set$epsilon) #mask NAs
      tt = try({
        lm_all = nls(formula = fmla, 
                     data = common_set[common_set$training & !ignore,], 
                     start = res$par,
                     lower=-Inf,
                     upper= Inf,
                     trace=FALSE,
                     nls.control(maxiter = 100, warnOnly = TRUE)             )
      }, silent=TRUE
      )
      if (class(tt) =="try-error")
      {
        warning(paste0("Failed to fit ", eq," (", attr(tt, "condition"),")"))
      } else
      {  
        mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
        
        ftemp = function(common_set)
        {  
          theta_pred = predict(get(x = "lm_theta_Zhao_adj",  envir = globvars), newdata = common_set)
          return(theta_pred)
        }
        
        eps2theta_function_list [[eq]] = ftemp #store conversion function
        common_set             [, eq] = ftemp(common_set) #apply conversion function
        r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
        r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])
        
        #collect equation output for plotting
        common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
        common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
        
      }  
    }
    
    #   Singh et al 2019 (10.1016/j.agwat.2019.02.024) ####
    if (length(setdiff ("clay_perc", names(common_set))) ==0 &
       !("clay_perc" %in% names(which(all_equal))))
    {
      eq="theta_Singh_adj"
      #get initial estimate
      lm_all = lm(formula = theta ~ sqrt(epsilon), 
                  data = common_set[common_set$training,]) 
      a3 = coef(lm_all)["sqrt(epsilon)"] #retrieve coefficient
      b  = coef(lm_all)["(Intercept)"] #retrieve coefficient
      
      a2=a3/mean(common_set$clay_perc, na.rm=TRUE) #estimate starting value for a2
      
      #robust optimization using optim
      obj_fun = function(parms, data1)
      {  
        theta_mod = (parms[1]*data1$clay_perc^2 + parms[2]*data1$clay_perc + parms[3])*sqrt(data1$epsilon) + parms[4] 
        sse = sum((theta_mod - data1$theta)^2, na.rm=TRUE)
        return(sse)             
      }
      res=optim(par = c(a1=0, a2=as.numeric(a2), a3=0, b=as.numeric(b)), fn = obj_fun, data1 = common_set[common_set$training,])
      
      #refine with nlxb, the improved version of nls
      library(nlmrt) #the regular nls is not robust here
      ignore = is.na(common_set$clay_perc + common_set$theta + common_set$epsilon) #mask NAs
      lm_all = nlxb(formula = theta ~ (a1*clay_perc^2 + a2*clay_perc + a3)*sqrt(epsilon) + b, 
                    data = common_set[common_set$training & !ignore,], 
                    start = res$par,
                    upper = res$par + abs(res$par)*0.01, 
                    lower = res$par - abs(res$par)*0.01, 
                    #alg="port",
                    trace=FALSE,
                    nls.control(maxiter = 100, warnOnly = FALSE)  )
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        lm_t = get("lm_theta_Singh_adj", envir = globvars) 
        theta_pred = (lm_t$coefficients["a1"]*common_set$clay_perc^2 + lm_t$coefficients["a2"]*common_set$clay_perc + 
                        lm_t$coefficients["a3"])*sqrt(common_set$epsilon) + lm_t$coefficients["b"] 
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])       
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
    
    
    #own glm_bin ####   
{
    form_str = "theta ~  sqrt(epsilon)+epsilon+I(epsilon^2)"
    if ("BD" %in% names(common_set) & !("BD" %in% names(which(all_equal))))
      form_str = paste0(form_str, "+ BD"
      )
    if ("om_perc" %in% names(common_set) &  !("om_perc" %in% names(which(all_equal))))
      form_str = paste0(form_str, "+ om_perc")
    
    if ("clay_perc" %in% names(common_set) &  !("clay_perc" %in% names(which(all_equal))))
      form_str = paste0(form_str, "+ clay_perc")
      
      eq="theta_glm_bin"
  
      lm_all = glm(formula = formula(form_str), data = common_set[common_set$training,], family=quasibinomial)
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      
      ftemp = function(common_set)
      {  
        if (ncol(common_set==0))
          theta_pred = NULL else
        theta_pred = predict(get(x = "lm_theta_glm_bin",  envir = globvars), newdata = common_set, type="response")
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])     
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }

    #own glm_gauss ####
    {
      eq="theta_glm_gauss"
      
      lm_all = glm(formula = formula(form_str), data = common_set[common_set$training,], family=gaussian)
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      
      ftemp = function(common_set)
      {  
        theta_pred = predict(get(x = "lm_theta_glm_gauss",  envir = globvars), newdata = common_set, type="response")
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])     
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
    }

    
    # #own glm_sqrt ####
    # {
    #   form_str = sub(x = form_str, pattern = "theta", repl="sqrt(theta)")
    #   eq="theta_glm_sqrt"
    #   
    #   lm_all = glm(formula = formula(form_str), data = common_set[common_set$training,], family=gaussian)
    #   mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
    #   
    #   
    #   ftemp = function(common_set)
    #   {  
    #     theta_pred = predict(get(x = "lm_theta_glm_sqrt",  envir = globvars), newdata = common_set, type="response")
    #     theta_pred = theta_pred^2  #convert sqrt(theta) to theta
    #     return(theta_pred)
    #   }
    #   
    #   eps2theta_function_list [[eq]] = ftemp #store conversion function
    #   common_set             [, eq] = ftemp(common_set) #apply conversion function
    #   r2_train   [eq] = r2(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
    #   r2_test    [eq] = r2(common_set[!common_set$training, eq], common_set$theta[!common_set$training])     
    # }

 
    

       
    
  # compare results in scatterplot matrix #### 
    models = names(common_set)
    models = models[grepl(models, pattern = "theta_")]
    
  windows(width = 40, height = 30) #open largest possible window in 4:3 format
    par(mfrow=c(length(models) %/% 4 +1, 4), oma=c(1.2,0,0,0), mar=c(0.7, 1.5, 4.2, 0.5), cex=0.6)
  for (mm in models)    
  {
    ax_lims = pmin(1, pmax(0, extendrange(common_set$theta, f = 0.1)))
    plot(1, 1, main=paste0(mm, "\n R2 = ", format(r2_train[mm], digits = 3), " / ", format(r2_test[mm], digits = 3)), xlim=ax_lims, ylim=ax_lims, type="n"
         , xlab="", ylab="")
  #    , xlab="theta_obs", ylab="theta_mod")
    points(common_set$theta[!common_set$training], common_set[!common_set$training, mm],  col=common_set$col[!common_set$training], pch=common_set$pch[!common_set$training])
    if (any(!common_set$training))
      lines (lowess(common_set$theta[!common_set$training], common_set[!common_set$training, mm], delta=0.01, f=2),  col="red", lty="dashed")
    points(common_set$theta[ common_set$training], common_set[ common_set$training, mm],  col=common_set$col[ common_set$training], pch=common_set$pch[ common_set$training])
    lines (lowess(common_set$theta[common_set$training], common_set[common_set$training, mm], delta=0.01, f=2),  col="red", lty="solid")
    
    abline(b=1, a=0)
    #r2_ = r2(common_set[, mm], common_set$theta)
    #text(x = 0.22, y=0.6, labels = paste0("R2 = ", format(r2_, digits = 3)))
  } 
  plot(1, 1, main="", type="n", axes=FALSE, xlab="", ylab="")    
  if (length(legend_args)>0)
    do.call(legend, args=legend_args)
#    legend("topleft", legend=c("mineral", "organic", "testdata", "1:1"), col=c(palette()[1:2], "black", "black"), pch=c(20, 20, 20, NA), lty=c(0,0,0,1))
  

# compare equations by plotting "characteristic" curves into a single diagram  
  windows(width = 40, height = 30) #open largest possible window in 4:3 format
  models = names(common_set_plot_train)
  models = models[grepl(models, pattern = "theta_")]
  
  
  #construct palette
  library(RColorBrewer)
  n = length(models)
  #construct palette
  pal = brewer.pal( min(n, 12), "Paired")
  lty = rep("solid", min(n, 12))
  
  if (n > 12)
  {
        pal = c(pal, pal)
        lty = c(lty, rep("dashed", max(n-12, 0)))
  }      
  
  palette(pal)
  
  plot(1, 1, xlim=range(eps_range), ylim=c(0.00,1.2), type="n", xlab="epsilon", ylab="theta_mod")
  
  
  for (i in 1:n)    
  {
    mm = models[i]
    lines (eps_range, common_set_plot_train[, mm], col=pal[i], lty=lty[i], lwd=2)
    if (nrow(common_set_plot_test)>0)
      lines (eps_range, common_set_plot_test [, mm], col=pal[i], lty=lty[i], lwd=1.0)
  } 
  legend("topleft", legend=c("train", "test", models), col=c("black", "black", pal[1:n]), 
         pch=NA, lty=c("solid","solid", lty), lwd=c(2, 1, rep(2, length(models))))
  
  
  return(list(r2_train=r2_train, r2_test=r2_test, eps2theta_function=eps2theta_function_list))
}  
