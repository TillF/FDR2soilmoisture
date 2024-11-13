#compare various equations for eps2theta conversion
compare_eps2theta_equations = function(common_set, legend_args=NULL, eq_subset=NULL, plot_extern=TRUE)
{  
  if (is.null(common_set$theta))   stop("common_set must contain a column 'theta'")
  if (is.null(common_set$epsilon)) stop("common_set must contain a column 'epsilon'")
  
    common_set  = common_set[!is.na(common_set$theta),] #discard NAs in theta
  
    if (any(common_set$theta > 1 | common_set$theta < 0)) stop("theta must be within [0,1].")
  
    if (is.null(common_set$training)) common_set$training   = TRUE #default: use all as training, none as test
    
    f_all_equal = function(x){all(is.na(x) | x[1]==x)} #test if all values in a vector are equal
    all_equal = apply(FUN = f_all_equal, X = common_set, MARGIN =2)
    
    if (all_equal["epsilon"]) stop("epsilon-values mustn't be all equal.")

    if (is.null(common_set$excluded)) common_set$excluded = FALSE #default: use all data
    
        
    legend_args2=list(x="topleft", legend="1:1", lty=2, pch=NA, col="black")
    
    if (is.null(common_set$pch))
    {  
      common_set$pch = 20  #default symbol for plotting
      legend_args2$pch = c(legend_args2$pch, 20, 20)
    }
    if (is.null(common_set$col))
    {  
      def_cols = c("black", "red") #default colours
      common_set$col = ifelse(common_set$training, def_cols[1], def_cols[2])  
      legend_args2$col = c(legend_args2$col, def_cols[1], def_cols[2])
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
    
    rmse = function(mod, obs) 
    {
      return(sqrt(mean((mod-obs)^2, na.rm=TRUE  ))) 
    }
    
    check_fields = function(required_fields, common_set)
    {
      if (!all(required_fields %in% names(common_set))) return(FALSE)
      if (any(apply(X = common_set[, required_fields, drop=FALSE], MARGIN =2, FUN=function(x){all(is.na(x))}))) return (FALSE)
          return(TRUE)
    }
    
    #generate "average" data for plotting
    #numeric_cols = sapply(common_set, class) == "numeric" #index to numeric columns
    median_modus = function(x){
      if (all(is.na(x))) 
        return(NA)
      else {
        if (inherits(x, "numeric") || inherits(x, "logical")) {
          res <- median(x, na.rm = TRUE)  #median
        } else {
          res <- names(sort(-table(x)))[1]  #mode
        }
        
        if (inherits(x, "factor")) {
          res <- factor(res, levels = levels(x))
        } else {
          class(res) <- class(x)  #force the same data type as the input
        }
        
        # as(res, Class = class(x))
        return(res)  
      }
      
    }
    
    eps_range = seq(from=1.1, to=80, by=1)
    
    
    #prepare arrays for plotting curves later using median/modus properties
    #browser()
    #clumsy, but "apply()" doesn't preserve column type
    common_set_plot_train = common_set[1,]
    for (cc in names(common_set))
        common_set_plot_train[1,cc] = median_modus(common_set[common_set$training, cc])
      
    # common_set_plot_train = t(sapply(common_set[common_set$training, numeric_cols], median_modus))
    # sapply(common_set[common_set$training,], median_modus, simplify = TRUE)
    # 
    # apply(common_set[common_set$training,], MARGIN=2, median_modus, simplify = FALSE)
    # 
    # median_modus(common_set[common_set$training,1])
    # 
    # browser()
    # a = t(common_set_plot_train)
    # b = t(sapply(common_set[common_set$training, !numeric_cols], modus, simplify = TRUE))
    # 
    # common_set_plot_train = data.frame(a, b)
    
    common_set_plot_train$epsilon = NULL #discard original epsilon column
    common_set_plot_train = cbind(common_set_plot_train, epsilon=eps_range)
    
    
    if (all(common_set$training))
      common_set_plot_test=common_set_plot_train[1,][-1,] else #empty dataframe
    {    
      #clumsy, but "apply()" doesn't preserve column type
      common_set_plot_test = common_set[1,]
      for (cc in names(common_set))
        common_set_plot_test[1,cc] = median_modus(common_set[common_set$training, cc])
      
      #common_set_plot_test = sapply(common_set[!common_set$training, numeric_cols], median, na.rm=TRUE)
      #common_set_plot_test = data.frame(t(common_set_plot_test), t(sapply(common_set[!common_set$training, !numeric_cols, drop=FALSE], modus)))
      #common_set_plot_test = sapply(common_set[!common_set$training, ], median_modus)

      common_set_plot_test$epsilon = NULL
      common_set_plot_test = cbind(common_set_plot_test, epsilon=eps_range)
    }
    
          

    r2_train=NULL      #for collecting r2 values (training data set)
    r2_train_ex =NULL #for collecting r2 values (training data set without excluded samples)
    r2_test =NULL #for collecting r2 values (test data set)
    rmse_train=NULL
    rmse_train_ex =NULL #for collecting rmse values (training data set without excluded samples)
    rmse_test =NULL 
    eps2theta_function_list = list() #collect conversion functions
    
    supported_eqs = eps2theta(epsdata = NULL, equation = "list") #get list of all supported equations
    if (is.null(eq_subset)) eq_subset=names(supported_eqs) #use all available equations
    eq_subset =  intersect(eq_subset, names(supported_eqs)) #restrict to available equations
    if (length(eq_subset) == 0) stop("No supported equations selected. See eps2theta(equation=\"list\") for available options.")
    supported_eqs = supported_eqs[eq_subset] #use only the selected ones
    
    for (i in 1:length(supported_eqs))
    {
      eq = names(supported_eqs)[i]
      print(eq)
      if (eq=="SinghEtal2019")
      {  
        b=33
      }  
      missing_fields = setdiff (supported_eqs[[i]], names(common_set))
      if (length(missing_fields) > 0)
      {
        print(paste0("Skipped ", eq, "; missing fields: ", paste0(missing_fields, collapse = ", ")))
        next
      }
      #browser()
      common_set[, paste0("theta_", eq)] = eps2theta(common_set, equation = eq) #apply equation
      r2_train     [paste0("theta_", eq)] = r2  (common_set[ common_set$training, paste0("theta_", eq)], common_set$theta[ common_set$training]) 
      r2_train_ex  [paste0("theta_", eq)] = r2  (common_set[ common_set$training & !common_set$excluded, paste0("theta_", eq)], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [paste0("theta_", eq)] = r2  (common_set[!common_set$training, paste0("theta_", eq)], common_set$theta[!common_set$training]) 
      rmse_train   [paste0("theta_", eq)] = rmse(common_set[ common_set$training, paste0("theta_", eq)], common_set$theta[ common_set$training]) 
      rmse_train_ex[paste0("theta_", eq)] = rmse(common_set[ common_set$training & !common_set$excluded, paste0("theta_", eq)], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [paste0("theta_", eq)] = rmse(common_set[!common_set$training, paste0("theta_", eq)], common_set$theta[!common_set$training]) 
      
      
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
    if (length(setdiff (c("soil"), names(common_set))) ==0 &
        any(grepl(eq_subset, pattern = "deltaT")))
    {
      eq="theta_deltaT_adj"
      if (any (!(unique(common_set$soil, na.rm=TRUE) %in% c("mineral", "organic", "clay"))))
        stop("Field 'soil' must be 'mineral', 'organic' or 'clay'")
      
      
      common_set$a0 = NA #auxiliary column to fix intercept (Profile Probe User Manual 5.0, p. 47)
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
      
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      common_set$a0 = NULL #remove auxiliary col
      
    }
    
    #adjusted Delta_T with variable intercept, regression on sqrt(eps)####
    if (length(setdiff (c("soil"), names(common_set))) == 0 &
        any(grepl(eq_subset, pattern = "deltaT")))
    {
      eq="theta_deltaT_adj2"
      if (any (!(unique(common_set$soil, na.rm=TRUE) %in% c("mineral", "organic", "clay"))))
        stop("Field 'soil' must be 'mineral', 'organic' or 'clay'")
      
      if (length(unique(common_set$soil))==1)
        lm_all = lm(theta ~  sqrt(epsilon), data=common_set[common_set$training,]) else
        lm_all = lm(theta ~  sqrt(epsilon) * soil, data=common_set[common_set$training,])

      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        if (nrow(common_set)==0)
          theta_pred = NULL else
        {
            theta_pred = predict(get(x = "lm_theta_deltaT_adj2",  envir = globvars), newdata = common_set)
        }
        return(theta_pred)
      }
      
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      common_set$a0 = NULL #remove auxiliary col
      
    }
    
    
    # Ledieu et al. (1986). adjusted (eq. 16.62 in Mohamed, 2018) ####
    if (length(setdiff ("BD", names(common_set))) ==0 &
        any(grepl(eq_subset, pattern = "Ledieu")))
    {  
      eq="theta_Ledieu_adj"
      lm_all = lm(theta ~ epsilon+BD, data=common_set[common_set$training,])
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use

      ftemp = function(common_set)
      {  
        #valid_rows = is.finite(common_set$theta + common_set$epsilon+common_set$BD)
        #theta_pred = rep(NA, nrow(common_set)) #create empty array for result
        if (nrow(common_set)==0)
          theta_pred = NULL else
        theta_pred = predict(get(x = "lm_theta_Ledieu_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
    
    #Malicki, adjusted ####
    if (length(setdiff ("BD", names(common_set))) ==0 &
        any(grepl(eq_subset, pattern = "Malicki")))
    {
      eq="theta_malicki_adj"
      lm_all = nls(formula = theta ~ (sqrt(epsilon) -a - b*BD- c*BD^2)/(d+e*BD), data = common_set[common_set$training,], start = c(a=0.819, b=0.168, c=0.168, d=7.17, e=2.18),
                   nls.control(maxiter = 100, warnOnly = TRUE)             )
      
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        if (nrow(common_set)==0)
          theta_pred = NULL else
        theta_pred = predict(get(x = "lm_theta_malicki_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
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
    
    #browser()
    if (check_fields(required_fields, common_set) &
        any(grepl(eq_subset, pattern = "Jacobsen")))
    {
      eq="theta_jacsch_adj"
      #coefs_org = c(BD =- 3.7*1e-2,  clay_perc = 7.36*1e-4, om_perc= 47.7*1e-4) #coefficients of original equation
      fmla = "theta ~  epsilon+epsilon^2+epsilon^3+ BD + clay_perc + om_perc"
      #replace coefficients with original values, for those where there is no variation in the column (otherwise, the fitting gets problematic)
      #exclude "all-equal" predictors from regression formula
      #browser()
      for (cc in names(which(all_equal)))
        fmla = gsub(x = fmla, pattern = paste0("\\+ *",cc), replacement = "")
      #    fmla = gsub(x = fmla, pattern = cc, replacement = paste0("I(",coefs_org[cc],")"))
      
      fmla = formula(fmla) #convert to formula object
      lm_all = lm(formula = fmla, data = common_set[common_set$training,])
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use

      ftemp = function(common_set)
      {  
        if (nrow(common_set)==0)
          theta_pred = NULL else
        theta_pred = predict(get(x = "lm_theta_jacsch_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
  
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
  
    #Drnevich et al (2005) adjusted ####
    required_fields = eps2theta(equation = "list")$DrnevichEtal2005
    if (all(required_fields %in% names(common_set)) &
          any(grepl(eq_subset, pattern = "Drnevich")) &
        !all(is.na(common_set$cohesive)))
    {
      eq="theta_Drnevich_adj"
      #there must be at least two different classes, otherwise, the fitting fails
      #start=lower=upper=NULL

      fmla = "theta ~ (sqrt(epsilon) - (cohesive*a_coh + (1-cohesive)*a_ncoh ) * BD) /
                     (cohesive*b_coh + (1-cohesive)*b_ncoh )"
    
      start = c(a_coh=1, a_ncoh=1, b_coh=1, b_ncoh=1)
      lower = c(a_coh=0.01, a_ncoh=0.01, b_coh=0.01, b_ncoh=0.01)
      upper = c(a_coh=100, a_ncoh=100, b_coh=100, b_ncoh=100)
      
      #adjust formula, in case only one class (cohesive/non-cohesive) is present
      if (sum(sapply(FUN=isTRUE, X=common_set$cohesive==1)) == 0)   #no cohesive
      {
        #remove cohesive terms 
        fmla = gsub(fmla, pattern="cohesive\\*[^ ]*", replacement="0") #remove cohesive coefficients
        remove_this = !grepl(names(start), pattern = "_coh")
        start = start[remove_this]
        lower = lower[remove_this]
        upper = upper[remove_this]
      }  
      
      if (sum(sapply(FUN=isTRUE, X=common_set$cohesive==0)) == 0)   #no non-cohesive
      {
        #remove non-cohesive terms 
        fmla = gsub(fmla, pattern="\\(1-cohesive\\)\\*[^ ]*", replacement="0") #remove cohesive coefficients
        
        remove_this = !grepl(names(start), pattern = "_ncoh")
        start = start[remove_this]
        lower = lower[remove_this]
        upper = upper[remove_this]
      }  
      

      fmla = formula(fmla) #convert to formula object
      lm_all = nls(formula = fmla, 
                   data = common_set[common_set$training,], 
                   #start = c(a_coh=0.95, a_ncoh=1, b_coh=8.8, b_ncoh=8.5),
                   start = start,
                   lower = lower,
                   upper = upper,
                   algorithm =  "port",
                   nls.control(maxiter = 100, warnOnly = FALSE))  
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        if (nrow(common_set)==0)
          theta_pred = NULL else
        theta_pred = predict(get(x = "lm_theta_Drnevich_adj",  envir = globvars), newdata = common_set)
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
    
    #Zhao et al., 2016, adjusted ####
    required_fields = eps2theta(equation = "list")$ZhaoEtal2016
    if (all(required_fields %in% names(common_set)) &
        any(grepl(eq_subset, pattern = "Zhao")))
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
      if (inherits(tt, "try-error"))
      {
        warning(paste0("Failed to fit ", eq," (", attr(tt, "condition"),")"))
      } else
      {  
        mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
        
        ftemp = function(common_set)
        {  
          if (nrow(common_set)==0)
            theta_pred = NULL else
          theta_pred = predict(get(x = "lm_theta_Zhao_adj",  envir = globvars), newdata = common_set)
          return(theta_pred)
        }
        
        eps2theta_function_list [[eq]] = ftemp #store conversion function
        common_set             [, eq] = ftemp(common_set) #apply conversion function
        r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
        r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
        r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
        rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
        rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
        rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
        
        #collect equation output for plotting
        common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
        common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
        
      }  
    }
    
    #   Singh et al 2019 (10.1016/j.agwat.2019.02.024) ####
    if (length(setdiff ("clay_perc", names(common_set))) ==0 &
        !("clay_perc" %in% names(which(all_equal))) &
        any(grepl(eq_subset, pattern = "Singh")))
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
        a = (parms[1]*data1$clay_perc^2 + parms[2]*data1$clay_perc + parms[3])
        b = (parms[4]*data1$clay_perc^2 + parms[5]*data1$clay_perc + parms[6])
        theta_mod = a *sqrt(data1$epsilon) + b #Table 3
        sse = sum((theta_mod - data1$theta)^2, na.rm=TRUE)
        return(sse)             
      }
      #browser()
      res=optim(par = c(a1=0, a2=as.numeric(a2), a3=0, b1=0, b2=0, b3=as.numeric(b)), fn = obj_fun, data1 = common_set[common_set$training,])
      
      res$par = c(a1=-3.33e-5, a2=1.14e-3, a3=0.108, b1=6.52e-5, b2=-2.48e-3, b3=-0.16)
      
      ranges =  res$par %*% t(c(0.1, 10))
  
      
      #refine with nlxb, the improved version of nls
      #library(nlmrt) #the regular nls is not robust here
      ignore = is.na(common_set$clay_perc + common_set$theta + common_set$epsilon) #mask NAs
      lm_all = nlmrt::nlxb(formula = theta ~ (a1*clay_perc^2 + a2*clay_perc + a3)*sqrt(epsilon) + (b1*clay_perc^2 + b2*clay_perc + b3), 
                    data = common_set[common_set$training & !ignore,], 
                    start = res$par,
                    upper = apply(X=ranges, MARGIN=1, FUN=max), 
                    lower = apply(X=ranges, MARGIN=1, FUN=min), 
                    #alg="port",
                    trace=FALSE,
                    nls.control(maxiter = 100, warnOnly = FALSE)  )
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      ftemp = function(common_set)
      {  
        lm_t = get("lm_theta_Singh_adj", envir = globvars) 
        
        coeffss = lm_t$coefficients
        a = (coeffss["a1"]*common_set$clay_perc^2 + coeffss["a2"]*common_set$clay_perc + coeffss["a3"])
        b = (coeffss["b1"]*common_set$clay_perc^2 + coeffss["b2"]*common_set$clay_perc + coeffss["b3"])
        theta_pred = a *sqrt(common_set$epsilon) + b 
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }
    
    #adjusted Roth et al1992 regression on 3rd order polynom####
    if (length(setdiff (c("soil"), names(common_set))) == 0 &
        any(grepl(eq_subset, pattern = "RothEtal1992")))
    {
      eq="theta_RothEtal1992_adj"
      if (any (!(unique(common_set$soil, na.rm=TRUE) %in% c("mineral", "organic"))))
        stop("Field 'soil' must be 'mineral' or 'organic' ")
      
      if (length(unique(common_set$soil))==1)
        lm_all = lm(theta ~  epsilon + I(epsilon^2) + I(epsilon^3), data=common_set[common_set$training,]) else
          lm_all = lm(theta ~  epsilon * soil + I(epsilon^2) * soil + I(epsilon^3) * soil, data=common_set[common_set$training,])
        
        mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
        
        ftemp = function(common_set)
        {  
          if (nrow(common_set)==0)
            theta_pred = NULL else
            {
              theta_pred = predict(get(x = "lm_theta_RothEtal1992_adj",  envir = globvars), newdata = common_set)
            }
          return(theta_pred)
        }
        
        
        eps2theta_function_list [[eq]] = ftemp #store conversion function
        common_set             [, eq] = ftemp(common_set) #apply conversion function
        r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
        r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
        r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
        rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
        rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
        rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
        
        #collect equation output for plotting
        common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
        common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
        common_set$a0 = NULL #remove auxiliary col
        
    }
    
    
    #own glm_bin ####   
#  if (any(grepl(eq_subset, pattern = "glm_bin")))
{
    form_str = "theta ~  sqrt(epsilon)+epsilon+I(epsilon^2)"
    if ("BD" %in% names(common_set) & !("BD" %in% names(which(all_equal))))
      form_str = paste0(form_str, "+ BD")
    if ("om_perc" %in% names(common_set) &  !("om_perc" %in% names(which(all_equal))))
      form_str = paste0(form_str, "+ om_perc")
    
    if ("clay_perc" %in% names(common_set) &  !("clay_perc" %in% names(which(all_equal))))
      form_str = paste0(form_str, "+ clay_perc")
      
      eq="theta_glm_bin"
  
      lm_all = glm(formula = formula(form_str), data = common_set[common_set$training,], family=quasibinomial)
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      
      ftemp = function(common_set)
      {  
        if (nrow(common_set)==0)
          theta_pred = NULL else
        theta_pred = predict(get(x = "lm_theta_glm_bin",  envir = globvars), newdata = common_set, type="response")
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
      
    }

    #own glm_gauss ####
 #   if (any(grepl(eq_subset, pattern = "theta_glm_gauss")))
    {
      eq="theta_glm_gauss"
      
      lm_all = glm(formula = formula(form_str), data = common_set[common_set$training,], family=gaussian)
      mod_list = assign(paste0("lm_", eq),lm_all, envir = globvars) #keep this lm for later use
      
      
      ftemp = function(common_set)
      {  
        if (nrow(common_set)==0)
          theta_pred = NULL else
        theta_pred = predict(get(x = "lm_theta_glm_gauss",  envir = globvars), newdata = common_set, type="response")
        return(theta_pred)
      }
      
      eps2theta_function_list [[eq]] = ftemp #store conversion function
      common_set             [, eq] = ftemp(common_set) #apply conversion function
      r2_train     [eq] = r2  (common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      r2_train_ex  [eq] = r2  (common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      r2_test      [eq] = r2  (common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      rmse_train   [eq] = rmse(common_set[ common_set$training, eq], common_set$theta[ common_set$training]) 
      rmse_train_ex[eq] = rmse(common_set[ common_set$training & !common_set$excluded, eq], common_set$theta[common_set$training & !common_set$excluded]) 
      rmse_test    [eq] = rmse(common_set[!common_set$training, eq], common_set$theta[!common_set$training]) 
      
      #collect equation output for plotting
      common_set_plot_train[, eq] = ftemp(common_set_plot_train) #apply equation
      common_set_plot_test [, eq] = ftemp(common_set_plot_test ) #apply equation
    }

    
    # #own glm_sqrt ####
    # {
    #   form_str = sub(x = form_str, pattern = "theta", replacement="sqrt(theta)")
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

 
    
# compare R2-values of all modells in single scatterplot ####
    if(plot_extern==TRUE){
      if (Sys.info()["sysname"] == "Windows") {
        windows(width = 40, height = 30) #open largest possible window in 4:3 format
      }else{
        dev.new(width = 40, height = 30) #open largest possible window in 4:3 format
      }
    }
    llimit = max(-1, quantile(r2_train_ex, probs=0.1, na.rm=TRUE), na.rm=TRUE)
    xcoords = pmax(llimit, r2_train_ex)
    xcoords[!is.finite(xcoords)] = llimit #replace NAs and NaNs
    llimit = max(-1, quantile(r2_test, probs=0.1, na.rm=TRUE), na.rm=TRUE)
    ycoords = pmax(llimit, r2_test, na.rm=TRUE)
    ycoords[!is.finite(ycoords)] = llimit #replace NAs and NaNs
    xlab = ifelse(all(common_set$excluded==FALSE), "R2_training", "R2 training!ex")
    plot(xcoords, ycoords, xlab=xlab, ylab="R2 test")
    #ycoords = xcoords = runif(5)
    sorted_ix = sort.int(xcoords, index.return = TRUE)$ix #
    range_y = diff(range(ycoords))
    if (range_y==0) range_y=1 #if all y values are the same, the plot is scaled to 1
    y_offset = 1/80 * range_y * rep(c(-2,-1, 1,2), length(xcoords)%/% 4 +1 )[1:length(xcoords)] #create alternating offsets in y to prevent overlapping labels
    ycoords_labels = ycoords
    ycoords_labels[sorted_ix] = ycoords[sorted_ix] + y_offset
    text(xcoords, ycoords_labels, sub(names(r2_train_ex), pattern="theta_", replacement=""), cex=0.7, adj = c(0.5,0))
    abline(v=0)
    abline(h=0)
    
       
    
# compare results of each single model in a matrix of scatterplots #### 
    models = names(common_set)
    models = models[grepl(models, pattern = "theta_")]
    
    if(plot_extern==TRUE){
      if (Sys.info()["sysname"] == "Windows") {
        windows(width = 40, height = 30) #open largest possible window in 4:3 format
      }else{
        dev.new(width = 40, height = 30) #open largest possible window in 4:3 format
      }
    }      

    par(mfrow=c(length(models) %/% 4 +1, 4), oma=c(2.3,2.3,0,0), mar=c(1, 1.6, 4.2, 0.5), cex=0.6)
  for (mm in models)    
  {
    ax_lims = pmin(1, pmax(0, extendrange(common_set$theta, f = 0.1)))
    
    tstr = format(r2_train[mm], digits = 3)

    if(!all(common_set$excluded==FALSE)) #add R2_train_ex, if not identical
      tstr = paste0(tstr, " / ", format(r2_train_ex[mm], digits = 3))
    
    tstr = paste0(tstr, " / ",format(r2_test[mm], digits = 3))
    main = paste0(mm, "\n R2: ", tstr)
    
    tstr = format(rmse_train[mm], digits = 3)
    tstr = paste0(tstr, " / ",format(rmse_test[mm], digits = 3))
    main = paste0(main, " RMSE: ", tstr)    
    
    plot(1, 1, main=main, xlim=ax_lims, ylim=ax_lims, type="n"
         , xlab="", ylab="")
  #    , xlab="theta_obs", ylab="theta_mod")
    #test data
    points(common_set$theta[!common_set$training], common_set[!common_set$training, mm],  col=common_set$col[!common_set$training], pch=common_set$pch[!common_set$training])
    if (any(!common_set$training))
      lines (lowess(common_set$theta[!common_set$training], common_set[!common_set$training, mm], delta=0.01, f=2),  col=common_set$col[!common_set$training][1], lty=1)
    #training data
    points(common_set$theta[ common_set$training], common_set[ common_set$training, mm],  col=common_set$col[ common_set$training], pch=common_set$pch[ common_set$training])
    if (any(common_set$training))
      lines (lowess(common_set$theta[common_set$training], common_set[common_set$training, mm], delta=0.01, f=2),  col=common_set$col[common_set$training][1], lty=1)
    
    abline(b=1, a=0, lty=2)
    #r2_ = r2(common_set[, mm], common_set$theta)
    #text(x = 0.22, y=0.6, labels = paste0("R2 = ", format(r2_, digits = 3)))
  } 
  mtext(text = "theta, observed [-]" , side=1, outer=TRUE, line = 1.2)
  mtext(text = "theta, predicted [-]", side=2, outer=TRUE, line = 1.2)
  
    #add legend in extra plot panel
  plot(1, 1, main="[eps-theta-modell]\n[GOF: training, training w/o excluded, test]", type="n", axes=FALSE, xlab="", ylab="", frame.plot=TRUE) 

  text(1,1, "Legend")
  if (length(legend_args)>0)
    do.call(legend, args=legend_args)
#    legend("topleft", legend=c("mineral", "organic", "testdata", "1:1"), col=c(palette()[1:2], "black", "black"), pch=c(20, 20, 20, NA), lty=c(0,0,0,1))
  

# compare equations by plotting "characteristic" curves into a single diagram #### 
  
  if(plot_extern==TRUE){
    if (Sys.info()["sysname"] == "Windows") {
      windows(width = 40, height = 30) #open largest possible window in 4:3 format
    }else{
      dev.new(width = 40, height = 30) #open largest possible window in 4:3 format
    }
  }
  models = names(common_set_plot_train)
  models = models[grepl(models, pattern = "theta_")]
  par(mfrow=c(1, 1))
  
  #construct palette
  #library(RColorBrewer)
  n = length(models)
  #construct palette
  pal = RColorBrewer::brewer.pal( min(n, 12), "Paired")
  lty = rep("solid", min(n, 12))
  
  if (n > 12)
  {
    pal = c(pal, pal)
    lty = c(lty, rep("dashed", max(n-12, 0)))
  }      
  
  palette(pal)
  
  plot(1, 1, xlim=range(eps_range), ylim=c(0.00,1.2), type="n", xlab="epsilon [-]", ylab="theta, predicted [-]", log="x")
  
  
  for (i in 1:n)    
  {
    mm = models[i]
    lines (eps_range, common_set_plot_train[, mm], col=pal[i], lty=lty[i], lwd=2)
    if (nrow(common_set_plot_test)>0)
      lines (eps_range, common_set_plot_test [, mm], col=pal[i], lty=lty[i], lwd=1.0)
  } 
  legend("topleft", legend=c("train", "test", models), col=c("black", "black", pal[1:n]), 
         pch=NA, lty=c("solid","solid", lty), lwd=c(2, 1, rep(2, length(models))))
  
  
  return(list(n_train =    sum( common_set$training),
              n_train_ex = sum( common_set$training)-sum( common_set$excluded),
              n_test  = sum(!common_set$training),
              r2_train=r2_train, 
              r2_train_ex=r2_train_ex, 
              r2_test=r2_test,
              rmse_train=rmse_train, 
              rmse_train_ex=rmse_train_ex, 
              rmse_test=rmse_test, 
              eps2theta_function=eps2theta_function_list))
}  

