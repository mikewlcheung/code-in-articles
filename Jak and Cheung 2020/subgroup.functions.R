

library(OpenMx)
library(metaSEM)


# Function to return nicer output from subgroup analysis
#
# For random effects models, the models should first be run seperately in the groups with tssem2()
# The first argument is a list with the fitted tssem2() models in different subgroups
# The second argument is a fitted multigroup model with equality constraints
# The function will return parameter estimates and standard errors of the constrained model
# and fit indices of all relevant models
# When the second argument is not given it will only return fitindices of the unconstrained model

# These two functions are OpenMx helper functions to get the confidence interval for RMSEA


rmseaConfidenceIntervalHelper <- function(chi.squared, df, N, lower, upper){
  # Lower confidence interval
  if( pchisq(chi.squared, df=df, ncp=0) >= upper){ #sic
    lower.lam <- uniroot(f=pChiSqFun, interval=c(1e-10, 1e4), val=chi.squared, degf=df, goal=upper)$root
    # solve pchisq(ch, df=df, ncp=x) == upper for x
  } else{
    lower.lam <- 0
  }
  # Upper confidence interval
  if( pchisq(chi.squared, df=df, ncp=0) >= lower){ #sic
    upper.lam <- uniroot(f=pChiSqFun, interval=c(1e-10, 1e4), val=chi.squared, degf=df, goal=lower)$root
    # solve pchisq(ch, df=df, ncp=x) == lower for x
  } else{
    upper.lam <- 0
  }
  lower.rmsea <- sqrt(lower.lam/(N*df))
  upper.rmsea <- sqrt(upper.lam/(N*df))
  return(c(lower.rmsea=lower.rmsea, upper.rmsea=upper.rmsea))
}

pChiSqFun <- function(x, val, degf, goal){
  goal - pchisq(val, degf, ncp=x)
}

#submodels.fit <- submodels.fit
#constrained.fit <-  Stage2_constrained.fit


##### subgroup output function

subgroup.summary <- function(submodels.fit,constrained.fit=NULL,print.est=FALSE){
  

  if (length(submodels.fit)<2){
    stop("Please provide a list with fitted models in the different subgroups as the first argument")}
  
  
  nvar <- nrow(submodels.fit[[1]]$Cov)
  
  # extract from submodels
  subN <- c()
  submodelchi <- c()
  submodeldf <- c()
  subindepchi <- c()
  subindepdf <- c()
  sub_free_SRMR <- c()
  
  for (i in 1:length(submodels.fit)){
    
    subN[i] <- summary(submodels.fit[[i]])$stat["Sample size",]
    submodelchi[i] <- summary(submodels.fit[[i]])$stat["Chi-square of target model",]
    submodeldf[i] <- summary(submodels.fit[[i]])$stat["DF of target model",]
    subindepchi[i] <- summary(submodels.fit[[i]])$stat["Chi-square of independence model",] 
    subindepdf[i] <- summary(submodels.fit[[i]])$stat["DF of independence model",]
    sub_free_SRMR[i] <- sqrt(mean(submodels.fit[[i]]$mx.fit$algebras$vecS$result^2))
    
  }
  
  N <- sum(subN)
  Nobs <- nvar*(nvar+1)/2 * length(submodels.fit)
  free_chi <- sum(submodelchi)
  free_df <- sum(submodeldf)
  indep_chi <- sum(subindepchi)
  indep_df <- sum(subindepdf)
  
  
  # calculate fitindices RMSEA, CFI, TLI, AIC, BIC, SRMR
  
  RMSEA <- function(chi,df,N){sqrt(max(chi-df,0)/(df*(N-1)))}

  # free model
  
  free_RMSEA <- sqrt(length(submodels.fit)) * RMSEA(free_chi, free_df, N)
  free_RMSEA_CI <- sqrt(length(submodels.fit)) * rmseaConfidenceIntervalHelper(free_chi, free_df, N, .025, .975)
  free_CFI <- 1 - max((free_chi-free_df),0) / max(free_chi-free_df, indep_chi-indep_df,0)
  free_TLI <- (indep_chi/indep_df - free_chi/free_df) / (indep_chi/indep_df-1)
  free_AIC <- free_chi + 2* (Nobs-free_df)
  free_BIC <- free_chi + log(N) * (Nobs-free_df)
  weight <- (subN-1) / (sum(subN)-length(submodels.fit))
  free_SRMR <- sqrt(sum(weight * sub_free_SRMR^2))

  
  
  # constrained model
  
  if(is.null(constrained.fit)==FALSE){
    
    # SRMR and number of fixed variances per submodel
    sub_constr_SRMR <- c()
    fixedexo <- c()
    for(i in 1:length(constrained.fit$submodels)){
      sub_constr_SRMR[i] <- sqrt(mean(constrained.fit$submodels[[i]]$vecS$result^2))
      fixedexo[i] <- sum(diag(constrained.fit$submodels[[i]]$Smatrix$free)[1:nvar]==FALSE)
    }
    
    # implementation (S1 or Smatrix) differs for path model or factor model
    if(is.na(fixedexo[1])){
      for(i in 1:length(constrained.fit$submodels)){
        sub_constr_SRMR[i] <- sqrt(mean(constrained.fit$submodels[[i]]$vecS$result^2))
        fixedexo[i] <- sum(diag(constrained.fit$submodels[[i]]$S1$free)[1:nvar]==FALSE)
      }
    }
    
    sum_constr <- suppressWarnings(summary(constrained.fit))
    constr_chi <- sum_constr$Minus2LogLikelihood
    constr_df <- Nobs - sum_constr$estimatedParameters - sum(fixedexo)
                  
                  constr_RMSEA <- sqrt(length(submodels.fit)) * RMSEA(constr_chi, constr_df, N)
                  constr_RMSEA_CI <- sqrt(length(submodels.fit)) * rmseaConfidenceIntervalHelper(constr_chi, constr_df, N, .025, .975)
                  constr_CFI <- 1 - max((constr_chi-constr_df),0) / max(constr_chi-constr_df, indep_chi-indep_df,0)
                  constr_TLI <- (indep_chi/indep_df - constr_chi/constr_df) / (indep_chi/indep_df-1)
                  constr_AIC <- constr_chi + 2* (Nobs-constr_df)
                  constr_BIC <- constr_chi + log(N) * (Nobs-constr_df)
    
    # overall SRMR
    constr_SRMR <- sqrt(sum(weight * sub_constr_SRMR^2))
    
    
    # dataframe with fitindices
    
    fit <- data.frame(Statistic = c("Sample size","df","Chi-square","p","RMSEA","RMSEA lower 95% CI","RMSEA upper 95% CI","CFI","TLI","AIC","BIC","SRMR"),
                      Free_m1 = round(c(N,free_df,free_chi,1-pchisq(free_chi,free_df),free_RMSEA,free_RMSEA_CI,free_CFI,free_TLI,free_AIC,free_BIC,free_SRMR),3),
                      Constrained_m2 = round(c(N,constr_df,constr_chi,1-pchisq(constr_chi,constr_df),constr_RMSEA,constr_RMSEA_CI,constr_CFI,constr_TLI,constr_AIC,constr_BIC,constr_SRMR),3),
                      Diff_m1_m2 = round(c(N,constr_df-free_df,constr_chi-free_chi,1-pchisq(constr_chi-free_chi,constr_df-free_df),NA,NA,NA,NA,NA,NA,NA,NA),3)
                      )
    

    ## parameter estimates, standard errors, CI's
    if(nrow(sum_constr$CI)>0){
      para <- cbind(sum_constr$parameters[,1:5],sum_constr$CI[,c(1,3)])
    }else{
      para <- sum_constr$parameters[,1:6]
    }

    
    if(print.est==FALSE){
      para <- paste("Set 'print.est=TRUE' to print the parameter estimates of the constrained model")
    }
    

    printoutc <- function(para,fit){
      cat(rep("#",19))
      cat("\n Output for subgroup MASEM analysis \n")
      cat(rep("#",19))
      cat("\n\n Total sample size:",fit[1,2])
      cat("\n\n Parameter estimates of the constrained model\n\n")
      print(para)
      cat("\n",rep("-",35))
      cat("\n Fit indices of the free model:\n\n")
      print(fit[-1,1:2],row.names=FALSE)
      cat(rep("-",35))
      cat("\n Fit indices of the model with equality constraints:\n\n")
      print(fit[-1,c(1,3)],row.names=FALSE)
      cat(rep("-",35))
      cat("\n Chi-square difference between free and constrained model:\n\n")
      print(fit[2:4,c(1,4)],row.names=FALSE)
      cat("\n",rep("#",35))
    }
    
    printoutc(para,fit)
  }
  
  
  if(is.null(constrained.fit)==TRUE){
    
    fit <- data.frame(Statistic = c("Sample size","df","Chi-square","p","RMSEA","RMSEA lower 95% CI","RMSEA upper 95% CI","CFI","TLI","AIC","BIC","SRMR"),
                         Free_m1 = round(c(N,free_df,free_chi,1-pchisq(free_chi,free_df),free_RMSEA,free_RMSEA_CI,free_CFI,free_TLI,free_AIC,free_BIC,free_SRMR),3)
                         )
    
    printoutf <- function(fit){
      cat(rep("#",19))
      cat("\n Output for subgroup MASEM analysis \n")
      cat(rep("#",19))
      cat("\n\n Total sample size:",fit[1,2])
      cat("\n",rep("-",35))
      cat("\n Fit indices of the free model:\n\n")
      print(fit[-1,1:2],row.names=FALSE)
      cat(rep("-",35))
      cat("\n\n No constrained model provided\n")
      cat("\n",rep("#",35))
    }
    
    printoutf(fit)
    
    
  }

}

#############################################################################################################################################################





#### a function to test the equality of tau and mean correlations across subgroups
#### RE.type = "Symm" tau is symmetrical
#### when output.equaltau = TRUE, it also outputs the subgroup's pooled correlations and acov-matrices for stage 2 analysis 

tssem.equal.var.mean.test <- function(data.g1, n.g1, data.g2, n.g2, RE.type="Diag", output.equaltau=FALSE){
  
  # function to calculate asymptotic covariance matrix
  compacov <- function(r,n){
    
    varnames <- rownames(r)		    
    rlabels <- outer(1:nrow(r), 1:ncol(r), function(x, y) paste("v",x,"v", y, sep = ""))
    
    if (!is.null(varnames)) { 
      rlabels <- outer(1:nrow(r), 1:ncol(r), function(x, y) paste(varnames[x],varnames[y], sep = "_")) 
    }
    
    vec <- vechs(rlabels)    
    V <- matrix(0,nrow(r)*(nrow(r)-1)/2,nrow(r)*(nrow(r)-1)/2)
    dimnames(V) <- list(vec,vec)    
    
    for (i in 1:nrow(V)){
      for (j in 1:ncol(V)){
        
        row = rownames(V)[i]
        col = rownames(V)[j]
        
        s <- max(apply(rlabels,2,function(x) match(row,x)),na.rm=TRUE)
        t <- max(apply(rlabels,1,function(x) match(row,x)),na.rm=TRUE)
        u <- max(apply(rlabels,2,function(x) match(col,x)),na.rm=TRUE)
        v <- max(apply(rlabels,1,function(x) match(col,x)),na.rm=TRUE)
        
        # is one of cross correlations is missing -> fix at 0
        
        r_su <- ifelse(is.na(r[s,u]),0,r[s,u])
        r_sv <- ifelse(is.na(r[s,v]),0,r[s,v])
        r_tu <- ifelse(is.na(r[t,u]),0,r[t,u])
        r_tv <- ifelse(is.na(r[t,v]),0,r[t,v])
        r_st <- r[s,t]
        r_uv <- r[u,v]
        
        V[i,j] <- 
          (0.5*r_st*r_uv*(r_su^2+r_sv^2+r_tu^2+r_tv^2)
           + r_su*r_tv+r_sv*r_tu-(r_st*r_su*r_sv
                                  + r_st*r_tu*r_tv+r_su*r_tu*r_uv
                                  + r_sv*r_tv*r_uv))/n
      }}
    
    return(V)
  }
  
  # data
  nvar <- ncol(data.g1[[1]])
  ncor <- nvar*(nvar-1)/2
  
  nstudy.g1 <- length(data.g1)
  nstudy.g2 <- length(data.g2)
  
  corlabels.g1 <- paste("g1_r",1:ncor,sep="")
  corlabels.g2 <- paste("g2_r",1:ncor,sep="")
  
  r.g1 <- t(sapply(data.g1,vechs))
  r.g2 <- t(sapply(data.g2,vechs))
  
  colnames(r.g1) <- corlabels.g1
  colnames(r.g2) <- corlabels.g2
  
  
  # Vi's 
  V.g1 <- matrix(0,nstudy.g1,ncor*(ncor+1)/2)
  for (i in 1:nstudy.g1){V.g1[i,] <- vech(compacov(data.g1[[i]],n.g1[i]))}
  
  V.g2 <- matrix(0,nstudy.g2,ncor*(ncor+1)/2)
  for (i in 1:nstudy.g2){V.g2[i,] <- vech(compacov(data.g2[[i]],n.g2[i]))}
  
  Vlabelmatrix <- outer(1:ncor, 1:ncor, function(x, y) paste("r",x, "_", "r", y, sep = ""))
  
  Vlabels.g1 <- paste("g1_",vech(Vlabelmatrix),sep="")
  Vlabels.g2 <- paste("g2_",vech(Vlabelmatrix),sep="")
  
  colnames(V.g1) <- Vlabels.g1
  colnames(V.g2) <- Vlabels.g2
  
  V.g1[is.na(V.g1)] <- 10e10
  V.g2[is.na(V.g2)] <- 10e10
  
  
  # labels for definition var
  def.labels.g1 <- paste("data.",Vlabels.g1,sep="")
  def.labels.g2 <- paste("data.",Vlabels.g2,sep="")
  
  # selection matrices 
  X.g1 <- list()
  for (i in 1:nstudy.g1){
    X.g1[[i]] <- diag(1,ncor,ncor)
    X.g1[[i]] <- X.g1[[i]][is.na(r.g1[i,])==FALSE,]	
  }
  
  X.g2 <- list()
  for (i in 1:nstudy.g2){
    X.g2[[i]] <- diag(1,ncor,ncor)
    X.g2[[i]] <- X.g2[[i]][is.na(r.g2[i,])==FALSE,]  
  }
  
  
  # OpenMx
  # common matrices within g1 and g2
  
  taulabels.g1 <- outer(1:ncor, 1:ncor, function(x, y) paste("g1_tau_r",x, "_", "r", y, sep = ""))
  taulabels.g2 <- outer(1:ncor, 1:ncor, function(x, y) paste("g2_tau_r",x, "_", "r", y, sep = ""))
  
  if(RE.type == "Diag"){
    tau.g1       <- mxMatrix(type="Diag", 
                             free=TRUE, 
                             values=.1, 
                             lbound=0,
                             labels = diag(taulabels.g1),
                             nrow=ncor, 
                             ncol=ncor,
                             name="tau")
    
    
    tau.g2       <- mxMatrix(type="Diag", 
                             free=TRUE, 
                             values=.1, 
                             lbound=0,
                             labels = diag(taulabels.g2),
                             nrow=ncor, 
                             ncol=ncor,
                             name="tau")
  }
  
  if(RE.type == "Symm"){
    taustart <- matrix(.01,ncor,ncor); diag(taustart) <- .1
    tau.g1       <- mxMatrix(type="Symm", 
                             free=TRUE, 
                             values=taustart, 
                             labels = taulabels.g1[upper.tri(taulabels.g1,diag=TRUE)],
                             nrow=ncor, 
                             ncol=ncor,
                             name="tau")
    
    
    tau.g2       <- mxMatrix(type="Symm", 
                             free=TRUE, 
                             values=taustart,
                             labels = taulabels.g2[upper.tri(taulabels.g2,diag=TRUE)],
                             nrow=ncor, 
                             ncol=ncor,
                             name="tau")
  }
  
  Vi.g1      <- mxMatrix( type="Symm", nrow=ncor, ncol=ncor,
                          free=FALSE, labels=def.labels.g1, name="Vi")
  
  Vi.g2      <- mxMatrix( type="Symm", nrow=ncor, ncol=ncor,
                          free=FALSE, labels=def.labels.g2, name="Vi")
  
  mu.g1     <- mxMatrix( type="Full", nrow=1, ncol=ncor,labels = corlabels.g1,
                         free=TRUE, name="mu")
  
  mu.g2     <- mxMatrix( type="Full", nrow=1, ncol=ncor,labels = corlabels.g2,
                         free=TRUE, name="mu")
  
  
  sigma      <- mxAlgebra(expression=X%*%tau%*%t(X) + X%*%Vi%*%t(X), name="sigma")
  means         <- mxAlgebra(expression=mu%*%t(X), name="means")
  
  # study specific g1
  dataRaw.g1 <- list() # data
  matrixX.g1 <- list() # selection matrix for mu
  exp.g1 <- list()
  REmodel.g1 <- list()
  
  for (i in 1:nstudy.g1){
    
    ## model for each study
    
    dataRaw.g1[[i]]    <- mxData(observed=data.frame(r.g1,V.g1)[i,], type="raw")
    
    matrixX.g1[[i]] <- mxMatrix(
      type = 'Full',
      nrow = sum(X.g1[[i]]),
      ncol = ncor,
      free = FALSE,
      values = X.g1[[i]],
      byrow = TRUE,
      name = 'X')
    
    
    exp.g1[[i]]  <- mxExpectationNormal(covariance="sigma", means="means",dimnames = corlabels.g1[is.na(r.g1[i,])==FALSE])
    funML     <- mxFitFunctionML()
    
    REmodel.g1[[i]] <- mxModel(paste("g1_sample",i,sep=""),dataRaw.g1[[i]],matrixX.g1[[i]],exp.g1[[i]],mu.g1,tau.g1,Vi.g1,means,sigma,funML)  
  }
  
  
  # study specific g2
  dataRaw.g2 <- list() # data
  matrixX.g2 <- list() # selection matrix for mu
  exp.g2 <- list()
  REmodel.g2 <- list()
  
  for (i in 1:nstudy.g2){
    
    ## model for each study
    
    dataRaw.g2[[i]]    <- mxData(observed=data.frame(r.g2,V.g2)[i,], type="raw")
    
    matrixX.g2[[i]] <- mxMatrix(
      type = 'Full',
      nrow = sum(X.g2[[i]]),
      ncol = ncor,
      free = FALSE,
      values = X.g2[[i]],
      byrow = TRUE,
      name = 'X')
    
    exp.g2[[i]]  <- mxExpectationNormal(covariance="sigma", means="means",dimnames = corlabels.g2[is.na(r.g2[i,])==FALSE])
    funML     <- mxFitFunctionML()
    
    REmodel.g2[[i]] <- mxModel(paste("g2_sample",i,sep=""),dataRaw.g2[[i]],matrixX.g2[[i]],exp.g2[[i]],mu.g2,tau.g2,Vi.g2,means,sigma,funML)
    
  }
  
  
  #### run g1 and g2 together without equality constraints
  
  ffg1 <- paste(paste(rep("g1_sample",nstudy.g1),1:nstudy.g1,sep=""),"fitfunction",sep=".")
  ffg2 <- paste(paste(rep("g2_sample",nstudy.g2),1:nstudy.g2,sep=""),"fitfunction",sep=".")
  
  multifit <- mxFitFunctionMultigroup(c(ffg1,ffg2))
  
  MASEM_free <-  mxModel('No across group constraints',REmodel.g1[1:nstudy.g1],REmodel.g2[1:nstudy.g2],multifit)
  
  MASEM_out_free <- mxRun(MASEM_free)
  
  sum_free <- summary(MASEM_out_free)
  
  ##### Constraint mu.g1 = mu.g2
  
  # create new list with models for g2, with mulabels.g2 = mulabels.g1.g1
  
  for (i in 1:nstudy.g2){
    REmodel.g2[[i]]$mu$labels <- corlabels.g1
  }
  
  MASEM_mu <-  mxModel('Equal mu',REmodel.g1[1:nstudy.g1],REmodel.g2[1:nstudy.g2],multifit)
  
  MASEM_out_mu <- mxRun(MASEM_mu)
  
  sum_mu <- summary(MASEM_out_mu)
  
  ##### Constraint tau.g1 = tau.g2
  
  # create new list with models for g2, with mu free again but taulabels.g2 = taulabels.g1
  
  for (i in 1:nstudy.g2){
    REmodel.g2[[i]]$mu$labels <- corlabels.g2
    REmodel.g2[[i]]$tau$labels <- REmodel.g1[[1]]$tau$labels
  }
  
  MASEM_tau <-  mxModel('Equal tau',REmodel.g1[1:nstudy.g1],REmodel.g2[1:nstudy.g2],multifit)
  
  MASEM_out_tau <- mxRun(MASEM_tau)
  
  sum_tau <- summary(MASEM_out_tau)
  
  
  ##### tau.g1 = tau.g2 and mu.g1 = mu.g2
  
  # create new list with models for g2, with taulabels.g2 = taulabels.g1 and corlabels.g2 = corlabels.g1
  
  for (i in 1:nstudy.g2){
    REmodel.g2[[i]]$mu$labels <- corlabels.g1
  }
  
  MASEM_tau_mu <-  mxModel('Equal tau and mu',REmodel.g1[1:nstudy.g1],REmodel.g2[1:nstudy.g2],multifit)
  
  MASEM_out_tau_mu <- mxRun(MASEM_tau_mu)
  
  sum_tau_mu <- summary(MASEM_out_tau_mu)
  
  # LRTs
  
  ll_free <- sum_free$Minus2LogLikelihood
  ll_mu <- sum_mu$Minus2LogLikelihood
  ll_tau <- sum_tau$Minus2LogLikelihood
  ll_taumu <- sum_tau_mu$Minus2LogLikelihood
  
  chi_mu <- ll_mu - ll_free
  chi_tau <- ll_tau - ll_free
  chi_tau_mu <- ll_taumu - ll_tau
  
  
  # output
  
  testresults <- data.frame(model = c("No constraints","R equal","T2 equal","T2 and R equal"),
                            minus2loglik = c(ll_free,ll_mu,ll_tau,ll_taumu),
                            difference_with = c(NA,"No constraints","No constraints","T2 equal"),
                            chi2_diff = c(NA,chi_mu,chi_tau,chi_tau_mu),
                            df_diff = c(NA,ncor,ncor,ncor),
                            p_diff = c(NA,1-pchisq(chi_mu,ncor),1-pchisq(chi_tau,ncor),1-pchisq(chi_tau_mu,ncor))        
  )
  
  if(output.equaltau==TRUE){
    
    R.g1 <- vec2symMat(MASEM_out_tau$g1_sample1$mu$values,diag=FALSE) 
    R.g2 <- vec2symMat(MASEM_out_tau$g2_sample1$mu$values,diag=FALSE)
    
    varnames <- colnames(data.g1[[1]])
    dimnames(R.g1)<-list(varnames,varnames)
    dimnames(R.g2)<-list(varnames,varnames)
    
    acovR.g1 = 2 * solve(MASEM_out_tau$output$calculatedHessian)[corlabels.g1,corlabels.g1]
    acovR.g2 = 2 * solve(MASEM_out_tau$output$calculatedHessian)[corlabels.g2,corlabels.g2]
    
    N.g1 <- sum(n.g1)
    N.g2 <- sum(n.g2)
    
    equaltau.results <- list(testresults = testresults,
                             pooledR.g1 = R.g1,
                             acov.g1 = acovR.g1,
                             totalN.g1 = N.g1,
                             pooledR.g2 = R.g2,
                             acov.g2 = acovR.g2,
                             totalN.g2 = N.g2)  
    
  }
  
  
  else{return(testresults)
  }
  
}



