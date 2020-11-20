## Faster versions in metaSEM for simulation studies
## It only uses acov="weighted" with limited data checking
## Output dimensions are different when the inputs are 2x2 matrices
asyCov <- function(x, n, ...) {
  ## Use weighted correlation matrix to estimate acov
  if (is.list(x)) {
    ## Replace NA with 0 before calculations
    my.x <- lapply(x, function(x) {x[is.na(x)] <- 0; x})
    ## Calculate a weighted sum of r and n
    my.x <- mapply("*", my.x, n, SIMPLIFY = FALSE)
    ## Weighted means = Cummulative sum of r*n/sum of n
    x <- Reduce("+", my.x)/pattern.n(x, n)
  } 
  ## No. of variables
  p <- ncol(x)
  ## No. of correlations
  ps <- p*(p-1)/2
  
    ## n*acov
    n_acov <- function(R, s, t, u, v) {
      0.5*R[s,t]*R[u,v]*(R[s,u]^2 + R[s,v]^2 + R[t,u]^2 + R[t,v]^2) + 
        R[s,u]*R[t,v] + R[s,v]*R[t,u] - 
        (R[s,t]*R[s,u]*R[s,v] + R[t,s]*R[t,u]*R[t,v] + R[u,s]*R[u,t]*R[u,v] + 
           R[v,s]*R[v,t]*R[v,u])
    }
    
    ## index of variables  [1:p, 1:p] for correlation 1:p_s
    index <- matrix(NA, ncol=2, nrow=ps)
    k <- 1
    for (j in 1:(p-1))
      for (i in (j+1):p) {
        #cat(i, j, "\n")
        index[k, ] <- c(i, j)
        k <- k+1    
      }
    
    out <- matrix(NA, ncol=ps, nrow=ps)
    for (j in 1:ps) 
      for (i in j:ps) {
        out[i, j] <- n_acov(R=x, s=index[i,1], t=index[i,2], 
                          u=index[j,1], v=index[j,2])
    }
    
    out[upper.tri(out)] <- out[lower.tri(out)]

    if (is.vector(n)) {
      ## If n is a vector (more than 2 studies),
      ## output the acov divided by different n
      out <- lapply(n, function(z) out/z)
    } else {
      out <- out/(n)
    }  
    
    ## Convert the list into a matrix
  out <- t(sapply(out, function(x) {(vech(x))}))
  
  if (is.null(dimnames(x))) {
    Names <- paste0("x", 1:nrow(x))
  } else {
    Names <- dimnames(x)[[1]]
  }
  # create matrix of labels for ps
  Names <- vechs(outer(Names, Names, paste, sep = "_"))
  acovNames <- vech(outer(Names, Names, function(x, y) {paste0("C(", x, " ", y, ")")}))
  
  colnames(out) <- acovNames
  out
}


tssem1REM <- function(Cov, n, cor.analysis=TRUE, RE.type=c("Diag", "Symm", "Zero", "User"),
                      RE.startvalues=0.1, RE.lbound = 1e-10, RE.constraints=NULL,
                      I2="I2q", acov=c("weighted", "individual", "unweighted"),
                      model.name=NULL, suppressWarnings=TRUE, silent=TRUE, run=TRUE, ...) {
  ## It handles missing effect sizes rather than missing correlations. Thus, it is more flexible than tssem1FEM().
  ## ACOV is calculated without missing data by assuming 1 and 0 for the missing variances and covariances.
  ## Missing values are indicated by the missing effect sizes.
  
  acov <- match.arg(acov, c("weighted", "individual", "unweighted"))
  
  ## Throw an error when df and n are in different lengths.
  if (length(Cov) != length(n)) stop("Lengths of \"df\" and \"n\" are different.\n")   
  
  ## Get the original variable names
  original.names <- colnames(Cov[[1]])
  
  if (cor.analysis) {
    ## Replace diagonals with 1.0
    my.complete <- lapply(Cov, function (x) { Diag(x)[is.na(Diag(x))] <- 1; x })
  } else {
    ## Replace diagonals with the mean of diagonals
    my.complete <- lapply(Cov, function (x) { Diag(x)[is.na(Diag(x))] <- mean(Diag(x), na.rm=TRUE); x })
  }
  ## Replace missing variables with 0.0 regardless of cor.analysis
  my.complete <- lapply(my.complete, function (x) { x[is.na(x)] <- 0; x })
  
  ## Calculate the asymptotic sampling covariance matrix of the correlation matrix
  acovR <- asyCov(x=my.complete, n=n)
  
  ## Fixed a bug that Cov is covariance matrix while cor.analysis is TRUE
  ## When cor.analysis=TRUE, the old version just takes the lower triangle without converting covariance into correlation.
  if (cor.analysis) {
    ## Convert possible covariance matrices into correlation matrices
    ## When there are NA in diagonas, they become 1 after cov2cor()
    ## It is fine as the diagonals are not used in cor.analysis=TRUE
    ES <- list2matrix(x=suppressWarnings(lapply(Cov, cov2cor)), diag=FALSE)
  } else {
    ES <- list2matrix(x=Cov, diag=TRUE)
  }
  ## no. of effect sizes
  no.es <- ncol(ES)
  
  if (is.null(model.name)) {
    if (cor.analysis) {
      model.name <- "TSSEM1 Correlation"
    } else {
      model.name <- "TSSEM1 Covariance"
    }
  }
  
  RE.type <- match.arg(RE.type, c("Diag", "Symm", "Zero", "User"))
  switch( RE.type,
          Symm = mx.fit <- meta(y=ES, v=acovR, model.name=model.name, I2=I2, RE.startvalues=RE.startvalues,
                                RE.lbound=RE.lbound, suppressWarnings=TRUE, silent=silent, run=run, ...),
          Diag = mx.fit <- meta(y=ES, v=acovR, model.name=model.name, I2=I2,
                                RE.constraints=Diag(paste0(RE.startvalues, "*Tau2_", 1:no.es, "_", 1:no.es)),
                                RE.lbound=RE.lbound, suppressWarnings=TRUE, silent=silent, run=run, ...),
          Zero = mx.fit <- meta(y=ES, v=acovR, model.name=model.name, I2=I2, RE.constraints=matrix(0, ncol=no.es, nrow=no.es),
                                suppressWarnings=TRUE, silent=silent, run=run, ...),
          User = mx.fit <- meta(y=ES, v=acovR, model.name=model.name, I2=I2, RE.constraints=RE.constraints,
                                suppressWarnings=TRUE, silent=silent, run=run, ...) )
  
  ## Return mx model without running the analysis
  if (run==FALSE) return(mx.fit)

  out <- list(total.n=sum(n), cor.analysis=cor.analysis, RE.type=RE.type, no.es=no.es, original.names=original.names)
  out <- c(out, mx.fit)
  class(out) <- c("tssem1REM", "meta")
  return(out)
}

Cor2DataFrame <- function(x, n, v.na.replace=TRUE, row.names.unique=FALSE,
                          cor.analysis=TRUE, acov="weighted", ...) {
  if (length(x) != length(n)) stop("Lengths of 'x' and 'n' are different.\n")
  
  if (cor.analysis) {
    my.df <- list2matrix(x=suppressWarnings(lapply(x, cov2cor)), diag=FALSE)
  } else {
    my.df <- list2matrix(x=x, diag=TRUE)
  }
  
  acovR <- asyCov(x=x, n=n, cor.analysis=cor.analysis, acov=acov, ...)
  
  ## NA is not allowed in definition variables
  ## They are replaced by 1e10
  if (v.na.replace) acovR[is.na(acovR)] <- 1e10
  
  data <- suppressWarnings(data.frame(my.df, acovR, check.names=FALSE))
  
  ## Use unique row names if the row names are duplicated.
  if (row.names.unique) rownames(data) <- make.names(names(x), unique=TRUE)    
  
  list(data=data, n=n, ylabels=dimnames(my.df)[[2]], vlabels=dimnames(acovR)[[2]])
}


## Generate correlation matrices with missing variables
## It assumes equal sample sizes
genData <- function(k, n, missing_studies, missing_variables, missing_type=c("MCAR", "NMAR"), 
                    Sigma, Sigma_V) {
  my.Cor <- rCor(Sigma=Sigma, V=Sigma_V, n=rep(n, k), raw.data=TRUE)$R
  
  ## No. of studies with missing variables
  missing.studies <- round(k*missing_studies)
  ## Index of studies with missing variables. Studies on the top are missing.
  missing.studies.index <- rep(c(TRUE, FALSE), times=c(missing.studies, k-missing.studies))
  
  ## No. of variables are missing: Either 2 or 3 
  missing.variables <- round(5*missing_variables)
  ## Index of variables are missing
  missing.variables.index <- rep(c(TRUE, FALSE), times=c(missing.variables, 5-missing.variables))  
  
  ## Introduce missing variables with MCAR
  ## x: my.Cor
  ## study.index: missing.studies.index
  fn.mcar <- function(x, study.index) {
    ## If TRUE, no missing variables introduced
    if (!study.index) {
      x
    } else {
    ## If FALSE, introduce missing variables with MCAR
    ## Randomly sample missing.variables.index
      index <- sample(missing.variables.index)
      x[index, ] <- x[, index] <- NA
      diag(x) <- 1
      x
    }
  }
  
  ## Introduce missing variables with MNAR
  fn.nmar <- function(x, study.index, missing.these.index) {
    ## If TRUE, no missing variables introduced
    if (!study.index) {
      x
    } else {
    ## If FALSE, introduce missing variables with NMAR
    ## Missing variables are X2, (X4) and X5
      x[missing.these.index, ] <- x[, missing.these.index] <- NA
      diag(x) <- 1
      x
    }
  }  
  
  missing_type <- match.arg(missing_type)
  
  if (missing_type=="MCAR") {
    out <- mapply(fn.mcar, my.Cor, missing.studies.index, SIMPLIFY = FALSE)
  } else {
    ## Determine which variables are missing
    if (missing_variables==.4) {
      ## missing data are based on X2 and X5
      missing.these.index <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
    } else if (missing_variables==.6) {
      ## missing data are based on X2, X4, and X5
      missing.these.index <- c(FALSE, TRUE, FALSE, TRUE, TRUE)
    } else {
      stop("Pattern of missing variables is not defined in 'missing_type'.\n")
    }
    
    ## average correlation + a constant based on X2, (X4) and X5.
    ## The constant does not affect the rank order.
    average.cor <- sapply(my.Cor, function(x) sum(x[missing.these.index, ]))
    
    ## Reorder the correlation matrices so that studies with smallest average.cor
    ## are on the top. These studies are with missing variables.
    my.Cor <- my.Cor[order(average.cor)]
    out <- mapply(fn.nmar, my.Cor, missing.studies.index, 
                  MoreArgs=list(missing.these.index=missing.these.index), SIMPLIFY = FALSE)
  }
  out
}

## Output NA when not convergent
output.NA <- function(x) {
  Estimate <- rep(NA, length(x))
  names(Estimate) <- x
  list(Estimate=Estimate, Std.Error=Estimate, Chisq=NA, df=NA, p=NA)
}

## Get stage 2 results from either wls() and tssem2()
stage2.mx <- function(x, parameters) {
  ## Try to rerun it if it is not convergent
  if (!(x$mx.fit$output$status$code %in% c(0,1))) {
    x <- suppressMessages(rerun(x, extraTries=10)) 
  } 
  
  ## Return NA if it is still not convergent
  if (!(x$mx.fit$output$status$code %in% c(0,1))) {
    out <- output.NA(parameters)
  } else {
    stage2.fit <- summary(x)

    para <- stage2.fit$coefficients

    ## Select the parameters for output
    para <- para[parameters, ]

    Estimate <- para$Estimate
    names(Estimate) <- parameters

    Std.Error <- para$Std.Error
    names(Std.Error) <- parameters

    Chisq <- stage2.fit$stat["Chi-square of target model", ]
    df <- stage2.fit$stat["DF of target model", ]
    p <- stage2.fit$stat["p value of target model", ]
    out <- list(Estimate=Estimate, Std.Error=Std.Error, Chisq=Chisq, df=df, p=p)
  }
  out
}  

uniR <- function(Cor, n, RAM, parameters) {
  stage1 <- uniR1(Cor=Cor, n=n)
  stage2 <- uniR2mx(stage1, RAM=RAM)
  #stage2 <- uniR2mx(stage1, Amatrix=RAM$A, Smatrix=RAM$S, Fmatrix=RAM$F)

  if (stage2$output$status$code %in% c(0,1)) {
    stage2 <- suppressMessages(mxTryHard(stage2, extraTries=10))
  }
  
  ## Output NA if the status code is not either 0 or 1
  if (stage2$output$status$code %in% c(0,1)) {
    stage2.fit <- summary(stage2)
    
    para <- stage2.fit$parameters
    rownames(para) <- para$name
    
    ## Select the parameters for output
    para <- para[parameters, ]
    
    Estimate <- para$Estimate
    names(Estimate) <- parameters
    
    Std.Error <- para$Std.Error
    names(Std.Error) <- parameters
    
    out <- list(Estimate=Estimate, Std.Error=Std.Error, Chisq=stage2.fit$Chi, 
                df=stage2.fit$ChiDoF, p=stage2.fit$p)
  } else {
    out <- output.NA(parameters) 
  }
  out
}

gls <- function(Cor, n, RAM, parameters) {
  #### Stage 1 analysis with GLS
  ## Prepare y vector with NA
  ## Effect sizes are grouped by studies
  y.list <- lapply(Cor, vechs)
  y <- unlist(y.list)
  
  ## Prepare X design matrix with NA
  X <- matrix(1, ncol=1, nrow=length(n)) %x% diag(length(y.list[[1]]))
  
  ## Get tau2
  data.y <- t(sapply(Cor, vechs))
  
  ## Split the data by columns (effect sizes)
  ## Note. y.list here is different from the previous y.list
  ## It is grouped by effect sizes so that multiple univariate MA can be conducted.
  y.list <- asplit(data.y, 2)
  
  ## Sampling variances grouped by effect sizes
  v.list <- lapply(y.list, function(r) (1-r^2)^2/n)
  
  ## Helper function to get the tau2 from metafor
  rma.tmp <- function(y, v) {
    metafor::rma(yi=c(y), vi=c(v))$tau2
  }
  
  ## tau2 of effect sizes
  tau2 <- suppressWarnings(mapply(rma.tmp, y.list, v.list))
  ## T2 is a diagonal matrix
  T2 <- bdiagRep(diag(tau2), length(n))  
  
  ## Prepare V=Vi+T2
  ## Vi: Known sampling variance-covariance matrices
  ## Vi <- bdiagMat(metaSEM::asyCov(Cor, n, acov="weighted", as.matrix=FALSE))
  Cor.list <- split(asyCov(Cor, n), 1:length(n))
  Vi <- bdiagMat(lapply(Cor.list, vec2symMat))
  
  ## Inverse of the weight matrix
  V <- Vi + T2
  
  ## Remove NA
  index.na <- is.na(y)
  Y <- matrix(y[!index.na], ncol=1)
  X <- X[!index.na, ]
  V <- V[!index.na, !index.na]
  
  ## Calculate beta and its vcov
  XVX_1 <- solve(t(X) %*% solve(V) %*% X)
  r <- XVX_1 %*% t(X) %*% solve(V) %*% Y
  r.vcov <- XVX_1

  R <- vec2symMat(r, diag=FALSE, byrow=FALSE)
  
  ## Run stage 2 only when R is PD
  if (is.pd(R)&is.pd(r.vcov)) {
    #### Stage 2 analysis with WLS
    stage2 <- wls(Cov=R, aCov=r.vcov, n=sum(n), RAM=RAM)
    out <- stage2.mx(x=stage2, parameters=parameters)
  } else {
    out <- output.NA(parameters)
  }
  out
}  

tssem <- function(Cor, n, RAM, parameters) {
  stage1 <- tryCatch(tssem1REM(Cov=Cor, n=n, acov="weighted"),
                     error = function(e) e)
  i <- 0                    
  while(inherits(stage1, "error") & (i<3)) {
    stage1 <- tssem1REM(Cov=Cor, n=n, acov="weighted")
    i <- i+1
  }  
  
  ## Rerun stage 1 if it is not convergent
  i <- 0
  while(!(stage1$mx.fit$output$status$code %in% c(0,1)) & (i<5)) {
    stage1 <- suppressMessages(rerun(stage1, autofixtau2 = TRUE, extraTries=10))
    i <- i+1
  } 

  ## Rerun NA if it is still not convergent  
  if (!(stage1$mx.fit$output$status$code %in% c(0,1))) {
    out <- output.NA(parameters)
  } else {
    stage2 <- tssem2(stage1, RAM=RAM)
    out <- stage2.mx(x=stage2, parameters=parameters)
  }
  out
}

ossem <- function(Cor, n, RAM, parameters) {
  M0 <- create.vechsR(A0=RAM$A, S0=RAM$S, F0=RAM$F)
  T0 <- create.Tau2(RAM=RAM, RE.type="Diag")

  my.df <- tryCatch(Cor2DataFrame(Cor, n),
                     error = function(e) e)
  i <- 0                    
  while(inherits(my.df, "error") & (i<3)) {
    my.df <- Cor2DataFrame(Cor, n)
    i <- i+1
  } 

  ## Fit the model
  fit <- osmasem(Mmatrix=M0, Tmatrix=T0, data=my.df)
  
  i <- 0
  while(!(fit$mx.fit$output$status$code %in% c(0,1)) & (i<5)) {
    fit <- suppressMessages(rerun(fit, autofixtau2 = TRUE, extraTries=10))
    i <- i+1
  }
  
  if (!(fit$mx.fit$output$status$code %in% c(0,1))) {
    out <- output.NA(parameters)
  } else {
    fit.sum <- summary(fit, Saturated = TRUE)
    
    para <- fit.sum$parameters
    rownames(para) <- para$name
    
    ## Select the parameters for output
    para <- para[parameters, ]
    
    Estimate <- para$Estimate
    names(Estimate) <- parameters
    
    Std.Error <- para$Std.Error
    names(Std.Error) <- parameters
    
    out <- list(Estimate=Estimate, Std.Error=Std.Error, Chisq=fit.sum$Chi, 
                df=fit.sum$ChiDoF, p=fit.sum$p)
  }
  out
}  

run_it <- function(k, n, missing_studies, missing_variables, missing_type, Sigma, Sigma_V) {
  my.df <- genData(k=k, n=n, missing_studies=missing_studies, 
                   missing_variables=missing_variables, 
                   missing_type=missing_type, 
                   Sigma=Sigma, 
                   Sigma_V=Sigma_V)
  
  unir <- tryCatch(uniR(my.df, n=rep(n, k), RAM=RAM, parameters = parameters),
                   error = function(e) e)
  if (inherits(unir, "error")) unir <- output.NA(parameters) 
  
  gls <- tryCatch(gls(my.df, n=rep(n, k), RAM=RAM, parameters = parameters),
                   error = function(e) e)
  if (inherits(gls, "error")) gls <- output.NA(parameters)   
    
  tssem <- tryCatch(tssem(my.df, n=rep(n, k), RAM=RAM, parameters = parameters),
                    error = function(e) e)
  if (inherits(tssem, "error")) tssem <- output.NA(parameters) 
  
  osmasem <- tryCatch(ossem(my.df, n=rep(n, k), RAM=RAM, parameters = parameters),
                   error = function(e) e)
  if (inherits(osmasem, "error")) osmasem <- output.NA(parameters) 
  
  list(settings=c(k=k, n=n, missing_studies=missing_studies, 
                  missing_variables=missing_variables, missing_type=missing_type), 
                  unir=unir, gls=gls, tssem=tssem, osmasem=osmasem)
}

extractFit <- function(type=c("unir", "gls", "tssem", "osmasem"), x, 
                       pop=pop, adjDF=FALSE) {
  type <- match.arg(type)
  chisq <- eval(parse(text = paste0("t(sapply(x, function(x) c(", 
                                    type, "=x$", type,"[c('Chisq', 'df', 'p')])))")))
  ## Adjusted p value by adjusting the df
  ## Test the function of automatic constraints in rerun()
  if (adjDF) {
    chisq[,3] <- ifelse(chisq[,2]==3, yes=chisq[,3], 
                        no=pchisq(unlist(chisq[,1]), df=3, lower.tail=FALSE))
  }
  valid <- mean(!is.na(unlist(chisq[, 1])))
  ## chisq[, 1:3]: chisq, df, and p
  chisq.mean <- mean(unlist(chisq[, 1]), na.rm=TRUE)
  chisq.sd <- sd(unlist(chisq[, 1]), na.rm=TRUE)
  chisq.Type1 <- mean(ifelse(chisq[, 3]<=.05, yes=1, no=0), na.rm=TRUE)*100
  eval(parse(text=paste0("names(valid) <- '", type, ".valid'")))
  eval(parse(text=paste0("names(chisq.mean) <- '", type, ".chisq.mean'")))
  eval(parse(text=paste0("names(chisq.sd) <- '", type, ".chisq.sd'"))) 
  eval(parse(text=paste0("names(chisq.Type1) <- '", type, ".chisq.Type1'")))
  
  ## Parameter estimates
  est <- eval(parse(text = paste0("t(sapply(x, function(x) c(", 
                                  type, "=x$", type,"$Estimate)))"))) 
  se <- eval(parse(text = paste0("t(sapply(x, function(x) c(", 
                                 type, "=x$", type,"$Std.Error)))"))) 
  
  ## Validity checking: they should not be larger than 1 in abs
  est[abs(est)>1] <- NA
  se[abs(se)>1] <- NA
  
  est.mean <- apply(est, 2, mean, na.rm=TRUE)
  est.sd <- apply(est, 2, sd, na.rm=TRUE)
  est.se <- apply(se, 2, mean, na.rm=TRUE)
  
  bias.est <- (est.mean-pop)/pop*100
  names(bias.est) <- paste0(names(bias.est), ".est")
  bias.se <- (est.se-est.sd)/est.sd*100
  names(bias.se) <- paste0(names(bias.se), ".se")
  
  c(valid, chisq.mean, chisq.sd, chisq.Type1, bias.est, bias.se)
}


## Get indvidual stat in long format for ggplot2
get_individual_stat <- function(x,  type=c("est", "se"), 
                                combined.studies.variables=TRUE) {
  type=match.arg(type)
  method=c("unir", "gls", "tssem", "osmasem")
  
  ## Colnames in the dataframe
  col_names <- colnames(x)
  index <- grep(paste0(type,"$"), col_names)
  results <- x[, index]
  
  settings <- x[, c("k", "missing_studies", "missing_variables", "missing_type")]
  settings$k <- paste0("k=", settings$k)
  
  ## Creat a new condition (4 levels) by combining missing studies (2 levels) and missing variables (2 levels)
  if (combined.studies.variables) {
    settings$missing_studies_variables <- apply(settings, 1,
                                                function(x) paste0("Missing studies=", x[2], ", missing variables=",x[3]))
    settings <- settings[, c("k", "missing_studies_variables", "missing_type")]
  }
  
  my.df <- cbind(settings, results)
  
  if (combined.studies.variables) {
    out <- tidyr::gather(my.df, condition, y, -c(1:3), -c(1:3))
  } else {
    out <- tidyr::gather(my.df, condition, y, -c(1:4), -c(1:4))
  }
  out$Method <- sapply(strsplit(out$condition, split="\\."), function(x) x[[1]])
  out
}


