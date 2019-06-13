---
title: '(Appendix B) Example 2: Testing equality of direct effects in a path model (random-effects analysis)'
author: 'Suzanne Jak and Mike W.-L. Cheung'
date: 'June 13, 2019'
output:
  html_document:
    keep_md: yes
    self_contained: yes
    theme: united
    toc: yes
  pdf_document:
    toc: yes
  word_document: default
---

## Install the metaSEM package
* R can be downloaded at http://www.r-project.org/.
* We only need to install the metaSEM-package once.

```r
install.packages("metaSEM")
```

## Read in the data and load metaSEM package

```r
library(metaSEM)

## Try to use multiple cores in the analyses
mxOption(key='Number of Threads', value=parallel::detectCores()-2)

## Load the functions to facilitate analysis (link is anonymised for review)
source("subgroup.functions.R")

head(Roorda11$data)
```

```
## [[1]]
##          pos   neg enga achiev
## pos     1.00 -0.54   NA   0.18
## neg    -0.54  1.00   NA  -0.29
## enga      NA    NA   NA     NA
## achiev  0.18 -0.29   NA   1.00
## 
## [[2]]
##         pos neg enga achiev
## pos    1.00  NA 0.64   0.29
## neg      NA  NA   NA     NA
## enga   0.64  NA 1.00   0.23
## achiev 0.29  NA 0.23   1.00
## 
## [[3]]
##         pos neg enga achiev
## pos    1.00  NA 0.29     NA
## neg      NA  NA   NA     NA
## enga   0.29  NA 1.00     NA
## achiev   NA  NA   NA     NA
## 
## [[4]]
##         pos neg enga achiev
## pos    1.00  NA 0.29     NA
## neg      NA  NA   NA     NA
## enga   0.29  NA 1.00     NA
## achiev   NA  NA   NA     NA
## 
## [[5]]
##         pos   neg  enga achiev
## pos      NA    NA  0.22   0.08
## neg      NA  1.00 -0.45  -0.24
## enga   0.22 -0.45    NA     NA
## achiev 0.08 -0.24    NA   1.00
## 
## [[6]]
##          pos neg enga achiev
## pos     1.00  NA 0.06  -0.09
## neg       NA  NA   NA     NA
## enga    0.06  NA 1.00   0.20
## achiev -0.09  NA 0.20   1.00
```

```r
head(Roorda11$n)
```

```
## [1] 1310  427  123   66  179   93
```

```r
head(Roorda11$SES)
```

```
## [1] 70 78 83 30 27 39
```

## Stage 1 random-effects model on all data

```r
## Stage 1 analysis overall (random)
stage1random <- tssem1(Cov = Roorda11$data, n = Roorda11$n, method = "REM", RE.type = "Diag")
summary(stage1random)
```

```
## 
## Call:
## meta(y = ES, v = acovR, RE.constraints = Diag(paste0(RE.startvalues, 
##     "*Tau2_", 1:no.es, "_", 1:no.es)), RE.lbound = RE.lbound, 
##     I2 = I2, model.name = model.name, suppressWarnings = TRUE, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation (robust=FALSE)
## Coefficients:
##               Estimate   Std.Error      lbound      ubound z value
## Intercept1 -0.24107529  0.04115989 -0.32174718 -0.16040339 -5.8570
## Intercept2  0.31338740  0.03753845  0.23981338  0.38696141  8.3484
## Intercept3  0.13681828  0.02023234  0.09716363  0.17647294  6.7624
## Intercept4 -0.30422755  0.04541676 -0.39324275 -0.21521234 -6.6986
## Intercept5 -0.17997205  0.02540513 -0.22976519 -0.13017890 -7.0841
## Intercept6  0.27810412  0.03840753  0.20282676  0.35338149  7.2409
## Tau2_1_1    0.01875459  0.00861144  0.00187647  0.03563270  2.1779
## Tau2_2_2    0.02396817  0.00888233  0.00655912  0.04137721  2.6984
## Tau2_3_3    0.00611613  0.00273506  0.00075550  0.01147675  2.2362
## Tau2_4_4    0.01197419  0.00789113 -0.00349213  0.02744051  1.5174
## Tau2_5_5    0.00639193  0.00354052 -0.00054736  0.01333122  1.8054
## Tau2_6_6    0.01478755  0.00723847  0.00060041  0.02897469  2.0429
##             Pr(>|z|)    
## Intercept1 4.712e-09 ***
## Intercept2 < 2.2e-16 ***
## Intercept3 1.358e-11 ***
## Intercept4 2.105e-11 ***
## Intercept5 1.400e-12 ***
## Intercept6 4.459e-13 ***
## Tau2_1_1    0.029416 *  
## Tau2_2_2    0.006967 ** 
## Tau2_3_3    0.025339 *  
## Tau2_4_4    0.129159    
## Tau2_5_5    0.071018 .  
## Tau2_6_6    0.041061 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 604.4838
## Degrees of freedom of the Q statistic: 95
## P value of the Q statistic: 0
## 
## Heterogeneity indices (based on the estimated Tau2):
##                              Estimate
## Intercept1: I2 (Q statistic)   0.9130
## Intercept2: I2 (Q statistic)   0.9318
## Intercept3: I2 (Q statistic)   0.7745
## Intercept4: I2 (Q statistic)   0.8699
## Intercept5: I2 (Q statistic)   0.7833
## Intercept6: I2 (Q statistic)   0.8921
## 
## Number of studies (or clusters): 45
## Number of observed statistics: 101
## Number of estimated parameters: 12
## Degrees of freedom: 89
## -2 log likelihood: -125.0638 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

## Stage 2 random-effects model on all data
* Create a matrix with direct effects (A) and a matrix with variances and covariances (S)


```r
varnames <- c("pos","neg","enga","achiev")

A <- create.mxMatrix(c( 0,0,0,0,
                        0,0,0,0,
                        "0.1*b31","0.1*b32",0,0,
                        0,0,"0.1*b43",0),
                      type = "Full", nrow = 4, ncol = 4, byrow = TRUE,
                      name = "A", as.mxMatrix = FALSE)
dimnames(A) <- list(varnames, varnames)
A
```

```
##        pos       neg       enga      achiev
## pos    "0"       "0"       "0"       "0"   
## neg    "0"       "0"       "0"       "0"   
## enga   "0.1*b31" "0.1*b32" "0"       "0"   
## achiev "0"       "0"       "0.1*b43" "0"
```

```r
S <- create.mxMatrix(c(1,
                       ".5*p21",1,
                       0,0,"0.6*p33",
                       0,0,0,"0.6*p44"), 
                     type="Symm", byrow = TRUE,
                     name="S", as.mxMatrix = FALSE)
dimnames(S) <- list(varnames, varnames)
S
```

```
##        pos      neg      enga      achiev   
## pos    "1"      ".5*p21" "0"       "0"      
## neg    ".5*p21" "1"      "0"       "0"      
## enga   "0"      "0"      "0.6*p33" "0"      
## achiev "0"      "0"      "0"       "0.6*p44"
```

* Fitting the Stage 2 model on the pooled correlation matrix from the random effects Stage 1 analysis


```r
# Run the Stage 2 model
Stage2 <- tssem2(stage1random, Amatrix=A, Smatrix=S, diag.constraints=TRUE, intervals="LB", 
                 mx.algebras = list(Indirect_pos=mxAlgebra(b31*b43, name = "Indirect_pos"),
                                    Indirect_neg=mxAlgebra(b32*b43, name = "Indirect_neg")))

# Rerun it to remove the error code
Stage2 <- rerun(Stage2)
```

```
## Polite note from mxTryHard: Hessian not checked as model contains mxConstraints
```

```
## Running TSSEM2 Correlation with 6 parameters
```

```
## 
## Beginning initial fit attempt
```

```
## Running TSSEM2 Correlation with 6 parameters
```

```
## 
## Solution found
```

```
## Final run, for Hessian and/or standard errors and/or confidence intervals
```

```
## Running TSSEM2 Correlation with 6 parameters
```



```
## 
##  Solution found!  Final fit=10.769241 (started at 10.769241)  (1 attempt(s): 1 valid, 0 errors)
```

```r
summary(Stage2)
```

```
## 
## Call:
## wls(Cov = pooledS, aCov = aCov, n = tssem1.obj$total.n, Amatrix = Amatrix, 
##     Smatrix = Smatrix, Fmatrix = Fmatrix, diag.constraints = diag.constraints, 
##     cor.analysis = cor.analysis, intervals.type = intervals.type, 
##     mx.algebras = mx.algebras, model.name = model.name, suppressWarnings = suppressWarnings, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: Likelihood-based statistic
## Coefficients:
##     Estimate Std.Error   lbound   ubound z value Pr(>|z|)
## b43  0.34684        NA  0.28774  0.40730      NA       NA
## b32 -0.30047        NA -0.38373 -0.21632      NA       NA
## b31  0.26557        NA  0.19248  0.33739      NA       NA
## p44  0.87970        NA  0.83410  0.91720      NA       NA
## p33  0.80090        NA  0.73631  0.85462      NA       NA
## p21 -0.23990        NA -0.32057 -0.15923      NA       NA
## 
## mxAlgebras objects (and their 95% likelihood-based CIs):
##                        lbound    Estimate      ubound
## Indirect_pos[1,1]  0.06540604  0.09211069  0.12015958
## Indirect_neg[1,1] -0.13819051 -0.10421502 -0.07245218
## 
## Goodness-of-fit indices:
##                                                 Value
## Sample size                                29438.0000
## Chi-square of target model                    10.7692
## DF of target model                             2.0000
## p value of target model                        0.0046
## Number of constraints imposed on "Smatrix"     2.0000
## DF manually adjusted                           0.0000
## Chi-square of independence model             287.5058
## DF of independence model                       6.0000
## RMSEA                                          0.0122
## RMSEA lower 95% CI                             0.0058
## RMSEA upper 95% CI                             0.0198
## SRMR                                           0.0450
## TLI                                            0.9065
## CFI                                            0.9688
## AIC                                            6.7692
## BIC                                           -9.8108
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

# Subgroup analysis
* We split the studies based on the % SES


```r
# Data for studies with majority low SES 
data_low <- Roorda11$data[Roorda11$SES>50]
n_low <- Roorda11$n[Roorda11$SES>50]

# Data for studies with majority high SES
data_high <- Roorda11$data[Roorda11$SES<=50]
n_high <- Roorda11$n[Roorda11$SES<=50]
```

## Fitting a random-effects Stage 1 model in both subgroups


```r
# Stage 1 analysis per subgroup (random-effects analysis)
stage1_low.fit <- tssem1(Cov = data_low, n = n_low, method = "REM", RE.type = "Diag")
stage1_high.fit <- tssem1(Cov = data_high, n = n_high, method = "REM", RE.type = "Diag")

## Rerun it to remove the error code
stage1_high.fit <- rerun(stage1_high.fit)
```



```r
summary(stage1_low.fit)
```

```
## 
## Call:
## meta(y = ES, v = acovR, RE.constraints = Diag(paste0(RE.startvalues, 
##     "*Tau2_", 1:no.es, "_", 1:no.es)), RE.lbound = RE.lbound, 
##     I2 = I2, model.name = model.name, suppressWarnings = TRUE, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation (robust=FALSE)
## Coefficients:
##              Estimate  Std.Error     lbound     ubound z value  Pr(>|z|)
## Intercept1 -0.3255820  0.0486926 -0.4210178 -0.2301463 -6.6865 2.286e-11
## Intercept2  0.3454445  0.0446010  0.2580282  0.4328608  7.7452 9.548e-15
## Intercept3  0.1182343  0.0262574  0.0667708  0.1696979  4.5029 6.703e-06
## Intercept4 -0.3470853  0.0539934 -0.4529103 -0.2412602 -6.4283 1.290e-10
## Intercept5 -0.1820452  0.0346216 -0.2499023 -0.1141880 -5.2581 1.455e-07
## Intercept6  0.2259556  0.0478080  0.1322538  0.3196575  4.7263 2.286e-06
## Tau2_1_1    0.0115658  0.0073927 -0.0029235  0.0260552  1.5645   0.11770
## Tau2_2_2    0.0256783  0.0107413  0.0046256  0.0467309  2.3906   0.01682
## Tau2_3_3    0.0047712  0.0030903 -0.0012858  0.0108281  1.5439   0.12261
## Tau2_4_4    0.0096331  0.0092138 -0.0084257  0.0276919  1.0455   0.29579
## Tau2_5_5    0.0050064  0.0044194 -0.0036555  0.0136683  1.1328   0.25729
## Tau2_6_6    0.0121947  0.0080943 -0.0036698  0.0280592  1.5066   0.13192
##               
## Intercept1 ***
## Intercept2 ***
## Intercept3 ***
## Intercept4 ***
## Intercept5 ***
## Intercept6 ***
## Tau2_1_1      
## Tau2_2_2   *  
## Tau2_3_3      
## Tau2_4_4      
## Tau2_5_5      
## Tau2_6_6      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 384.3886
## Degrees of freedom of the Q statistic: 48
## P value of the Q statistic: 0
## 
## Heterogeneity indices (based on the estimated Tau2):
##                              Estimate
## Intercept1: I2 (Q statistic)   0.8469
## Intercept2: I2 (Q statistic)   0.9292
## Intercept3: I2 (Q statistic)   0.6930
## Intercept4: I2 (Q statistic)   0.8197
## Intercept5: I2 (Q statistic)   0.7031
## Intercept6: I2 (Q statistic)   0.8520
## 
## Number of studies (or clusters): 24
## Number of observed statistics: 54
## Number of estimated parameters: 12
## Degrees of freedom: 42
## -2 log likelihood: -73.28023 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

```r
summary(stage1_high.fit)
```

```
## 
## Call:
## meta(y = ES, v = acovR, RE.constraints = Diag(paste0(RE.startvalues, 
##     "*Tau2_", 1:no.es, "_", 1:no.es)), RE.lbound = RE.lbound, 
##     I2 = I2, model.name = model.name, suppressWarnings = TRUE, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation (robust=FALSE)
## Coefficients:
##               Estimate   Std.Error      lbound      ubound  z value
## Intercept1 -1.6590e-01  5.5074e-02 -2.7384e-01 -5.7957e-02  -3.0123
## Intercept2  2.2757e-01  5.6579e-02  1.1667e-01  3.3846e-01   4.0221
## Intercept3  1.5606e-01  3.0095e-02  9.7072e-02  2.1504e-01   5.1855
## Intercept4 -2.0042e-01  1.5801e-02 -2.3139e-01 -1.6945e-01 -12.6837
## Intercept5 -1.8111e-01  3.7838e-02 -2.5527e-01 -1.0695e-01  -4.7866
## Intercept6  3.3881e-01  5.1177e-02  2.3851e-01  4.3912e-01   6.6204
## Tau2_1_1    1.6380e-02  1.0984e-02 -5.1469e-03  3.7908e-02   1.4914
## Tau2_2_2    1.0305e-02  9.4006e-03 -8.1200e-03  2.8730e-02   1.0962
## Tau2_3_3    7.2232e-03  4.4293e-03 -1.4582e-03  1.5905e-02   1.6308
## Tau2_4_4    1.0000e-10          NA          NA          NA       NA
## Tau2_5_5    7.5556e-03  5.4904e-03 -3.2053e-03  1.8317e-02   1.3762
## Tau2_6_6    1.0879e-02  8.9628e-03 -6.6874e-03  2.8446e-02   1.2138
##             Pr(>|z|)    
## Intercept1  0.002593 ** 
## Intercept2 5.769e-05 ***
## Intercept3 2.155e-07 ***
## Intercept4 < 2.2e-16 ***
## Intercept5 1.696e-06 ***
## Intercept6 3.583e-11 ***
## Tau2_1_1    0.135866    
## Tau2_2_2    0.272999    
## Tau2_3_3    0.102943    
## Tau2_4_4          NA    
## Tau2_5_5    0.168774    
## Tau2_6_6    0.224810    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 145.7945
## Degrees of freedom of the Q statistic: 41
## P value of the Q statistic: 1.156852e-13
## 
## Heterogeneity indices (based on the estimated Tau2):
##                              Estimate
## Intercept1: I2 (Q statistic)   0.8916
## Intercept2: I2 (Q statistic)   0.8384
## Intercept3: I2 (Q statistic)   0.7852
## Intercept4: I2 (Q statistic)   0.0000
## Intercept5: I2 (Q statistic)   0.7949
## Intercept6: I2 (Q statistic)   0.8455
## 
## Number of studies (or clusters): 21
## Number of observed statistics: 47
## Number of estimated parameters: 12
## Degrees of freedom: 35
## -2 log likelihood: -65.30101 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

## Fitting the Stage 2 model in both subgroups


```r
# Stage 2 analysis per subgroup (random-effect analysis)

stage2_low.fit <- tssem2(stage1_low.fit, Amatrix=A, Smatrix=S)

stage2_high.fit <- tssem2(stage1_high.fit, Amatrix=A, Smatrix=S)
```

```
## Warning in .solve(x = object$mx.fit@output$calculatedHessian, parameters = my.name): Error in solving the Hessian matrix. Generalized inverse is used. The standard errors may not be trustworthy.
```

```r
summary(stage2_low.fit)
```

```
## 
## Call:
## wls(Cov = pooledS, aCov = aCov, n = tssem1.obj$total.n, Amatrix = Amatrix, 
##     Smatrix = Smatrix, Fmatrix = Fmatrix, diag.constraints = diag.constraints, 
##     cor.analysis = cor.analysis, intervals.type = intervals.type, 
##     mx.algebras = mx.algebras, model.name = model.name, suppressWarnings = suppressWarnings, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation
## Coefficients:
##      Estimate Std.Error    lbound    ubound z value  Pr(>|z|)    
## b43  0.292034  0.037772  0.218002  0.366066  7.7315 1.066e-14 ***
## b32 -0.310883  0.057137 -0.422869 -0.198896 -5.4410 5.298e-08 ***
## b31  0.256519  0.050231  0.158068  0.354971  5.1068 3.277e-07 ***
## p21 -0.326245  0.048691 -0.421677 -0.230814 -6.7004 2.079e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Goodness-of-fit indices:
##                                                 Value
## Sample size                                11883.0000
## Chi-square of target model                     6.8032
## DF of target model                             2.0000
## p value of target model                        0.0333
## Number of constraints imposed on "Smatrix"     0.0000
## DF manually adjusted                           0.0000
## Chi-square of independence model             206.2211
## DF of independence model                       6.0000
## RMSEA                                          0.0142
## RMSEA lower 95% CI                             0.0034
## RMSEA upper 95% CI                             0.0266
## SRMR                                           0.0436
## TLI                                            0.9280
## CFI                                            0.9760
## AIC                                            2.8032
## BIC                                          -11.9625
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

```r
summary(stage2_high.fit)
```

```
## 
## Call:
## wls(Cov = pooledS, aCov = aCov, n = tssem1.obj$total.n, Amatrix = Amatrix, 
##     Smatrix = Smatrix, Fmatrix = Fmatrix, diag.constraints = diag.constraints, 
##     cor.analysis = cor.analysis, intervals.type = intervals.type, 
##     mx.algebras = mx.algebras, model.name = model.name, suppressWarnings = suppressWarnings, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation
## Coefficients:
##      Estimate Std.Error    lbound    ubound z value  Pr(>|z|)    
## b43  0.408205  0.045315  0.319390  0.497021  9.0082 < 2.2e-16 ***
## b32 -0.165732  0.022046 -0.208942 -0.122523 -7.5175 5.573e-14 ***
## b31  0.258813  0.047237  0.166231  0.351395  5.4791 4.275e-08 ***
## p21 -0.160643  0.055053 -0.268545 -0.052741 -2.9180  0.003523 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Goodness-of-fit indices:
##                                                 Value
## Sample size                                17555.0000
## Chi-square of target model                    11.2780
## DF of target model                             2.0000
## p value of target model                        0.0036
## Number of constraints imposed on "Smatrix"     0.0000
## DF manually adjusted                           0.0000
## Chi-square of independence model             276.8330
## DF of independence model                       6.0000
## RMSEA                                          0.0163
## RMSEA lower 95% CI                             0.0079
## RMSEA upper 95% CI                             0.0260
## SRMR                                           0.0564
## TLI                                            0.8972
## CFI                                            0.9657
## AIC                                            7.2780
## BIC                                           -8.2682
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```


## Testing the equality of regression coefficients
* We create and fit a model with equal direct effects (we use the same matrix A for both groups),but different variances and covariances, so we create an S matrix with different labels for the 'high' group


```r
S_high <- create.mxMatrix(c(1,
                            ".1*p21_high",1,
                             0,0,"0.6*p33_high",
                             0,0,0,"0.6*p44_high"), 
                      type="Symm", byrow = TRUE,
                      name="S", as.mxMatrix = FALSE)
S_high
```

```
##      [,1]          [,2]          [,3]           [,4]          
## [1,] "1"           ".1*p21_high" "0"            "0"           
## [2,] ".1*p21_high" "1"           "0"            "0"           
## [3,] "0"           "0"           "0.6*p33_high" "0"           
## [4,] "0"           "0"           "0"            "0.6*p44_high"
```

```r
# Create the models for the two groups, make sure to set the argument run=FALSE
stage2_low <- tssem2(stage1_low.fit, Amatrix=A, Smatrix=S, run=FALSE, model.name="low")

stage2_high <- tssem2(stage1_high.fit, Amatrix=A, Smatrix=S_high, run=FALSE, model.name="high")
```

```
## Warning in .solve(x = object$mx.fit@output$calculatedHessian, parameters = my.name): Error in solving the Hessian matrix. Generalized inverse is used. The standard errors may not be trustworthy.
```

```r
# Create the multigroup model
stage2_constrained <- mxModel(model="same_regression_coef", stage2_low, stage2_high,
                              mxFitFunctionMultigroup(c("low", "high")))

# Fit multigroup model with equality constraints
Stage2_constrained.fit <- mxRun(stage2_constrained)
```

```
## Running same_regression_coef with 5 parameters
```

```r
# first make a list of the fitted models in the separate groups
submodels.fit <- list(stage2_low.fit,stage2_high.fit)

subgroup.summary(submodels.fit,Stage2_constrained.fit)
```

```
## # # # # # # # # # # # # # # # # # # #
##  Output for subgroup MASEM analysis 
## # # # # # # # # # # # # # # # # # # #
## 
##  Total sample size: 29438
## 
##  Parameter estimates of the constrained model
## 
## [1] "Set 'print.est=TRUE' to print the parameter estimates of the constrained model"
## 
##  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##  Fit indices of the free model:
## 
##           Statistic Free_m1
##                  df   4.000
##          Chi-square  18.081
##                   p   0.001
##               RMSEA   0.015
##  RMSEA lower 95% CI   0.007
##  RMSEA upper 95% CI   0.024
##                 CFI   0.970
##                 TLI   0.910
##                 AIC  50.081
##                 BIC 182.722
##                SRMR   0.052
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##  Fit indices of the model with equality constraints:
## 
##           Statistic Constrained_m2
##                  df          7.000
##          Chi-square         28.234
##                   p          0.000
##               RMSEA          0.014
##  RMSEA lower 95% CI          0.008
##  RMSEA upper 95% CI          0.021
##                 CFI          0.955
##                 TLI          0.923
##                 AIC         54.234
##                 BIC        162.005
##                SRMR          0.063
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##  Chi-square difference between free and constrained model:
## 
##   Statistic Diff_m1_m2
##          df      3.000
##  Chi-square     10.153
##           p      0.017
## 
##  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
```

```r
sessionInfo()
```

```
## R version 3.6.0 (2019-04-26)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Linux Mint 19.1
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
## LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so
## 
## locale:
##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] metaSEM_1.2.2.1 OpenMx_2.13.2  
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.1      mvtnorm_1.0-10  lattice_0.20-38 digest_0.6.18  
##  [5] MASS_7.3-51.4   grid_3.6.0      stats4_3.6.0    magrittr_1.5   
##  [9] ellipse_0.4.1   evaluate_0.13   stringi_1.4.3   Matrix_1.2-17  
## [13] pbivnorm_0.6.0  rmarkdown_1.12  tools_3.6.0     stringr_1.4.0  
## [17] xfun_0.6        yaml_2.2.0      parallel_3.6.0  compiler_3.6.0 
## [21] mnormt_1.5-5    htmltools_0.3.6 lavaan_0.6-3    knitr_1.22
```

