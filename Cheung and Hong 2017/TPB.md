# Illustration of MASEM: Theory of planned behavior
Mike Cheung and Ryan Hong  
`r format(Sys.Date(), "%B %d, %Y")`  

This file includes examples used in Cheung and Hong (2017):
Cheung, M. W.-L., & Hong, R. Y. (2017). Applications of meta-analytic structural equation modeling in health psychology: Examples, issues, and recommendations. *Health Psychology Review*, *11*(3), 265-279. http://doi.org/10.1080/17437199.2017.1343678

# Displaying some descriptive statistics of the data

```r
## Load the libraries required for the analysis
library("metaSEM")
library("semPlot")

## Read the data
# source("Cooke16.R")

## Check whether the correlation matrices are valid (positive definite)
## Since the correlation matrix in Study 3 is not valid, we excluded it from the analyses.
is.pd(Cooke16$data)
```

```
##                     Ajzen and Sheikh (2013) 
##                                        TRUE 
##         Armitage, Norman, and Conner (2002) 
##                                        TRUE 
## Conner, Warren, Close, and Sparks (1999a) 1 
##                                       FALSE 
## Conner, Warren, Close, and Sparks (1999a) 2 
##                                        TRUE 
## Conner, Warren, Close, and Sparks (1999a) 3 
##                                        TRUE 
## Conner, Warren, Close, and Sparks (1999b) 1 
##                                        TRUE 
## Conner, Warren, Close, and Sparks (1999b) 2 
##                                        TRUE 
##   Conner, Warren, Close, and Sparks (1999c) 
##                                        TRUE 
##                    Cooke and French (2011a) 
##                                        TRUE 
##                    Cooke and French (2011b) 
##                                        TRUE 
##          Cooke, Sniehotta and Schuez (2007) 
##                                        TRUE 
##               Elliot and Ainsworth (2012) 1 
##                                        TRUE 
##               Elliot and Ainsworth (2012) 2 
##                                        TRUE 
##               Elliot and Ainsworth (2012) 3 
##                                        TRUE 
##               Elliot and Ainsworth (2012) 4 
##                                        TRUE 
##                       Gagnon, et al. (2012) 
##                                        TRUE 
##        Gardner, de Bruijn, and Lally (2012) 
##                                        TRUE 
##                    Glassman, et al. (2010a) 
##                                        TRUE 
##                    Glassman, et al. (2010b) 
##                                        TRUE 
##                    Glassman, et al. (2010c) 
##                                        TRUE 
##                    Glassman, et al. (2010d) 
##                                        TRUE 
##                       Hagger, et al. (2012) 
##                                        TRUE 
##                    Jamison and Myers (2008) 
##                                        TRUE 
##                   Johnston and White (2003) 
##                                        TRUE 
##                         Kim and Hong (2013) 
##                                        TRUE 
##                             Norman (2011) 1 
##                                        TRUE 
##                             Norman (2011) 2 
##                                        TRUE 
##                             Norman (2011) 3 
##                                        TRUE 
##                             Norman (2011) 4 
##                                        TRUE 
##      Norman, Armitage, and Quigley (2007) 1 
##                                        TRUE 
##      Norman, Armitage, and Quigley (2007) 2 
##                                        TRUE 
##                    Norman and Conner (2006) 
##                                        TRUE 
##         Norman, Conner, and Stride (2012) 1 
##                                        TRUE 
##         Norman, Conner, and Stride (2012) 2 
##                                        TRUE
```

```r
## Since the correlation matrix in Study 3 is not positive definite,
## we may exclude it the following analyses
my.data <- Cooke16$data[-3]
my.n <- Cooke16$n[-3]

## Show the no. of studies per correlation
pattern.na(my.data, show.na = FALSE)
```

```
##     SN ATT PBC BI BEH
## SN  33  33  33 29  19
## ATT 33  33  33 29  19
## PBC 33  33  33 29  19
## BI  29  29  29 29  19
## BEH 19  19  19 19  19
```

```r
## Show the total sample sizes per correlation
pattern.n(my.data, my.n)
```

```
##       SN  ATT  PBC   BI  BEH
## SN  7973 7973 7973 7227 3628
## ATT 7973 7973 7973 7227 3628
## PBC 7973 7973 7973 7227 3628
## BI  7227 7227 7227 7227 3628
## BEH 3628 3628 3628 3628 3628
```

# Stage 1 analysis

```r
## Fixed-effects model: method="FEM"
fixed1 <- tssem1(my.data, my.n, method="FEM")
summary(fixed1)
```

```
## 
## Call:
## tssem1FEM(my.df = my.df, n = n, cor.analysis = cor.analysis, 
##     model.name = model.name, cluster = cluster, suppressWarnings = suppressWarnings, 
##     silent = silent, run = run)
## 
## Coefficients:
##         Estimate Std.Error z value  Pr(>|z|)    
## S[1,2] 0.4214478 0.0093842 44.9105 < 2.2e-16 ***
## S[1,3] 0.2368401 0.0107831 21.9640 < 2.2e-16 ***
## S[1,4] 0.4728029 0.0091713 51.5524 < 2.2e-16 ***
## S[1,5] 0.1686525 0.0161875 10.4187 < 2.2e-16 ***
## S[2,3] 0.2924019 0.0105878 27.6168 < 2.2e-16 ***
## S[2,4] 0.5461977 0.0084675 64.5049 < 2.2e-16 ***
## S[2,5] 0.2030472 0.0158711 12.7935 < 2.2e-16 ***
## S[3,4] 0.3623555 0.0104881 34.5492 < 2.2e-16 ***
## S[3,5] 0.0613639 0.0170259  3.6041 0.0003132 ***
## S[4,5] 0.3934428 0.0153311 25.6631 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Goodness-of-fit indices:
##                                      Value
## Sample size                      7973.0000
## Chi-square of target model       3372.5637
## DF of target model                252.0000
## p value of target model             0.0000
## Chi-square of independence model 9819.9708
## DF of independence model          262.0000
## RMSEA                               0.2264
## RMSEA lower 95% CI                  0.2201
## RMSEA upper 95% CI                  0.2337
## SRMR                                0.1992
## TLI                                 0.6606
## CFI                                 0.6735
## AIC                              2868.5637
## BIC                              1108.6421
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

```r
## Common correlation matrix under a fixed-effects model
coef(fixed1)
```

```
##            SN       ATT       PBC        BI       BEH
## SN  1.0000000 0.4214478 0.2368401 0.4728029 0.1686525
## ATT 0.4214478 1.0000000 0.2924019 0.5461977 0.2030472
## PBC 0.2368401 0.2924019 1.0000000 0.3623555 0.0613639
## BI  0.4728029 0.5461977 0.3623555 1.0000000 0.3934428
## BEH 0.1686525 0.2030472 0.0613639 0.3934428 1.0000000
```

```r
## Random-effects model
random1 <- tssem1(my.data, my.n, method="REM", RE.type="Diag")
summary(random1)
```

```
## 
## Call:
## meta(y = ES, v = acovR, RE.constraints = Diag(x = paste(RE.startvalues, 
##     "*Tau2_", 1:no.es, "_", 1:no.es, sep = "")), RE.lbound = RE.lbound, 
##     I2 = I2, model.name = model.name, suppressWarnings = TRUE, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation
## Coefficients:
##               Estimate  Std.Error     lbound     ubound z value  Pr(>|z|)
## Intercept1   0.4200809  0.0283378  0.3645397  0.4756220 14.8240 < 2.2e-16
## Intercept2   0.1760101  0.0356973  0.1060447  0.2459755  4.9306 8.197e-07
## Intercept3   0.4095008  0.0287725  0.3531078  0.4658938 14.2324 < 2.2e-16
## Intercept4   0.1608085  0.0462025  0.0702532  0.2513638  3.4805 0.0005005
## Intercept5   0.2540755  0.0465868  0.1627672  0.3453839  5.4538 4.930e-08
## Intercept6   0.5623705  0.0326338  0.4984095  0.6263315 17.2328 < 2.2e-16
## Intercept7   0.2781574  0.0530789  0.1741247  0.3821902  5.2405 1.602e-07
## Intercept8   0.2985265  0.0598784  0.1811669  0.4158861  4.9855 6.179e-07
## Intercept9   0.0385766  0.0782510 -0.1147926  0.1919459  0.4930 0.6220228
## Intercept10  0.4280697  0.0736125  0.2837919  0.5723475  5.8152 6.057e-09
## Tau2_1_1     0.0226391  0.0065919  0.0097192  0.0355590  3.4344 0.0005939
## Tau2_2_2     0.0369127  0.0100774  0.0171615  0.0566640  3.6629 0.0002493
## Tau2_3_3     0.0200703  0.0063447  0.0076348  0.0325058  3.1633 0.0015599
## Tau2_4_4     0.0352123  0.0125255  0.0106627  0.0597619  2.8112 0.0049350
## Tau2_5_5     0.0671048  0.0175075  0.0327907  0.1014189  3.8329 0.0001266
## Tau2_6_6     0.0284971  0.0079682  0.0128798  0.0441144  3.5764 0.0003484
## Tau2_7_7     0.0490051  0.0169658  0.0157528  0.0822574  2.8885 0.0038712
## Tau2_8_8     0.1001037  0.0272640  0.0466672  0.1535402  3.6716 0.0002410
## Tau2_9_9     0.1113739  0.0372769  0.0383125  0.1844352  2.9877 0.0028104
## Tau2_10_10   0.0997398  0.0330917  0.0348812  0.1645984  3.0140 0.0025779
##                
## Intercept1  ***
## Intercept2  ***
## Intercept3  ***
## Intercept4  ***
## Intercept5  ***
## Intercept6  ***
## Intercept7  ***
## Intercept8  ***
## Intercept9     
## Intercept10 ***
## Tau2_1_1    ***
## Tau2_2_2    ***
## Tau2_3_3    ** 
## Tau2_4_4    ** 
## Tau2_5_5    ***
## Tau2_6_6    ***
## Tau2_7_7    ** 
## Tau2_8_8    ***
## Tau2_9_9    ** 
## Tau2_10_10  ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 6627.506
## Degrees of freedom of the Q statistic: 252
## P value of the Q statistic: 0
## 
## Heterogeneity indices (based on the estimated Tau2):
##                               Estimate
## Intercept1: I2 (Q statistic)    0.9000
## Intercept2: I2 (Q statistic)    0.9160
## Intercept3: I2 (Q statistic)    0.8897
## Intercept4: I2 (Q statistic)    0.9012
## Intercept5: I2 (Q statistic)    0.9592
## Intercept6: I2 (Q statistic)    0.9502
## Intercept7: I2 (Q statistic)    0.9317
## Intercept8: I2 (Q statistic)    0.9755
## Intercept9: I2 (Q statistic)    0.9691
## Intercept10: I2 (Q statistic)   0.9803
## 
## Number of studies (or clusters): 33
## Number of observed statistics: 262
## Number of estimated parameters: 20
## Degrees of freedom: 242
## -2 log likelihood: -32.24907 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

```r
## Average correlation matrix under a random-effects model
vec2symMat(coef(random1, select="fixed"), diag = FALSE)
```

```
##           [,1]      [,2]       [,3]      [,4]       [,5]
## [1,] 1.0000000 0.4200809 0.17601010 0.4095008 0.16080851
## [2,] 0.4200809 1.0000000 0.25407554 0.5623705 0.27815744
## [3,] 0.1760101 0.2540755 1.00000000 0.2985265 0.03857663
## [4,] 0.4095008 0.5623705 0.29852648 1.0000000 0.42806966
## [5,] 0.1608085 0.2781574 0.03857663 0.4280697 1.00000000
```

# Stage 2 analysis: original model (Model A)
* The models are specified in the [RAM formulation](http://openmx.psyc.virginia.edu/docs/OpenMx/latest/Examples_Matrix.html). Matrices `A`, `S` and `F` represent the regression coefficients, variance-covariance matrix of variables, and selection matrix of the latent variables, respectively. We may skip the `F` matrix when there is no latent variable.
* Consider the element `SN2BI` in `A1` as an example. It is the regression coefficient from `SN` to `BI` with a starting value of 0.2.

```r
A1 <- create.mxMatrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        "0.2*SN2BI","0.2*ATT2BI","0.2*PBC2BI",0,0,
                        0,0,"0.2*PBC2BEH","0.2*BI2BEH",0),
                        type="Full", ncol=5, nrow=5,
                        byrow=TRUE, as.mxMatrix=FALSE)

## This step is not necessary but it is useful for inspecting the model.
dimnames(A1)[[1]] <- dimnames(A1)[[2]] <- colnames(Cooke16$data[[1]])

## Display A1
A1
```

```
##     SN          ATT          PBC           BI           BEH
## SN  "0"         "0"          "0"           "0"          "0"
## ATT "0"         "0"          "0"           "0"          "0"
## PBC "0"         "0"          "0"           "0"          "0"
## BI  "0.2*SN2BI" "0.2*ATT2BI" "0.2*PBC2BI"  "0"          "0"
## BEH "0"         "0"          "0.2*PBC2BEH" "0.2*BI2BEH" "0"
```

```r
S1 <- create.mxMatrix(c(1,
                        "0.1*ATT_SN", 1,
                        "0.1*PBC_SN", "0.1*PBC_ATT", 1,
                        0, 0, 0, "0.5*VarBI",
                        0, 0, 0, 0, "0.5*VarBEH"),
                      type = "Symm", ncol=5, nrow=5,
                      byrow=TRUE, as.mxMatrix=FALSE)

dimnames(S1)[[1]] <- dimnames(S1)[[2]] <- colnames(Cooke16$data[[1]])
S1
```

```
##     SN           ATT           PBC           BI          BEH         
## SN  "1"          "0.1*ATT_SN"  "0.1*PBC_SN"  "0"         "0"         
## ATT "0.1*ATT_SN" "1"           "0.1*PBC_ATT" "0"         "0"         
## PBC "0.1*PBC_SN" "0.1*PBC_ATT" "1"           "0"         "0"         
## BI  "0"          "0"           "0"           "0.5*VarBI" "0"         
## BEH "0"          "0"           "0"           "0"         "0.5*VarBEH"
```

* The columns `lbound` and `ubound` are the lower and upper bounds of the 95% confidence intervals. If they include 0, the parameter estimates are not statistically significant at $\alpha=.05$.

```r
ModelA <- tssem2(random1, Amatrix=A1, Smatrix=S1, diag.constraints=TRUE, intervals.type="LB")
summary(ModelA)
```

```
## 
## Call:
## wls(Cov = pooledS, asyCov = asyCov, n = tssem1.obj$total.n, Amatrix = Amatrix, 
##     Smatrix = Smatrix, Fmatrix = Fmatrix, diag.constraints = diag.constraints, 
##     cor.analysis = cor.analysis, intervals.type = intervals.type, 
##     mx.algebras = mx.algebras, model.name = model.name, suppressWarnings = suppressWarnings, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: Likelihood-based statistic
## Coefficients:
##          Estimate Std.Error    lbound    ubound z value Pr(>|z|)
## BI2BEH   0.476467        NA  0.343551  0.618420      NA       NA
## PBC2BEH -0.100206        NA -0.294219  0.081169      NA       NA
## ATT2BI   0.449443        NA  0.361810  0.535924      NA       NA
## PBC2BI   0.151839        NA  0.015286  0.284514      NA       NA
## SN2BI    0.191328        NA  0.111581  0.267448      NA       NA
## ATT_SN   0.420176        NA  0.364636  0.475715      NA       NA
## VarBEH   0.791467        NA  0.657918  0.883379      NA       NA
## VarBI    0.621455        NA  0.543113  0.688221      NA       NA
## PBC_ATT  0.251642        NA  0.160458  0.342905      NA       NA
## PBC_SN   0.176850        NA  0.106971  0.246702      NA       NA
## 
## Goodness-of-fit indices:
##                                                Value
## Sample size                                7973.0000
## Chi-square of target model                    0.6250
## DF of target model                            2.0000
## p value of target model                       0.7316
## Number of constraints imposed on "Smatrix"    2.0000
## DF manually adjusted                          0.0000
## Chi-square of independence model            782.6509
## DF of independence model                     10.0000
## RMSEA                                         0.0000
## RMSEA lower 95% CI                            0.0000
## RMSEA upper 95% CI                            0.0157
## SRMR                                          0.0131
## TLI                                           1.0089
## CFI                                           1.0000
## AIC                                          -3.3750
## BIC                                         -17.3427
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

* To facilitate the interpretations, we may print the model with the parameter estimates using the `semPlot` package.

```r
## Convert the model to semPlotModel object
my.plota <- meta2semPlot(ModelA)

## Plot the parameter estimates
semPaths(my.plota, whatLabels="est", edge.label.cex=1, color="yellow")
```

![](TPB_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# Stage 2 analysis: no direct effect from PBC (Model C)

```r
## Delete the direct effect from PBC to BEH
A2 <- create.mxMatrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        "0.2*SN2BI","0.2*ATT2BI","0.2*PBC2BI",0,0,
                        0,0,0,"0.2*BI2BEH",0),
                        type="Full", ncol=5, nrow=5,
                        byrow=TRUE, as.mxMatrix=FALSE)

## This step is not necessary but it is useful for inspecting the model.
dimnames(A2)[[1]] <- dimnames(A2)[[2]] <- colnames(Cooke16$data[[1]])

## Display A1
A2
```

```
##     SN          ATT          PBC          BI           BEH
## SN  "0"         "0"          "0"          "0"          "0"
## ATT "0"         "0"          "0"          "0"          "0"
## PBC "0"         "0"          "0"          "0"          "0"
## BI  "0.2*SN2BI" "0.2*ATT2BI" "0.2*PBC2BI" "0"          "0"
## BEH "0"         "0"          "0"          "0.2*BI2BEH" "0"
```

```r
ModelC <- tssem2(random1, Amatrix=A2, Smatrix=S1, diag.constraints=TRUE, intervals.type="LB")
summary(ModelC)
```

```
## 
## Call:
## wls(Cov = pooledS, asyCov = asyCov, n = tssem1.obj$total.n, Amatrix = Amatrix, 
##     Smatrix = Smatrix, Fmatrix = Fmatrix, diag.constraints = diag.constraints, 
##     cor.analysis = cor.analysis, intervals.type = intervals.type, 
##     mx.algebras = mx.algebras, model.name = model.name, suppressWarnings = suppressWarnings, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: Likelihood-based statistic
## Coefficients:
##           Estimate Std.Error     lbound     ubound z value Pr(>|z|)
## BI2BEH   0.4301748        NA  0.3276283  0.5340757      NA       NA
## ATT2BI   0.4530919        NA  0.3650729  0.5398119      NA       NA
## PBC2BI   0.1293778        NA -0.0010319  0.2561868      NA       NA
## SN2BI    0.1942875        NA  0.1144424  0.2704043      NA       NA
## ATT_SN   0.4201695        NA  0.3646303  0.4757087      NA       NA
## VarBEH   0.8149497        NA  0.7147632  0.8926597      NA       NA
## VarBI    0.6275782        NA  0.5525931  0.6925178      NA       NA
## PBC_ATT  0.2543742        NA  0.1630667  0.3456816      NA       NA
## PBC_SN   0.1759440        NA  0.1059799  0.2459081      NA       NA
## 
## Goodness-of-fit indices:
##                                                Value
## Sample size                                7973.0000
## Chi-square of target model                    1.7699
## DF of target model                            3.0000
## p value of target model                       0.6215
## Number of constraints imposed on "Smatrix"    2.0000
## DF manually adjusted                          0.0000
## Chi-square of independence model            782.6509
## DF of independence model                     10.0000
## RMSEA                                         0.0000
## RMSEA lower 95% CI                            0.0000
## RMSEA upper 95% CI                            0.0154
## SRMR                                          0.0290
## TLI                                           1.0053
## CFI                                           1.0000
## AIC                                          -4.2301
## BIC                                         -25.1816
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

```r
## Convert the model to semPlotModel object
my.plotc <- meta2semPlot(ModelC)

## Plot the parameter estimates
semPaths(my.plotc, whatLabels="est", edge.label.cex=1, color="yellow")
```

![](TPB_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# Stage 2 analysis: all direct effects (Model B)

```r
## Delete the direct effect from PBC to BEH
A3 <- create.mxMatrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        "0.2*SN2BI","0.2*ATT2BI","0.2*PBC2BI",0,0,
                        "0.2*SN2BEH","0.2*ATT2BEH","0.2*PBC2BEH","0.2*BI2BEH",0),
                        type="Full", ncol=5, nrow=5,
                        byrow=TRUE, as.mxMatrix=FALSE)

## This step is not necessary but it is useful for inspecting the model.
dimnames(A3)[[1]] <- dimnames(A3)[[2]] <- colnames(Cooke16$data[[1]])

## Display A1
A3
```

```
##     SN           ATT           PBC           BI           BEH
## SN  "0"          "0"           "0"           "0"          "0"
## ATT "0"          "0"           "0"           "0"          "0"
## PBC "0"          "0"           "0"           "0"          "0"
## BI  "0.2*SN2BI"  "0.2*ATT2BI"  "0.2*PBC2BI"  "0"          "0"
## BEH "0.2*SN2BEH" "0.2*ATT2BEH" "0.2*PBC2BEH" "0.2*BI2BEH" "0"
```

* We may calculate functions of parameters with the `mx.algebras` argument. In this example, we calculate the indirect and direct effects. After the analysis, 95% CI on the indirect and direct effects are estimated.

```r
ModelB <- tssem2(random1, Amatrix=A3, Smatrix=S1, diag.constraints=TRUE, intervals.type="LB",
                 mx.algebras=list(indirect=mxAlgebra((SN2BI+ATT2BI+PBC2BI)*BI2BEH, name="indirect"),
                                  direct=mxAlgebra(SN2BEH+ATT2BEH+PBC2BEH, name="direct")))
summary(ModelB)
```

```
## 
## Call:
## wls(Cov = pooledS, asyCov = asyCov, n = tssem1.obj$total.n, Amatrix = Amatrix, 
##     Smatrix = Smatrix, Fmatrix = Fmatrix, diag.constraints = diag.constraints, 
##     cor.analysis = cor.analysis, intervals.type = intervals.type, 
##     mx.algebras = mx.algebras, model.name = model.name, suppressWarnings = suppressWarnings, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: Likelihood-based statistic
## Coefficients:
##          Estimate Std.Error    lbound    ubound z value Pr(>|z|)
## ATT2BEH  0.075531        NA -0.128632  0.270608      NA       NA
## BI2BEH   0.427917        NA  0.182890  0.681225      NA       NA
## PBC2BEH -0.103440        NA -0.293781  0.076180      NA       NA
## SN2BEH  -0.027946        NA -0.162520  0.102501      NA       NA
## ATT2BI   0.440799        NA  0.349555  0.530085      NA       NA
## PBC2BI   0.151747        NA  0.016496  0.283172      NA       NA
## SN2BI    0.197621        NA  0.115813  0.275563      NA       NA
## ATT_SN   0.420081        NA  0.364540  0.475622      NA       NA
## VarBEH   0.804297        NA  0.639226  0.903561      NA       NA
## VarBI    0.625881        NA  0.547062  0.692934      NA       NA
## PBC_ATT  0.254076        NA  0.162768  0.345384      NA       NA
## PBC_SN   0.176010        NA  0.106045  0.245981      NA       NA
## 
## mxAlgebras objects (and their 95% likelihood-based CIs):
##                   lbound   Estimate    ubound
## indirect[1,1]  0.1429937  0.3381256 0.5693841
## direct[1,1]   -0.3522892 -0.0558552 0.2151028
## 
## Goodness-of-fit indices:
##                                              Value
## Sample size                                7973.00
## Chi-square of target model                    0.00
## DF of target model                            0.00
## p value of target model                       0.00
## Number of constraints imposed on "Smatrix"    2.00
## DF manually adjusted                          0.00
## Chi-square of independence model            782.65
## DF of independence model                     10.00
## RMSEA                                         0.00
## RMSEA lower 95% CI                            0.00
## RMSEA upper 95% CI                            0.00
## SRMR                                          0.00
## TLI                                           -Inf
## CFI                                           1.00
## AIC                                           0.00
## BIC                                           0.00
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

```r
## Convert the model to semPlotModel object
my.plotb <- meta2semPlot(ModelB)

## Plot the parameter estimates
semPaths(my.plotb, whatLabels="est", edge.label.cex=1, color="yellow")
```

![](TPB_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
## Settings of R for the analysis
sessionInfo()
```

```
## R version 3.4.1 (2017-06-30)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Linux Mint 18.2
## 
## Matrix products: default
## BLAS: /usr/lib/openblas-base/libblas.so.3
## LAPACK: /usr/lib/libopenblasp-r0.2.18.so
## 
## locale:
##  [1] LC_CTYPE=en_SG.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_SG.UTF-8        LC_COLLATE=en_SG.UTF-8    
##  [5] LC_MONETARY=en_SG.UTF-8    LC_MESSAGES=en_SG.UTF-8   
##  [7] LC_PAPER=en_SG.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_SG.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] semPlot_1.1    metaSEM_0.9.14 OpenMx_2.7.13  rmarkdown_1.6 
## 
## loaded via a namespace (and not attached):
##  [1] splines_3.4.1        ellipse_0.3-8        gtools_3.5.0        
##  [4] network_1.13.0       Formula_1.2-2        semTools_0.4-14     
##  [7] stats4_3.4.1         latticeExtra_0.6-28  d3Network_0.5.2.1   
## [10] yaml_2.1.14          lisrelToR_0.1.4      pbivnorm_0.6.0      
## [13] backports_1.1.0      lattice_0.20-35      quantreg_5.33       
## [16] quadprog_1.5-5       digest_0.6.12        RColorBrewer_1.1-2  
## [19] checkmate_1.8.3      ggm_2.3              minqa_1.2.4         
## [22] colorspace_1.3-2     htmltools_0.3.6      Matrix_1.2-10       
## [25] plyr_1.8.4           psych_1.7.5          XML_3.98-1.9        
## [28] SparseM_1.77         corpcor_1.6.9        mvtnorm_1.0-6       
## [31] scales_0.4.1         whisker_0.3-2        glasso_1.8          
## [34] sna_2.4              jpeg_0.1-8           fdrtool_1.2.15      
## [37] lme4_1.1-13          MatrixModels_0.4-1   huge_1.2.7          
## [40] arm_1.9-3            tibble_1.3.3         htmlTable_1.9       
## [43] rockchalk_1.8.101    mgcv_1.8-18          car_2.1-5           
## [46] ggplot2_2.2.1        nnet_7.3-12          lazyeval_0.2.0      
## [49] pbkrtest_0.4-7       mnormt_1.5-5         statnet.common_3.3.0
## [52] survival_2.41-3      magrittr_1.5         evaluate_0.10.1     
## [55] nlme_3.1-131         MASS_7.3-47          foreign_0.8-69      
## [58] tools_3.4.1          data.table_1.10.4    stringr_1.2.0       
## [61] munsell_0.4.3        cluster_2.0.6        compiler_3.4.1      
## [64] sem_3.1-9            rlang_0.1.1          grid_3.4.1          
## [67] nloptr_1.0.4         rjson_0.2.15         htmlwidgets_0.9     
## [70] igraph_1.0.1         lavaan_0.5-23.1097   base64enc_0.1-3     
## [73] boot_1.3-20          mi_1.0               gtable_0.2.0        
## [76] abind_1.4-5          reshape2_1.4.2       qgraph_1.4.3        
## [79] gridExtra_2.2.1      knitr_1.16           Hmisc_4.0-3         
## [82] rprojroot_1.2        stringi_1.1.5        matrixcalc_1.0-3    
## [85] parallel_3.4.1       Rcpp_0.12.12         rpart_4.1-11        
## [88] acepack_1.4.1        png_0.1-7            coda_0.19-1
```
