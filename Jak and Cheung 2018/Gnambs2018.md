---
title: "MASEM on Gnambs et al. (2018)"
author: "Suzanne Jak and Mike Cheung"
date: 'November 08, 2018'
output:
  html_document:
    keep_md: yes
    self_contained: yes
    theme: united
    toc: yes
  pdf_document:
    toc: no
  word_document: default
editor_options: 
  chunk_output_type: console
---

# Data preparation

```r
library(metaSEM)
```

```
## Loading required package: OpenMx
```

```
## To take full advantage of multiple cores, use:
##   mxOption(NULL, 'Number of Threads', parallel::detectCores()) #now
##   Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
```

```
## "SLSQP" is set as the default optimizer in OpenMx.
```

```
## mxOption(NULL, "Gradient algorithm") is set at "central".
```

```
## mxOption(NULL, "Optimality tolerance") is set at "6.3e-14".
```

```
## mxOption(NULL, "Gradient iterations") is set at "2".
```

```r
## Create a new dataset without missing value in Individualism
index_na <- is.na(Gnambs18$Individualism)
Gnambs18 <- lapply(Gnambs18, function(x) x[!index_na])

## Select data with the correlation matrices, i.e., exclude studies with factor loadings
index <- Gnambs18$CorMat==1
Gnambs18 <- lapply(Gnambs18, function(x) x[index])

my.df <- Cor2DataFrame(Gnambs18$data, Gnambs18$n, acov = "weighted")

## Add the standardized individualism as the moderator
## Standardization of the moderator improves the convergence.
my.df$data <- data.frame(my.df$data,
                         Individualism=scale(Gnambs18$Individualism),
                         check.names=FALSE)
summary(my.df)
```

```
##         Length Class      Mode     
## data    1081   data.frame list     
## n         34   -none-     numeric  
## ylabels   45   -none-     character
## vlabels 1035   -none-     character
```

# TSSEM
## One general factor model


```r
rand1 <- tssem1(Gnambs18$data, Gnambs18$n, method="REM", RE.type="Diag")
summary(rand1)
```

```
## 
## Call:
## meta(y = ES, v = acovR, RE.constraints = Diag(paste0(RE.startvalues, 
##     "*Tau2_", 1:no.es, "_", 1:no.es)), RE.lbound = RE.lbound, 
##     I2 = I2, model.name = model.name, suppressWarnings = TRUE, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation
## Coefficients:
##               Estimate  Std.Error     lbound     ubound z value  Pr(>|z|)
## Intercept1  0.40544334 0.01700760 0.37210906 0.43877762 23.8390 < 2.2e-16
## Intercept2  0.42482398 0.01416898 0.39705329 0.45259468 29.9827 < 2.2e-16
## Intercept3  0.38473948 0.01462386 0.35607723 0.41340172 26.3090 < 2.2e-16
## Intercept4  0.36586257 0.01938259 0.32787339 0.40385175 18.8758 < 2.2e-16
## Intercept5  0.39648312 0.01211314 0.37274181 0.42022443 32.7317 < 2.2e-16
## Intercept6  0.43750520 0.01803655 0.40215421 0.47285619 24.2566 < 2.2e-16
## Intercept7  0.30700478 0.02738420 0.25333272 0.36067683 11.2110 < 2.2e-16
## Intercept8  0.43551697 0.01545528 0.40522518 0.46580877 28.1792 < 2.2e-16
## Intercept9  0.61867757 0.01938984 0.58067417 0.65668096 31.9073 < 2.2e-16
## Intercept10 0.31115068 0.01386292 0.28397986 0.33832151 22.4448 < 2.2e-16
## Intercept11 0.28199046 0.01296425 0.25658099 0.30739993 21.7514 < 2.2e-16
## Intercept12 0.46342457 0.01288562 0.43816923 0.48867991 35.9645 < 2.2e-16
## Intercept13 0.65004662 0.01638476 0.61793308 0.68216015 39.6739 < 2.2e-16
## Intercept14 0.33699047 0.01641088 0.30482574 0.36915520 20.5346 < 2.2e-16
## Intercept15 0.42779365 0.02233832 0.38401136 0.47157595 19.1507 < 2.2e-16
## Intercept16 0.53672585 0.01316706 0.51091889 0.56253281 40.7628 < 2.2e-16
## Intercept17 0.43644202 0.01462191 0.40778361 0.46510043 29.8485 < 2.2e-16
## Intercept18 0.47471644 0.01161683 0.45194788 0.49748501 40.8645 < 2.2e-16
## Intercept19 0.34696794 0.01620620 0.31520437 0.37873152 21.4096 < 2.2e-16
## Intercept20 0.29135514 0.01193875 0.26795562 0.31475466 24.4042 < 2.2e-16
## Intercept21 0.53844907 0.02326915 0.49284238 0.58405576 23.1400 < 2.2e-16
## Intercept22 0.20430117 0.02068883 0.16375181 0.24485053  9.8750 < 2.2e-16
## Intercept23 0.35037988 0.01325967 0.32439141 0.37636836 26.4245 < 2.2e-16
## Intercept24 0.46524880 0.01297880 0.43981082 0.49068677 35.8468 < 2.2e-16
## Intercept25 0.28945140 0.01417683 0.26166533 0.31723747 20.4172 < 2.2e-16
## Intercept26 0.26916109 0.01228358 0.24508572 0.29323646 21.9123 < 2.2e-16
## Intercept27 0.41788882 0.01903188 0.38058702 0.45519062 21.9573 < 2.2e-16
## Intercept28 0.18007213 0.02179444 0.13735581 0.22278845  8.2623 2.220e-16
## Intercept29 0.30411048 0.01453262 0.27562706 0.33259390 20.9261 < 2.2e-16
## Intercept30 0.40237128 0.01198721 0.37887678 0.42586577 33.5667 < 2.2e-16
## Intercept31 0.45712965 0.01165835 0.43427970 0.47997960 39.2105 < 2.2e-16
## Intercept32 0.33996169 0.01689869 0.30684087 0.37308250 20.1176 < 2.2e-16
## Intercept33 0.35205879 0.01719223 0.31836264 0.38575494 20.4778 < 2.2e-16
## Intercept34 0.48878831 0.01976477 0.45005008 0.52752655 24.7303 < 2.2e-16
## Intercept35 0.38684314 0.01616980 0.35515092 0.41853537 23.9238 < 2.2e-16
## Intercept36 0.31923597 0.01294698 0.29386036 0.34461158 24.6572 < 2.2e-16
## Intercept37 0.41903670 0.02305625 0.37384727 0.46422613 18.1745 < 2.2e-16
## Intercept38 0.53204294 0.01110224 0.51028296 0.55380293 47.9221 < 2.2e-16
## Intercept39 0.41762201 0.01165509 0.39477844 0.44046557 35.8317 < 2.2e-16
## Intercept40 0.23011000 0.02292368 0.18518041 0.27503959 10.0381 < 2.2e-16
## Intercept41 0.37995735 0.01560324 0.34937557 0.41053914 24.3512 < 2.2e-16
## Intercept42 0.48256778 0.01496372 0.45323943 0.51189613 32.2492 < 2.2e-16
## Intercept43 0.40299700 0.02280660 0.35829690 0.44769711 17.6702 < 2.2e-16
## Intercept44 0.33626862 0.03025806 0.27696391 0.39557334 11.1134 < 2.2e-16
## Intercept45 0.47867194 0.01509742 0.44908155 0.50826233 31.7056 < 2.2e-16
## Tau2_1_1    0.00899248 0.00234096 0.00440428 0.01358068  3.8414 0.0001224
## Tau2_2_2    0.00597143 0.00156993 0.00289442 0.00904844  3.8036 0.0001426
## Tau2_3_3    0.00634602 0.00167524 0.00306262 0.00962943  3.7881 0.0001518
## Tau2_4_4    0.01183496 0.00305283 0.00585151 0.01781840  3.8767 0.0001059
## Tau2_5_5    0.00418792 0.00115877 0.00191676 0.00645907  3.6141 0.0003014
## Tau2_6_6    0.01017277 0.00259142 0.00509368 0.01525187  3.9256 8.653e-05
## Tau2_7_7    0.02436994 0.00609217 0.01242952 0.03631037  4.0002 6.329e-05
## Tau2_8_8    0.00731122 0.00190005 0.00358720 0.01103525  3.8479 0.0001191
## Tau2_9_9    0.01223889 0.00308711 0.00618827 0.01828951  3.9645 7.355e-05
## Tau2_10_10  0.00560344 0.00148236 0.00269806 0.00850883  3.7801 0.0001568
## Tau2_11_11  0.00474973 0.00127090 0.00225881 0.00724066  3.7373 0.0001860
## Tau2_12_12  0.00485436 0.00131435 0.00227827 0.00743044  3.6933 0.0002213
## Tau2_13_13  0.00865192 0.00223541 0.00427059 0.01303324  3.8704 0.0001087
## Tau2_14_14  0.00822563 0.00209868 0.00411230 0.01233897  3.9194 8.876e-05
## Tau2_15_15  0.01599769 0.00407848 0.00800401 0.02399137  3.9225 8.765e-05
## Tau2_16_16  0.00524250 0.00143220 0.00243544 0.00804955  3.6605 0.0002518
## Tau2_17_17  0.00649345 0.00166437 0.00323135 0.00975555  3.9015 9.562e-05
## Tau2_18_18  0.00382432 0.00104892 0.00176847 0.00588017  3.6460 0.0002664
## Tau2_19_19  0.00796917 0.00208550 0.00388167 0.01205667  3.8212 0.0001328
## Tau2_20_20  0.00393870 0.00109851 0.00178567 0.00609173  3.5855 0.0003364
## Tau2_21_21  0.01764400 0.00444088 0.00894004 0.02634797  3.9731 7.095e-05
## Tau2_22_22  0.01334151 0.00346405 0.00655209 0.02013093  3.8514 0.0001174
## Tau2_23_23  0.00505709 0.00133773 0.00243518 0.00767900  3.7803 0.0001566
## Tau2_24_24  0.00495230 0.00135884 0.00228901 0.00761558  3.6445 0.0002679
## Tau2_25_25  0.00582686 0.00158023 0.00272966 0.00892405  3.6874 0.0002266
## Tau2_26_26  0.00417652 0.00113885 0.00194442 0.00640862  3.6673 0.0002451
## Tau2_27_27  0.01137474 0.00296818 0.00555722 0.01719227  3.8322 0.0001270
## Tau2_28_28  0.01489647 0.00383467 0.00738065 0.02241228  3.8847 0.0001025
## Tau2_29_29  0.00620231 0.00166274 0.00294340 0.00946123  3.7302 0.0001914
## Tau2_30_30  0.00404338 0.00110875 0.00187027 0.00621648  3.6468 0.0002655
## Tau2_31_31  0.00387673 0.00110635 0.00170833 0.00604513  3.5041 0.0004582
## Tau2_32_32  0.00873813 0.00226101 0.00430664 0.01316962  3.8647 0.0001112
## Tau2_33_33  0.00901030 0.00238152 0.00434261 0.01367800  3.7834 0.0001547
## Tau2_34_34  0.01249858 0.00318576 0.00625461 0.01874255  3.9233 8.736e-05
## Tau2_35_35  0.00800328 0.00208910 0.00390872 0.01209783  3.8310 0.0001276
## Tau2_36_36  0.00479049 0.00126289 0.00231527 0.00726571  3.7933 0.0001487
## Tau2_37_37  0.01708829 0.00438831 0.00848736 0.02568921  3.8940 9.858e-05
## Tau2_38_38  0.00356696 0.00099372 0.00161930 0.00551461  3.5895 0.0003313
## Tau2_39_39  0.00386869 0.00103489 0.00184035 0.00589703  3.7383 0.0001853
## Tau2_40_40  0.01666838 0.00422690 0.00838382 0.02495295  3.9434 8.033e-05
## Tau2_41_41  0.00737642 0.00187626 0.00369902 0.01105381  3.9315 8.443e-05
## Tau2_42_42  0.00684146 0.00174951 0.00341247 0.01027044  3.9105 9.211e-05
## Tau2_43_43  0.01670166 0.00424158 0.00838832 0.02501500  3.9376 8.230e-05
## Tau2_44_44  0.03006333 0.00750561 0.01535260 0.04477406  4.0054 6.190e-05
## Tau2_45_45  0.00701308 0.00182025 0.00344547 0.01058070  3.8528 0.0001168
##                
## Intercept1  ***
## Intercept2  ***
## Intercept3  ***
## Intercept4  ***
## Intercept5  ***
## Intercept6  ***
## Intercept7  ***
## Intercept8  ***
## Intercept9  ***
## Intercept10 ***
## Intercept11 ***
## Intercept12 ***
## Intercept13 ***
## Intercept14 ***
## Intercept15 ***
## Intercept16 ***
## Intercept17 ***
## Intercept18 ***
## Intercept19 ***
## Intercept20 ***
## Intercept21 ***
## Intercept22 ***
## Intercept23 ***
## Intercept24 ***
## Intercept25 ***
## Intercept26 ***
## Intercept27 ***
## Intercept28 ***
## Intercept29 ***
## Intercept30 ***
## Intercept31 ***
## Intercept32 ***
## Intercept33 ***
## Intercept34 ***
## Intercept35 ***
## Intercept36 ***
## Intercept37 ***
## Intercept38 ***
## Intercept39 ***
## Intercept40 ***
## Intercept41 ***
## Intercept42 ***
## Intercept43 ***
## Intercept44 ***
## Intercept45 ***
## Tau2_1_1    ***
## Tau2_2_2    ***
## Tau2_3_3    ***
## Tau2_4_4    ***
## Tau2_5_5    ***
## Tau2_6_6    ***
## Tau2_7_7    ***
## Tau2_8_8    ***
## Tau2_9_9    ***
## Tau2_10_10  ***
## Tau2_11_11  ***
## Tau2_12_12  ***
## Tau2_13_13  ***
## Tau2_14_14  ***
## Tau2_15_15  ***
## Tau2_16_16  ***
## Tau2_17_17  ***
## Tau2_18_18  ***
## Tau2_19_19  ***
## Tau2_20_20  ***
## Tau2_21_21  ***
## Tau2_22_22  ***
## Tau2_23_23  ***
## Tau2_24_24  ***
## Tau2_25_25  ***
## Tau2_26_26  ***
## Tau2_27_27  ***
## Tau2_28_28  ***
## Tau2_29_29  ***
## Tau2_30_30  ***
## Tau2_31_31  ***
## Tau2_32_32  ***
## Tau2_33_33  ***
## Tau2_34_34  ***
## Tau2_35_35  ***
## Tau2_36_36  ***
## Tau2_37_37  ***
## Tau2_38_38  ***
## Tau2_39_39  ***
## Tau2_40_40  ***
## Tau2_41_41  ***
## Tau2_42_42  ***
## Tau2_43_43  ***
## Tau2_44_44  ***
## Tau2_45_45  ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 30323.94
## Degrees of freedom of the Q statistic: 1485
## P value of the Q statistic: 0
## 
## Heterogeneity indices (based on the estimated Tau2):
##                               Estimate
## Intercept1: I2 (Q statistic)    0.9748
## Intercept2: I2 (Q statistic)    0.9616
## Intercept3: I2 (Q statistic)    0.9606
## Intercept4: I2 (Q statistic)    0.9795
## Intercept5: I2 (Q statistic)    0.9463
## Intercept6: I2 (Q statistic)    0.9777
## Intercept7: I2 (Q statistic)    0.9887
## Intercept8: I2 (Q statistic)    0.9707
## Intercept9: I2 (Q statistic)    0.9893
## Intercept10: I2 (Q statistic)   0.9524
## Intercept11: I2 (Q statistic)   0.9416
## Intercept12: I2 (Q statistic)   0.9571
## Intercept13: I2 (Q statistic)   0.9867
## Intercept14: I2 (Q statistic)   0.9688
## Intercept15: I2 (Q statistic)   0.9853
## Intercept16: I2 (Q statistic)   0.9674
## Intercept17: I2 (Q statistic)   0.9672
## Intercept18: I2 (Q statistic)   0.9471
## Intercept19: I2 (Q statistic)   0.9679
## Intercept20: I2 (Q statistic)   0.9320
## Intercept21: I2 (Q statistic)   0.9896
## Intercept22: I2 (Q statistic)   0.9767
## Intercept23: I2 (Q statistic)   0.9493
## Intercept24: I2 (Q statistic)   0.9576
## Intercept25: I2 (Q statistic)   0.9529
## Intercept26: I2 (Q statistic)   0.9339
## Intercept27: I2 (Q statistic)   0.9792
## Intercept28: I2 (Q statistic)   0.9787
## Intercept29: I2 (Q statistic)   0.9558
## Intercept30: I2 (Q statistic)   0.9416
## Intercept31: I2 (Q statistic)   0.9476
## Intercept32: I2 (Q statistic)   0.9708
## Intercept33: I2 (Q statistic)   0.9710
## Intercept34: I2 (Q statistic)   0.9842
## Intercept35: I2 (Q statistic)   0.9705
## Intercept36: I2 (Q statistic)   0.9459
## Intercept37: I2 (Q statistic)   0.9859
## Intercept38: I2 (Q statistic)   0.9527
## Intercept39: I2 (Q statistic)   0.9446
## Intercept40: I2 (Q statistic)   0.9820
## Intercept41: I2 (Q statistic)   0.9669
## Intercept42: I2 (Q statistic)   0.9706
## Intercept43: I2 (Q statistic)   0.9857
## Intercept44: I2 (Q statistic)   0.9914
## Intercept45: I2 (Q statistic)   0.9720
## 
## Number of studies (or clusters): 34
## Number of observed statistics: 1530
## Number of estimated parameters: 90
## Degrees of freedom: 1440
## -2 log likelihood: -2647.346 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

```r
## One general factor
model1 <- "G =~ g1*I1 + g2*I2 + g3*I3 + g4*I4 + g5*I5 +
                g6*I6 + g7*I7 + g8*I8 + g9*I9 + g10*I10"

RAM1 <- lavaan2RAM(model1, obs.variables = paste0("I", 1:10))
RAM1
```

```
## $A
##     I1  I2  I3  I4  I5  I6  I7  I8  I9  I10 G      
## I1  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g1" 
## I2  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g2" 
## I3  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g3" 
## I4  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g4" 
## I5  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g5" 
## I6  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g6" 
## I7  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g7" 
## I8  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g8" 
## I9  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g9" 
## I10 "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g10"
## G   "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"    
## 
## $S
##     I1           I2           I3           I4           I5          
## I1  "0*I1WITHI1" "0"          "0"          "0"          "0"         
## I2  "0"          "0*I2WITHI2" "0"          "0"          "0"         
## I3  "0"          "0"          "0*I3WITHI3" "0"          "0"         
## I4  "0"          "0"          "0"          "0*I4WITHI4" "0"         
## I5  "0"          "0"          "0"          "0"          "0*I5WITHI5"
## I6  "0"          "0"          "0"          "0"          "0"         
## I7  "0"          "0"          "0"          "0"          "0"         
## I8  "0"          "0"          "0"          "0"          "0"         
## I9  "0"          "0"          "0"          "0"          "0"         
## I10 "0"          "0"          "0"          "0"          "0"         
## G   "0"          "0"          "0"          "0"          "0"         
##     I6           I7           I8           I9           I10            G  
## I1  "0"          "0"          "0"          "0"          "0"            "0"
## I2  "0"          "0"          "0"          "0"          "0"            "0"
## I3  "0"          "0"          "0"          "0"          "0"            "0"
## I4  "0"          "0"          "0"          "0"          "0"            "0"
## I5  "0"          "0"          "0"          "0"          "0"            "0"
## I6  "0*I6WITHI6" "0"          "0"          "0"          "0"            "0"
## I7  "0"          "0*I7WITHI7" "0"          "0"          "0"            "0"
## I8  "0"          "0"          "0*I8WITHI8" "0"          "0"            "0"
## I9  "0"          "0"          "0"          "0*I9WITHI9" "0"            "0"
## I10 "0"          "0"          "0"          "0"          "0*I10WITHI10" "0"
## G   "0"          "0"          "0"          "0"          "0"            "1"
## 
## $F
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10 G
## I1   1  0  0  0  0  0  0  0  0   0 0
## I2   0  1  0  0  0  0  0  0  0   0 0
## I3   0  0  1  0  0  0  0  0  0   0 0
## I4   0  0  0  1  0  0  0  0  0   0 0
## I5   0  0  0  0  1  0  0  0  0   0 0
## I6   0  0  0  0  0  1  0  0  0   0 0
## I7   0  0  0  0  0  0  1  0  0   0 0
## I8   0  0  0  0  0  0  0  1  0   0 0
## I9   0  0  0  0  0  0  0  0  1   0 0
## I10  0  0  0  0  0  0  0  0  0   1 0
## 
## $M
##   I1 I2 I3 I4 I5 I6 I7 I8 I9 I10 G
## 1  0  0  0  0  0  0  0  0  0   0 0
```

```r
rand2a <- tssem2(rand1, Amatrix = RAM1$A, Smatrix = RAM1$S, Fmatrix = RAM1$F)
summary(rand2a)
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
## g1  0.6718578 0.0092049 0.6538164 0.6898991  72.989 < 2.2e-16 ***
## g10 0.7190289 0.0084647 0.7024384 0.7356194  84.945 < 2.2e-16 ***
## g2  0.6875434 0.0085785 0.6707298 0.7043569  80.147 < 2.2e-16 ***
## g3  0.5970915 0.0082730 0.5808768 0.6133062  72.174 < 2.2e-16 ***
## g4  0.5367416 0.0081164 0.5208338 0.5526494  66.131 < 2.2e-16 ***
## g5  0.6322076 0.0088254 0.6149101 0.6495050  71.635 < 2.2e-16 ***
## g6  0.6562634 0.0075209 0.6415226 0.6710041  87.258 < 2.2e-16 ***
## g7  0.5964133 0.0094531 0.5778856 0.6149411  63.092 < 2.2e-16 ***
## g8  0.5099223 0.0124635 0.4854942 0.5343504  40.913 < 2.2e-16 ***
## g9  0.7131074 0.0084100 0.6966241 0.7295907  84.793 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Goodness-of-fit indices:
##                                                 Value
## Sample size                                1.0468e+05
## Chi-square of target model                 1.0343e+03
## DF of target model                         3.5000e+01
## p value of target model                    0.0000e+00
## Number of constraints imposed on "Smatrix" 0.0000e+00
## DF manually adjusted                       0.0000e+00
## Chi-square of independence model           2.0602e+04
## DF of independence model                   4.5000e+01
## RMSEA                                      1.6500e-02
## RMSEA lower 95% CI                         1.5700e-02
## RMSEA upper 95% CI                         1.7400e-02
## SRMR                                       7.7500e-02
## TLI                                        9.3750e-01
## CFI                                        9.5140e-01
## AIC                                        9.6426e+02
## BIC                                        6.2971e+02
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

```r
plot(rand2a, col="green")
```

![](Gnambs2018_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Bifactor model with positive and negative Self-Esteem

```r
model2 <- "G =~ g1*I1 + g2*I2 + g3*I3 + g4*I4 + g5*I5 + 
                g6*I6 + g7*I7 + g8*I8 + g9*I9 + g10*I10
           POS =~ p1*I1 + p3*I3 + p4*I4 + p7*I7 + p10*I10
           NEG =~ n2*I2 + n5*I5 + n6*I6 + n8*I8 + n9*I9"

RAM2 <- lavaan2RAM(model2, obs.variables = paste0("I", 1:10))
RAM2
```

```
## $A
##     I1  I2  I3  I4  I5  I6  I7  I8  I9  I10 G       POS     NEG   
## I1  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g1"  "0*p1"  "0"   
## I2  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g2"  "0"     "0*n2"
## I3  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g3"  "0*p3"  "0"   
## I4  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g4"  "0*p4"  "0"   
## I5  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g5"  "0"     "0*n5"
## I6  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g6"  "0"     "0*n6"
## I7  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g7"  "0*p7"  "0"   
## I8  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g8"  "0"     "0*n8"
## I9  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g9"  "0"     "0*n9"
## I10 "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*g10" "0*p10" "0"   
## G   "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"     "0"     "0"   
## POS "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"     "0"     "0"   
## NEG "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"     "0"     "0"   
## 
## $S
##     I1           I2           I3           I4           I5          
## I1  "0*I1WITHI1" "0"          "0"          "0"          "0"         
## I2  "0"          "0*I2WITHI2" "0"          "0"          "0"         
## I3  "0"          "0"          "0*I3WITHI3" "0"          "0"         
## I4  "0"          "0"          "0"          "0*I4WITHI4" "0"         
## I5  "0"          "0"          "0"          "0"          "0*I5WITHI5"
## I6  "0"          "0"          "0"          "0"          "0"         
## I7  "0"          "0"          "0"          "0"          "0"         
## I8  "0"          "0"          "0"          "0"          "0"         
## I9  "0"          "0"          "0"          "0"          "0"         
## I10 "0"          "0"          "0"          "0"          "0"         
## G   "0"          "0"          "0"          "0"          "0"         
## POS "0"          "0"          "0"          "0"          "0"         
## NEG "0"          "0"          "0"          "0"          "0"         
##     I6           I7           I8           I9           I10            G  
## I1  "0"          "0"          "0"          "0"          "0"            "0"
## I2  "0"          "0"          "0"          "0"          "0"            "0"
## I3  "0"          "0"          "0"          "0"          "0"            "0"
## I4  "0"          "0"          "0"          "0"          "0"            "0"
## I5  "0"          "0"          "0"          "0"          "0"            "0"
## I6  "0*I6WITHI6" "0"          "0"          "0"          "0"            "0"
## I7  "0"          "0*I7WITHI7" "0"          "0"          "0"            "0"
## I8  "0"          "0"          "0*I8WITHI8" "0"          "0"            "0"
## I9  "0"          "0"          "0"          "0*I9WITHI9" "0"            "0"
## I10 "0"          "0"          "0"          "0"          "0*I10WITHI10" "0"
## G   "0"          "0"          "0"          "0"          "0"            "1"
## POS "0"          "0"          "0"          "0"          "0"            "0"
## NEG "0"          "0"          "0"          "0"          "0"            "0"
##     POS NEG
## I1  "0" "0"
## I2  "0" "0"
## I3  "0" "0"
## I4  "0" "0"
## I5  "0" "0"
## I6  "0" "0"
## I7  "0" "0"
## I8  "0" "0"
## I9  "0" "0"
## I10 "0" "0"
## G   "0" "0"
## POS "1" "0"
## NEG "0" "1"
## 
## $F
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10 G POS NEG
## I1   1  0  0  0  0  0  0  0  0   0 0   0   0
## I2   0  1  0  0  0  0  0  0  0   0 0   0   0
## I3   0  0  1  0  0  0  0  0  0   0 0   0   0
## I4   0  0  0  1  0  0  0  0  0   0 0   0   0
## I5   0  0  0  0  1  0  0  0  0   0 0   0   0
## I6   0  0  0  0  0  1  0  0  0   0 0   0   0
## I7   0  0  0  0  0  0  1  0  0   0 0   0   0
## I8   0  0  0  0  0  0  0  1  0   0 0   0   0
## I9   0  0  0  0  0  0  0  0  1   0 0   0   0
## I10  0  0  0  0  0  0  0  0  0   1 0   0   0
## 
## $M
##   I1 I2 I3 I4 I5 I6 I7 I8 I9 I10 G POS NEG
## 1  0  0  0  0  0  0  0  0  0   0 0   0   0
```

```r
rand2b <- tssem2(rand1, Amatrix = RAM2$A, Smatrix = RAM2$S, Fmatrix = RAM2$F)
summary(rand2b)
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
## g1   0.752662  0.014051  0.725123  0.780201 53.5671 < 2.2e-16 ***
## p1  -0.047642  0.042723 -0.131377  0.036092 -1.1152    0.2648    
## g10  0.801858  0.014488  0.773461  0.830254 55.3446 < 2.2e-16 ***
## p10 -0.034061  0.044048 -0.120393  0.052271 -0.7733    0.4394    
## g2   0.535602  0.013252  0.509629  0.561576 40.4170 < 2.2e-16 ***
## n2   0.587798  0.024187  0.540394  0.635203 24.3027 < 2.2e-16 ***
## g3   0.595890  0.017363  0.561858  0.629921 34.3190 < 2.2e-16 ***
## p3   0.531255  0.070413  0.393248  0.669261  7.5449 4.530e-14 ***
## g4   0.522442  0.012646  0.497655  0.547228 41.3113 < 2.2e-16 ***
## p4   0.306155  0.037994  0.231688  0.380622  8.0580 6.661e-16 ***
## g5   0.527211  0.014154  0.499470  0.554953 37.2483 < 2.2e-16 ***
## n5   0.326077  0.021632  0.283679  0.368475 15.0738 < 2.2e-16 ***
## g6   0.514349  0.011522  0.491766  0.536932 44.6391 < 2.2e-16 ***
## n6   0.597994  0.022935  0.553042  0.642945 26.0733 < 2.2e-16 ***
## g7   0.620789  0.013670  0.593995  0.647582 45.4110 < 2.2e-16 ***
## p7   0.317100  0.041470  0.235821  0.398379  7.6466 2.065e-14 ***
## g8   0.384995  0.018314  0.349100  0.420890 21.0219 < 2.2e-16 ***
## n8   0.399932  0.030376  0.340396  0.459467 13.1661 < 2.2e-16 ***
## g9   0.594682  0.013886  0.567465  0.621899 42.8245 < 2.2e-16 ***
## n9   0.386959  0.022302  0.343248  0.430670 17.3510 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Goodness-of-fit indices:
##                                                  Value
## Sample size                                104684.0000
## Chi-square of target model                     37.6212
## DF of target model                             25.0000
## p value of target model                         0.0503
## Number of constraints imposed on "Smatrix"      0.0000
## DF manually adjusted                            0.0000
## Chi-square of independence model            20601.8319
## DF of independence model                       45.0000
## RMSEA                                           0.0022
## RMSEA lower 95% CI                              0.0000
## RMSEA upper 95% CI                              0.0036
## SRMR                                            0.0161
## TLI                                             0.9989
## CFI                                             0.9994
## AIC                                           -12.3788
## BIC                                          -251.3463
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

```r
plot(rand2b, col="green")
```

![](Gnambs2018_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## Compare the one general factor to the bifactor model
anova(rand2b, rand2a)
```

```
##                 base         comparison ep   minus2LL  df AIC   diffLL
## 1 TSSEM2 Correlation               <NA> 20   37.62119 -20  NA       NA
## 2 TSSEM2 Correlation TSSEM2 Correlation 10 1034.26325 -10  NA 996.6421
##   diffdf             p
## 1     NA            NA
## 2     10 9.891117e-208
```

## Models with two subgroup analysis

```r
# Data for studies with individualism below the mean
data_g1 <- Gnambs18$data[my.df$data$Individualism < 0 ]
n_g1 <- Gnambs18$n[my.df$data$Individualism < 0 ]

# Data for studies with individualism above or equal the mean
data_g2 <- Gnambs18$data[my.df$data$Individualism >= 0 ]
n_g2 <- Gnambs18$n[my.df$data$Individualism >= 0 ]
```

### Fitting a random-effects Stage 1 model in two subgroups

```r
## Stage 1 analysis per subgroup (random-effects analysis)
stage1_g1.fit <- tssem1(Cov = data_g1, n = n_g1, method = "REM", RE.type = "Diag")
stage1_g2.fit <- tssem1(Cov = data_g2, n = n_g2, method = "REM", RE.type = "Diag")

summary(stage1_g1.fit)
```

```
## 
## Call:
## meta(y = ES, v = acovR, RE.constraints = Diag(paste0(RE.startvalues, 
##     "*Tau2_", 1:no.es, "_", 1:no.es)), RE.lbound = RE.lbound, 
##     I2 = I2, model.name = model.name, suppressWarnings = TRUE, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation
## Coefficients:
##               Estimate  Std.Error     lbound     ubound z value  Pr(>|z|)
## Intercept1  0.33167588 0.02715325 0.27845648 0.38489528 12.2150 < 2.2e-16
## Intercept2  0.40117619 0.01977393 0.36241999 0.43993239 20.2881 < 2.2e-16
## Intercept3  0.38259138 0.01859916 0.34613770 0.41904507 20.5704 < 2.2e-16
## Intercept4  0.31549394 0.03409816 0.24866278 0.38232511  9.2525 < 2.2e-16
## Intercept5  0.32992831 0.01978119 0.29115788 0.36869873 16.6789 < 2.2e-16
## Intercept6  0.41402770 0.02716928 0.36077689 0.46727851 15.2388 < 2.2e-16
## Intercept7  0.20248214 0.04867329 0.10708425 0.29788003  4.1600 3.182e-05
## Intercept8  0.38622520 0.02597717 0.33531088 0.43713952 14.8679 < 2.2e-16
## Intercept9  0.55983870 0.02678699 0.50733716 0.61234024 20.8996 < 2.2e-16
## Intercept10 0.27582769 0.02050975 0.23562931 0.31602606 13.4486 < 2.2e-16
## Intercept11 0.26795101 0.01837378 0.23193906 0.30396297 14.5833 < 2.2e-16
## Intercept12 0.45166886 0.02201503 0.40852020 0.49481753 20.5164 < 2.2e-16
## Intercept13 0.61521065 0.02924605 0.55788944 0.67253186 21.0357 < 2.2e-16
## Intercept14 0.28548018 0.02453158 0.23739916 0.33356120 11.6373 < 2.2e-16
## Intercept15 0.35726974 0.04264713 0.27368290 0.44085658  8.3773 < 2.2e-16
## Intercept16 0.49587702 0.02420583 0.44843447 0.54331957 20.4859 < 2.2e-16
## Intercept17 0.38773325 0.02400266 0.34068889 0.43477760 16.1538 < 2.2e-16
## Intercept18 0.45691507 0.01643044 0.42471199 0.48911814 27.8091 < 2.2e-16
## Intercept19 0.31836746 0.02564599 0.26810225 0.36863267 12.4139 < 2.2e-16
## Intercept20 0.25372070 0.02085435 0.21284692 0.29459448 12.1663 < 2.2e-16
## Intercept21 0.51752818 0.03365553 0.45156456 0.58349181 15.3772 < 2.2e-16
## Intercept22 0.13433034 0.03907100 0.05775259 0.21090809  3.4381 0.0005858
## Intercept23 0.35092516 0.02103893 0.30968963 0.39216070 16.6798 < 2.2e-16
## Intercept24 0.46379963 0.02090951 0.42281774 0.50478151 22.1813 < 2.2e-16
## Intercept25 0.26522338 0.02410123 0.21798583 0.31246093 11.0046 < 2.2e-16
## Intercept26 0.24760368 0.01770786 0.21289691 0.28231046 13.9827 < 2.2e-16
## Intercept27 0.41562845 0.03266209 0.35161193 0.47964497 12.7251 < 2.2e-16
## Intercept28 0.12945081 0.04324212 0.04469781 0.21420381  2.9936 0.0027568
## Intercept29 0.31700999 0.02237872 0.27314850 0.36087148 14.1657 < 2.2e-16
## Intercept30 0.41581715 0.01407476 0.38823114 0.44340316 29.5435 < 2.2e-16
## Intercept31 0.43546905 0.02011310 0.39604810 0.47488999 21.6510 < 2.2e-16
## Intercept32 0.30333739 0.02913028 0.24624309 0.36043170 10.4131 < 2.2e-16
## Intercept33 0.29160800 0.03325091 0.22643743 0.35677858  8.7699 < 2.2e-16
## Intercept34 0.47252751 0.03919728 0.39570225 0.54935277 12.0551 < 2.2e-16
## Intercept35 0.36672800 0.02867654 0.31052302 0.42293298 12.7884 < 2.2e-16
## Intercept36 0.27445662 0.02144165 0.23243175 0.31648149 12.8002 < 2.2e-16
## Intercept37 0.34424109 0.04385797 0.25828105 0.43020113  7.8490 4.219e-15
## Intercept38 0.48484989 0.01917083 0.44727575 0.52242402 25.2910 < 2.2e-16
## Intercept39 0.36024113 0.01925784 0.32249645 0.39798580 18.7062 < 2.2e-16
## Intercept40 0.13863001 0.03705688 0.06599986 0.21126016  3.7410 0.0001833
## Intercept41 0.36297892 0.02500677 0.31396656 0.41199128 14.5152 < 2.2e-16
## Intercept42 0.46488766 0.02397583 0.41789591 0.51187942 19.3898 < 2.2e-16
## Intercept43 0.30610506 0.03950062 0.22868527 0.38352485  7.7494 9.326e-15
## Intercept44 0.21569603 0.05177750 0.11421401 0.31717806  4.1658 3.102e-05
## Intercept45 0.44021906 0.02709357 0.38711664 0.49332149 16.2481 < 2.2e-16
## Tau2_1_1    0.01071138 0.00402001 0.00283230 0.01859046  2.6645 0.0077100
## Tau2_2_2    0.00524199 0.00210861 0.00110920 0.00937479  2.4860 0.0129189
## Tau2_3_3    0.00447399 0.00192034 0.00071020 0.00823778  2.3298 0.0198169
## Tau2_4_4    0.01744201 0.00653040 0.00464267 0.03024136  2.6709 0.0075649
## Tau2_5_5    0.00516805 0.00203710 0.00117540 0.00916070  2.5370 0.0111819
## Tau2_6_6    0.01078929 0.00404608 0.00285913 0.01871946  2.6666 0.0076621
## Tau2_7_7    0.03634258 0.01316406 0.01054149 0.06214368  2.7607 0.0057670
## Tau2_8_8    0.00977142 0.00372093 0.00247854 0.01706430  2.6261 0.0086376
## Tau2_9_9    0.01083363 0.00403794 0.00291941 0.01874785  2.6830 0.0072974
## Tau2_10_10  0.00558500 0.00216826 0.00133528 0.00983472  2.5758 0.0100010
## Tau2_11_11  0.00422684 0.00171915 0.00085737 0.00759631  2.4587 0.0139449
## Tau2_12_12  0.00679010 0.00262815 0.00163902 0.01194118  2.5836 0.0097774
## Tau2_13_13  0.01315428 0.00485512 0.00363842 0.02267013  2.7094 0.0067413
## Tau2_14_14  0.00850214 0.00314483 0.00233838 0.01466590  2.7035 0.0068608
## Tau2_15_15  0.02773193 0.01021927 0.00770254 0.04776133  2.7137 0.0066538
## Tau2_16_16  0.00850975 0.00333747 0.00196843 0.01505107  2.5498 0.0107797
## Tau2_17_17  0.00821659 0.00305319 0.00223244 0.01420073  2.6911 0.0071207
## Tau2_18_18  0.00338446 0.00145460 0.00053350 0.00623541  2.3267 0.0199795
## Tau2_19_19  0.00932954 0.00352314 0.00242432 0.01623476  2.6481 0.0080951
## Tau2_20_20  0.00575029 0.00228891 0.00126411 0.01023647  2.5122 0.0119968
## Tau2_21_21  0.01736799 0.00642856 0.00476825 0.02996773  2.7017 0.0068987
## Tau2_22_22  0.02277622 0.00850873 0.00609941 0.03945303  2.6768 0.0074328
## Tau2_23_23  0.00593480 0.00227128 0.00148316 0.01038643  2.6130 0.0089759
## Tau2_24_24  0.00606894 0.00247650 0.00121509 0.01092279  2.4506 0.0142613
## Tau2_25_25  0.00799377 0.00316932 0.00178202 0.01420553  2.5222 0.0116612
## Tau2_26_26  0.00382339 0.00157930 0.00072803 0.00691876  2.4209 0.0154802
## Tau2_27_27  0.01601131 0.00602983 0.00419307 0.02782955  2.6554 0.0079226
## Tau2_28_28  0.02823315 0.01040531 0.00783911 0.04862719  2.7133 0.0066609
## Tau2_29_29  0.00681373 0.00266925 0.00158211 0.01204536  2.5527 0.0106897
## Tau2_30_30  0.00222426 0.00103971 0.00018647 0.00426205  2.1393 0.0324104
## Tau2_31_31  0.00547458 0.00222236 0.00111884 0.00983033  2.4634 0.0137623
## Tau2_32_32  0.01236863 0.00466228 0.00323073 0.02150654  2.6529 0.0079800
## Tau2_33_33  0.01623386 0.00615825 0.00416390 0.02830381  2.6361 0.0083862
## Tau2_34_34  0.02364842 0.00863940 0.00671551 0.04058134  2.7373 0.0061950
## Tau2_35_35  0.01204409 0.00456957 0.00308790 0.02100028  2.6357 0.0083960
## Tau2_36_36  0.00617154 0.00234380 0.00157778 0.01076530  2.6331 0.0084601
## Tau2_37_37  0.02937964 0.01085048 0.00811308 0.05064619  2.7077 0.0067755
## Tau2_38_38  0.00499666 0.00206105 0.00095708 0.00903625  2.4243 0.0153367
## Tau2_39_39  0.00489560 0.00187797 0.00121485 0.00857635  2.6069 0.0091376
## Tau2_40_40  0.02038488 0.00752299 0.00564008 0.03512967  2.7097 0.0067349
## Tau2_41_41  0.00890503 0.00329829 0.00244050 0.01536957  2.6999 0.0069362
## Tau2_42_42  0.00829893 0.00308997 0.00224270 0.01435516  2.6858 0.0072364
## Tau2_43_43  0.02352135 0.00867536 0.00651796 0.04052474  2.7113 0.0067023
## Tau2_44_44  0.04136941 0.01500004 0.01196987 0.07076895  2.7580 0.0058165
## Tau2_45_45  0.01075401 0.00404531 0.00282536 0.01868266  2.6584 0.0078514
##                
## Intercept1  ***
## Intercept2  ***
## Intercept3  ***
## Intercept4  ***
## Intercept5  ***
## Intercept6  ***
## Intercept7  ***
## Intercept8  ***
## Intercept9  ***
## Intercept10 ***
## Intercept11 ***
## Intercept12 ***
## Intercept13 ***
## Intercept14 ***
## Intercept15 ***
## Intercept16 ***
## Intercept17 ***
## Intercept18 ***
## Intercept19 ***
## Intercept20 ***
## Intercept21 ***
## Intercept22 ***
## Intercept23 ***
## Intercept24 ***
## Intercept25 ***
## Intercept26 ***
## Intercept27 ***
## Intercept28 ** 
## Intercept29 ***
## Intercept30 ***
## Intercept31 ***
## Intercept32 ***
## Intercept33 ***
## Intercept34 ***
## Intercept35 ***
## Intercept36 ***
## Intercept37 ***
## Intercept38 ***
## Intercept39 ***
## Intercept40 ***
## Intercept41 ***
## Intercept42 ***
## Intercept43 ***
## Intercept44 ***
## Intercept45 ***
## Tau2_1_1    ** 
## Tau2_2_2    *  
## Tau2_3_3    *  
## Tau2_4_4    ** 
## Tau2_5_5    *  
## Tau2_6_6    ** 
## Tau2_7_7    ** 
## Tau2_8_8    ** 
## Tau2_9_9    ** 
## Tau2_10_10  *  
## Tau2_11_11  *  
## Tau2_12_12  ** 
## Tau2_13_13  ** 
## Tau2_14_14  ** 
## Tau2_15_15  ** 
## Tau2_16_16  *  
## Tau2_17_17  ** 
## Tau2_18_18  *  
## Tau2_19_19  ** 
## Tau2_20_20  *  
## Tau2_21_21  ** 
## Tau2_22_22  ** 
## Tau2_23_23  ** 
## Tau2_24_24  *  
## Tau2_25_25  *  
## Tau2_26_26  *  
## Tau2_27_27  ** 
## Tau2_28_28  ** 
## Tau2_29_29  *  
## Tau2_30_30  *  
## Tau2_31_31  *  
## Tau2_32_32  ** 
## Tau2_33_33  ** 
## Tau2_34_34  ** 
## Tau2_35_35  ** 
## Tau2_36_36  ** 
## Tau2_37_37  ** 
## Tau2_38_38  *  
## Tau2_39_39  ** 
## Tau2_40_40  ** 
## Tau2_41_41  ** 
## Tau2_42_42  ** 
## Tau2_43_43  ** 
## Tau2_44_44  ** 
## Tau2_45_45  ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 14909.19
## Degrees of freedom of the Q statistic: 675
## P value of the Q statistic: 0
## 
## Heterogeneity indices (based on the estimated Tau2):
##                               Estimate
## Intercept1: I2 (Q statistic)    0.9656
## Intercept2: I2 (Q statistic)    0.9347
## Intercept3: I2 (Q statistic)    0.9185
## Intercept4: I2 (Q statistic)    0.9784
## Intercept5: I2 (Q statistic)    0.9279
## Intercept6: I2 (Q statistic)    0.9689
## Intercept7: I2 (Q statistic)    0.9866
## Intercept8: I2 (Q statistic)    0.9654
## Intercept9: I2 (Q statistic)    0.9811
## Intercept10: I2 (Q statistic)   0.9282
## Intercept11: I2 (Q statistic)   0.9022
## Intercept12: I2 (Q statistic)   0.9534
## Intercept13: I2 (Q statistic)   0.9874
## Intercept14: I2 (Q statistic)   0.9541
## Intercept15: I2 (Q statistic)   0.9847
## Intercept16: I2 (Q statistic)   0.9666
## Intercept17: I2 (Q statistic)   0.9582
## Intercept18: I2 (Q statistic)   0.9082
## Intercept19: I2 (Q statistic)   0.9571
## Intercept20: I2 (Q statistic)   0.9262
## Intercept21: I2 (Q statistic)   0.9864
## Intercept22: I2 (Q statistic)   0.9769
## Intercept23: I2 (Q statistic)   0.9344
## Intercept24: I2 (Q statistic)   0.9486
## Intercept25: I2 (Q statistic)   0.9450
## Intercept26: I2 (Q statistic)   0.8907
## Intercept27: I2 (Q statistic)   0.9783
## Intercept28: I2 (Q statistic)   0.9810
## Intercept29: I2 (Q statistic)   0.9394
## Intercept30: I2 (Q statistic)   0.8546
## Intercept31: I2 (Q statistic)   0.9399
## Intercept32: I2 (Q statistic)   0.9676
## Intercept33: I2 (Q statistic)   0.9720
## Intercept34: I2 (Q statistic)   0.9875
## Intercept35: I2 (Q statistic)   0.9695
## Intercept36: I2 (Q statistic)   0.9333
## Intercept37: I2 (Q statistic)   0.9854
## Intercept38: I2 (Q statistic)   0.9411
## Intercept39: I2 (Q statistic)   0.9264
## Intercept40: I2 (Q statistic)   0.9753
## Intercept41: I2 (Q statistic)   0.9583
## Intercept42: I2 (Q statistic)   0.9642
## Intercept43: I2 (Q statistic)   0.9808
## Intercept44: I2 (Q statistic)   0.9884
## Intercept45: I2 (Q statistic)   0.9693
## 
## Number of studies (or clusters): 16
## Number of observed statistics: 720
## Number of estimated parameters: 90
## Degrees of freedom: 630
## -2 log likelihood: -1115.203 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

```r
summary(stage1_g2.fit)
```

```
## 
## Call:
## meta(y = ES, v = acovR, RE.constraints = Diag(paste0(RE.startvalues, 
##     "*Tau2_", 1:no.es, "_", 1:no.es)), RE.lbound = RE.lbound, 
##     I2 = I2, model.name = model.name, suppressWarnings = TRUE, 
##     silent = silent, run = run)
## 
## 95% confidence intervals: z statistic approximation
## Coefficients:
##               Estimate  Std.Error     lbound     ubound z value  Pr(>|z|)
## Intercept1  0.45259119 0.01574490 0.42173175 0.48345062 28.7453 < 2.2e-16
## Intercept2  0.43684172 0.01924377 0.39912462 0.47455882 22.7004 < 2.2e-16
## Intercept3  0.38096623 0.02095458 0.33989602 0.42203644 18.1806 < 2.2e-16
## Intercept4  0.39632829 0.01974205 0.35763458 0.43502200 20.0753 < 2.2e-16
## Intercept5  0.43659380 0.00917316 0.41861474 0.45457287 47.5947 < 2.2e-16
## Intercept6  0.44828365 0.02312261 0.40296417 0.49360313 19.3872 < 2.2e-16
## Intercept7  0.38103930 0.01694213 0.34783333 0.41424528 22.4906 < 2.2e-16
## Intercept8  0.46421686 0.01571545 0.43341515 0.49501857 29.5389 < 2.2e-16
## Intercept9  0.66187609 0.02395608 0.61492304 0.70882914 27.6287 < 2.2e-16
## Intercept10 0.32824835 0.01727698 0.29438610 0.36211061 18.9992 < 2.2e-16
## Intercept11 0.28549532 0.01727298 0.25164090 0.31934974 16.5284 < 2.2e-16
## Intercept12 0.46107342 0.01340570 0.43479872 0.48734811 34.3938 < 2.2e-16
## Intercept13 0.66907181 0.01607814 0.63755924 0.70058438 41.6138 < 2.2e-16
## Intercept14 0.36845725 0.01940718 0.33041987 0.40649463 18.9856 < 2.2e-16
## Intercept15 0.47021797 0.01292433 0.44488675 0.49554919 36.3824 < 2.2e-16
## Intercept16 0.55896826 0.01058251 0.53822692 0.57970960 52.8200 < 2.2e-16
## Intercept17 0.46372400 0.01408537 0.43611717 0.49133083 32.9224 < 2.2e-16
## Intercept18 0.48639878 0.01576873 0.45549264 0.51730492 30.8458 < 2.2e-16
## Intercept19 0.36191329 0.01952116 0.32365252 0.40017406 18.5395 < 2.2e-16
## Intercept20 0.30950351 0.01213457 0.28572019 0.33328683 25.5059 < 2.2e-16
## Intercept21 0.55026195 0.03193285 0.48767472 0.61284919 17.2318 < 2.2e-16
## Intercept22 0.24997434 0.01243588 0.22560046 0.27434822 20.1011 < 2.2e-16
## Intercept23 0.33964993 0.01495441 0.31033983 0.36896003 22.7124 < 2.2e-16
## Intercept24 0.45903486 0.01508592 0.42946700 0.48860271 30.4280 < 2.2e-16
## Intercept25 0.30407739 0.01531058 0.27406920 0.33408558 19.8606 < 2.2e-16
## Intercept26 0.27912787 0.01623619 0.24730551 0.31095022 17.1917 < 2.2e-16
## Intercept27 0.41464459 0.02121022 0.37307331 0.45621586 19.5493 < 2.2e-16
## Intercept28 0.21615078 0.01184356 0.19293782 0.23936374 18.2505 < 2.2e-16
## Intercept29 0.28709183 0.01718263 0.25341450 0.32076916 16.7083 < 2.2e-16
## Intercept30 0.38605120 0.01637177 0.35396311 0.41813929 23.5803 < 2.2e-16
## Intercept31 0.46548587 0.01224555 0.44148503 0.48948671 38.0127 < 2.2e-16
## Intercept32 0.36126973 0.01780326 0.32637599 0.39616347 20.2923 < 2.2e-16
## Intercept33 0.38905849 0.00947605 0.37048576 0.40763121 41.0570 < 2.2e-16
## Intercept34 0.49230467 0.01531797 0.46228200 0.52232733 32.1390 < 2.2e-16
## Intercept35 0.39232400 0.01675993 0.35947514 0.42517286 23.4085 < 2.2e-16
## Intercept36 0.34441785 0.01271484 0.31949722 0.36933848 27.0879 < 2.2e-16
## Intercept37 0.46358539 0.01451396 0.43513855 0.49203222 31.9407 < 2.2e-16
## Intercept38 0.55994275 0.00959118 0.54114439 0.57874111 58.3810 < 2.2e-16
## Intercept39 0.45220750 0.00867050 0.43521363 0.46920137 52.1547 < 2.2e-16
## Intercept40 0.29470571 0.02008718 0.25533556 0.33407585 14.6713 < 2.2e-16
## Intercept41 0.38424723 0.01833454 0.34831219 0.42018228 20.9576 < 2.2e-16
## Intercept42 0.48964019 0.01774693 0.45485684 0.52442354 27.5901 < 2.2e-16
## Intercept43 0.47182809 0.01202383 0.44826181 0.49539437 39.2411 < 2.2e-16
## Intercept44 0.42644895 0.01955401 0.38812379 0.46477410 21.8088 < 2.2e-16
## Intercept45 0.50090513 0.01294312 0.47553708 0.52627319 38.7005 < 2.2e-16
## Tau2_1_1    0.00386957 0.00139175 0.00114179 0.00659735  2.7804  0.005430
## Tau2_2_2    0.00597739 0.00208778 0.00188542 0.01006935  2.8630  0.004196
## Tau2_3_3    0.00715106 0.00248272 0.00228502 0.01201710  2.8803  0.003973
## Tau2_4_4    0.00632987 0.00219848 0.00202094 0.01063881  2.8792  0.003987
## Tau2_5_5    0.00105283 0.00044340 0.00018379 0.00192188  2.3745  0.017574
## Tau2_6_6    0.00890460 0.00307523 0.00287727 0.01493194  2.8956  0.003784
## Tau2_7_7    0.00449414 0.00160023 0.00135774 0.00763053  2.8084  0.004978
## Tau2_8_8    0.00387541 0.00136209 0.00120576 0.00654507  2.8452  0.004439
## Tau2_9_9    0.00989314 0.00337410 0.00328004 0.01650625  2.9321  0.003367
## Tau2_10_10  0.00464463 0.00166738 0.00137663 0.00791263  2.7856  0.005343
## Tau2_11_11  0.00462153 0.00166724 0.00135380 0.00788927  2.7720  0.005572
## Tau2_12_12  0.00265685 0.00100884 0.00067956 0.00463414  2.6336  0.008449
## Tau2_13_13  0.00426619 0.00151295 0.00130086 0.00723152  2.8198  0.004806
## Tau2_14_14  0.00606032 0.00212769 0.00189014 0.01023051  2.8483  0.004395
## Tau2_15_15  0.00244249 0.00089822 0.00068200 0.00420297  2.7192  0.006543
## Tau2_16_16  0.00159221 0.00061816 0.00038063 0.00280378  2.5757  0.010003
## Tau2_17_17  0.00304256 0.00109351 0.00089933 0.00518579  2.7824  0.005396
## Tau2_18_18  0.00385935 0.00138206 0.00115056 0.00656814  2.7925  0.005231
## Tau2_19_19  0.00611546 0.00224607 0.00171323 0.01051768  2.7227  0.006474
## Tau2_20_20  0.00202109 0.00077246 0.00050709 0.00353508  2.6164  0.008885
## Tau2_21_21  0.01766560 0.00602098 0.00586470 0.02946650  2.9340  0.003346
## Tau2_22_22  0.00211280 0.00081799 0.00050957 0.00371603  2.5829  0.009797
## Tau2_23_23  0.00335963 0.00121992 0.00096862 0.00575063  2.7540  0.005888
## Tau2_24_24  0.00350624 0.00125013 0.00105604 0.00595644  2.8047  0.005036
## Tau2_25_25  0.00349698 0.00131369 0.00092219 0.00607178  2.6619  0.007769
## Tau2_26_26  0.00402183 0.00147687 0.00112721 0.00691645  2.7232  0.006465
## Tau2_27_27  0.00733879 0.00257683 0.00228830 0.01238928  2.8480  0.004400
## Tau2_28_28  0.00185163 0.00073590 0.00040930 0.00329396  2.5162  0.011864
## Tau2_29_29  0.00457893 0.00165444 0.00133629 0.00782156  2.7677  0.005646
## Tau2_30_30  0.00415769 0.00148189 0.00125324 0.00706214  2.8057  0.005021
## Tau2_31_31  0.00218966 0.00089892 0.00042782 0.00395151  2.4359  0.014855
## Tau2_32_32  0.00498728 0.00175723 0.00154318 0.00843139  2.8382  0.004538
## Tau2_33_33  0.00110323 0.00045261 0.00021612 0.00199034  2.4375  0.014791
## Tau2_34_34  0.00367604 0.00130016 0.00112778 0.00622431  2.8274  0.004693
## Tau2_35_35  0.00442314 0.00157322 0.00133968 0.00750659  2.8115  0.004931
## Tau2_36_36  0.00229146 0.00084951 0.00062645 0.00395647  2.6974  0.006988
## Tau2_37_37  0.00317693 0.00123437 0.00075760 0.00559625  2.5737  0.010061
## Tau2_38_38  0.00127289 0.00049938 0.00029413 0.00225166  2.5489  0.010805
## Tau2_39_39  0.00093722 0.00040016 0.00015293 0.00172152  2.3421  0.019174
## Tau2_40_40  0.00646955 0.00226568 0.00202889 0.01091021  2.8555  0.004298
## Tau2_41_41  0.00537046 0.00188049 0.00168477 0.00905615  2.8559  0.004292
## Tau2_42_42  0.00506080 0.00176898 0.00159366 0.00852794  2.8609  0.004225
## Tau2_43_43  0.00208101 0.00080287 0.00050741 0.00365461  2.5920  0.009543
## Tau2_44_44  0.00624521 0.00216685 0.00199827 0.01049216  2.8822  0.003950
## Tau2_45_45  0.00253720 0.00092872 0.00071694 0.00435746  2.7319  0.006296
##                
## Intercept1  ***
## Intercept2  ***
## Intercept3  ***
## Intercept4  ***
## Intercept5  ***
## Intercept6  ***
## Intercept7  ***
## Intercept8  ***
## Intercept9  ***
## Intercept10 ***
## Intercept11 ***
## Intercept12 ***
## Intercept13 ***
## Intercept14 ***
## Intercept15 ***
## Intercept16 ***
## Intercept17 ***
## Intercept18 ***
## Intercept19 ***
## Intercept20 ***
## Intercept21 ***
## Intercept22 ***
## Intercept23 ***
## Intercept24 ***
## Intercept25 ***
## Intercept26 ***
## Intercept27 ***
## Intercept28 ***
## Intercept29 ***
## Intercept30 ***
## Intercept31 ***
## Intercept32 ***
## Intercept33 ***
## Intercept34 ***
## Intercept35 ***
## Intercept36 ***
## Intercept37 ***
## Intercept38 ***
## Intercept39 ***
## Intercept40 ***
## Intercept41 ***
## Intercept42 ***
## Intercept43 ***
## Intercept44 ***
## Intercept45 ***
## Tau2_1_1    ** 
## Tau2_2_2    ** 
## Tau2_3_3    ** 
## Tau2_4_4    ** 
## Tau2_5_5    *  
## Tau2_6_6    ** 
## Tau2_7_7    ** 
## Tau2_8_8    ** 
## Tau2_9_9    ** 
## Tau2_10_10  ** 
## Tau2_11_11  ** 
## Tau2_12_12  ** 
## Tau2_13_13  ** 
## Tau2_14_14  ** 
## Tau2_15_15  ** 
## Tau2_16_16  *  
## Tau2_17_17  ** 
## Tau2_18_18  ** 
## Tau2_19_19  ** 
## Tau2_20_20  ** 
## Tau2_21_21  ** 
## Tau2_22_22  ** 
## Tau2_23_23  ** 
## Tau2_24_24  ** 
## Tau2_25_25  ** 
## Tau2_26_26  ** 
## Tau2_27_27  ** 
## Tau2_28_28  *  
## Tau2_29_29  ** 
## Tau2_30_30  ** 
## Tau2_31_31  *  
## Tau2_32_32  ** 
## Tau2_33_33  *  
## Tau2_34_34  ** 
## Tau2_35_35  ** 
## Tau2_36_36  ** 
## Tau2_37_37  *  
## Tau2_38_38  *  
## Tau2_39_39  *  
## Tau2_40_40  ** 
## Tau2_41_41  ** 
## Tau2_42_42  ** 
## Tau2_43_43  ** 
## Tau2_44_44  ** 
## Tau2_45_45  ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 11626.22
## Degrees of freedom of the Q statistic: 765
## P value of the Q statistic: 0
## 
## Heterogeneity indices (based on the estimated Tau2):
##                               Estimate
## Intercept1: I2 (Q statistic)    0.9510
## Intercept2: I2 (Q statistic)    0.9658
## Intercept3: I2 (Q statistic)    0.9686
## Intercept4: I2 (Q statistic)    0.9666
## Intercept5: I2 (Q statistic)    0.8408
## Intercept6: I2 (Q statistic)    0.9770
## Intercept7: I2 (Q statistic)    0.9527
## Intercept8: I2 (Q statistic)    0.9525
## Intercept9: I2 (Q statistic)    0.9884
## Intercept10: I2 (Q statistic)   0.9493
## Intercept11: I2 (Q statistic)   0.9469
## Intercept12: I2 (Q statistic)   0.9319
## Intercept13: I2 (Q statistic)   0.9754
## Intercept14: I2 (Q statistic)   0.9628
## Intercept15: I2 (Q statistic)   0.9271
## Intercept16: I2 (Q statistic)   0.9139
## Intercept17: I2 (Q statistic)   0.9414
## Intercept18: I2 (Q statistic)   0.9542
## Intercept19: I2 (Q statistic)   0.9636
## Intercept20: I2 (Q statistic)   0.8898
## Intercept21: I2 (Q statistic)   0.9900
## Intercept22: I2 (Q statistic)   0.8881
## Intercept23: I2 (Q statistic)   0.9335
## Intercept24: I2 (Q statistic)   0.9467
## Intercept25: I2 (Q statistic)   0.9336
## Intercept26: I2 (Q statistic)   0.9397
## Intercept27: I2 (Q statistic)   0.9709
## Intercept28: I2 (Q statistic)   0.8712
## Intercept29: I2 (Q statistic)   0.9473
## Intercept30: I2 (Q statistic)   0.9488
## Intercept31: I2 (Q statistic)   0.9224
## Intercept32: I2 (Q statistic)   0.9558
## Intercept33: I2 (Q statistic)   0.8330
## Intercept34: I2 (Q statistic)   0.9531
## Intercept35: I2 (Q statistic)   0.9537
## Intercept36: I2 (Q statistic)   0.9060
## Intercept37: I2 (Q statistic)   0.9417
## Intercept38: I2 (Q statistic)   0.8981
## Intercept39: I2 (Q statistic)   0.8324
## Intercept40: I2 (Q statistic)   0.9618
## Intercept41: I2 (Q statistic)   0.9599
## Intercept42: I2 (Q statistic)   0.9643
## Intercept43: I2 (Q statistic)   0.9180
## Intercept44: I2 (Q statistic)   0.9684
## Intercept45: I2 (Q statistic)   0.9372
## 
## Number of studies (or clusters): 18
## Number of observed statistics: 810
## Number of estimated parameters: 90
## Degrees of freedom: 720
## -2 log likelihood: -1932.122 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

### Fitting the Stage 2 bifactor model in both subgroups

```r
## Stage 2 analysis per subgroup (random-effect analysis)
stage2_g1.fit <- tssem2(stage1_g1.fit, Amatrix=RAM2$A, Smatrix=RAM2$S, Fmatrix=RAM2$F)
stage2_g2.fit <- tssem2(stage1_g2.fit, Amatrix=RAM2$A, Smatrix=RAM2$S, Fmatrix=RAM2$F)

summary(stage2_g1.fit)
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
## g1   0.703309  0.021985  0.660219  0.746399 31.9904 < 2.2e-16 ***
## p1  -0.040534  0.068556 -0.174901  0.093833 -0.5913    0.5543    
## g10  0.785613  0.024000  0.738573  0.832652 32.7336 < 2.2e-16 ***
## p10 -0.038489  0.079228 -0.193772  0.116794 -0.4858    0.6271    
## g2   0.476162  0.020282  0.436409  0.515914 23.4770 < 2.2e-16 ***
## n2   0.621038  0.039422  0.543773  0.698303 15.7538 < 2.2e-16 ***
## g3   0.603297  0.029138  0.546188  0.660406 20.7050 < 2.2e-16 ***
## p3   0.475223  0.119782  0.240454  0.709993  3.9674 7.267e-05 ***
## g4   0.548822  0.019713  0.510186  0.587459 27.8409 < 2.2e-16 ***
## p4   0.264587  0.063194  0.140729  0.388445  4.1869 2.828e-05 ***
## g5   0.488373  0.023388  0.442534  0.534213 20.8814 < 2.2e-16 ***
## n5   0.364434  0.033167  0.299428  0.429441 10.9878 < 2.2e-16 ***
## g6   0.452088  0.017943  0.416921  0.487256 25.1958 < 2.2e-16 ***
## n6   0.612245  0.037455  0.538835  0.685654 16.3463 < 2.2e-16 ***
## g7   0.610677  0.024081  0.563479  0.657875 25.3592 < 2.2e-16 ***
## p7   0.312816  0.077039  0.161822  0.463811  4.0605 4.897e-05 ***
## g8   0.254635  0.031566  0.192767  0.316503  8.0668 6.661e-16 ***
## n8   0.401006  0.050727  0.301583  0.500429  7.9052 2.665e-15 ***
## g9   0.573783  0.022536  0.529614  0.617953 25.4610 < 2.2e-16 ***
## n9   0.374163  0.034294  0.306948  0.441377 10.9105 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Goodness-of-fit indices:
##                                                 Value
## Sample size                                39984.0000
## Chi-square of target model                    10.4229
## DF of target model                            25.0000
## p value of target model                        0.9953
## Number of constraints imposed on "Smatrix"     0.0000
## DF manually adjusted                           0.0000
## Chi-square of independence model            6506.6067
## DF of independence model                      45.0000
## RMSEA                                          0.0000
## RMSEA lower 95% CI                             0.0000
## RMSEA upper 95% CI                             0.0000
## SRMR                                           0.0147
## TLI                                            1.0041
## CFI                                            1.0000
## AIC                                          -39.5771
## BIC                                         -254.4830
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

```r
summary(stage2_g2.fit)
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
## g1   0.780511  0.015989  0.749173  0.811850 48.8141 < 2.2e-16 ***
## p1  -0.023204  0.047416 -0.116138  0.069731 -0.4894    0.6246    
## g10  0.811451  0.016224  0.779653  0.843250 50.0160 < 2.2e-16 ***
## p10 -0.013344  0.045178 -0.101891  0.075203 -0.2954    0.7677    
## g2   0.572566  0.014856  0.543449  0.601683 38.5414 < 2.2e-16 ***
## n2   0.541137  0.023866  0.494361  0.587913 22.6742 < 2.2e-16 ***
## g3   0.569016  0.018897  0.531979  0.606052 30.1120 < 2.2e-16 ***
## p3   0.601699  0.084431  0.436216  0.767181  7.1265 1.030e-12 ***
## g4   0.490944  0.014440  0.462642  0.519246 33.9988 < 2.2e-16 ***
## p4   0.342473  0.045610  0.253079  0.431868  7.5087 5.973e-14 ***
## g5   0.549628  0.016325  0.517632  0.581624 33.6682 < 2.2e-16 ***
## n5   0.305603  0.026085  0.254477  0.356729 11.7156 < 2.2e-16 ***
## g6   0.555869  0.012223  0.531912  0.579825 45.4780 < 2.2e-16 ***
## n6   0.562852  0.021262  0.521179  0.604526 26.4717 < 2.2e-16 ***
## g7   0.619682  0.014714  0.590843  0.648521 42.1148 < 2.2e-16 ***
## p7   0.328312  0.045099  0.239919  0.416704  7.2798 3.344e-13 ***
## g8   0.479939  0.015573  0.449417  0.510461 30.8191 < 2.2e-16 ***
## n8   0.385427  0.024792  0.336835  0.434019 15.5463 < 2.2e-16 ***
## g9   0.611708  0.015837  0.580668  0.642749 38.6243 < 2.2e-16 ***
## n9   0.405911  0.025352  0.356222  0.455599 16.0112 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Goodness-of-fit indices:
##                                                 Value
## Sample size                                64700.0000
## Chi-square of target model                    69.7364
## DF of target model                            25.0000
## p value of target model                        0.0000
## Number of constraints imposed on "Smatrix"     0.0000
## DF manually adjusted                           0.0000
## Chi-square of independence model           22862.0387
## DF of independence model                      45.0000
## RMSEA                                          0.0053
## RMSEA lower 95% CI                             0.0038
## RMSEA upper 95% CI                             0.0067
## SRMR                                           0.0199
## TLI                                            0.9965
## CFI                                            0.9980
## AIC                                           19.7364
## BIC                                         -207.2015
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values indicate problems.)
```

# OSMASEM
## One general factor model without any moderator

```r
## Create matrices with implicit diagonal constraints
M0a <- create.vechsR(A0=RAM1$A, S0=RAM1$S, F0=RAM1$F)

## Create heterogeneity variances
T0a <- create.Tau2(RAM=RAM1, RE.type="Diag", Transform="expLog", RE.startvalues=0.05)

fit0a <- osmasem(model.name="No moderator", Mmatrix=M0a, Tmatrix=T0a, data=my.df)
```

```
## Running No moderator with 55 parameters
```

```r
summary(fit0a, Saturated=TRUE)
```

```
## Summary of No moderator 
##  
## free parameters:
##       name  matrix row col   Estimate  Std.Error A    z value Pr(>|z|)
## 1       g1      A0  I1   G  0.6910146 0.01493326    46.273518        0
## 2       g2      A0  I2   G  0.6461911 0.01717243    37.629555        0
## 3       g3      A0  I3   G  0.5589421 0.01440829    38.793104        0
## 4       g4      A0  I4   G  0.4978956 0.01274545    39.064574        0
## 5       g5      A0  I5   G  0.6500574 0.01499549    43.350194        0
## 6       g6      A0  I6   G  0.6029515 0.01618965    37.243032        0
## 7       g7      A0  I7   G  0.5862808 0.01402788    41.793981        0
## 8       g8      A0  I8   G  0.5099272 0.01744019    29.238630        0
## 9       g9      A0  I9   G  0.6908699 0.01721646    40.128443        0
## 10     g10      A0 I10   G  0.7437415 0.01590130    46.772379        0
## 11  Tau1_1 vecTau1   1   1 -2.2540572 0.13500212   -16.696458        0
## 12  Tau1_2 vecTau1   2   1 -2.4153159 0.14609023   -16.533042        0
## 13  Tau1_3 vecTau1   3   1 -2.4006356 0.14378450   -16.696067        0
## 14  Tau1_4 vecTau1   4   1 -1.9794119 0.13932893   -14.206755        0
## 15  Tau1_5 vecTau1   5   1 -2.6637284 0.14125756   -18.857245        0
## 16  Tau1_6 vecTau1   6   1 -2.2157915 0.13292916   -16.668964        0
## 17  Tau1_7 vecTau1   7   1 -1.8032872 0.12666363   -14.236819        0
## 18  Tau1_8 vecTau1   8   1 -2.3426847 0.13766690   -17.017052        0
## 19  Tau1_9 vecTau1   9   1 -1.8633459 0.14719647   -12.658904        0
## 20 Tau1_10 vecTau1  10   1 -2.4060740 0.14030222   -17.149222        0
## 21 Tau1_11 vecTau1  11   1 -2.5329964 0.13978957   -18.120066        0
## 22 Tau1_12 vecTau1  12   1 -2.4914650 0.16332941   -15.254234        0
## 23 Tau1_13 vecTau1  13   1 -1.2787915 0.13556970    -9.432723        0
## 24 Tau1_14 vecTau1  14   1 -2.2868096 0.13178280   -17.352869        0
## 25 Tau1_15 vecTau1  15   1 -1.8106300 0.13695466   -13.220652        0
## 26 Tau1_16 vecTau1  16   1 -2.1126515 0.17695897   -11.938652        0
## 27 Tau1_17 vecTau1  17   1 -2.3882475 0.13771921   -17.341426        0
## 28 Tau1_18 vecTau1  18   1 -1.5822147 0.13327063   -11.872193        0
## 29 Tau1_19 vecTau1  19   1 -2.3555493 0.13096005   -17.986778        0
## 30 Tau1_20 vecTau1  20   1 -2.5788502 0.14879477   -17.331592        0
## 31 Tau1_21 vecTau1  21   1 -1.3839097 0.12974976   -10.665991        0
## 32 Tau1_22 vecTau1  22   1 -1.9675084 0.13547394   -14.523151        0
## 33 Tau1_23 vecTau1  23   1 -2.5222944 0.13808081   -18.266799        0
## 34 Tau1_24 vecTau1  24   1 -2.4153295 0.16427822   -14.702677        0
## 35 Tau1_25 vecTau1  25   1 -2.4614378 0.13874697   -17.740479        0
## 36 Tau1_26 vecTau1  26   1 -2.6581247 0.14122745   -18.821587        0
## 37 Tau1_27 vecTau1  27   1 -1.7847396 0.13428920   -13.290269        0
## 38 Tau1_28 vecTau1  28   1 -1.9568452 0.13224274   -14.797374        0
## 39 Tau1_29 vecTau1  29   1 -2.4297498 0.13878293   -17.507556        0
## 40 Tau1_30 vecTau1  30   1 -2.6107853 0.15599504   -16.736336        0
## 41 Tau1_31 vecTau1  31   1 -2.3704596 0.17266746   -13.728467        0
## 42 Tau1_32 vecTau1  32   1 -2.2676707 0.13369238   -16.961855        0
## 43 Tau1_33 vecTau1  33   1 -2.2955685 0.13477772   -17.032256        0
## 44 Tau1_34 vecTau1  34   1 -2.0937553 0.13356726   -15.675662        0
## 45 Tau1_35 vecTau1  35   1 -2.0313860 0.14932983   -13.603351        0
## 46 Tau1_36 vecTau1  36   1 -2.5486834 0.13732267   -18.559815        0
## 47 Tau1_37 vecTau1  37   1 -1.7406075 0.13527629   -12.867055        0
## 48 Tau1_38 vecTau1  38   1 -2.0166979 0.17471875   -11.542539        0
## 49 Tau1_39 vecTau1  39   1 -2.6621353 0.14286316   -18.634162        0
## 50 Tau1_40 vecTau1  40   1 -1.9188699 0.13083829   -14.665966        0
## 51 Tau1_41 vecTau1  41   1 -2.3943073 0.12889045   -18.576296        0
## 52 Tau1_42 vecTau1  42   1 -2.3283684 0.14701629   -15.837486        0
## 53 Tau1_43 vecTau1  43   1 -1.9370125 0.13222595   -14.649261        0
## 54 Tau1_44 vecTau1  44   1 -1.7084411 0.12601818   -13.557100        0
## 55 Tau1_45 vecTau1  45   1 -2.3856610 0.13575323   -17.573512        0
## 
## Model Statistics: 
##                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
##        Model:             55                   1475             -2050.360
##    Saturated:             90                   1440             -2647.331
## Independence:             90                   1440                    NA
## Number of observations/statistics: 104684/1530
## 
## chi-square:  χ² ( df=35 ) = 596.9713,  p = 1.983891e-103
## Information Criteria: 
##       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
## AIC:       -5000.36              -1940.360                -1940.301
## BIC:      -19099.44              -1414.631                -1589.423
## CFI: NA 
## TLI: NA   (also known as NNFI) 
## RMSEA:  0.01238463  [95% CI (0.01135531, 0.01343402)]
## Prob(RMSEA <= 0.05): 1
## To get additional fit indices, see help(mxRefModels)
## timestamp: 2018-11-08 11:29:39 
## Wall clock time: 103.3458 secs 
## optimizer:  SLSQP 
## OpenMx version number: 2.11.5 
## Need help?  See help(mxSummary)
```

```r
## SRMR
osmasemSRMR(fit0a)
```

```
## [1] 0.0821058
```

```r
## Show the heterogeneity variances
diag(VarCorr(fit0a))
```

```
##      Tau2_1      Tau2_2      Tau2_3      Tau2_4      Tau2_5      Tau2_6 
## 0.011019218 0.007981477 0.008219292 0.019085548 0.004856405 0.011895645 
##      Tau2_7      Tau2_8      Tau2_9     Tau2_10     Tau2_11     Tau2_12 
## 0.027144675 0.009229324 0.024072338 0.008130377 0.006307646 0.006853951 
##     Tau2_13     Tau2_14     Tau2_15     Tau2_16     Tau2_17     Tau2_18 
## 0.077491809 0.010320539 0.026748951 0.014620903 0.008425479 0.042238237 
##     Tau2_19     Tau2_20     Tau2_21     Tau2_22     Tau2_23     Tau2_24 
## 0.008994891 0.005754919 0.062798790 0.019545371 0.006444110 0.007981259 
##     Tau2_25     Tau2_26     Tau2_27     Tau2_28     Tau2_29     Tau2_30 
## 0.007278172 0.004911139 0.028170522 0.019966680 0.007754363 0.005398843 
##     Tau2_31     Tau2_32     Tau2_33     Tau2_34     Tau2_35     Tau2_36 
## 0.008730617 0.010723245 0.010141320 0.015184037 0.017201270 0.006112822 
##     Tau2_37     Tau2_38     Tau2_39     Tau2_40     Tau2_41     Tau2_42 
## 0.030770006 0.017714073 0.004871903 0.021542236 0.008323982 0.009497403 
##     Tau2_43     Tau2_44     Tau2_45 
## 0.020774584 0.032814584 0.008469176
```

## One general factor model with `Individualism` as a moderator on the A matrix

```r
## Replace the A matrix with the moderator "Individualism"
Ax1a <- RAM1$A
Ax1a[grep("\\*", Ax1a)] <- "0*data.Individualism"
Ax1a
```

```
##     I1  I2  I3  I4  I5  I6  I7  I8  I9  I10 G                     
## I1  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I2  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I3  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I4  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I5  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I6  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I7  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I8  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I9  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I10 "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## G   "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"
```

```r
## Create matrices with implicit diagonal constraints
M1a <- create.vechsR(A0=RAM1$A, S0=RAM1$S, F0=RAM1$F, Ax=Ax1a)

fit1a <- osmasem(model.name="Moderator with individualism", Mmatrix=M1a, Tmatrix=T0a, data=my.df)
```

```
## Running Moderator with individualism with 65 parameters
```

```r
summary(fit1a)
```

```
## Summary of Moderator with individualism 
##  
## free parameters:
##       name  matrix row col     Estimate  Std.Error A     z value
## 1       g1      A0  I1   G  0.687243594 0.01624185    42.3131253
## 2       g2      A0  I2   G  0.637173534 0.02038771    31.2528310
## 3       g3      A0  I3   G  0.558812555 0.01591108    35.1209794
## 4       g4      A0  I4   G  0.500824936 0.01345553    37.2207379
## 5       g5      A0  I5   G  0.642002120 0.01742950    36.8342278
## 6       g6      A0  I6   G  0.592497272 0.01777077    33.3411102
## 7       g7      A0  I7   G  0.583593747 0.01521931    38.3456136
## 8       g8      A0  I8   G  0.502524781 0.01699447    29.5699012
## 9       g9      A0  I9   G  0.689466481 0.02012559    34.2582080
## 10     g10      A0 I10   G  0.742171599 0.01766732    42.0081680
## 11    g1_1      A1  I1   G  0.044377795 0.01135331     3.9087977
## 12    g2_1      A1  I2   G  0.052193857 0.01227063     4.2535613
## 13    g3_1      A1  I3   G  0.009687639 0.01189364     0.8145226
## 14    g4_1      A1  I4   G -0.009776466 0.01132208    -0.8634868
## 15    g5_1      A1  I5   G  0.016021947 0.01414522     1.1326757
## 16    g6_1      A1  I6   G  0.053061765 0.01069047     4.9634649
## 17    g7_1      A1  I7   G  0.026102926 0.01221249     2.1373965
## 18    g8_1      A1  I8   G  0.141437115 0.01414679     9.9978271
## 19    g9_1      A1  I9   G  0.027557843 0.01189919     2.3159422
## 20   g10_1      A1 I10   G  0.016788462 0.01130534     1.4850034
## 21  Tau1_1 vecTau1   1   1 -2.380453932 0.13597411   -17.5066701
## 22  Tau1_2 vecTau1   2   1 -2.421511903 0.15495629   -15.6270644
## 23  Tau1_3 vecTau1   3   1 -2.402880148 0.14876440   -16.1522526
## 24  Tau1_4 vecTau1   4   1 -2.034675726 0.13972554   -14.5619460
## 25  Tau1_5 vecTau1   5   1 -2.884372619 0.14422007   -19.9998007
## 26  Tau1_6 vecTau1   6   1 -2.223746951 0.13766630   -16.1531682
## 27  Tau1_7 vecTau1   7   1 -2.005572932 0.12855123   -15.6013516
## 28  Tau1_8 vecTau1   8   1 -2.420623684 0.13920840   -17.3884887
## 29  Tau1_9 vecTau1   9   1 -1.906614533 0.16052881   -11.8770861
## 30 Tau1_10 vecTau1  10   1 -2.448767919 0.14006595   -17.4829636
## 31 Tau1_11 vecTau1  11   1 -2.560197386 0.14046998   -18.2259396
## 32 Tau1_12 vecTau1  12   1 -2.396783945 0.19484767   -12.3008088
## 33 Tau1_13 vecTau1  13   1 -1.246973401 0.13977431    -8.9213348
## 34 Tau1_14 vecTau1  14   1 -2.362411063 0.13194116   -17.9050354
## 35 Tau1_15 vecTau1  15   1 -1.898662876 0.14396525   -13.1883413
## 36 Tau1_16 vecTau1  16   1 -2.111708128 0.20519135   -10.2914089
## 37 Tau1_17 vecTau1  17   1 -2.491201788 0.13866453   -17.9656738
## 38 Tau1_18 vecTau1  18   1 -1.589937778 0.13571753   -11.7150513
## 39 Tau1_19 vecTau1  19   1 -2.371793163 0.13255708   -17.8926177
## 40 Tau1_20 vecTau1  20   1 -2.642046573 0.14785874   -17.8687206
## 41 Tau1_21 vecTau1  21   1 -1.384503952 0.13180996   -10.5037883
## 42 Tau1_22 vecTau1  22   1 -2.113307538 0.13884090   -15.2210738
## 43 Tau1_23 vecTau1  23   1 -2.533343987 0.13852574   -18.2878934
## 44 Tau1_24 vecTau1  24   1 -2.416807042 0.18171529   -13.2999656
## 45 Tau1_25 vecTau1  25   1 -2.455866228 0.14255286   -17.2277586
## 46 Tau1_26 vecTau1  26   1 -2.700584836 0.14094098   -19.1611046
## 47 Tau1_27 vecTau1  27   1 -1.789035473 0.13701147   -13.0575596
## 48 Tau1_28 vecTau1  28   1 -2.064428688 0.13446222   -15.3532252
## 49 Tau1_29 vecTau1  29   1 -2.441078999 0.13951797   -17.4965201
## 50 Tau1_30 vecTau1  30   1 -2.644653430 0.16694435   -15.8415273
## 51 Tau1_31 vecTau1  31   1 -2.268835160 0.19261503   -11.7791181
## 52 Tau1_32 vecTau1  32   1 -2.311603739 0.13444638   -17.1934994
## 53 Tau1_33 vecTau1  33   1 -2.388678646 0.14338590   -16.6590898
## 54 Tau1_34 vecTau1  34   1 -2.075528991 0.14186090   -14.6307330
## 55 Tau1_35 vecTau1  35   1 -2.072208880 0.14903275   -13.9043858
## 56 Tau1_36 vecTau1  36   1 -2.652639831 0.13756613   -19.2826515
## 57 Tau1_37 vecTau1  37   1 -1.833268207 0.14045129   -13.0526972
## 58 Tau1_38 vecTau1  38   1 -1.999904977 0.19178283   -10.4279667
## 59 Tau1_39 vecTau1  39   1 -2.861986992 0.14612442   -19.5859597
## 60 Tau1_40 vecTau1  40   1 -2.096274120 0.13343309   -15.7103014
## 61 Tau1_41 vecTau1  41   1 -2.429799285 0.12923185   -18.8018611
## 62 Tau1_42 vecTau1  42   1 -2.326588732 0.15831891   -14.6955835
## 63 Tau1_43 vecTau1  43   1 -2.156922597 0.14433722   -14.9436336
## 64 Tau1_44 vecTau1  44   1 -1.957609908 0.12804315   -15.2886739
## 65 Tau1_45 vecTau1  45   1 -2.451261435 0.13871189   -17.6716028
##        Pr(>|z|)
## 1  0.000000e+00
## 2  0.000000e+00
## 3  0.000000e+00
## 4  0.000000e+00
## 5  0.000000e+00
## 6  0.000000e+00
## 7  0.000000e+00
## 8  0.000000e+00
## 9  0.000000e+00
## 10 0.000000e+00
## 11 9.275658e-05
## 12 2.103973e-05
## 13 4.153457e-01
## 14 3.878699e-01
## 15 2.573505e-01
## 16 6.924655e-07
## 17 3.256575e-02
## 18 0.000000e+00
## 19 2.056142e-02
## 20 1.375429e-01
## 21 0.000000e+00
## 22 0.000000e+00
## 23 0.000000e+00
## 24 0.000000e+00
## 25 0.000000e+00
## 26 0.000000e+00
## 27 0.000000e+00
## 28 0.000000e+00
## 29 0.000000e+00
## 30 0.000000e+00
## 31 0.000000e+00
## 32 0.000000e+00
## 33 0.000000e+00
## 34 0.000000e+00
## 35 0.000000e+00
## 36 0.000000e+00
## 37 0.000000e+00
## 38 0.000000e+00
## 39 0.000000e+00
## 40 0.000000e+00
## 41 0.000000e+00
## 42 0.000000e+00
## 43 0.000000e+00
## 44 0.000000e+00
## 45 0.000000e+00
## 46 0.000000e+00
## 47 0.000000e+00
## 48 0.000000e+00
## 49 0.000000e+00
## 50 0.000000e+00
## 51 0.000000e+00
## 52 0.000000e+00
## 53 0.000000e+00
## 54 0.000000e+00
## 55 0.000000e+00
## 56 0.000000e+00
## 57 0.000000e+00
## 58 0.000000e+00
## 59 0.000000e+00
## 60 0.000000e+00
## 61 0.000000e+00
## 62 0.000000e+00
## 63 0.000000e+00
## 64 0.000000e+00
## 65 0.000000e+00
## 
## Model Statistics: 
##                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
##        Model:             65                   1465             -2246.388
##    Saturated:           1080                    450                    NA
## Independence:             90                   1440                    NA
## Number of observations/statistics: 104684/1530
## 
## Information Criteria: 
##       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
## AIC:      -5176.388              -2116.388                -2116.306
## BIC:     -19179.886              -1495.073                -1701.645
## To get additional fit indices, see help(mxRefModels)
## timestamp: 2018-11-08 11:41:10 
## Wall clock time: 178.8105 secs 
## optimizer:  SLSQP 
## OpenMx version number: 2.11.5 
## Need help?  See help(mxSummary)
```

```r
## Get the R2
osmasemR2(fit1a, fit0a)
```

```
## $Tau2.0
##    Tau2_1_1    Tau2_2_2    Tau2_3_3    Tau2_4_4    Tau2_5_5    Tau2_6_6 
## 0.011019218 0.007981477 0.008219292 0.019085548 0.004856405 0.011895645 
##    Tau2_7_7    Tau2_8_8    Tau2_9_9  Tau2_10_10  Tau2_11_11  Tau2_12_12 
## 0.027144675 0.009229324 0.024072338 0.008130377 0.006307646 0.006853951 
##  Tau2_13_13  Tau2_14_14  Tau2_15_15  Tau2_16_16  Tau2_17_17  Tau2_18_18 
## 0.077491809 0.010320539 0.026748951 0.014620903 0.008425479 0.042238237 
##  Tau2_19_19  Tau2_20_20  Tau2_21_21  Tau2_22_22  Tau2_23_23  Tau2_24_24 
## 0.008994891 0.005754919 0.062798790 0.019545371 0.006444110 0.007981259 
##  Tau2_25_25  Tau2_26_26  Tau2_27_27  Tau2_28_28  Tau2_29_29  Tau2_30_30 
## 0.007278172 0.004911139 0.028170522 0.019966680 0.007754363 0.005398843 
##  Tau2_31_31  Tau2_32_32  Tau2_33_33  Tau2_34_34  Tau2_35_35  Tau2_36_36 
## 0.008730617 0.010723245 0.010141320 0.015184037 0.017201270 0.006112822 
##  Tau2_37_37  Tau2_38_38  Tau2_39_39  Tau2_40_40  Tau2_41_41  Tau2_42_42 
## 0.030770006 0.017714073 0.004871903 0.021542236 0.008323982 0.009497403 
##  Tau2_43_43  Tau2_44_44  Tau2_45_45 
## 0.020774584 0.032814584 0.008469176 
## 
## $Tau2.1
##    Tau2_1_1    Tau2_2_2    Tau2_3_3    Tau2_4_4    Tau2_5_5    Tau2_6_6 
## 0.008557837 0.007883181 0.008182478 0.017088468 0.003123675 0.011707871 
##    Tau2_7_7    Tau2_8_8    Tau2_9_9  Tau2_10_10  Tau2_11_11  Tau2_12_12 
## 0.018112629 0.007897197 0.022076776 0.007464955 0.005973664 0.008282852 
##  Tau2_13_13  Tau2_14_14  Tau2_15_15  Tau2_16_16  Tau2_17_17  Tau2_18_18 
## 0.082583382 0.008872292 0.022430677 0.014648516 0.006857560 0.041590831 
##  Tau2_19_19  Tau2_20_20  Tau2_21_21  Tau2_22_22  Tau2_23_23  Tau2_24_24 
## 0.008707363 0.005071629 0.062724202 0.014601733 0.006303262 0.007957709 
##  Tau2_25_25  Tau2_26_26  Tau2_27_27  Tau2_28_28  Tau2_29_29  Tau2_30_30 
## 0.007359727 0.004511301 0.027929524 0.016101266 0.007580637 0.005045256 
##  Tau2_31_31  Tau2_32_32  Tau2_33_33  Tau2_34_34  Tau2_35_35  Tau2_36_36 
## 0.010698301 0.009821244 0.008418216 0.015747747 0.015852663 0.004965309 
##  Tau2_37_37  Tau2_38_38  Tau2_39_39  Tau2_40_40  Tau2_41_41  Tau2_42_42 
## 0.025564863 0.018319120 0.003266703 0.015107738 0.007753596 0.009531269 
##  Tau2_43_43  Tau2_44_44  Tau2_45_45 
## 0.013381994 0.019936166 0.007427820 
## 
## $R2
##    Tau2_1_1    Tau2_2_2    Tau2_3_3    Tau2_4_4    Tau2_5_5    Tau2_6_6 
## 0.223371668 0.012315484 0.004479025 0.104638355 0.356792855 0.015785083 
##    Tau2_7_7    Tau2_8_8    Tau2_9_9  Tau2_10_10  Tau2_11_11  Tau2_12_12 
## 0.332737326 0.144336314 0.082898539 0.081843914 0.052948733 0.000000000 
##  Tau2_13_13  Tau2_14_14  Tau2_15_15  Tau2_16_16  Tau2_17_17  Tau2_18_18 
## 0.000000000 0.140326710 0.161437138 0.000000000 0.186092519 0.015327489 
##  Tau2_19_19  Tau2_20_20  Tau2_21_21  Tau2_22_22  Tau2_23_23  Tau2_24_24 
## 0.031965756 0.118731382 0.001187724 0.252931394 0.021856837 0.002950672 
##  Tau2_25_25  Tau2_26_26  Tau2_27_27  Tau2_28_28  Tau2_29_29  Tau2_30_30 
## 0.000000000 0.081414516 0.008554987 0.193593230 0.022403554 0.065493024 
##  Tau2_31_31  Tau2_32_32  Tau2_33_33  Tau2_34_34  Tau2_35_35  Tau2_36_36 
## 0.000000000 0.084116402 0.169909221 0.000000000 0.078401565 0.187722255 
##  Tau2_37_37  Tau2_38_38  Tau2_39_39  Tau2_40_40  Tau2_41_41  Tau2_42_42 
## 0.169162883 0.000000000 0.329481081 0.298692223 0.068523199 0.000000000 
##  Tau2_43_43  Tau2_44_44  Tau2_45_45 
## 0.355847778 0.392460206 0.122958357
```

```r
## Compare the models with and without Individualism
anova(fit1a, fit0a)
```

```
##                           base   comparison ep  minus2LL   df       AIC
## 1 Moderator with individualism         <NA> 65 -2246.388 1465 -5176.388
## 2 Moderator with individualism No moderator 55 -2050.360 1475 -5000.360
##     diffLL diffdf            p
## 1       NA     NA           NA
## 2 196.0286     10 1.085856e-36
```

```r
## Get the estimated A0 and A1
A0 <- mxEval(A0, fit1a$mx.fit)
A0
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G
## I1   0  0  0  0  0  0  0  0  0   0 0.6872436
## I2   0  0  0  0  0  0  0  0  0   0 0.6371735
## I3   0  0  0  0  0  0  0  0  0   0 0.5588126
## I4   0  0  0  0  0  0  0  0  0   0 0.5008249
## I5   0  0  0  0  0  0  0  0  0   0 0.6420021
## I6   0  0  0  0  0  0  0  0  0   0 0.5924973
## I7   0  0  0  0  0  0  0  0  0   0 0.5835937
## I8   0  0  0  0  0  0  0  0  0   0 0.5025248
## I9   0  0  0  0  0  0  0  0  0   0 0.6894665
## I10  0  0  0  0  0  0  0  0  0   0 0.7421716
## G    0  0  0  0  0  0  0  0  0   0 0.0000000
```

```r
A1 <- mxEval(A1, fit1a$mx.fit)
A1
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10            G
## I1   0  0  0  0  0  0  0  0  0   0  0.044377795
## I2   0  0  0  0  0  0  0  0  0   0  0.052193857
## I3   0  0  0  0  0  0  0  0  0   0  0.009687639
## I4   0  0  0  0  0  0  0  0  0   0 -0.009776466
## I5   0  0  0  0  0  0  0  0  0   0  0.016021947
## I6   0  0  0  0  0  0  0  0  0   0  0.053061765
## I7   0  0  0  0  0  0  0  0  0   0  0.026102926
## I8   0  0  0  0  0  0  0  0  0   0  0.141437115
## I9   0  0  0  0  0  0  0  0  0   0  0.027557843
## I10  0  0  0  0  0  0  0  0  0   0  0.016788462
## G    0  0  0  0  0  0  0  0  0   0  0.000000000
```

```r
## Compute the estimated A matrix at -1SD (-1) of the standardized individualism
A0 - A1
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G
## I1   0  0  0  0  0  0  0  0  0   0 0.6428658
## I2   0  0  0  0  0  0  0  0  0   0 0.5849797
## I3   0  0  0  0  0  0  0  0  0   0 0.5491249
## I4   0  0  0  0  0  0  0  0  0   0 0.5106014
## I5   0  0  0  0  0  0  0  0  0   0 0.6259802
## I6   0  0  0  0  0  0  0  0  0   0 0.5394355
## I7   0  0  0  0  0  0  0  0  0   0 0.5574908
## I8   0  0  0  0  0  0  0  0  0   0 0.3610877
## I9   0  0  0  0  0  0  0  0  0   0 0.6619086
## I10  0  0  0  0  0  0  0  0  0   0 0.7253831
## G    0  0  0  0  0  0  0  0  0   0 0.0000000
```

```r
## Compute the estimated A matrix at 0 (mean) of the standardized individualism
A0
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G
## I1   0  0  0  0  0  0  0  0  0   0 0.6872436
## I2   0  0  0  0  0  0  0  0  0   0 0.6371735
## I3   0  0  0  0  0  0  0  0  0   0 0.5588126
## I4   0  0  0  0  0  0  0  0  0   0 0.5008249
## I5   0  0  0  0  0  0  0  0  0   0 0.6420021
## I6   0  0  0  0  0  0  0  0  0   0 0.5924973
## I7   0  0  0  0  0  0  0  0  0   0 0.5835937
## I8   0  0  0  0  0  0  0  0  0   0 0.5025248
## I9   0  0  0  0  0  0  0  0  0   0 0.6894665
## I10  0  0  0  0  0  0  0  0  0   0 0.7421716
## G    0  0  0  0  0  0  0  0  0   0 0.0000000
```

```r
## Compute the estimated A matrix at +1SD (+1) of the standardized individualism
A0 + A1
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G
## I1   0  0  0  0  0  0  0  0  0   0 0.7316214
## I2   0  0  0  0  0  0  0  0  0   0 0.6893674
## I3   0  0  0  0  0  0  0  0  0   0 0.5685002
## I4   0  0  0  0  0  0  0  0  0   0 0.4910485
## I5   0  0  0  0  0  0  0  0  0   0 0.6580241
## I6   0  0  0  0  0  0  0  0  0   0 0.6455590
## I7   0  0  0  0  0  0  0  0  0   0 0.6096967
## I8   0  0  0  0  0  0  0  0  0   0 0.6439619
## I9   0  0  0  0  0  0  0  0  0   0 0.7170243
## I10  0  0  0  0  0  0  0  0  0   0 0.7589601
## G    0  0  0  0  0  0  0  0  0   0 0.0000000
```

## Bifactor model without any moderator

```r
## Create matrices with implicit diagonal constraints
M0b <- create.vechsR(A0=RAM2$A, S0=RAM2$S, F0=RAM2$F)

## Create heterogeneity variances
T0b <- create.Tau2(RAM=RAM2, RE.type="Diag", Transform="expLog", RE.startvalues=0.05)

fit0b <- osmasem(model.name="No moderator", Mmatrix=M0b, Tmatrix=T0b, data=my.df)
```

```
## Running No moderator with 65 parameters
```

```r
summary(fit0b, Saturated=TRUE)
```

```
## Summary of No moderator 
##  
## free parameters:
##       name  matrix row col    Estimate  Std.Error A     z value
## 1       g1      A0  I1   G  0.75244751 0.01446324    52.0248188
## 2       g2      A0  I2   G  0.53608161 0.01349401    39.7273792
## 3       g3      A0  I3   G  0.59497627 0.01812562    32.8251528
## 4       g4      A0  I4   G  0.52191895 0.01293407    40.3522515
## 5       g5      A0  I5   G  0.52813056 0.01556588    33.9287211
## 6       g6      A0  I6   G  0.51469206 0.01182050    43.5423361
## 7       g7      A0  I7   G  0.62026937 0.01400771    44.2805749
## 8       g8      A0  I8   G  0.38546304 0.01892836    20.3643130
## 9       g9      A0  I9   G  0.59453818 0.01423267    41.7727706
## 10     g10      A0 I10   G  0.80265357 0.01495072    53.6866003
## 11      p1      A0  I1 POS -0.04575755 0.04371503    -1.0467235
## 12      p3      A0  I3 POS  0.53259630 0.07105339     7.4957198
## 13      p4      A0  I4 POS  0.30699516 0.03823729     8.0286861
## 14      p7      A0  I7 POS  0.31793449 0.04172797     7.6192178
## 15     p10      A0 I10 POS -0.03367364 0.04534098    -0.7426756
## 16      n2      A0  I2 NEG  0.58803033 0.02531052    23.2326467
## 17      n5      A0  I5 NEG  0.32329305 0.02319228    13.9396833
## 18      n6      A0  I6 NEG  0.59884585 0.02421493    24.7304347
## 19      n8      A0  I8 NEG  0.39899648 0.03153361    12.6530547
## 20      n9      A0  I9 NEG  0.38519625 0.02312968    16.6537630
## 21  Tau1_1 vecTau1   1   1 -2.35055647 0.13014459   -18.0611152
## 22  Tau1_2 vecTau1   2   1 -2.55664325 0.13146106   -19.4479127
## 23  Tau1_3 vecTau1   3   1 -2.52515705 0.13230785   -19.0854664
## 24  Tau1_4 vecTau1   4   1 -2.17354231 0.13170655   -16.5029170
## 25  Tau1_5 vecTau1   5   1 -2.71991798 0.13902191   -19.5646707
## 26  Tau1_6 vecTau1   6   1 -2.27956999 0.12784330   -17.8309698
## 27  Tau1_7 vecTau1   7   1 -1.84782715 0.12524402   -14.7538154
## 28  Tau1_8 vecTau1   8   1 -2.44594340 0.13081350   -18.6979434
## 29  Tau1_9 vecTau1   9   1 -2.19253284 0.12784463   -17.1499799
## 30 Tau1_10 vecTau1  10   1 -2.58088901 0.13241971   -19.4902178
## 31 Tau1_11 vecTau1  11   1 -2.67043598 0.13382645   -19.9544704
## 32 Tau1_12 vecTau1  12   1 -2.64664066 0.13600198   -19.4603097
## 33 Tau1_13 vecTau1  13   1 -2.34563428 0.13584704   -17.2667305
## 34 Tau1_14 vecTau1  14   1 -2.39550842 0.12763898   -18.7678439
## 35 Tau1_15 vecTau1  15   1 -2.05949253 0.12808186   -16.0795015
## 36 Tau1_16 vecTau1  16   1 -2.61226647 0.13731762   -19.0235344
## 37 Tau1_17 vecTau1  17   1 -2.51183169 0.12844369   -19.5558985
## 38 Tau1_18 vecTau1  18   1 -2.78117559 0.13708290   -20.2882757
## 39 Tau1_19 vecTau1  19   1 -2.34790979 0.13428792   -17.4841470
## 40 Tau1_20 vecTau1  20   1 -2.73824166 0.14075166   -19.4544177
## 41 Tau1_21 vecTau1  21   1 -2.01793711 0.12584573   -16.0350070
## 42 Tau1_22 vecTau1  22   1 -2.13513108 0.13108485   -16.2881605
## 43 Tau1_23 vecTau1  23   1 -2.63654353 0.13213039   -19.9541037
## 44 Tau1_24 vecTau1  24   1 -2.64701381 0.13780749   -19.2080541
## 45 Tau1_25 vecTau1  25   1 -2.55204999 0.13651302   -18.6945536
## 46 Tau1_26 vecTau1  26   1 -2.73493812 0.13613761   -20.0895118
## 47 Tau1_27 vecTau1  27   1 -2.23643849 0.13055910   -17.1297017
## 48 Tau1_28 vecTau1  28   1 -2.08846290 0.12935950   -16.1446430
## 49 Tau1_29 vecTau1  29   1 -2.53509380 0.13405327   -18.9110927
## 50 Tau1_30 vecTau1  30   1 -2.74817062 0.13738419   -20.0035428
## 51 Tau1_31 vecTau1  31   1 -2.76150114 0.14356737   -19.2348801
## 52 Tau1_32 vecTau1  32   1 -2.35286549 0.12975594   -18.1330087
## 53 Tau1_33 vecTau1  33   1 -2.32745894 0.13326974   -17.4642712
## 54 Tau1_34 vecTau1  34   1 -2.09259042 0.13007993   -16.0869587
## 55 Tau1_35 vecTau1  35   1 -2.32912418 0.13729327   -16.9645910
## 56 Tau1_36 vecTau1  36   1 -2.66553737 0.13172561   -20.2355289
## 57 Tau1_37 vecTau1  37   1 -2.02208011 0.12939269   -15.6274679
## 58 Tau1_38 vecTau1  38   1 -2.81013450 0.13973632   -20.1102658
## 59 Tau1_39 vecTau1  39   1 -2.76843604 0.13396273   -20.6657179
## 60 Tau1_40 vecTau1  40   1 -2.04200177 0.12698442   -16.0807271
## 61 Tau1_41 vecTau1  41   1 -2.44276134 0.12767395   -19.1328094
## 62 Tau1_42 vecTau1  42   1 -2.48741071 0.12792760   -19.4438942
## 63 Tau1_43 vecTau1  43   1 -2.02990264 0.12748819   -15.9222793
## 64 Tau1_44 vecTau1  44   1 -1.73666756 0.12526872   -13.8635374
## 65 Tau1_45 vecTau1  45   1 -2.47512140 0.12975628   -19.0751569
##        Pr(>|z|)
## 1  0.000000e+00
## 2  0.000000e+00
## 3  0.000000e+00
## 4  0.000000e+00
## 5  0.000000e+00
## 6  0.000000e+00
## 7  0.000000e+00
## 8  0.000000e+00
## 9  0.000000e+00
## 10 0.000000e+00
## 11 2.952271e-01
## 12 6.594725e-14
## 13 8.881784e-16
## 14 2.553513e-14
## 15 4.576781e-01
## 16 0.000000e+00
## 17 0.000000e+00
## 18 0.000000e+00
## 19 0.000000e+00
## 20 0.000000e+00
## 21 0.000000e+00
## 22 0.000000e+00
## 23 0.000000e+00
## 24 0.000000e+00
## 25 0.000000e+00
## 26 0.000000e+00
## 27 0.000000e+00
## 28 0.000000e+00
## 29 0.000000e+00
## 30 0.000000e+00
## 31 0.000000e+00
## 32 0.000000e+00
## 33 0.000000e+00
## 34 0.000000e+00
## 35 0.000000e+00
## 36 0.000000e+00
## 37 0.000000e+00
## 38 0.000000e+00
## 39 0.000000e+00
## 40 0.000000e+00
## 41 0.000000e+00
## 42 0.000000e+00
## 43 0.000000e+00
## 44 0.000000e+00
## 45 0.000000e+00
## 46 0.000000e+00
## 47 0.000000e+00
## 48 0.000000e+00
## 49 0.000000e+00
## 50 0.000000e+00
## 51 0.000000e+00
## 52 0.000000e+00
## 53 0.000000e+00
## 54 0.000000e+00
## 55 0.000000e+00
## 56 0.000000e+00
## 57 0.000000e+00
## 58 0.000000e+00
## 59 0.000000e+00
## 60 0.000000e+00
## 61 0.000000e+00
## 62 0.000000e+00
## 63 0.000000e+00
## 64 0.000000e+00
## 65 0.000000e+00
## 
## Model Statistics: 
##                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
##        Model:             65                   1465             -2611.393
##    Saturated:             90                   1440             -2647.331
## Independence:             90                   1440                    NA
## Number of observations/statistics: 104684/1530
## 
## chi-square:  χ² ( df=25 ) = 35.93803,  p = 0.07254544
## Information Criteria: 
##       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
## AIC:      -5541.393              -2481.393                -2481.311
## BIC:     -19544.891              -1860.077                -2066.650
## CFI: NA 
## TLI: NA   (also known as NNFI) 
## RMSEA:  0.002044369  [95% CI (0, 0.003674941)]
## Prob(RMSEA <= 0.05): 1
## To get additional fit indices, see help(mxRefModels)
## timestamp: 2018-11-08 11:43:41 
## Wall clock time: 148.3895 secs 
## optimizer:  SLSQP 
## OpenMx version number: 2.11.5 
## Need help?  See help(mxSummary)
```

```r
## SRMR
osmasemSRMR(fit0b)
```

```
## [1] 0.01620706
```

```r
diag(VarCorr(fit0b))
```

```
##      Tau2_1      Tau2_2      Tau2_3      Tau2_4      Tau2_5      Tau2_6 
## 0.009085160 0.006016278 0.006407321 0.012944496 0.004340195 0.010471060 
##      Tau2_7      Tau2_8      Tau2_9     Tau2_10     Tau2_11     Tau2_12 
## 0.024831201 0.007507244 0.012462070 0.005731500 0.004791691 0.005025244 
##     Tau2_13     Tau2_14     Tau2_15     Tau2_16     Tau2_17     Tau2_18 
## 0.009175040 0.008304009 0.016261010 0.005382873 0.006580376 0.003839738 
##     Tau2_19     Tau2_20     Tau2_21     Tau2_22     Tau2_23     Tau2_24 
## 0.009133379 0.004184018 0.017670226 0.013978118 0.005127756 0.005021495 
##     Tau2_25     Tau2_26     Tau2_27     Tau2_28     Tau2_29     Tau2_30 
## 0.006071801 0.004211753 0.011414429 0.015345611 0.006281242 0.004101751 
##     Tau2_31     Tau2_32     Tau2_33     Tau2_34     Tau2_35     Tau2_36 
## 0.003993839 0.009043301 0.009514695 0.015219453 0.009483059 0.004838867 
##     Tau2_37     Tau2_38     Tau2_39     Tau2_40     Tau2_41     Tau2_42 
## 0.017524415 0.003623666 0.003938828 0.016839911 0.007555174 0.006909753 
##     Tau2_43     Tau2_44     Tau2_45 
## 0.017252378 0.031013425 0.007081689
```

## Bifactor model  with `Individualism` as a moderator on the A matrix

```r
## Replace the A matrix with the moderator "Individualism"
Ax1b <- RAM2$A
Ax1b[grep("\\*", Ax1b)] <- "0*data.Individualism"
Ax1b
```

```
##     I1  I2  I3  I4  I5  I6  I7  I8  I9  I10 G                     
## I1  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I2  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I3  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I4  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I5  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I6  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I7  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I8  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I9  "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## I10 "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0*data.Individualism"
## G   "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"                   
## POS "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"                   
## NEG "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"                   
##     POS                    NEG                   
## I1  "0*data.Individualism" "0"                   
## I2  "0"                    "0*data.Individualism"
## I3  "0*data.Individualism" "0"                   
## I4  "0*data.Individualism" "0"                   
## I5  "0"                    "0*data.Individualism"
## I6  "0"                    "0*data.Individualism"
## I7  "0*data.Individualism" "0"                   
## I8  "0"                    "0*data.Individualism"
## I9  "0"                    "0*data.Individualism"
## I10 "0*data.Individualism" "0"                   
## G   "0"                    "0"                   
## POS "0"                    "0"                   
## NEG "0"                    "0"
```

```r
## Create matrices with implicit diagonal constraints
M1b <- create.vechsR(A0=RAM2$A, S0=RAM2$S, F0=RAM2$F, Ax=Ax1b)

fit1b <- osmasem(model.name="Moderator with individualism", Mmatrix=M1b, Tmatrix=T0b, data=my.df)
```

```
## Running Moderator with individualism with 85 parameters
```

```r
summary(fit1b)
```

```
## Summary of Moderator with individualism 
##  
## free parameters:
##       name  matrix row col      Estimate  Std.Error A      z value
## 1       g1      A0  I1   G  0.7267695868 0.01945719    37.35224549
## 2       g2      A0  I2   G  0.5513976575 0.01850721    29.79367457
## 3       g3      A0  I3   G  0.5685769932 0.02076089    27.38693177
## 4       g4      A0  I4   G  0.5043280124 0.01818298    27.73627578
## 5       g5      A0  I5   G  0.5462133754 0.02102795    25.97559456
## 6       g6      A0  I6   G  0.5307457804 0.01691091    31.38481383
## 7       g7      A0  I7   G  0.5959702258 0.01882252    31.66261440
## 8       g8      A0  I8   G  0.3917125624 0.01951401    20.07340591
## 9       g9      A0  I9   G  0.6162445563 0.02143739    28.74625348
## 10     g10      A0 I10   G  0.7773655617 0.02123788    36.60279089
## 11      p1      A0  I1 POS  0.0032447482 0.05182225     0.06261303
## 12      p3      A0  I3 POS  0.5045007853 0.04440479    11.36140402
## 13      p4      A0  I4 POS  0.3537946517 0.03871782     9.13777315
## 14      p7      A0  I7 POS  0.3585447176 0.04183079     8.57131075
## 15     p10      A0 I10 POS  0.0157771665 0.05396967     0.29233395
## 16      n2      A0  I2 NEG  0.5690580157 0.02840291    20.03519919
## 17      n5      A0  I5 NEG  0.3003201363 0.03141528     9.55968296
## 18      n6      A0  I6 NEG  0.5804902336 0.02622199    22.13753804
## 19      n8      A0  I8 NEG  0.3904778492 0.03059107    12.76443728
## 20      n9      A0  I9 NEG  0.3578781632 0.03402111    10.51929579
## 21    g1_1      A1  I1   G  0.0875109922 0.02096164     4.17481624
## 22    g2_1      A1  I2   G  0.0221053952 0.01956071     1.13009146
## 23    g3_1      A1  I3   G  0.0727628784 0.01649132     4.41219149
## 24    g4_1      A1  I4   G  0.0353538125 0.01633830     2.16386172
## 25    g5_1      A1  I5   G  0.0006416487 0.02102265     0.03052177
## 26    g6_1      A1  I6   G  0.0200860920 0.01811602     1.10874752
## 27    g7_1      A1  I7   G  0.0727057766 0.01762035     4.12623796
## 28    g8_1      A1  I8   G  0.1228572231 0.02008039     6.11826813
## 29    g9_1      A1  I9   G -0.0168895367 0.02261644    -0.74678134
## 30   g10_1      A1 I10   G  0.0658334496 0.02271764     2.89790025
## 31    p1_1      A1  I1 POS -0.1620770297 0.03394853    -4.77419840
## 32    p3_1      A1  I3 POS -0.0600522017 0.02963867    -2.02614369
## 33    p4_1      A1  I4 POS -0.0424613510 0.02578748    -1.64658777
## 34    p7_1      A1  I7 POS -0.0749926912 0.03046905    -2.46127440
## 35   p10_1      A1 I10 POS -0.1798671231 0.03488792    -5.15556993
## 36    n2_1      A1  I2 NEG  0.0067391986 0.02955413     0.22802899
## 37    n5_1      A1  I5 NEG -0.0012599337 0.03076880    -0.04094842
## 38    n6_1      A1  I6 NEG  0.0045910063 0.02693434     0.17045178
## 39    n8_1      A1  I8 NEG  0.0329755333 0.03050542     1.08097305
## 40    n9_1      A1  I9 NEG  0.0650190505 0.03508394     1.85324238
## 41  Tau1_1 vecTau1   1   1 -2.4635902306 0.13083665   -18.82951233
## 42  Tau1_2 vecTau1   2   1 -2.6006945979 0.13325905   -19.51608278
## 43  Tau1_3 vecTau1   3   1 -2.5348885674 0.13286369   -19.07886677
## 44  Tau1_4 vecTau1   4   1 -2.2190577555 0.13199821   -16.81127219
## 45  Tau1_5 vecTau1   5   1 -2.9220466481 0.14420715   -20.26284213
## 46  Tau1_6 vecTau1   6   1 -2.3321960571 0.12888058   -18.09579063
## 47  Tau1_7 vecTau1   7   1 -2.0583355965 0.12605917   -16.32832953
## 48  Tau1_8 vecTau1   8   1 -2.5190304232 0.13232172   -19.03716532
## 49  Tau1_9 vecTau1   9   1 -2.3464678126 0.13988187   -16.77463838
## 50 Tau1_10 vecTau1  10   1 -2.6106398422 0.13184449   -19.80090277
## 51 Tau1_11 vecTau1  11   1 -2.6943203629 0.13390874   -20.12057180
## 52 Tau1_12 vecTau1  12   1 -2.6587908114 0.13646574   -19.48321115
## 53 Tau1_13 vecTau1  13   1 -2.3464080262 0.13840662   -16.95300457
## 54 Tau1_14 vecTau1  14   1 -2.4655041397 0.12765114   -19.31439166
## 55 Tau1_15 vecTau1  15   1 -2.2225550653 0.12863225   -17.27836559
## 56 Tau1_16 vecTau1  16   1 -2.7004911234 0.14093288   -19.16154048
## 57 Tau1_17 vecTau1  17   1 -2.6053210935 0.12953200   -20.11333914
## 58 Tau1_18 vecTau1  18   1 -2.8028712401 0.13852437   -20.23377694
## 59 Tau1_19 vecTau1  19   1 -2.3747237135 0.13528804   -17.55309464
## 60 Tau1_20 vecTau1  20   1 -2.7833311630 0.14052304   -19.80693747
## 61 Tau1_21 vecTau1  21   1 -2.0491883561 0.12737626   -16.08767877
## 62 Tau1_22 vecTau1  22   1 -2.2997448916 0.13308159   -17.28071341
## 63 Tau1_23 vecTau1  23   1 -2.6497355995 0.13501481   -19.62551745
## 64 Tau1_24 vecTau1  24   1 -2.7070449111 0.13721425   -19.72859874
## 65 Tau1_25 vecTau1  25   1 -2.5952705464 0.13859996   -18.72490124
## 66 Tau1_26 vecTau1  26   1 -2.7737674886 0.13634657   -20.34350766
## 67 Tau1_27 vecTau1  27   1 -2.2379924598 0.13303117   -16.82306792
## 68 Tau1_28 vecTau1  28   1 -2.2050111029 0.13157064   -16.75914213
## 69 Tau1_29 vecTau1  29   1 -2.5703169565 0.13418999   -19.15431222
## 70 Tau1_30 vecTau1  30   1 -2.8157988033 0.13780894   -20.43262756
## 71 Tau1_31 vecTau1  31   1 -2.8081980814 0.14351501   -19.56727733
## 72 Tau1_32 vecTau1  32   1 -2.4048929722 0.13015286   -18.47744947
## 73 Tau1_33 vecTau1  33   1 -2.4778881532 0.13554354   -18.28112358
## 74 Tau1_34 vecTau1  34   1 -2.1210698993 0.13031504   -16.27647829
## 75 Tau1_35 vecTau1  35   1 -2.3359215078 0.13875211   -16.83521408
## 76 Tau1_36 vecTau1  36   1 -2.7616157379 0.13292194   -20.77622157
## 77 Tau1_37 vecTau1  37   1 -2.1925801311 0.12998837   -16.86750941
## 78 Tau1_38 vecTau1  38   1 -2.9215666949 0.14367653   -20.33433552
## 79 Tau1_39 vecTau1  39   1 -2.9584649851 0.13861740   -21.34266657
## 80 Tau1_40 vecTau1  40   1 -2.2524428206 0.12755223   -17.65898387
## 81 Tau1_41 vecTau1  41   1 -2.4862312600 0.12824330   -19.38683226
## 82 Tau1_42 vecTau1  42   1 -2.5311748617 0.12819321   -19.74499967
## 83 Tau1_43 vecTau1  43   1 -2.2889353348 0.13088354   -17.48833630
## 84 Tau1_44 vecTau1  44   1 -1.9861082619 0.12672409   -15.67269702
## 85 Tau1_45 vecTau1  45   1 -2.5404026199 0.13271025   -19.14247541
##        Pr(>|z|)
## 1  0.000000e+00
## 2  0.000000e+00
## 3  0.000000e+00
## 4  0.000000e+00
## 5  0.000000e+00
## 6  0.000000e+00
## 7  0.000000e+00
## 8  0.000000e+00
## 9  0.000000e+00
## 10 0.000000e+00
## 11 9.500747e-01
## 12 0.000000e+00
## 13 0.000000e+00
## 14 0.000000e+00
## 15 7.700313e-01
## 16 0.000000e+00
## 17 0.000000e+00
## 18 0.000000e+00
## 19 0.000000e+00
## 20 0.000000e+00
## 21 2.982267e-05
## 22 2.584377e-01
## 23 1.023295e-05
## 24 3.047496e-02
## 25 9.756509e-01
## 26 2.675391e-01
## 27 3.687457e-05
## 28 9.459780e-10
## 29 4.551956e-01
## 30 3.756701e-03
## 31 1.804244e-06
## 32 4.275006e-02
## 33 9.964281e-02
## 34 1.384444e-02
## 35 2.528607e-07
## 36 8.196237e-01
## 37 9.673370e-01
## 38 8.646549e-01
## 39 2.797091e-01
## 40 6.384763e-02
## 41 0.000000e+00
## 42 0.000000e+00
## 43 0.000000e+00
## 44 0.000000e+00
## 45 0.000000e+00
## 46 0.000000e+00
## 47 0.000000e+00
## 48 0.000000e+00
## 49 0.000000e+00
## 50 0.000000e+00
## 51 0.000000e+00
## 52 0.000000e+00
## 53 0.000000e+00
## 54 0.000000e+00
## 55 0.000000e+00
## 56 0.000000e+00
## 57 0.000000e+00
## 58 0.000000e+00
## 59 0.000000e+00
## 60 0.000000e+00
## 61 0.000000e+00
## 62 0.000000e+00
## 63 0.000000e+00
## 64 0.000000e+00
## 65 0.000000e+00
## 66 0.000000e+00
## 67 0.000000e+00
## 68 0.000000e+00
## 69 0.000000e+00
## 70 0.000000e+00
## 71 0.000000e+00
## 72 0.000000e+00
## 73 0.000000e+00
## 74 0.000000e+00
## 75 0.000000e+00
## 76 0.000000e+00
## 77 0.000000e+00
## 78 0.000000e+00
## 79 0.000000e+00
## 80 0.000000e+00
## 81 0.000000e+00
## 82 0.000000e+00
## 83 0.000000e+00
## 84 0.000000e+00
## 85 0.000000e+00
## 
## Model Statistics: 
##                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
##        Model:             85                   1445             -2882.278
##    Saturated:           1080                    450                    NA
## Independence:             90                   1440                    NA
## Number of observations/statistics: 104684/1530
## 
## Information Criteria: 
##       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
## AIC:      -5772.278              -2712.278                -2712.139
## BIC:     -19584.602              -1899.789                -2169.922
## To get additional fit indices, see help(mxRefModels)
## timestamp: 2018-11-08 11:56:51 
## Wall clock time: 288.9516 secs 
## optimizer:  SLSQP 
## OpenMx version number: 2.11.5 
## Need help?  See help(mxSummary)
```

```r
## Get the R2
osmasemR2(fit1b, fit0b)
```

```
## $Tau2.0
##    Tau2_1_1    Tau2_2_2    Tau2_3_3    Tau2_4_4    Tau2_5_5    Tau2_6_6 
## 0.009085160 0.006016278 0.006407321 0.012944496 0.004340195 0.010471060 
##    Tau2_7_7    Tau2_8_8    Tau2_9_9  Tau2_10_10  Tau2_11_11  Tau2_12_12 
## 0.024831201 0.007507244 0.012462070 0.005731500 0.004791691 0.005025244 
##  Tau2_13_13  Tau2_14_14  Tau2_15_15  Tau2_16_16  Tau2_17_17  Tau2_18_18 
## 0.009175040 0.008304009 0.016261010 0.005382873 0.006580376 0.003839738 
##  Tau2_19_19  Tau2_20_20  Tau2_21_21  Tau2_22_22  Tau2_23_23  Tau2_24_24 
## 0.009133379 0.004184018 0.017670226 0.013978118 0.005127756 0.005021495 
##  Tau2_25_25  Tau2_26_26  Tau2_27_27  Tau2_28_28  Tau2_29_29  Tau2_30_30 
## 0.006071801 0.004211753 0.011414429 0.015345611 0.006281242 0.004101751 
##  Tau2_31_31  Tau2_32_32  Tau2_33_33  Tau2_34_34  Tau2_35_35  Tau2_36_36 
## 0.003993839 0.009043301 0.009514695 0.015219453 0.009483059 0.004838867 
##  Tau2_37_37  Tau2_38_38  Tau2_39_39  Tau2_40_40  Tau2_41_41  Tau2_42_42 
## 0.017524415 0.003623666 0.003938828 0.016839911 0.007555174 0.006909753 
##  Tau2_43_43  Tau2_44_44  Tau2_45_45 
## 0.017252378 0.031013425 0.007081689 
## 
## $Tau2.1
##    Tau2_1_1    Tau2_2_2    Tau2_3_3    Tau2_4_4    Tau2_5_5    Tau2_6_6 
## 0.007246907 0.005508906 0.006283820 0.011818189 0.002896960 0.009424976 
##    Tau2_7_7    Tau2_8_8    Tau2_9_9  Tau2_10_10  Tau2_11_11  Tau2_12_12 
## 0.016298679 0.006486314 0.009159757 0.005400414 0.004568179 0.004904601 
##  Tau2_13_13  Tau2_14_14  Tau2_15_15  Tau2_16_16  Tau2_17_17  Tau2_18_18 
## 0.009160852 0.007219221 0.011735813 0.004512147 0.005458167 0.003676690 
##  Tau2_19_19  Tau2_20_20  Tau2_21_21  Tau2_22_22  Tau2_23_23  Tau2_24_24 
## 0.008656477 0.003823220 0.016599599 0.010056966 0.004994234 0.004453389 
##  Tau2_25_25  Tau2_26_26  Tau2_27_27  Tau2_28_28  Tau2_29_29  Tau2_30_30 
## 0.005568993 0.003897052 0.011379009 0.012154908 0.005853978 0.003582847 
##  Tau2_31_31  Tau2_32_32  Tau2_33_33  Tau2_34_34  Tau2_35_35  Tau2_36_36 
## 0.003637727 0.008149604 0.007042611 0.014376795 0.009355012 0.003992924 
##  Tau2_37_37  Tau2_38_38  Tau2_39_39  Tau2_40_40  Tau2_41_41  Tau2_42_42 
## 0.012460891 0.002899742 0.002693456 0.011054854 0.006926071 0.006330667 
##  Tau2_43_43  Tau2_44_44  Tau2_45_45 
## 0.010276756 0.018831646 0.006214903 
## 
## $R2
##    Tau2_1_1    Tau2_2_2    Tau2_3_3    Tau2_4_4    Tau2_5_5    Tau2_6_6 
## 0.202335757 0.084333157 0.019274855 0.087010496 0.332527671 0.099902459 
##    Tau2_7_7    Tau2_8_8    Tau2_9_9  Tau2_10_10  Tau2_11_11  Tau2_12_12 
## 0.343620986 0.135992693 0.264989100 0.057766026 0.046645799 0.024007419 
##  Tau2_13_13  Tau2_14_14  Tau2_15_15  Tau2_16_16  Tau2_17_17  Tau2_18_18 
## 0.001546294 0.130634330 0.278285084 0.161758725 0.170538687 0.042463373 
##  Tau2_19_19  Tau2_20_20  Tau2_21_21  Tau2_22_22  Tau2_23_23  Tau2_24_24 
## 0.052215241 0.086232393 0.060589276 0.280520776 0.026039114 0.113134734 
##  Tau2_25_25  Tau2_26_26  Tau2_27_27  Tau2_28_28  Tau2_29_29  Tau2_30_30 
## 0.082810437 0.074719869 0.003103114 0.207922788 0.068022228 0.126508053 
##  Tau2_31_31  Tau2_32_32  Tau2_33_33  Tau2_34_34  Tau2_35_35  Tau2_36_36 
## 0.089165339 0.098824231 0.259817445 0.055367161 0.013502657 0.174822476 
##  Tau2_37_37  Tau2_38_38  Tau2_39_39  Tau2_40_40  Tau2_41_41  Tau2_42_42 
## 0.288941120 0.199776638 0.316178179 0.343532511 0.083267752 0.083807068 
##  Tau2_43_43  Tau2_44_44  Tau2_45_45 
## 0.404328173 0.392790499 0.122398307
```

```r
## Compare the models with and without Individualism
anova(fit1b, fit0b)
```

```
##                           base   comparison ep  minus2LL   df       AIC
## 1 Moderator with individualism         <NA> 85 -2882.278 1445 -5772.278
## 2 Moderator with individualism No moderator 65 -2611.393 1465 -5541.393
##     diffLL diffdf            p
## 1       NA     NA           NA
## 2 270.8854     20 6.817805e-46
```

```r
## Get the estimated A0 and A1
A0 <- mxEval(A0, fit1b$mx.fit)
A0
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G         POS       NEG
## I1   0  0  0  0  0  0  0  0  0   0 0.7267696 0.003244748 0.0000000
## I2   0  0  0  0  0  0  0  0  0   0 0.5513977 0.000000000 0.5690580
## I3   0  0  0  0  0  0  0  0  0   0 0.5685770 0.504500785 0.0000000
## I4   0  0  0  0  0  0  0  0  0   0 0.5043280 0.353794652 0.0000000
## I5   0  0  0  0  0  0  0  0  0   0 0.5462134 0.000000000 0.3003201
## I6   0  0  0  0  0  0  0  0  0   0 0.5307458 0.000000000 0.5804902
## I7   0  0  0  0  0  0  0  0  0   0 0.5959702 0.358544718 0.0000000
## I8   0  0  0  0  0  0  0  0  0   0 0.3917126 0.000000000 0.3904778
## I9   0  0  0  0  0  0  0  0  0   0 0.6162446 0.000000000 0.3578782
## I10  0  0  0  0  0  0  0  0  0   0 0.7773656 0.015777166 0.0000000
## G    0  0  0  0  0  0  0  0  0   0 0.0000000 0.000000000 0.0000000
## POS  0  0  0  0  0  0  0  0  0   0 0.0000000 0.000000000 0.0000000
## NEG  0  0  0  0  0  0  0  0  0   0 0.0000000 0.000000000 0.0000000
```

```r
A1 <- mxEval(A1, fit1b$mx.fit)
A1
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10             G         POS          NEG
## I1   0  0  0  0  0  0  0  0  0   0  0.0875109922 -0.16207703  0.000000000
## I2   0  0  0  0  0  0  0  0  0   0  0.0221053952  0.00000000  0.006739199
## I3   0  0  0  0  0  0  0  0  0   0  0.0727628784 -0.06005220  0.000000000
## I4   0  0  0  0  0  0  0  0  0   0  0.0353538125 -0.04246135  0.000000000
## I5   0  0  0  0  0  0  0  0  0   0  0.0006416487  0.00000000 -0.001259934
## I6   0  0  0  0  0  0  0  0  0   0  0.0200860920  0.00000000  0.004591006
## I7   0  0  0  0  0  0  0  0  0   0  0.0727057766 -0.07499269  0.000000000
## I8   0  0  0  0  0  0  0  0  0   0  0.1228572231  0.00000000  0.032975533
## I9   0  0  0  0  0  0  0  0  0   0 -0.0168895367  0.00000000  0.065019050
## I10  0  0  0  0  0  0  0  0  0   0  0.0658334496 -0.17986712  0.000000000
## G    0  0  0  0  0  0  0  0  0   0  0.0000000000  0.00000000  0.000000000
## POS  0  0  0  0  0  0  0  0  0   0  0.0000000000  0.00000000  0.000000000
## NEG  0  0  0  0  0  0  0  0  0   0  0.0000000000  0.00000000  0.000000000
```

```r
## Compute the estimated A matrix at -1SD (-1) of the standardized individualism
A0 - A1
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G       POS       NEG
## I1   0  0  0  0  0  0  0  0  0   0 0.6392586 0.1653218 0.0000000
## I2   0  0  0  0  0  0  0  0  0   0 0.5292923 0.0000000 0.5623188
## I3   0  0  0  0  0  0  0  0  0   0 0.4958141 0.5645530 0.0000000
## I4   0  0  0  0  0  0  0  0  0   0 0.4689742 0.3962560 0.0000000
## I5   0  0  0  0  0  0  0  0  0   0 0.5455717 0.0000000 0.3015801
## I6   0  0  0  0  0  0  0  0  0   0 0.5106597 0.0000000 0.5758992
## I7   0  0  0  0  0  0  0  0  0   0 0.5232644 0.4335374 0.0000000
## I8   0  0  0  0  0  0  0  0  0   0 0.2688553 0.0000000 0.3575023
## I9   0  0  0  0  0  0  0  0  0   0 0.6331341 0.0000000 0.2928591
## I10  0  0  0  0  0  0  0  0  0   0 0.7115321 0.1956443 0.0000000
## G    0  0  0  0  0  0  0  0  0   0 0.0000000 0.0000000 0.0000000
## POS  0  0  0  0  0  0  0  0  0   0 0.0000000 0.0000000 0.0000000
## NEG  0  0  0  0  0  0  0  0  0   0 0.0000000 0.0000000 0.0000000
```

```r
## Compute the estimated A matrix at 0 (mean) of the standardized individualism
A0
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G         POS       NEG
## I1   0  0  0  0  0  0  0  0  0   0 0.7267696 0.003244748 0.0000000
## I2   0  0  0  0  0  0  0  0  0   0 0.5513977 0.000000000 0.5690580
## I3   0  0  0  0  0  0  0  0  0   0 0.5685770 0.504500785 0.0000000
## I4   0  0  0  0  0  0  0  0  0   0 0.5043280 0.353794652 0.0000000
## I5   0  0  0  0  0  0  0  0  0   0 0.5462134 0.000000000 0.3003201
## I6   0  0  0  0  0  0  0  0  0   0 0.5307458 0.000000000 0.5804902
## I7   0  0  0  0  0  0  0  0  0   0 0.5959702 0.358544718 0.0000000
## I8   0  0  0  0  0  0  0  0  0   0 0.3917126 0.000000000 0.3904778
## I9   0  0  0  0  0  0  0  0  0   0 0.6162446 0.000000000 0.3578782
## I10  0  0  0  0  0  0  0  0  0   0 0.7773656 0.015777166 0.0000000
## G    0  0  0  0  0  0  0  0  0   0 0.0000000 0.000000000 0.0000000
## POS  0  0  0  0  0  0  0  0  0   0 0.0000000 0.000000000 0.0000000
## NEG  0  0  0  0  0  0  0  0  0   0 0.0000000 0.000000000 0.0000000
```

```r
## Compute the estimated A matrix at +1SD (+1) of the standardized individualism
A0 + A1
```

```
##     I1 I2 I3 I4 I5 I6 I7 I8 I9 I10         G        POS       NEG
## I1   0  0  0  0  0  0  0  0  0   0 0.8142806 -0.1588323 0.0000000
## I2   0  0  0  0  0  0  0  0  0   0 0.5735031  0.0000000 0.5757972
## I3   0  0  0  0  0  0  0  0  0   0 0.6413399  0.4444486 0.0000000
## I4   0  0  0  0  0  0  0  0  0   0 0.5396818  0.3113333 0.0000000
## I5   0  0  0  0  0  0  0  0  0   0 0.5468550  0.0000000 0.2990602
## I6   0  0  0  0  0  0  0  0  0   0 0.5508319  0.0000000 0.5850812
## I7   0  0  0  0  0  0  0  0  0   0 0.6686760  0.2835520 0.0000000
## I8   0  0  0  0  0  0  0  0  0   0 0.5145698  0.0000000 0.4234534
## I9   0  0  0  0  0  0  0  0  0   0 0.5993550  0.0000000 0.4228972
## I10  0  0  0  0  0  0  0  0  0   0 0.8431990 -0.1640900 0.0000000
## G    0  0  0  0  0  0  0  0  0   0 0.0000000  0.0000000 0.0000000
## POS  0  0  0  0  0  0  0  0  0   0 0.0000000  0.0000000 0.0000000
## NEG  0  0  0  0  0  0  0  0  0   0 0.0000000  0.0000000 0.0000000
```

```r
save.image(file="Gnambs2018.RData")

sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.1 LTS
## 
## Matrix products: default
## BLAS: /opt/microsoft/ropen/3.5.1/lib64/R/lib/libRblas.so
## LAPACK: /opt/microsoft/ropen/3.5.1/lib64/R/lib/libRlapack.so
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] metaSEM_1.2.0        OpenMx_2.11.5        RevoUtils_11.0.1    
## [4] RevoUtilsMath_11.0.0
## 
## loaded via a namespace (and not attached):
##   [1] nlme_3.1-137         RColorBrewer_1.1-2   rprojroot_1.3-2     
##   [4] mi_1.0               tools_3.5.1          backports_1.1.2     
##   [7] R6_2.3.0             rpart_4.1-13         d3Network_0.5.2.1   
##  [10] Hmisc_4.1-1          lazyeval_0.2.1       colorspace_1.3-2    
##  [13] nnet_7.3-12          tidyselect_0.2.5     gridExtra_2.3       
##  [16] mnormt_1.5-5         compiler_3.5.1       qgraph_1.5          
##  [19] fdrtool_1.2.15       htmlTable_1.12       network_1.13.0.1    
##  [22] scales_1.0.0         checkmate_1.8.5      mvtnorm_1.0-8       
##  [25] psych_1.8.10         pbapply_1.3-4        sem_3.1-9           
##  [28] stringr_1.3.1        digest_0.6.18        pbivnorm_0.6.0      
##  [31] foreign_0.8-71       minqa_1.2.4          rmarkdown_1.10      
##  [34] base64enc_0.1-3      jpeg_0.1-8           pkgconfig_2.0.2     
##  [37] htmltools_0.3.6      lme4_1.1-18-1        lisrelToR_0.1.4     
##  [40] htmlwidgets_1.3      rlang_0.3.0.1        rstudioapi_0.8      
##  [43] huge_1.2.7           bindr_0.1.1          gtools_3.8.1        
##  [46] statnet.common_4.1.4 acepack_1.4.1        dplyr_0.7.7         
##  [49] magrittr_1.5         Formula_1.2-3        Matrix_1.2-14       
##  [52] Rcpp_0.12.19         munsell_0.5.0        abind_1.4-5         
##  [55] rockchalk_1.8.117    whisker_0.3-2        stringi_1.2.4       
##  [58] yaml_2.2.0           carData_3.0-2        MASS_7.3-50         
##  [61] plyr_1.8.4           matrixcalc_1.0-3     lavaan_0.6-3        
##  [64] grid_3.5.1           parallel_3.5.1       crayon_1.3.4        
##  [67] lattice_0.20-35      semPlot_1.1          splines_3.5.1       
##  [70] sna_2.4              knitr_1.20           pillar_1.3.0        
##  [73] igraph_1.2.2         rjson_0.2.20         boot_1.3-20         
##  [76] corpcor_1.6.9        BDgraph_2.52         reshape2_1.4.3      
##  [79] stats4_3.5.1         XML_3.98-1.16        glue_1.3.0          
##  [82] evaluate_0.12        latticeExtra_0.6-28  data.table_1.11.8   
##  [85] png_0.1-7            nloptr_1.2.1         gtable_0.2.0        
##  [88] purrr_0.2.5          assertthat_0.2.0     ggplot2_3.1.0       
##  [91] semTools_0.5-1       coda_0.19-2          survival_2.43-1     
##  [94] glasso_1.10          tibble_1.4.2         arm_1.10-1          
##  [97] ggm_2.3              ellipse_0.4.1        bindrcpp_0.2.2      
## [100] cluster_2.0.7-1
```

