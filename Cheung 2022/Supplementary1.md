---
title: 'Synthesizing Indirect Effects in Mediation Models with Meta-Analytic Methods: Supplementary Materials 1'
author: "Mike Cheung"
date: 'June 01, 2021'
output:
  html_document:
    keep_md: yes
    self_contained: yes
    theme: united
    toc: yes
    toc_depth: 3
  pdf_document:
    toc: yes
    toc_depth: 3
  word_document:
    toc: yes
---

* This file demonstrates how to compute effect sizes and their sampling covariance matrix with two approaches using the delta method. The first one uses a numeric approach with the structural equation modeling (SEM) framework. The second approach computes the sampling covariance matrix with the symbolic calculations.

# Numeric calculations with the SEM approach

## One mediator

```r
library(metaSEM)

## Model with one mediator: x -> m -> y
model1 <- "y ~ c*x + b*m
           m ~ a*x
           # Define indirect and direct effects
           Indirect := a*b
           Direct := c"

## Diplay the model
plot(model1)
```

![](Supplementary1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
## Sample correlation matrix
my.cor <- matrix(c(1, .5, .3,
                   .5, 1, .4,
                   .3, .4, 1),
                 nrow = 3,
                 ncol = 3,
                 dimnames = list(c("y", "m", "x"),
                                 c("y", "m", "x")))
my.cor
```

```
##     y   m   x
## y 1.0 0.5 0.3
## m 0.5 1.0 0.4
## x 0.3 0.4 1.0
```

```r
## Calculate the indirect and direct effects and their sampling covariance matrix
calEffSizes(model=model1, n=300, Cov=my.cor)
```

```
## $ES
##  Indirect    Direct 
## 0.1809524 0.1190476 
## 
## $VCOV
##               Indirect        Direct
## Indirect  0.0010416478 -0.0004686319
## Direct   -0.0004686319  0.0029289494
```

## Two parallel mediators

```r
## Model with two specific mediators: x -> m1 -> y and x -> m2 -> y
model2 <- "y ~ e*x + b*m1 + d*m2
           m1 ~ a*x
           m2 ~ c*x
           # m1 and m2 are correlated
           m1 ~~ m2
           # Define indirect and direct effects
           Ind_m1 := a*b
           Ind_m2 := c*d
           Direct := e"

plot(model2, layout="circle")
```

![](Supplementary1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## Sample correlation matrix
my.cor <- matrix(c(1, .5, .6, .3, 
                   .5, 1, .4, .2,
                   .6, .4, 1, .3,
                   .3, .2, .3, 1),
                 nrow = 4,
                 ncol = 4,
                 dimnames = list(c("y", "m1", "m2", "x"),
                                 c("y", "m1", "m2", "x")))
my.cor
```

```
##      y  m1  m2   x
## y  1.0 0.5 0.6 0.3
## m1 0.5 1.0 0.4 0.2
## m2 0.6 0.4 1.0 0.3
## x  0.3 0.2 0.3 1.0
```

```r
## Calculate the indirect and direct effects and their sampling covariance matrix
calEffSizes(model=model2, n=300, Cov=my.cor)
```

```
## $ES
##     Ind_m1     Ind_m2     Direct 
## 0.05989446 0.13456464 0.10554090 
## 
## $VCOV
##               Ind_m1        Ind_m2        Direct
## Ind_m1  0.0003749419  0.0001029453 -0.0000386612
## Ind_m2  0.0001029453  0.0008190651 -0.0001594774
## Direct -0.0000386612 -0.0001594774  0.0020297130
```

## Two serial mediators

```r
## Model with two intermediate mediators: x -> m1 -> m2 -> y
model3 <- "y ~ e*x + f*m1 + c*m2
           m1 ~ a*x
           m2 ~ b*m1
           m2 ~ d*x
           # Define indirect and direct effects
           Ind_m1m2 := a*b*c
           Ind_m1 := a*f
           Ind_m2 := d*c
           Direct := e"

plot(model3, layout="circle")
```

![](Supplementary1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## Calculate the indirect and direct effects and their sampling covariance matrix
calEffSizes(model=model3, n=300, Cov=my.cor)
```

```
## $ES
##   Ind_m1m2     Ind_m1     Ind_m2     Direct 
## 0.03177221 0.05989446 0.10279244 0.10554090 
## 
## $VCOV
##               Ind_m1m2        Ind_m1        Ind_m2        Direct
## Ind_m1m2  1.144605e-04  0.0001405997  1.559037e-05 -0.0000376544
## Ind_m1    1.405997e-04  0.0003749419 -3.765440e-05 -0.0000386612
## Ind_m2    1.559037e-05 -0.0000376544  6.734239e-04 -0.0001218231
## Direct   -3.765440e-05 -0.0000386612 -1.218231e-04  0.0020297130
```

# Symbolic calculations

## One mediator

```r
library(symSEM)

## fn: The effect sizes
## Covfn: Sampling covariance matrix of fn: "b^2*Va+2*b*a*Cba+a^2*Vb"
## Va: Sampling variance of a
## Vb: Sampling variance of b
## Cba: Sampling covariance of a and b
deltamethod(fn="a*b")
```

```
## $fn
##     [,1] 
## fn1 "a*b"
## 
## $Covfn
##     fn1                      
## fn1 "b^2*Va+2*b*a*Cba+a^2*Vb"
## 
## $vars
## [1] "a" "b"
## 
## $Covvars
##   a     b    
## a "Va"  "Cba"
## b "Cba" "Vb" 
## 
## $Jmatrix
##     a   b  
## fn1 "b" "a"
```

## Two parallel mediators

```r
deltamethod(fn=c("a*b", "c*d"))
```

```
## $fn
##     [,1] 
## fn1 "a*b"
## fn2 "c*d"
## 
## $Covfn
##     fn1                               fn2                              
## fn1 "b^2*Va+2*b*a*Cba+a^2*Vb"         "b*Cca*d+b*Cda*c+a*Ccb*d+a*Cdb*c"
## fn2 "d*Cca*b+d*Ccb*a+c*Cda*b+c*Cdb*a" "d^2*Vc+2*d*c*Cdc+c^2*Vd"        
## 
## $vars
## [1] "a" "b" "c" "d"
## 
## $Covvars
##   a     b     c     d    
## a "Va"  "Cba" "Cca" "Cda"
## b "Cba" "Vb"  "Ccb" "Cdb"
## c "Cca" "Ccb" "Vc"  "Cdc"
## d "Cda" "Cdb" "Cdc" "Vd" 
## 
## $Jmatrix
##     a   b   c   d  
## fn1 "b" "a" "0" "0"
## fn2 "0" "0" "d" "c"
```

## Two serial mediators

```r
deltamethod(fn="a*b*c")
```

```
## $fn
##     [,1]   
## fn1 "a*b*c"
## 
## $Covfn
##     fn1                                                                         
## fn1 "b^2*c^2*Va+2*b^2*c*a*Cca+b^2*a^2*Vc+2*b*c^2*a*Cba+2*b*c*a^2*Ccb+c^2*a^2*Vb"
## 
## $vars
## [1] "a" "b" "c"
## 
## $Covvars
##   a     b     c    
## a "Va"  "Cba" "Cca"
## b "Cba" "Vb"  "Ccb"
## c "Cca" "Ccb" "Vc" 
## 
## $Jmatrix
##     a     b     c    
## fn1 "b*c" "a*c" "a*b"
```

```r
sessionInfo()
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.2 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
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
## [1] symSEM_0.1.1    metaSEM_1.2.5.1 OpenMx_2.19.5  
## 
## loaded via a namespace (and not attached):
##   [1] nlme_3.1-152        RColorBrewer_1.1-2  rprojroot_1.3-2    
##   [4] mi_1.0              tools_4.0.3         backports_1.2.1    
##   [7] R6_2.5.0            rpart_4.1-15        Hmisc_4.4-1        
##  [10] colorspace_1.4-1    nnet_7.3-14         withr_2.3.0        
##  [13] tidyselect_1.1.0    gridExtra_2.3       mnormt_2.0.2       
##  [16] compiler_4.0.3      fdrtool_1.2.15      qgraph_1.6.9       
##  [19] htmlTable_2.1.0     regsem_1.6.2        desc_1.2.0         
##  [22] scales_1.1.1        checkmate_2.0.0     psych_2.0.9        
##  [25] mvtnorm_1.1-1       pbapply_1.4-3       sem_3.1-11         
##  [28] stringr_1.4.0       digest_0.6.27       pbivnorm_0.6.0     
##  [31] foreign_0.8-80      minqa_1.2.4         rmarkdown_2.7      
##  [34] base64enc_0.1-3     jpeg_0.1-8.1        pkgconfig_2.0.3    
##  [37] htmltools_0.5.0     lme4_1.1-26         lisrelToR_0.1.4    
##  [40] htmlwidgets_1.5.2   rlang_0.4.10        rstudioapi_0.11    
##  [43] generics_0.0.2      gtools_3.8.2        dplyr_1.0.2        
##  [46] zip_2.1.1           magrittr_1.5        Formula_1.2-3      
##  [49] Matrix_1.2-18       Rcpp_1.0.5          munsell_0.5.0      
##  [52] abind_1.4-5         rockchalk_1.8.144   lifecycle_0.2.0    
##  [55] stringi_1.5.3       yaml_2.2.1          carData_3.0-4      
##  [58] MASS_7.3-53         plyr_1.8.6          matrixcalc_1.0-3   
##  [61] lavaan_0.6-8        grid_4.0.3          parallel_4.0.3     
##  [64] crayon_1.3.4        lattice_0.20-41     semPlot_1.1.2      
##  [67] kutils_1.70         splines_4.0.3       Ryacas_1.1.3.1     
##  [70] tmvnsim_1.0-2       knitr_1.30          pillar_1.4.6       
##  [73] igraph_1.2.6        boot_1.3-25         corpcor_1.6.9      
##  [76] pkgload_1.1.0       reshape2_1.4.4      stats4_4.0.3       
##  [79] XML_3.99-0.5        glue_1.4.2          evaluate_0.14      
##  [82] latticeExtra_0.6-29 data.table_1.13.2   png_0.1-7          
##  [85] vctrs_0.3.4         nloptr_1.2.2.2      testthat_3.0.2     
##  [88] gtable_0.3.0        purrr_0.3.4         assertthat_0.2.1   
##  [91] ggplot2_3.3.2       xfun_0.19           openxlsx_4.2.2     
##  [94] xtable_1.8-4        coda_0.19-4         Rsolnp_1.16        
##  [97] glasso_1.11         survival_3.2-7      truncnorm_1.0-8    
## [100] tibble_3.0.4        arm_1.11-2          ellipse_0.4.2      
## [103] cluster_2.1.0       statmod_1.4.35      ellipsis_0.3.1
```




