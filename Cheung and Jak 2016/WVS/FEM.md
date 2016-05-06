# Comparisons of random split with with the World Values Survey data
Mike Cheung and Suzanne Jak  
1 Apr 2016  


# Multiple regression: Fixed-effects model
* We randomly split the data into *k*=1 (raw data) to *k*=1000 *studies*.
* We regress *satisfaction with your life* (A170) on *subjective state of health* (A009), *freedom of choice and control* (A173), *satisfaction with financial situation of household* (C006), *sex* (X001), and *age* (X003) in each *study*.
* The following figure displays the regression model.

![](FEM_files/figure-html/unnamed-chunk-1-1.png)

* The estimated regression coefficients with their estimated sampling covariance matrices are treated as multiple effect sizes for a multivariate fixed-effects meta-analysis.
* The variables used in this demonstration are:
    + *State of health (subjective)* (A009): 1 (Very good); 4 (Very poor) (it is reversed before the analyses)
    + *Satisfaction with your life* (A170): 1 (Dissatisfied); 10 (Satisfied)
    + *How much freedom of choice and control* (A173): 1 (None at all); 10 (A great deal)
    + *Satisfaction with financial situation of household* (C006): 1 (None at all); 10 (A great deal)
    + *Sex* (X001): 1 (Male); 2 (Female)
    + *Age* (X003)
    + Negative values in the original dataset represent missing values. They are recoded into missing values (NA) before the analysis.

```r
## Load the libraries
library("data.table")
library("lavaan")
library("metaSEM")

## Load the data
load("WVS.Rdata")
  
## Select the relevant variables to minimize the memory usage
WVS <- WVS[, list(A009, A170, A173, C006, X001, X003, S002, S003)]

## Reverse coding for A009
## Recode all negative values as NA
## Age (X003) is divided by 10 to improve numerical stability.
WVS[, `:=`(A009 = 5-ifelse(A009 < 0, yes=NA, no=A009),
           A170 =   ifelse(A170 < 0, yes=NA, no=A170),
           A173 =   ifelse(A173 < 0, yes=NA, no=A173),
           C006 =   ifelse(C006 < 0, yes=NA, no=C006),
           X001 =   ifelse(X001 < 0, yes=NA, no=X001),
           X003 =   ifelse(X003 < 0, yes=NA, no=X003/10))]
  
## x: no. of studies
random_split <- function(x) {
  ## Make a copy of WVS
  my.df <- copy(WVS)

  ## Randomly split the data into 100 studies
  Study <- sample(1:nrow(my.df)) %% x + 1

  ## Append "Study" into the dataset
  my.df[, Study:=Study]

  ## Set "Study" as the key for grouping
  setkeyv(my.df, "Study")

## Function to fit regression analysis
## y1 to y5: Regression coefficients from A170, A009, A173, C006, X001, and X003.
## v11 to v55: Sampling covariance matrix of the parameter estimates
  fun.reg <- function(dt) { fit <- try(lm(A170~A009+A173+C006+X001+X003, data=dt), silent=TRUE)

                            ## If there are errors during the analysis, it returns missing values.
                            if (is.element("try-error", class(fit))) {
                              list(y1=NaN,y2=NaN,y3=NaN,y4=NaN,y5=NaN,
                                   v11=NaN,v21=NaN,v31=NaN,v41=NaN,v51=NaN,
                                   v22=NaN,v32=NaN,v42=NaN,v52=NaN,v33=NaN,
                                   v43=NaN,v53=NaN,v44=NaN,v54=NaN,v55=NaN)
                            } else {
                              ## Extract the regression coefficients excluding the intercept
                              y <- coef(fit)
                              ## Extract the sampling covariance matrix excluding the intercept
                              v <- lav_matrix_vech(vcov(fit)[-1,-1])
                              list(y1=y[2],y2=y[3],y3=y[4],y4=y[5],y5=y[6],
                                   v11=v[1],v21=v[2],v31=v[3],v41=v[4],v51=v[5],
                                   v22=v[6],v32=v[7],v42=v[8],v52=v[9],v33=v[10],
                                   v43=v[11],v53=v[12],v44=v[13],v54=v[14],v55=v[15])
                              }
                            }

  ########## Split data by "Study" and analyze data with the fun.reg() function on each "Study"
  FEM1.reg <- my.df[, fun.reg(.SD), by=list(Study)]

  ########## Meta-analyze results with a multivariate fixed-effects meta-analysis:
  ########## Variance component is fixed at 0: RE.constraints=matrix(0, ncol=5, nrow=5)
  FEM2.reg <- meta(y=cbind(y1,y2,y3,y4,y5),
                   v=cbind(v11,v21,v31,v41,v51,v22,v32,v42,v52,v33,v43,v53,v44,v54,v55),
                   data=FEM1.reg, RE.constraints=matrix(0, ncol=5, nrow=5),
                   model.name="Regression analysis FEM")
  FEM2.reg
}

## Set seed for replicability
set.seed (871139100)
my.reg <- lapply(list(5,10,50,100,500,1000), random_split)

my.coeff <- sapply(my.reg, coef)
my.se <- sapply(my.reg, function(x) sqrt(diag(vcov(x))))

## Analysis of raw data (k=1)
my.lm <- lm(A170~A009+A173+C006+X001+X003, data=WVS)

my.coef <- cbind( coef(my.lm)[-1], my.coeff )
colnames(my.coef) <- paste0("k=",c(1,5,10,50,100,500,1000))

my.se <- cbind( sqrt(diag(vcov(my.lm)))[-1], my.se )
colnames(my.se) <- paste0("k=",c(1,5,10,50,100,500,1000))

write.csv(round(my.coef,4), file="coefficients.csv")
write.csv(round(my.se,4), file="SEs.csv")

knitr::kable(my.coef, digits=4, caption="Regression coefficients")
```



Table: Regression coefficients

           k=1      k=5     k=10     k=50    k=100    k=500   k=1000
-----  -------  -------  -------  -------  -------  -------  -------
A009    0.4333   0.4333   0.4333   0.4333   0.4334   0.4330   0.4336
A173    0.2313   0.2313   0.2313   0.2313   0.2314   0.2315   0.2322
C006    0.4243   0.4243   0.4243   0.4244   0.4245   0.4257   0.4259
X001    0.1708   0.1707   0.1708   0.1708   0.1705   0.1701   0.1698
X003    0.0580   0.0580   0.0580   0.0580   0.0581   0.0579   0.0575

```r
knitr::kable(my.se, digits=4, caption="Standard errors")
```



Table: Standard errors

           k=1      k=5     k=10     k=50    k=100    k=500   k=1000
-----  -------  -------  -------  -------  -------  -------  -------
A009    0.0043   0.0043   0.0043   0.0043   0.0043   0.0043   0.0043
A173    0.0015   0.0015   0.0015   0.0015   0.0015   0.0015   0.0015
C006    0.0015   0.0015   0.0015   0.0015   0.0015   0.0014   0.0014
X001    0.0070   0.0070   0.0070   0.0070   0.0070   0.0069   0.0069
X003    0.0023   0.0023   0.0023   0.0023   0.0023   0.0022   0.0022

# Settings of the R system

```r
sessionInfo()
```

```
## R version 3.2.4 Revised (2016-03-16 r70336)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 14.04.4 LTS
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
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] metaSEM_0.9.7-3  OpenMx_2.5.2     Rcpp_0.12.4      Matrix_1.2-4    
## [5] MASS_7.3-45      digest_0.6.9     data.table_1.9.6 lavaan_0.5-20   
## [9] semPlot_1.0.1   
## 
## loaded via a namespace (and not attached):
##  [1] jsonlite_0.9.19     splines_3.2.4       ellipse_0.3-8      
##  [4] gtools_3.5.0        Formula_1.2-1       highr_0.5.1        
##  [7] stats4_3.2.4        latticeExtra_0.6-28 d3Network_0.5.2.1  
## [10] yaml_2.1.13         lisrelToR_0.1.4     pbivnorm_0.6.0     
## [13] lattice_0.20-33     quantreg_5.21       quadprog_1.5-5     
## [16] chron_2.3-47        RColorBrewer_1.1-2  ggm_2.3            
## [19] minqa_1.2.4         colorspace_1.2-6    htmltools_0.3.5    
## [22] plyr_1.8.3          psych_1.5.8         XML_3.98-1.4       
## [25] SparseM_1.7         DiagrammeR_0.8.2    corpcor_1.6.8      
## [28] scales_0.4.0        whisker_0.3-2       glasso_1.8         
## [31] sna_2.3-2           jpeg_0.1-8          fdrtool_1.2.15     
## [34] lme4_1.1-11         MatrixModels_0.4-1  huge_1.2.7         
## [37] arm_1.8-6           rockchalk_1.8.101   mgcv_1.8-12        
## [40] car_2.1-2           ggplot2_2.1.0       nnet_7.3-12        
## [43] pbkrtest_0.4-6      mnormt_1.5-4        survival_2.38-3    
## [46] magrittr_1.5        evaluate_0.8.3      nlme_3.1-126       
## [49] foreign_0.8-66      tools_3.2.4         formatR_1.3        
## [52] stringr_1.0.0       munsell_0.4.3       cluster_2.0.3      
## [55] sem_3.1-6           grid_3.2.4          nloptr_1.0.4       
## [58] rstudioapi_0.5      rjson_0.2.15        htmlwidgets_0.6    
## [61] visNetwork_0.2.1    igraph_1.0.1        tcltk_3.2.4        
## [64] rmarkdown_0.9.5     boot_1.3-18         mi_1.0             
## [67] gtable_0.2.0        abind_1.4-3         reshape2_1.4.1     
## [70] qgraph_1.3.2        gridExtra_2.2.1     knitr_1.12.3       
## [73] Hmisc_3.17-2        stringi_1.0-1       matrixcalc_1.0-3   
## [76] rpart_4.1-10        acepack_1.3-3.3     png_0.1-7          
## [79] coda_0.18-1
```
