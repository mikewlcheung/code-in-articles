* Read the data file "data.csv".
import delim using data.csv

* Generate se (standard error) from v (sampling variance)
generate se=sqrt(v)

* Run a meta-analysis on y with se as the standard error.
meta y se

* Run a mixed-effects meta-analysis on y with x as the predictor and se as the standard error.
metareg y x, wsse(se)

