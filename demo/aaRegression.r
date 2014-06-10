#*# --------- demo/aaRegression.r ---------
#*# This demo shows a simple data mining process (phase 1 of TDMR) for regression on
#*# dataset iris.
#*# The data mining process in tdmRegressLoop calls randomForest as the prediction model.
#*# It is called opts$NRUN=2 times with different random train-validation set splits.
#*#
opts=tdmOptsDefaultsSet()                       # set all defaults for data mining process
gdObj <- tdmGraAndLogInitialize(opts);          # init graphics and log file

data(iris)
response.variables="Petal.Length"                # names, not data (!)
input.variables=setdiff(names(iris),"Petal.Length")
opts$rgain.type="rmae"

result = tdmRegressLoop(iris,response.variables,input.variables,opts)
