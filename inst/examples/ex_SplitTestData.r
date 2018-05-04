opts= tdmOptsDefaultsSet();
tdm = list(umode="RSUB",runList="");
data(iris);
dataObj=tdmReadAndSplit(opts,tdm,dset=iris);
print(dataObj)
