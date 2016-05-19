opts= tdmOptsDefaultsSet();
tdm = list(umode="RSUB",runList="");
data(iris);
dataObj=tdmSplitTestData(opts,tdm,dset=iris);
print(dataObj)
