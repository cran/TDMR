#library(Rcmdr)      # no longer necessary
library(car)
library(rgl)  #, pos=4)
library(mgcv) #, pos=4)
library(twiddler)

#res = read.table("cmaes/sonar_04_cmaes_01.res",header=T, sep=" ");
res = envT$res;
cat("Number of evaluations = ",length(res$Y),"\n");

attach(res)
nam = names(res)
a=get("XPERC")
twiddle(scatter3d(xname, Y, zname, fit="quadratic", residuals=TRUE, bg="white"
                 ,axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)
       ,xname=combo(CUTOFF1,CLASSWT2,XPERC)
       ,zname=combo(CUTOFF1,CLASSWT2,XPERC)
       );

rgl.bringtotop();
detach(res);
#rgl.snapshot("plotAppAcid.png")

