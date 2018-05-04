mapSC2Ctrl <- function(sC) {
  d=data.frame(replace=c("numeric","integer","factor"),
               name=c("FLOAT","INT","FACTOR"))
  control <- list();
  control$types = as.vector(d$replace[match(sC$alg.roi$type, d$name)] )
  control$funEvals = 50; #sC$auto.loop.nevals;
  control$replicates = sC$replicates;
  control$OCBA = sC$OCBA;
  control$seedSPOT = sC$seedSPOT;
  control$noise = TRUE;
  control$plots = TRUE;
  control$design = sC$design;
  control$designControl = list(
    size = sC$designControl.size,
    replicates = sC$designControl.replicates
  );
  # designControl$size = sC$init.design.size;
  # designControl$retries = sC$init.design.retries;
  # designControl$replicates = sC$init.design.repeats;
  # designControl$types = control$types;
  # control$designControl = designControl;
  control$model = sC$model;
  modelControl <- list();
  modelControl$useLambda = TRUE;    ## test WK
  optimizerControl$retries = sC$seq.design.retries;
  control$modelControl = modelControl;
  control;
}

#
# --- start of main
# 

mySC=list(alg.roi=data.frame(lower=c(0.1, 5,0.9),
                             upper=c(0.8,15,1.0),type=rep("FLOAT",3)));
row.names(mySC$alg.roi) <- c("CUTOFF1","CLASSWT2","XPERC");
mySC <- setParams(mySC,defaultSC());

roiLower <- mySC$alg.roi[,1];
roiUpper <- mySC$alg.roi[,2];

spotControl <- mapSC2Ctrl(mySC);

# i=0;
# fun <- function(x){i<<-i+1;cat(i,"\n"); funSphere(x)+rnorm(nrow(x))}

fun <- function(x){funSphere(x)+rnorm(nrow(x))}
#fun <- function(x){funSphere(x)}

#res <- spot(,fun,roiLower,roiUpper,control=spotControl);


##
## this is from the spot examples and it shows that the best solution is *not* merged over 
## replicates. Then, what is the reason for using replicates???
##

res1 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
		control=list(funEvals=100,noise=TRUE)) #noisy objective
res2 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
		control=list(funEvals=100,noise=TRUE,replicates=2,
		designControl=list(replicates=2))) #noise with replicated evaluations
res3 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
		control=list(funEvals=100,noise=TRUE,replicates=2,OCBA=T,OCBABudget=1,
		designControl=list(replicates=2))) #and with OCBA
### Check results with non-noisy function:
funSphere(res1$xbest)
funSphere(res2$xbest)
funSphere(res3$xbest)
