require(TDMR);
load("cma_j1.rda");
load("cma_j2.rda");

x <- read.table("cma_j.des",header=FALSE);

oldwd=getwd(); setwd(tdm$path);   #getwd();
result <- tdmStartOther(x);
setwd(oldwd);

spotConfig = envT$spotConfig;
res=envT$res;
bst=envT$bst;
#print(getwd());
#print("Saving cma_j2,j3.rda");
save(list=ls(all=TRUE),file="cma_j2.rda");    # save modified envT   
save(list=c("spotConfig","res","bst"),file="cma_j3.rda");   # save the things modified in envT  for re-load in cma_jTuner
                                                            # (we can not re-load envT there, because this would constitute 
                                                            # *another* environment envT than the one passed into cma_jTuner

cat("cma_j.r successfully finished with result\n");
cat(result,"\n");