######################################################################################
######################################################################################
#
# HELPER FUNCTIONS FOR MODELING
#
######################################################################################
######################################################################################
#
#
######################################################################################
# tdmModCreateCVindex:
#'   Create and return a training-test-set index vector 
#' 
#'   Depending on the value of TST.kind, the returned index cvi is
#'   \enumerate{
#'   \item TST.kind="cv": a random cross validation index P([111...222...333...]) - or -
#'   \item TST.kind="rand": a random index with P([000...111...]) for training (0) and test (1) cases - or -
#'   \item TST.kind="col": the column dset[,opts$TST.COL] contains the training (0) and test (1) set division
#'         (and all records with a value <0 in column TST.COL are disregarded).
#'   }
#'   Here P(.) denotes random permutation of the sequence.
#'   Special case TST.kind="cv" and TST.NFOLD=1: make *every* record a training record, i.e. index [000...]
#'   In case TST.kind="rand" and stratified=TRUE a \emph{stratified} sample is drawn, where the strata in the 
#'   training case reflect the rel. frequency of each level of the **1st** response variable
#'   and are ensured to be at least of size 1.
#'
#'  @param dset    the data frame for which cvi is needed
#'  @param response.variables  issue a warning if \code{length(response.variables)>1}. Use the first
#'      response variable for determining strata size.
#'  @param opts    a list from which we need here the following entries
#'    \itemize{
#'      \item TST.kind:  ["cv"|"rand"|"col"]
#'      \item TST.NFOLD: number of CV folds (only relevant in case TST.kind=="cv")
#'      \item TST.COL:   column of dset containing the (0/1/<0) index (only relevant in case TST.kind=="col")
#'                       or NULL if no such column exists
#'      \item TST.FRAC:  fraction of records to set aside for testing (only relevant in case TST.kind=="rand")
#'    }
#'  @param stratified [F] do stratified sampling for TST.kind="rand" with at least one training 
#'         record for each response variable level (classification)
#'
#'  @return cvi  training-test-set (0/>0) index vector
#'               (all records with cvi<0, e.g. from column TST.COL, are disregarded)
#'
#' @note Currently stratified sampling in case TST.KIND='rand' does only work correctly for \emph{one} response variable.  
#'    If there are more than one, the right fraction of test records is taken, but the strata are drawn w.r.t. the 
#'    first response variable. (For multiple response variables we would have to return a list of cvi's or to
#'    call tdmModCreateCVindex for each response variable anew.)
#
######################################################################################
tdmModCreateCVindex <- function(dset,response.variables,opts,stratified=FALSE) {
        L = nrow(dset)
        if (opts$TST.kind=="cv") {
            #===============================================
            # CREATE NFOLD CROSSVALIDATION INDEX
            #===============================================
            cat1(opts,opts$filename,": Creating cross validation index cvi ...\n")
            nfold = opts$TST.NFOLD
            if (opts$TST.NFOLD <= 0)
              stop(sprintf("tdmModCreateCVindex: opts$TST.NFOLD must be > 0. Current value is opts$TST.NFOLD = %d", opts$TST.NFOLD));
            cvi = NULL
            ilow=1
            for (m in 1:nfold) {
                ihigh = floor(L/nfold*m)
                cvi[ilow:ihigh] = m         # i.e. [111..2222...333...]
                ilow = ihigh+1
            }
            cvi = sample(cvi)               # permute CVI randomly
            if (opts$TST.NFOLD == 1)
              cvi = 0*cvi;                  # special case: if NFOLD==1 make every record a training record
        } else {
            #=============================================
            # DIVIDE INTO TRAINING SET / TEST SET
            #=============================================
            if (opts$TST.kind=="rand") {   # make a (random) division
              if (is.null(opts$TST.FRAC))
                stop(sprintf("tdmModCreateCVindex: opts$TST.FRAC is not defined"));
              if (opts$TST.FRAC >= 1)
                stop(sprintf("tdmModCreateCVindex: opts$TST.FRAC must be < 1. Current value is opts$TST.FRAC = %f", opts$TST.FRAC));
              if (stratified) {
                # ** NEW 06/2011 ** the division is done by ***stratified*** random sampling (recommended for classification):
                cat1(opts,opts$filename,": Stratified random training-test-index with opts$TST.FRAC = ",opts$TST.FRAC*100,"%\n");
                rv <- dset[,response.variables[1]];
                lrv = length(response.variables);
                if (lrv>1) warning(sprintf("Stratified sampling is only done w.r.t. 1st response variable. It is not guaranteed to work for all %d response variables",lrv))  
                # calculate tfr, the number of training set records for each level of the response variable            
                tfr <- sapply(unique(rv),function(x) { round((1-opts$TST.FRAC)*length(which(rv==x))) })
                # (this seems complicated, but the simpler command: tfr <- round((1-opts$TST.FRAC)*table(rv));
                # does not work, because 'table' orders the levels alphabetically but 'strata' below require them in the 
                # order they appear in column rv. 'unique' sorts the levels also in the order of appearance.) 
                #
                tfr[tfr<1] <- 1;      # ensure that there is at least one record for each class 
                cvi <- rep(1,L);
                urv <- unique(rv);
                for (i in 1:length(urv))  cvi[ sample(which(rv==urv[i]), tfr[i]) ] <- 0;
                #
                #--- OLD and slow ---: 
                # the code below with strata from package sampling does the same as the two lines above,
                # but it is prohibitively slow when dset gets larger(50000 rows and more)
                #require(sampling);
                #s2=strata(dset,c(response.variables[1]),size=tfr, method="srswor");
                #cvi[s2$ID_unit] <- 0;         # set the training records   
              } else {  # i.e. stratified=FALSE
                # simple random sampling (recommended for regression):
                p <- sample(L)                  # random permutation of indices 1:L  
                # calculate tfr, the record where the test set starts (TST.FRAC)
                tfr <- (1-opts$TST.FRAC)*L
                cat1(opts,opts$filename,": Random training-test-index with opts$TST.FRAC = ",opts$TST.FRAC*100,"%\n")
                cvi <- rep(0,L);
                cvi[p[(tfr+1):L]] <- 1;         # test set index ( TST.FRAC  percent of the data)
              }
            }
            else           # i.e. opts$TST.kind=="col"
            {              # take the test-training-division as delivered in dset[,opts$TST.COL]
              if (is.null(opts$TST.COL))
                stop(sprintf("tdmModCreateCVindex: opts$TST.COL is NULL, but opts$TST.kind=='col'"));
              if (!(opts$TST.COL %in% names(dset)))
                stop(sprintf("tdmModCreateCVindex: Data frame dset does not contain a column opts$TST.COL named '%s'", opts$TST.COL));
              cat1(opts,opts$filename,": Using training-test-index from column",opts$TST.COL,"\n")
              cvi <- dset[,opts$TST.COL];
            }
        }

        cvi;
}

######################################################################################
# adjust sampsize (RF) to the response.variable's class frequency or the number of training records
# (take samp, if possible)
######################################################################################
tdmModAdjustSampsize <- function(samp, to.model, response.variable, opts) {
    if (is.null(samp)) samp=3000;
    if (length(samp)==1) newsamp = min(nrow(to.model),samp)
    else {
      ind = which(samp>as.vector(summary(to.model[,response.variable])));
      newsamp = samp;
      newsamp[ind] <- as.vector(summary(to.model[,response.variable]))[ind];
    }
    if (any(samp!=newsamp)) cat1(opts,"Clipping sampsize to ",newsamp,"\n");
    newsamp;
}

######################################################################################
# adjust cutoff (optional parameter for Random Forest)
#   a) if length(cutoff)=n.class-1,  add cutoff[n.class] as the remainder to 1.
#   b) if sum(cutoff) != 1, scale it to 1
#   c) if sum(cutoff) > 1 by a small amount (<=1e-8), reduce it to 1 or smaller.
######################################################################################
tdmModAdjustCutoff <- function(cutoff,n.class)
{
    if (!is.null(cutoff)) {
      if (length(cutoff)!=n.class) {
        if (length(cutoff)!=n.class-1)
          stop("Length of cutoff is not n.class-1!");
        cutoff[n.class] = 1-sum(cutoff[1:(n.class-1)]);
      }
      if (abs(sum(cutoff)-1)>1e-8) cutoff = cutoff/sum(cutoff);
      #
      # Due to rounding inaccurarcies, it can happen that sum(cutoff)=1+ 2.2e-16,
      # i.e. slightly larger than 1. randomForest does not allow this,
      # sum(cutoff) may not be larger than 1. The following code enforces this
      # by modifying cutoff as little as possible:
      eps = sum(cutoff)-1;
      if (eps>0) {
        if (eps>1e-8) stop("Something wrong with eps");
        eps = max(1.5*eps,1e-10);
        cutoff = cutoff-eps/n.class;
        if (sum(cutoff)>1) stop("Something wrong, sum(cutoff) still >1 ");
      }
    }

    cutoff;
}
######################################################################################
# tdmModSortedRFimport:
#
#'       Return the list of input variables sorted decreasingly by their RF-importance
#'       (use na.roughfix for missing value replacement)
#'       Decide which input variables to keep and return them in SRF$input.variables
#'
#'   @param d_train   training set
#'   @param response.variable   the target column from \code{d_train} to use for the RF-model
#'   @param input.variables   the input columns from \code{d_train} to use for the RF-model
#'   @param opts options, here we use the elements [defaults in brackets]:
#'    \itemize{
#'     \item SRF.kind:  \cr
#'          ="xperc": keep a certain importance percentage, starting from the most important variable \cr
#'          ="ndrop": drop a certain number of least important variables \cr
#'          ="nkeep": keep a certain number of most important variables \cr
#'          ="none": do not call \code{\link{tdmModSortedRFimport}} at all (see tdmRegress.r and tdmClassify.r)
#'     \item SRF.ndrop:   [0] how many variables to drop (if SRF.kind=="ndrop")
#'     \item SRF.XPerc:   [0.95] if >=0, keep that importance percentage, starting with the most
#'                   important variables (if SRF.kind=="xperc")
#'     \item SRF.calc:    [TRUE] =TRUE: calculate importance & save on SRF.file, =F: load from SRF.file
#'                   (SRF.file = Output/<filename>.SRF.<response.variable>.Rdata)
#'     \item SRF.ntree:   [50] number of RF trees
#'     \item SRF.samp    sampsize for RF
#'     \item SRF.verbose [2]
#'     \item SRF.maxS    [40] how many variables to show in plot
#'     \item SRF.minlsi  [1] a lower bound for the length of SRF$input.variables
#'     \item GD.DEVICE   if !="non", then make a bar plot on current graphic device
#'     \item CLS.CLASSWT class weight vector to use in random forest training
#'    }
#' @return SRF    a list with the following elements:
#'     \item{input.variables}{   the vector of input variables which remain after importance
#'                    processing. These are sorted by decreasing importance.}
#'     \item{s_input}{all input.variables sorted by decreasing (**NEW**) importance}
#'     \item{s_imp1}{ the importance for s_input}
#'     \item{s_dropped}{   vector with name of dropped variables}
#'     \item{lsd}{    length of s_dropped}
#'     \item{perc}{   the percentage of total importance which is in the dropped variables}
#'     \item{opts}{   some defaults might have been added}
#'
#' @author Wolfgang Konen, Patrick Koch \email{wolfgang.konen@@fh-koeln.de}
#' @export
######################################################################################
tdmModSortedRFimport <- function(d_train, response.variable, input.variables, opts)
{
    opts <- tdmOptsDefaultsFill(opts);

    ptm <- proc.time();
    filename <- opts$filename;
    dir.output <- paste(dirname(opts$dir.output),basename(opts$dir.output),sep="/")  # remove trailing "/", if it exists
    if (!file.exists(dir.output)) dir.create(dir.output);     
    SRF.file <- paste(paste(dir.output,filename,sep="/"),"SRF",response.variable,"Rdata",sep=".")
    

    if (opts$SRF.calc==TRUE) {
      if (opts$k>1) {
        load(file=SRF.file);  # load accum_imp1 as stored from previous CV-fold
      }
      
      formul <- formula(paste(response.variable, "~ ."))   # use all possible input variables
      to.model <- d_train[,c(response.variable,input.variables)]
      cat1(opts,filename,": Train RF (importance, sampsize=", opts$RF.sampsize,") ...\n")
      if (!is.null(opts$CLS.CLASSWT)) {
        cat1(opts,"Class weights: ", opts$CLS.CLASSWT,"\n")
        cwt = opts$CLS.CLASSWT*1;     # strange, but necessary: if we omit '*1' then cwt seems to be a copy-by-reference of
                                  # opts$CLS.CLASSWT. After randomForest call  cwt is changed and also opts$CLS.CLASSWT would be changed (!)
      } else { cwt=NULL; }
      if (!is.null(opts$SRF.cutoff)) cat1(opts,"Cutoff: ", opts$SRF.cutoff,"\n")

      fsRfImportance <- function(opts,cwt) {
        # we work here with a command text string and eval(...) to allow for the presence or
        # absence of certain options like "mtry" or "cutoff" which are not allowed to be NULL:
        rf.options = "ntree=opts$SRF.ntree, importance=TRUE";    # NEW: only with 'importance=TRUE' we see MeanDecreaseAccuracy,
                                                              # otherwise we get only MeanDecreaseGini (faster, but sometimes unreliable)
        rf.options = paste(rf.options,"sampsize=opts$RF.sampsize",sep=",")
        rf.options = paste(rf.options,"classwt=cwt","na.action=na.roughfix","proximity=F",sep=",")
        if (!is.null(opts$SRF.mtry)) rf.options = paste(rf.options,"mtry=opts$RF.mtry",sep=",")
        if (!is.null(opts$SRF.cutoff)) rf.options = paste(rf.options,"cutoff=opts$SRF.cutoff",sep=",")
        #dbg_chase_cutoff_bug(formul,to.model,d_train,response.variable,rf.options,opts);
        flush.console();
        res.SRF <- NULL;
        eval(parse(text=paste("res.SRF <- randomForest( formul, data=to.model,",rf.options,")")));
        res.SRF$imp1 <- importance(res.SRF, type=1);      # select MeanDecreaseAccuracy-importance (NEW, with switch 'importance=TRUE' above)
        
        res.SRF;
      }
#      fsLasso <- function(opts) {
#        # *** does not yet work in all cases !!! ***
#        require(lasso2);
#        formul <- formula(paste("as.numeric(",response.variable,") ~ ."))   # use all possible input variables
#        res.SRF <- NULL;
#        #browser()
#        eval(parse(text=paste("res.SRF <- l1ce( formul, data=to.model, bound=0.5)")));
#        res.SRF$imp1 <- as.matrix(res.SRF$coefficients[-1]);
#        
#        res.SRF;
#      }
       res.SRF = switch(opts$SRF.method
        ,"RFimp" = fsRfImportance(opts,cwt)
        #,"lasso" = fsLasso(opts)
        ,"INVALID"
        ); 
      if (res.SRF[1]=="INVALID1") {
        stop(sprintf("*** Invalid FS method=%s ***\n",opts$SRF.method));
      } 
      if (is.null(res.SRF)) stop(sprintf("FS method %s did not return a suitable result 'res.SRF'",opts$SRF.method));
      
      imp1 <- res.SRF$imp1;
      # the following calculation and saving of accum_imp1 is just a test to check whether accumulated
      # importance in case of CV (the accumulation is done over all CV-folds) has a stabilizing effect
      # on the most important variables. To test it, run accSRF-test.r (2 runs with different seeds)
      # --- Currently accum_imp1 has no influence whatsoever on the TDMR-workflow ---
      if (opts$k==1) {
        accum_imp1 = imp1;
      } else { 
        accum_imp1 = accum_imp1 + imp1;
      }
      
      s_input <- input.variables[order(imp1,decreasing=TRUE)]  # input.variables sorted by increasing importance
      s_imp1 <- imp1[order(imp1,decreasing=TRUE)]
      cat1(opts,filename,": Saving sorted RF importance to file", SRF.file, "...\n")
      save(file=SRF.file,s_input,s_imp1,input.variables,accum_imp1);
    } 
    else {
      cat1(opts,filename,": Loading sorted RF importance from file", SRF.file, "...\n")
      load(file=SRF.file);
    } # if (opts$SRF.calc)
    
    s_imp2 <- s_imp1 - min(0,min(s_imp1)-0.001);
    # s_imp2 is a copy of s_imp1, shifted by -min(s_imp1)-0.001 if s_imp1 contains negative values
    # which is not allowed for cumsum (MeanDecreaseAccuracy can be negative)

    if (opts$SRF.kind=="ndrop") {
        # remove the SRF.ndrop input variables which have the lowest importance
        # (but keep at least one):
        opts$SRF.ndrop <- min(opts$SRF.ndrop,length(s_input)-1)
        lsi <- length(s_input)-opts$SRF.ndrop;
    } else {    # i.e. opts$SRF.kind=="xperc" or "nkeep"
        if (opts$SRF.kind=="nkeep") {
          opts$SRF.nkeep <- lsi <- min(opts$SRF.nkeep,length(s_input));
        } else {    # i.e. opts$SRF.kind=="xperc"
          # keep the minimal number of those most important input variables which
          # sum up together to more than the fraction opts$SRF.XPerc of the sum of
          # all importances
          w = which(cumsum(s_imp2)/sum(s_imp2)<=opts$SRF.XPerc)
          lsi <- max(w)+1;
          lsi <- min(lsi,length(s_imp1)); # for the case SRF.XPerc==1: lsi may not be larger than #input.variables
        }
    }
    # don't let lsi = length(input.variables) become smaller than minlsi
    minlsi <- min(length(s_input),opts$SRF.minlsi);
    lsi <- max(lsi,minlsi);
    input.variables <- s_input[1:lsi]

    s_dropped <- setdiff(s_input,input.variables);
    lsd <- length(s_dropped);
    # what percentage of total importance is in the dropped variables:
    perc <- (1-sum(s_imp2[1:(length(s_imp2)-lsd)])/sum(s_imp2))*100;
    if (opts$SRF.kind=="xperc" & (perc/100>1-opts$SRF.XPerc | perc<0)) {
        cat("Something wrong with perc:",perc,"\n");
        browser();
    }

    if (opts$SRF.verbose>=2) {
        lmi <- length(s_input);
        cat(sprintf("Variables sorted by importance (%d %s):\n", lmi,
                    ifelse(lmi>100,", we show first 100 only","")));
        print(s_input[1:min(lmi,100)]);
        if (opts$SRF.kind=="ndrop" | (opts$SRF.kind=="xperc" & opts$SRF.XPerc>0.5)) {
          cat(sprintf("Dropped columns (%d [=%5.1f%% of total importance]%s):\n", lsd, perc,
                      ifelse(lsd>100,", we show first 100 only","")));
          if (lsd>0) print(s_dropped[1:min(lsd,100)]);
        } else { # i.e. opts$SRF.kind=="nkeep" or (..=="xperc" & SRF.XPerc <= 0.5)
          cat(sprintf("Kept columns (%d [=%5.1f%% of total importance]%s):\n", lsi, 100-perc,
                      ifelse(lsi>100,", we show first 100 only","")));
          if (lsi>0) print(s_input[1:min(lsi,100)]);
        }
        cat(sprintf("Proc time: %5.2f\n",(proc.time()-ptm)[1]));
    }

    # plot it:
    if (opts$GD.DEVICE!="non") {
      maxS <- min(length(s_imp1),opts$SRF.maxS)
      tdmGraphicNewWin(opts)
      par(mar=c(20,3,2,2)+0.1)
      barplot(t(s_imp1[1:maxS]), names.arg=s_input[1:maxS], las=3, cex.lab=0.75, col=2, main=paste(filename, response.variable, sep=" : "))
      tdmGraphicCloseWin(opts);
      par(mar=c(5,4,4,4) + 0.1)   # reset to standard (for multi-page PDF graphic device)
      if (opts$SRF.calc==TRUE & opts$SRF.method=="RFimp") {
        tdmGraphicNewWin(opts)
        varImpPlot(res.SRF,n.var=maxS)    # NEW: plot both MeanDecreaseAccuracy and MeanDecreaseGini
        tdmGraphicCloseWin(opts);
      }
    }

    SRF = list(input.variables=input.variables
              , s_input=s_input
              , s_imp1=s_imp1
              , s_dropped=s_dropped   # vector with name of dropped variables
              , lsd=lsd               # length of s_dropped
      	      , perc=perc             # the percentage of total importance which is in the dropped variables
      	      , opts=opts             # some defaults might have been added
              );
}

######################################################################################
# tdmModVote2Target
#
#'     Analyze how the vote fraction corresponds to reliability of prediction
#'     (for RF-prediction in case of binary (0/1) classification)
#' 
#' This function analyzes whether in different vote bins the trained RF makes
#' predictions with different reliability. Expected result: The larger the fraction
#' of trees voting for class 0 is, the smaller is the percentage of true class-1-
#' cases in this vote bin.
#' This function is somewhat specialized for the DMC2010-task.
#'
#' @param  vote0     vector: which fraction of trees votes for class 0?
#' @param  pred      vector: the predicted class for each record (0/1)
#' @param  target    vector: the true class for each vector (0/1)
#' 
#' @return a data frame with columns 
#'    \item{vcut}{  vote cut v}
#'    \item{count}{ number of cases with vote fraction in [v[i-1],v[i]]}
#'    \item{pred0}{ fraction of 0-predictions}
#'    \item{pCorr}{ fraction of correct predictions}
#'    \item{pR}{    fraction of true 1-cases}
#'
#' @author Wolfgang Konen \email{wolfgang.konen@@fh-koeln.de}
#' @export
######################################################################################
tdmModVote2Target <- function(vote0,pred,target) {
    tvot <- data.frame(v0=vote0,pred=pred,targ=target);
    vcut <- c(0.4,0.5,0.6,0.65,0.70,0.73,0.75,0.80,0.85,0.90,0.95,1.0)
    R <- nrow(tvot)
    tres <- data.frame()
    i=1; vprev=0;
    for (v in vcut) {
      ind <- which(vprev< tvot$v0 & tvot$v0 <= v)
      I <- length(ind);
      tres[i,"vcut"] <- v;
      tres[i,"count"] <- I;
      tres[i,"frac"] <- I/R;
      tres[i,"pred0"] <- length(which(tvot$pred[ind]==0))/I;
      tres[i,"pCorr"] <- length(which(tvot$targ[ind]==tvot$pred[ind]))/I;
      tres[i,"pR"] <- length(which(tvot$targ[ind]==1))/I;
      i=i+1;  vprev=v;
    }
    tres;
}

######################################################################################
# tdmModConfmat
#
#'     Calculate confusion matrix and gain (used by \code{\link{tdmClassify}})
#'
#'  @param  d 		      data frame
#'  @param  colreal     name of column in d which contains the real class
#'  @param  colpred 		name of column in d which contains the predicted class
#'  @param  opts, a list from which we use the elements: \itemize{
#'     \item gainmat   the gain matrix for each possible outcome, same size as cm$mat (see below);
#'               gainmat[R1,P2]: the gain associated with a record of real class R1 which we
#'               predict as class P2. (gain matrix = - cost matrix)
#'     \item rgain.type  {"rgain" | "meanCA" | "minCA" } see below, affects output cm$mat and cm$rgain
#'    }
#'  @return cm   a list containing: 
#'     \item{mat}{ matrix with real class levels as rows, predicted class levels columns.
#'               mat[R1,P2]: number of records with real class R1
#'               predicted as class P2, if opts$rgain.type=="rgain".
#'               If opts$rgain.type=="meanCA" or "minCA", then show this number as percentage
#'               of "records with real class R1" (percentage of each row).
#'               CAUTION: If colpred contains NA's, those cases are missing in mat (!)
#'               (but the class errors are correct as long as there are no NA's in colreal)}
#'     \item{cerr}{ class error rates, vector of size nlevels(colreal)+1.
#'               cerr[X] is the misclassification rate for real class X.
#'               cerr["Total"] is the total classification error rate.}
#'     \item{gain}{ the total gain (sum of pointwise product gainmat*cm$mat)}
#'     \item{gain.vector}{ gain.vector[X] is the gain attributed to real class label X.
#'               gain.vector["Total"] is again the total gain.}
#'     \item{gainmax}{    the maximum achievable gain, assuming perfect prediction}
#'     \item{rgain}{      ratio gain/gainmax in percent, if opts$rgain.type=="rgain";
#'               mean class accuracy percentage (i.e. mean(diag(mat)), if opts$rgain.type=="meanCA";
#'               min class accuracy percentage (i.e. min(diag(mat)), if opts$rgain.type=="minCA";}
#' 
#' @author Wolfgang Konen, Patrick Koch \email{wolfgang.konen@@fh-koeln.de}
#' @export
######################################################################################
tdmModConfmat <- function(d,colreal,colpred,opts)
{
    if (is.null(opts$rgain.type)) opts$rgain.type="rgain";

    gainmat = opts$CLS.gainmat;
    col.real <- factor(d[,colreal],levels=colnames(gainmat));
    col.pred <- factor(d[,colpred],levels=colnames(gainmat));
    #cmat = matrix(0,nrow=nlevels(col.real),ncol=nlevels(col.pred),
    #              dimnames=list(levels(col.real),levels(col.pred)))
    #-- cmat now simpler to obtain with function table (see below)
    ccase = matrix(0,nrow=1,ncol=nlevels(col.real)+1,
                  dimnames=list("class cases:",c(levels(col.real),"Total")))

    # class cases
    for (rc in levels(col.real)) {
        ccase[1,rc] <-  length(which(col.real==rc))
        #d1 <- d[col.real==rc, ]      # all records with real class rc
        #ccase[1,rc] <- dim(d1)[1]
        #for (pc in levels(col.pred)) {
        #    cmat[rc,pc] <- length(which(d1[,colpred]==pc))
        #}
    }
    ccase[1,"Total"] <- sum(ccase)      # all cases, including those with NA in col.pred
                                        # (but excluding those with NA in col.real)
    # confusion matrix
    cmat <- table(actual=col.real,predicted=col.pred)
                           # each case with NA in colpred is in *no* cell of cmat

    # class errors
    cerr = matrix(0,nrow=1,ncol=nlevels(col.pred)+1,
                  dimnames=list("class errors:",c(levels(col.pred),"Total")))
    for (rc in levels(col.real)) {
        cerr[1,rc] <- 1-cmat[rc,rc]/ccase[1,rc]
    }
    cerr[1,"Total"] <- 1-sum(diag(cmat))/ccase[1,"Total"]

    gain = sum(gainmat*cmat)
    gain.vector = matrix(0,nrow=1,ncol=nlevels(col.pred)+1,
                  dimnames=list("gain.vector",c(levels(col.pred),"Total")))
    for (rc in levels(col.real)) {
        gain.vector[1,rc] <- sum(gainmat[rc,] * cmat[rc,])
    }
    gain.vector[1,"Total"] <- sum(gain.vector)

    gainmax = sum(apply(gainmat,1,max)*rowSums(cmat))
    # the 1st term is a vector containing the max gain for each row (true class)
    # the 2nd term is a vector containing the #records for each true class

    rgain = gain/gainmax*100;
    nacase = length(which(is.na(col.pred)));
    if (sum(cmat)+nacase!=ccase[1,"Total"])
        stop("tdmModConfmat: Something wrong in NA-counting!");

    if (opts$rgain.type %in% c("meanCA","minCA")) {
      for (rc in levels(col.real)) {
          cmat[rc,] <- cmat[rc,]/ccase[1,rc];
      }
      rgain=switch(opts$rgain.type
            , "meanCA" = mean(diag(cmat))
            , "minCA" = min(diag(cmat))
            );
    }

    cm = list( mat=cmat
              ,cerr=cerr
              ,ccase=ccase
              ,nacase=nacase
              ,gain=gain
              ,gain.vector=gain.vector
              ,gainmax=gainmax
              ,rgain=rgain
              );

    return(cm)
}

# debug info to chase the "Incorrect cutoff specified bug" in training randomForest
dbg_chase_cutoff_bug <- function(formul,to.model,d_train,response.variable,rf.options,opts) {
  print2(opts,c(Targets=NA,table(to.model[,response.variable])))
  dir.Rdata <- paste(dirname(opts$dir.Rdata),basename(opts$dir.Rdata),sep="/")  # remove trailing "/", if it exists
  if (!file.exists(dir.Rdata)) dir.create(dir.Rdata);     
  save(formul,to.model,response.variable,rf.options,opts,file=paste(dir.Rdata,"rf_input_dbg.Rdata",sep="/"));
  cat1(opts,"RF-debug-data saved to", paste(dir.Rdata,"rf_input_dbg.Rdata",sep="/"),"\n");
  if (!is.null(opts$SRF.cutoff))
    if (length(levels(d_train[,response.variable]))!=length(opts$SRF.cutoff)) {
      warning("Cutoff problems ahead!!",immediate.=TRUE)
      browser()
    }
    if (sum(opts$SRF.cutoff)-1>0) {
      warning(sprintf("Sorry, but sum(opts$SRF.cutoff) is larger than 1 by %f",sum(opts$SRF.cutoff)-1),immediate.=TRUE)
      browser()
    }
}


