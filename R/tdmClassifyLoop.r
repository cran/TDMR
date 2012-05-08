######################################################################################
# tdmClassifyLoop:
#
#'    Core classification double loop of TDMR returning a \code{\link{TDMclassifier}} object. 
#'    
#'    tdmClassifyLoop contains a double loop (opts$NRUN and CV-folds)
#'    and calls \code{\link{tdmClassify}}. It is called  by all R-functions main_*. \cr
#'    It returns an object of class \code{\link{TDMclassifier}}.
#'
#'
#'   @param dset    the data frame for which cvi is needed
#'   @param response.variables   name of column which carries the target variable - or -
#'                   vector of names specifying multiple target columns
#'                   (these columns are not used during prediction, only for evaluation)
#'   @param input.variables     vector with names of input columns
#'   @param opts    a list from which we need here the following entries
#'     \describe{
#'       \item{\code{NRUN}}{ number of runs (outer loop)}
#'       \item{\code{TST.SEED}}{ =NULL: get a new random number seed with \code{\link{tdmRandomSeed}}. =any value: set the random number seed
#'               to this value to get reproducible random numbers and thus reproducible training-test-set-selection.
#'               (only relevant in case TST.kind=="cv" or "rand") (see also MOD.SEED in \code{\link{tdmClassify}})}
#'       \item{\code{TST.kind}}{ how to create cvi, handed over to \code{\link{tdmModCreateCVindex}}. If TST.kind="col", then cvi is taken from dset[,opts$TST.col].}
#'       \item{\code{GD.RESTART}}{ [TRUE] =TRUE/FALSE: do/don't restart graphic devices}
#'       \item{\code{GD.DEVICE}}{ ["non"|"win"|"pdf"|"png"]}
#'     }
#'
#'   @return \code{result},  an object of class \code{\link{TDMclassifier}}, this is a list with results, containing
#'       \item{lastRes}{ last run, last fold: result from \code{\link{tdmClassify}}}
#'       \item{C_train}{ classification error on training set}
#'       \item{G_train}{ gain on training set}
#'       \item{R_train}{ relative gain on training set (percentage of max. gain on this set)}
#'       \item{*_test}{ --- similar, with test set instead of training set}
#'       \item{*_test2}{ --- similar, with test2 set instead of training set}
#'
#'    Each performance measure \code{C_*, G_*, R_*} is a vector of length \code{opts$NRUN}. To be specific, \code{C_train[i]} is the
#'    classification error on the training set from the \code{i}-th run. This error is \code{mean(res$allEVAL$cerr.trn)}, i.e. the
#'    mean of the classification errors from all response variables when \code{res} is the return value of  \code{\link{tdmClassify}}.
#'    In the case of cross validation, for each performance measure an additional averaging over all folds is done.
#'
#' @seealso   \code{\link{print.TDMclassifier}}, \code{\link{tdmClassify}}, \code{\link{tdmRegress}}, \code{\link{tdmRegressLoop}}
#' @author Wolfgang Konen (\email{wolfgang.konen@@fh-koeln.de}), FHK, Sep'2010 - Apr'2012
#' @aliases TDMclassifier 
#' @example demo/demo00sonar.r
#' @export
######################################################################################
tdmClassifyLoop <- function(dset,response.variables,input.variables,opts) {
  	if (exists(".Random.seed")) SAVESEED<-.Random.seed	   #save the Random Number Generator RNG status
#print(.Random.seed[1:6])
    if (is.null(opts$PRE.PCA.numericV)) opts$PRE.PCA.numericV <- input.variables;

    C_train <- C_test <- C_test2 <- G_train <- G_test <- NULL # reserve names (dynamic extension of
    R_train <- R_test <- R_test2 <- G_test2 <- NULL           # these vectors at and of for-i-loop)

    if (opts$READ.TST==TRUE & opts$TST.kind!="col")
      warning(sprintf("Are you sure you want opts$READ.TST=TRUE, but opts$TST.kind!='col'? Actual value is opts$TST.kind='%s'.",opts$TST.kind));

    predProbList=list();
    #predProbList=list();
    
    for (i in 1:opts$NRUN) {
        opts$i = i;

        #=============================================================================
        # PART 3: CREATE NFOLD CROSSVALIDATION INDEX  OR DIVIDE IN TRAINING / TEST SET
        #=============================================================================
        if (is.null(opts$TST.SEED)) {
          # NEW: when called via SPOT, the RNG might be at (different but) fixed seed in each call.
          #      But we want different seeds (for test set selection) to see the variability
          set.seed(tdmRandomSeed());
        } else if (opts$TST.SEED=="algSeed") {    # use the seed from SPOT:
          # opts$ALG.SEED is set in tdmStartSpot to des$SEED[k]+r. This meens that the overall r'th 
          # evaluation of a design point gets the seed spotConfig$alg.seed+r
          newseed=opts$ALG.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed); 
        } else {
          newseed=opts$TST.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed);  # if you want reproducably the same training/test sets,
        }                     # but different for each run i and each repeat (opts$rep)
#print(.Random.seed[1:6])

        cvi <- tdmModCreateCVindex(dset,response.variables,opts,stratified=TRUE);    
        nfold = max(cvi);

#print(cvi[1:20])
#browser();
        #=============================================
        # PART 4: MODELING, EVALUATION, VISUALIZATION
        #=============================================
        allerr = NULL;
        predProb=list();
        
        for (k in 1:nfold) {
            opts$k=k;
            opts$the.nfold = nfold;
            cat1(opts,"\n")
            d_test  <- dset[cvi==k, ]            # test set
            d_train <- dset[cvi!=k & cvi>=0, ]   # training set (disregard records with cvi<0)
            d_dis   <- dset[cvi!=k & cvi==-1, ]  # disregard set (needed for active learning)
            d_test  <- bind_response(d_test , "IND.dset", which(cvi==k));
            d_train <- bind_response(d_train, "IND.dset", which(cvi!=k & cvi>=0));
            d_dis   <- bind_response(d_dis  , "IND.dset", which(cvi!=k & cvi==-1));
            #if (nrow(d_train)+nrow(d_test)+nrow(d_dis) != nrow(dset))
            #  stop("Something wrong, the union of d_train, d_test and d_dis does not cover dset");
 
            if (opts$PRE.PCA.npc>0 | opts$PRE.PCA!="none") {
              # a) do PCA on the numeric variables of d_train, if opts$PRE.PCA!="none"
              # b) add monomials of degree 2 for the first opts$PRE.PCA.npc numeric variables
              # c) apply this PCA and monomials to d_test and d_dis in the same way
              other.variables <- setdiff(input.variables,opts$PRE.PCA.numericV);
              pca <- tdmPrePCA.train(d_train,opts);                 # see tdmPreprocUtils.r
              d_train <- pca$dset;
              d_test <- tdmPrePCA.apply(d_test,pca$pcaList,opts,d_train)$dset;
              d_dis <- tdmPrePCA.apply(d_dis,pca$pcaList,opts,d_train)$dset;

              input.variables <- union(pca$numeric.variables,other.variables);
              opts$PRE.SFA.numericV <- pca$numeric.variables;
              if (length(setdiff(input.variables,names(d_train)))>0) 
                  stop("Some elements of input.variables are not columns of d_train");
            }

            # the SFA preprocessing is now in tdmClassify, inside response.variable for-loop
            # (because SFA depends on response.variable)

            res <- tdmClassify(d_train,d_test,d_dis,response.variables,input.variables,opts)

            # predProb$Val: bind the different folds of CV (cross validation) together. If no CV, then nfold=1 ->> only results from one vali set.
            predProb$Val = rbind(predProb$Val,res$predProb$Val);
            # predProb$Trn: take only the results from the first fold
            if (k==1) predProb$Trn = res$predProb$Trn;
            
            allerr = rbind(allerr,as.data.frame(list(cerr.trn=mean(res$allEVAL$cerr.trn)
                                                    ,gain.trn=mean(res$allEVAL$gain.trn)
                                                    ,rgain.trn=mean(res$allEVAL$rgain.trn)
                                                    ,cerr.tst=mean(res$allEVAL$cerr.tst)
                                                    ,gain.tst=mean(res$allEVAL$gain.tst)
                                                    ,rgain.tst=mean(res$allEVAL$rgain.tst)
                                                    ,cerr.tst2=mean(res$allEVAL$cerr.tst2)
                                                    ,gain.tst2=mean(res$allEVAL$gain.tst2)
                                                    ,rgain.tst2=mean(res$allEVAL$rgain.tst2)
                                                    )));
#            # OLD VERSION until Dec'2011
#            allerr = rbind(allerr,as.data.frame(list(cerr.trn=res$sumEVAL$cerr.trn
#                                                    ,gain.trn=res$sumEVAL$gain.trn
#                                                    ,rgain.trn=res$sumEVAL$rgain.trn
#                                                    ,cerr.tst=res$sumEVAL$cerr.tst
#                                                    ,gain.tst=res$sumEVAL$gain.tst
#                                                    ,rgain.tst=res$sumEVAL$rgain.tst
#                                                    ,cerr.tst2=res$sumEVAL$cerr.tst2
#                                                    ,gain.tst2=res$sumEVAL$gain.tst2
#                                                    ,rgain.tst2=res$sumEVAL$rgain.tst2
#                                                    )));
        }  # for (k in 1:nfold)

        Err = colSums(allerr)/nfold     # assumes that each fold has the same (or nearly the same) size
        predProbList[[i]] <- list()
        predProbList[[i]]$Val <- predProb$Val;
        predProbList[[i]]$Trn <- predProb$Trn;

        cat1(opts,"\n",ifelse(opts$TST.kind=="cv","CV",""),"Relative gain on training set   ",Err["rgain.trn"],"%\n")
        cat1(opts,"",  ifelse(opts$TST.kind=="cv","CV",""),"Relative gain on     test set   ",Err["rgain.tst"],"%\n\n")

        #opts$name.of.prediction <- paste("pred_", response.variable, sep="")
        C_train[i] = Err["cerr.trn"]
        G_train[i] = Err["gain.trn"]
        R_train[i] = Err["rgain.trn"]
        C_test[i] = Err["cerr.tst"]
        G_test[i] = Err["gain.tst"]
        R_test[i] = Err["rgain.tst"]
        C_test2[i] = Err["cerr.tst2"]
        G_test2[i] = Err["gain.tst2"]             # for comparision in case of opts$MOD.method="RF": results with
        R_test2[i] = Err["rgain.tst2"]            # default cutoff 1/n.class instead of opts$CLS.cutoff

        #=============================================
        # PART 5: SOME MORE GRAPHICS
        #=============================================
        if (opts$GD.DEVICE!="non" & !is.null(opts$gr.fctcall)) {
          # execute the graphics command given in text string opts$gr.fctcall
          eval(parse(text=opts$gr.fctcall));
        }
    } # for (i in 1:opts$NRUN)

    #write.table(d_test, file=paste(opts$dir.output, sub(".csv", "", filename), "_predictions.csv", sep=""), quote=FALSE, sep=";", dec=".", row.names=FALSE, col.names=TRUE)

    #=============================================
    # PART 6: OVERALL EVALUATION
    #=============================================
    if (opts$NRUN>1) {
        # print output averaged over all NRUN runs "for (i)"
        cat1(opts,"\nAverage over all ",opts$NRUN," runs: \n")
        cat1(opts,sprintf("cerr$train: (%7.5f +- %7.5f)%%\n", mean(C_train)*100, sd(C_train)*100));
        cat1(opts,sprintf("cerr$test:  (%7.5f +- %7.5f)%%\n", mean(C_test)*100, sd(C_test)*100));
        cat1(opts,sprintf("gain$train: (%7.2f +- %4.2f)\n", mean(G_train), sd(G_train)));
        cat1(opts,sprintf("gain$test:  (%7.2f +- %4.2f)\n", mean(G_test), sd(G_test)));
        cat1(opts,sprintf("rgain.train: %7.3f%%\n", mean(R_train)));
        cat1(opts,sprintf("rgain.test:  %7.3f%%\n\n", mean(R_test)));
    }
    result = list(lastRes = res     # last run, last fold: result from tdmClassify
                	#, opts = res$opts   # deprecated (12/2011), use result$lastRes$opts or Opts(result)
                	, C_train = C_train
                	, G_train = G_train
                	, R_train = R_train
                	, C_test = C_test
                	, G_test = G_test
                	, R_test = R_test
                	, C_test2 = C_test2
                	, G_test2 = G_test2
                	, R_test2 = R_test2
                	, predProbList = predProbList
                	);
    if (!is.null(opts$TST.COL))         # needed when result is input for coTraining  (see ssl_methods.r)
      if (opts$TST.COL %in% names(dset))  result$TST = dset[,opts$TST.COL]

    class(result) <- c("TDMclassifier","TDM")     # this causes > result; or > print(result);
                                                  # NOT to print out the whole list (might be very long!!)
                                                  # but to call instead the function  print.TDMclassifier
                                                  # (which in turn calls tdmClassifySummary)
   	if (exists("SAVESEED")) assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
    result;
} # tdmClassifyLoop

######################################################################################
# tdmClassifySummary
#
#'   Print summary output for \code{result} from \code{tdmClassifiyLoop} and add \code{result$y}.
#'
#'   \code{result$y} is "minus OOB rgain" on training set for methods RF or MC.RF.
#'   \code{result$y} is "minus rgain" on test set (=validation set) for all other methods.
#'   \code{result$y} is the quantity which the tuner seeks to minimize.
#'
#'   @param result  return value from a prior call to \code{\link{tdmClassifyLoop}}, an object of class \code{TDMclassifier}.
#'   @param opts    a list from which we need here the following entries
#'     \describe{
#'       \item{\code{NRUN}}{ number of runs (outer loop)}
#'       \item{\code{method}}{}
#'       \item{\code{VERBOSE}}{}
#'       \item{\code{dset}}{ [NULL] if !=NULL, attach it to result}
#'     }
#'   @param dset    [NULL] if not NULL, add this data frame to the return value (may cost a lot of memory!)
#'
#'   @return \code{result},  an object of class \code{TDMclassifier}, with \code{result$y}, \code{result$sd.y}
#'          (and optionally also \code{result$dset}) added
#'
#' @seealso   \code{\link{tdmClassify}}, \code{\link{tdmClassifyLoop}}, \code{\link{print.TDMclassifier}}, \code{\link{tdmRegressSummary}}
#' @author Wolfgang Konen, FHK, Sep'2010 - Oct'2011
#' @export
######################################################################################
tdmClassifySummary <- function(result,opts,dset=NULL)
{
    res <- result$lastRes;
    cat1Records <- function (nrow_noCV) {
      cat1(opts,ifelse(opts$TST.kind=="cv"
                ,  sprintf("   (on %d records in %d folds)",nrow(res$d_train)+nrow(res$d_test),opts$TST.NFOLD)
                ,  sprintf("   (on %d records)",nrow_noCV)
                ),"\n");
    }
    #print2(opts,res$allEVAL);		      # EVAL for each response variable , but only for lastRes
    y = mean(result$R_test);
    ytr = mean(result$R_train);
    maxScore    = result$G_test[1]/(result$R_test[1]/100);
    maxScore.tr = result$G_train[1]/(result$R_train[1]/100);
    z=data.frame(TYPE=c("rgain","meanCA","minCA")
                ,DESC=c("relative gain, i.e. percent of correctly classified records"
                        ,"mean class accuracy, i.e. average over class levels","minimum class accuracy"));
    cat1(opts,sprintf("\nRelative gain is '%s'",opts$rgain.type));
    cat1(opts,sprintf(" (%s)", z$DESC[which(z$TYPE==opts$rgain.type)]));
    if (opts$MOD.method %in% c("RF","MC.RF") & opts$RF.OOB==TRUE) {
      cat1(opts,sprintf("\n%sTrain OOB relative gain: %7.3f",ifelse(opts$TST.kind=="cv","CV ",""),ytr));
      cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_train)),""));
      cat1Records(nrow(res$d_train));
#      cat1(opts,sprintf("   (on %d records)",nrow(res$d_train)));
      result$y=-ytr;           # the score (to be minimized by SPOT) is "minus OOB score"
      result$sd.y=sd(result$R_train);
    } else {
      cat1(opts,"\n");
      result$y=-y;             # the score (to be minimized by SPOT) is "minus test score"
      result$sd.y=sd(result$R_test);
    }
    cat1(opts,sprintf("%s  Test    relative gain: %7.3f",ifelse(opts$TST.kind=="cv","CV ",""),y));
    cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_test)),""));
    cat1Records(nrow(res$d_test));

    cat1(opts,sprintf("%s Test2    relative gain (predict always with %s): %7.3f"
                     ,ifelse(opts$TST.kind=="cv","CV ",""), res$allEVAL$test2.string, mean(result$R_test2)));
    cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_test2)),""));
    cat1Records(nrow(res$d_test));

    if (!is.null(dset)) result$dset=dset;          # might cost a lot of memory

    result;

} # tdmClassifySummary

