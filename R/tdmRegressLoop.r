######################################################################################
# tdmRegressLoop
#
#'    Core regression double loop of TDMR. It contains a doublee loop (opts$NRUN and CV-folds)
#'    and calls \code{\link{tdmRegress}}. It is called  by all R-functions main_*
#'
#' INPUT
#'   @param dset    the data frame for which cvi is needed
#'   @param response.variables   name of column which carries the target variable - or -
#'                   vector of names specifying multiple target columns
#'                   (these columns are not used during prediction, only for evaluation)
#'   @param input.variables     vector with names of input columns
#'   @param opts    a list from which we need here the following entries
#'     \describe{
#'       \item{\code{NRUN}}{ number of runs (outer loop)}
#'       \item{\code{TST.SEED}}{ =NULL: leave the random number seed as it is. =any value: set the random number seed
#'               to this value to get reproducible random numbers and thus reproducible training-test-set-selection.
#'               (only relevant in case TST.kind=="cv" or "rand") (see also MOD.SEED in \code{\link{tdmClassify}})}
#'       \item{\code{TST.kind}}{ how to create cvi, handed over to \code{\link{tdmModCreateCVindex}}. If TST.kind="col", then cvi is taken from dset[,opts$TST.col].}
#'       \item{\code{GD.RESTART}}{ [TRUE] =TRUE/FALSE: do/don't restart graphic devices}
#'       \item{\code{GRAPHDEV}}{ ["non"| other ]}
#'     }
#'
#'   @return \code{result},  an object of class TDMregressor, this is a list with results, containing
#     \describe{
#'       \item{opts}{ the res$opts from \code{\link{tdmRegress}}}
#'       \item{lastRes}{ last run, last fold: result from \code{\link{tdmRegress}}}
#'       \item{R_train}{ RMAE / RMSE on training set (vector of length NRUN), depending on opts$rgain.type=="rmae" or "rmse"}
#'       \item{S_train}{ RMSE on training set (vector of length NRUN)}
#'       \item{T_train}{ Theil's U for RMAE on training set (vector of length NRUN)}
#'       \item{*_test}{ --- similar, with test set instead of training set}
#    }
#'
#' @seealso   \code{\link{tdmRegress}}, \code{\link{tdmClassifyLoop}}, \code{\link{tdmClassify}}
#' @author Wolfgang Konen, FHK, Sep'2010 - Oct'2011
#' @export
######################################################################################
tdmRegressLoop <- function(dset,response.variables,input.variables,opts) {
    if (is.null(opts$NRUN)) opts$NRUN=1;
    if (is.null(opts$GD.RESTART)) opts$GD.RESTART=TRUE;

    R_train <- R_test <- T_train <- T_test <- NULL       # reserve names (dynamic extension of
    S_train <- S_test <- NULL                            # these vectors at and of for-i-loop)

    for (i in 1:opts$NRUN) {
        if (opts$NRUN>1) {
          if (opts$GD.RESTART) tdmGraphicCloseDev(opts);
          tdmGraphicInit(opts);
        }
        opts$i = i;

        #=============================================================================
        # PART 3: CREATE NFOLD CROSSVALIDATION INDEX  OR DIVIDE IN TRAINING / TEST SET
        #=============================================================================
        if (is.null(opts$TST.SEED)) {
          # NEW: when called via SPOT, the RNG might be at (different but) fixed seed in each call.
          #      But we want different seeds (for test set selection) to see the variability
          set.seed(tdmRandomSeed());
        } else if (opts$TST.SEED=="algSeed") {    # use the seed from SPOT
          newseed=opts$ALG.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed); 
        } else {
          newseed=opts$TST.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed);  # if you want reproducably the same training/test sets,
        }                     # but different for each run i and each repeat (opts$rep)

        cvi <- tdmModCreateCVindex(dset,response.variables,opts)
        nfold = max(cvi)

#--- very probably obsolete now with MOD.SEED ---
#        # if the seed was fixed above, set it here again to a random value:
#        # (use different random numbers for each RF run)
#        if (!is.null(opts$TST.SEED)) set.seed(as.integer(Sys.time()));

        #=============================================
        # PART 4: MODELING, EVALUATION, VISUALIZATION
        #=============================================
        allerr = NULL;
        for (k in 1:nfold) {
            opts$k=k;
            opts$the.nfold = nfold;
            d_test  <- dset[cvi==k, ]             # test set
            d_train <- dset[cvi!=k & cvi>=0, ]    # training set (disregard records with cvi<0)

            res <- tdmRegress(d_train,d_test,response.variables,input.variables,opts)
            #res <- regress_lm(d_train,d_test,response.variables,input.variables,opts)

            cat1(opts,sprintf("k=%d  res$rmae$train=%7.5f  res$rmae$test=%7.5f\n",k,res$rmae$train,res$rmae$test))
            allerr = rbind(allerr,as.data.frame(list(rmae.trn=res$rmae$train
                                                    ,rmae.tst=res$rmae$test
                                                    ,rmse.trn=res$rmse$train
                                                    ,rmse.tst=res$rmse$test
                                                    ,rmae.Theil.trn=res$rmae$Theil.train
                                                    ,rmae.Theil.tst=res$rmae$Theil.test
                                                    )));
        } # for (k)
        Err = colSums(allerr)/nfold     # assumes that each fold has the same (or nearly the same) size

        cat1(opts,"\n",ifelse(opts$TST.kind=="cv","CV",""),"RMAE on training set   ",Err["rmae.trn"]*100,"%\n")
        cat1(opts,"",  ifelse(opts$TST.kind=="cv","CV",""),"RMAE on     test set   ",Err["rmae.tst"]*100,"%\n\n")

        R_train[i] = Err["rmae.trn"];
        if (opts$rgain.type=="rmse") R_train[i] = Err["rmse.trn"];
        S_train[i] = Err["rmse.trn"];
        T_train[i] = Err["rmae.Theil.trn"];
        R_test[i] = Err["rmae.tst"]
        if (opts$rgain.type=="rmse") R_test[i] = Err["rmse.tst"];
        S_test[i] = Err["rmse.tst"]
        T_test[i] = Err["rmae.Theil.tst"]

        #=============================================
        # PART 5: SOME MORE GRAPHICS
        #=============================================
        if (opts$GD.DEVICE!="non" & !is.null(opts$gr.fctcall)) {
          # execute the graphics command given in text string opts$gr.fctcall
          eval(parse(text=opts$gr.fctcall));
        }
    } # for (i in 1:opts$NRUN)

    #=============================================
    # PART 6: OVERALL EVALUATION
    #=============================================
    if (opts$NRUN>1) {
        # print output averaged over all NRUN runs "for (i)"
        # Expected result: rmse$test & rmse$train should approach same value
        cat1(opts,"\nAverage over all ",opts$NRUN," runs: \n")
        cat1(opts,sprintf("RMAE.trn: (%7.5f +- %7.5f)%%\n", mean(R_train)*100, sd(R_train)*100));
        cat1(opts,sprintf("RMAE.tst: (%7.5f +- %7.5f)%%\n", mean(R_test)*100, sd(R_test)*100));
        cat1(opts,sprintf("Theil.train: (%7.2f +- %4.2f)%%\n", mean(T_train), sd(T_train)));
        cat1(opts,sprintf("Theil.test: (%7.2f +- %4.2f)%%\n", mean(T_test), sd(T_test)));
        cat1(opts,sprintf("RMSE.train: (%7.2f +- %4.2f)%%\n", mean(S_train), sd(S_train)));
        cat1(opts,sprintf("RMSE.test: (%7.2f +- %4.2f)%%\n", mean(S_test), sd(S_test)));
    }

    result = list(opts = res$opts
              	, lastRes = res     # last run, last fold: result from tdmRegress
              	, R_train = R_train
              	, R_test = R_test
              	, T_train = T_train
              	, T_test = T_test
              	, S_train = S_train
              	, S_test = S_test
              	);
    class(result) <- c("TDMregressor", "TDM")     # this causes > result; or > print(result);
                                                  # NOT to print out the whole list (might be very long!!)
                                                  # but to call instead the function  print.TDMregressor
                                                  # (which in turn calls tdmRegressSummary)
    result;
} # tdmRegressLoop

######################################################################################
# tdmRegressSummary
#
#'   Print summary output for \code{result} from \code{tdmRegressLoop} and add \code{result$y}.
#'
#'   \code{result$y} is "OOB RMAE" on training set for methods RF or MC.RF.
#'   \code{result$y} is "RMAE" on test set (=validation set) for all other methods.
#'   \code{result$y} is the quantity which the tuner seeks to minimize.
#'
#'   @param result  return value from a prior call to \code{\link{tdmRegressLoop}}, an object of class \code{TDMregressor}.
#'   @param opts    a list from which we need here the following entries
#'     \describe{
#'       \item{\code{NRUN}}{ number of runs (outer loop)}
#'       \item{\code{method}}{}
#'       \item{\code{VERBOSE}}{}
#'       \item{\code{dset}}{ [NULL] if !=NULL, attach it to result}
#'     }
#'   @param dset    [NULL] if not NULL, add this data frame to the return value (may cost a lot of memory!)
#'
#'   @return \code{result},  an object of class \code{TDMregressor}, with \code{result$y}, \code{result$sd.y}
#'          (and optionally also \code{result$dset}) added
#'
#' @seealso   \code{\link{tdmRegress}}, \code{\link{tdmRegressLoop}}, \code{\link{tdmClassifySummary}}
#' @author Wolfgang Konen, FHK, Sep'2010 - Oct'2011
#' @export
######################################################################################
tdmRegressSummary <- function(result,opts,dset=NULL)
{
    res <- result$lastRes;
    cat1Records <- function (nrow_noCV) {
      cat1(opts,ifelse(opts$TST.kind=="cv"
                ,  sprintf("   (on %d records in %d folds)",nrow(res$d_train)+nrow(res$d_test),opts$TST.NFOLD)
                ,  sprintf("   (on %d records)",nrow_noCV)
                ),"\n");
    }
    #print2(opts,res$allRMAE);		   # RMAE for each response variable , but only for lastRes
    y = mean(result$R_test);       # RMAE, average of opts$NRUN runs
    ytr = mean(result$R_train);
    if (opts$MOD.method %in% c("RF","MC.RF")) {
      cat1(opts,sprintf("\n%sTrain OOB RMAE: %7.3f",ifelse(opts$TST.kind=="cv","CV ",""),ytr));
      cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_train)),""));
      cat1Records(nrow(res$d_train));
      result$y=ytr;           # the score (to be minimized by SPOT) is "RMAE OOB"
      result$sd.y=sd(result$R_train);
    } else {
      result$y=y;             # the score (to be minimized by SPOT) is "RMAE test set"
      result$sd.y=sd(result$R_test);
    }
    cat1(opts,sprintf("%s  Test    RMAE: %7.3f",ifelse(opts$TST.kind=="cv","CV ",""),y));
    cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_test)),""));
    cat1Records(nrow(res$d_test));

    if (!is.null(dset)) result$dset=dset;          # might cost a lot of memory

    result;

} # tdmRegressSummary

