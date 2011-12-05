######################################################################################
# tdmRegress
#
#'       Core regression function of TDMR. It is called by \code{\link{tdmRegressLoop}}.
#'
#'   @param d_train     training set
#'   @param d_test      test set, same columns as training set
#'   @param response.variables   name of column which carries the target variable - or - 
#'                   vector of names specifying multiple target columns
#'                   (these columns are not used during prediction, only for evaluation)
#'   @param input.variables     vector with names of input columns 
#'   @param opts        additional parameters [defaults in brackets]
#'     \describe{
#'     \item{\code{SRF.*}}{ several parameters for sorted_rf_importance (see tdmModelingUtils.r) }
#'     \item{\code{RF.*}}{ several parameters for RF (Random Forest, defaults are set, if omitted)  }
#'     \item{\code{SVM.*}}{ several parameters for SVM (Support Vector Machines, defaults are set, if omitted)}
#'     \item{\code{method}}{ ["RF"] the main training method
#'                   ["RF"|"SVM"|"LM"]: use [Random forest|  SVM| linear model] for the main model}
#'     \item{\code{filename}}{ }
#'     \item{\code{data.title}}{ }
#'     \item{\code{MOD.SEED}}{ =NULL: set the RNG to system time as seed (different RF trainings)
#'                   =any value: set the random number seed to this value (+i) to get reproducible random
#'                   numbers. In this way, the model training part (RF, NNET, ...) gets always a fixed seed.
#'                   (see also TST.SEED in tdmRegressLoop) }
#'     \item{\code{OUTTRAFO}}{ string, apply a transformation to the output variable}
#'     \item{\code{fct.postproc}}{ user-def'd function for postprocessing of predicted output  }
#'     \item{\code{gr.log}}{ =FALSE (def): make scatter plot as-is, 
#'                           =TRUE: transform output x with log(x+1) (x should be nonnegative) }
#'     \item{\code{GRAPHDEV}}{ if !="non", then make a pairs-plot of the 5 most important variables
#'                   and make a true-false bar plot }
#'     \item{\code{VERBOSE}}{ [2] =2: most printed output, =1: less, =0: no output }
#'     }
#'         
#'   @return  a list \code{res} with results, containing
#---     \describe{
#'       \item{\code{d_train}}{ training set + predicted class column(s) }
#'       \item{\code{d_test}}{ test set + predicted target output }
#'       \item{\code{rmse}}{ root mean square error (on test + train set) + Theil's U (on test + train set) }
#'       \item{\code{rmae}}{ relative mean absolute error (on test + train set) + Theil's U  (on test + train set).
#'                   rmse and rmae are lists. If there is more than one response variable, then rmse and rmae 
#'                   contain the *sum* over response.variables for each list-entry. }
#'       \item{\code{allRMAE}}{ data frame with columns = list-entries in rmae and rows = response variables  }
#'       \item{\code{opts}}{ parameter list from input, some default values might have been added }
#---      }
#'
#'
#' @author Wolfgang Konen, FHK, Sep'2009 - Oct'2011
#'
#' @export
######################################################################################
tdmRegress <- function(d_train,d_test,response.variables,input.variables,opts)
{    
    first <- TRUE; 
    filename <- opts$filename;
    input.variables_0 <- input.variables;      # save copy for response.variable loop

    if (is.null(opts$method)) opts$method="RF";
    if (is.null(opts$SRF.kind)) opts$SRF.kind="xperc";
    if (is.null(opts$RF.ntree)) opts$RF.ntree=500;
    if (is.null(opts$RF.samp)) opts$RF.samp=3000;
    if (is.null(opts$RF.mtry)) opts$RF.mtry=NULL;
    if (is.null(opts$RF.p.all)) opts$RF.p.all=F;
    if (is.null(opts$SVM.gamma)) opts$SVM.gamma=0.01;
    if (is.null(opts$SVM.epsilon)) opts$SVM.epsilon=0.01;
    if (is.null(opts$SVM.tolerance)) opts$SVM.tolerance=0.001;
    if (is.null(opts$VERBOSE)) opts$VERBOSE=2;
    if (is.null(opts$gr.log)) opts$gr.log=F;
    
    
    for (response.variable in response.variables) {    
        input.variables <- input.variables_0;
        
        if (opts$OUTTRAFO=="log") {
            cat1(opts,filename,": Applying log-transform to response.variable ...\n")
            d_train[,response.variable] <- log(d_train[,response.variable]+1)
            d_test[,response.variable] <- log(d_test[,response.variable]+1)
        }
        if (opts$OUTTRAFO=="mean.shift") {
            cat1(opts,filename,": Applying mean.shift-transform to response.variable ...\n")
            response.mean = mean(d_train[,response.variable])
            d_train[,response.variable] <- d_train[,response.variable]-response.mean
            d_test[,response.variable] <- d_test[,response.variable]-response.mean
        }
        #=============================================
        # PART 4.1: SUMMARY OF DATA
        #=============================================
        #cat1(opts,filename,": Summary of data ...\n")
        #cat1(opts,"Summary of d_train:\n")
        #print(summary(d_train))            # most columns are of numeric type
                                           # -> summary min,max,quantiles...
        cat1(opts,"\n")

        #=============================================
        # PART 4.2: IMPORTANCE SELECTION (BY USING RF)
        #=============================================
        # determine the importance of all input var's by constructing a test RF
        # --- this step is skipped if SRF.kind=="none", then you use all     ---
        # --- input variables and you do not see the importance of variables ---
        if (opts$SRF.kind!="none") {
          cat1(opts,filename,": Importance check ...\n")
          opts$RF.sampsize <- tdmModAdjustSampsize(opts$SRF.samp, d_train, response.variable, opts);
          SRF <- tdmModSortedRFimport(d_train,response.variable,
                                          input.variables,opts)
          input.variables <- SRF$input.variables;  
          opts <- SRF$opts;       # some defaults might have been added  
         
        }  else {
          if (opts$i==1) {
            cat1(opts,filename,": Using all input variables: \n");
            if (opts$VERBOSE>=1) print(input.variables);
          }
        }

        # We set here the random number generator (RNG) seed again such that the subsequent RF training 
        # starts from the same seed, regardless whether opts$SRF.kind=="none" or !="none" (the latter
        # means extra calls to RNG in tdmModSortedRFimport)
        if (!is.null(opts$MOD.SEED)) set.seed(opts$MOD.SEED+2*opts$i+1) # if you want reproducably the same model training,
                                                                        # but different for each run i
        # NEW: when called via SPOT, the RNG might be at (different but) fixed seed in each call.
        #      But if MOD.SEED==NULL we want different seeds (for RF training) to see the variability                                                                   
        if (is.null(opts$MOD.SEED)) set.seed(as.integer(Sys.time()));                                                                  

    		# Boruta: Alternative to Calc variable importance
    		#cat1(opts,filename,": Boruta... This may take a while...\n")
    		#library(Boruta)
    		#na.omit(to.model)->"CH4";
    		#Boruta(formula, data=to.model, doTrace=2)->Bor.biogas
    		#print(Bor.biogas)
    		#stats<-attStats(Bor.biogas);
    		#print(stats);
    		#plot(normHits~meanZ,col=stats$decision,data=stats);
        ##TODO: NDROP columns which are unimportant to boruta


        #=========================================================
        # PART 4.3: MODELING: TRAIN RANDOM FOREST (OR OTHER METHOD)
        #=========================================================
		    formula <- formula(paste(response.variable, "~ ."))   # use all possible input variables
        # estimate random forest based on previous step:
        to.model <- d_train[,c(input.variables,response.variable)]
        to.test <- d_test[,c(input.variables,response.variable)]
        
        train.rf <- function(formula,to.model,opts) {
            cat1(opts,opts$filename,": Train RF ...\n")
            flush.console();
            formula <- formula(paste(response.variable, "~ ."))   # use all possible input variables
            opts$RF.sampsize <- tdmModAdjustSampsize(opts$RF.samp, to.model, response.variable, opts);
            # we work here with a command text string and eval(...) to allow for the presence or
            # absence of certain options like "mtry" or "cutoff" which are not allowed to be NULL:
            rf.options = "ntree=opts$RF.ntree";
            rf.options = paste(rf.options,"sampsize=opts$RF.sampsize",sep=",")
            rf.options = paste(rf.options,"na.action=na.roughfix","proximity=F",sep=",")
            if (!is.null(opts$RF.mtry)) rf.options = paste(rf.options,"mtry=opts$RF.mtry",sep=",")
            if (!is.null(opts$RF.cutoff)) rf.options = paste(rf.options,"cutoff=opts$RF.cutoff",sep=",")
            if (!is.null(opts$RF.nodesize)) rf.options = paste(rf.options,"nodesize=opts$RF.nodesize",sep=",")

            eval(parse(text=paste("res.rf <- randomForest( formula, data=to.model,",rf.options,")"))) 
            res.rf$HasVotes = TRUE; 
            res.rf;
        } 
        train.svm <- function(formula,to.model,opts) {
            cat1(opts,filename,": Train SVM ...\n")
            flush.console();
            #res.rf <- svm(formula, to.model, kernel="radial", gamma=0.02, epsilon=0.0001, tolerance=0.001, type="eps-regression", cachesize=512)
            res.rf <- svm(formula, to.model, kernel="radial"
                                    , gamma=opts$SVM.gamma, epsilon=opts$SVM.epsilon, tolerance=opts$SVM.tolerance, cost=opts$SVM.cost
                                    , type="eps-regression")
        }
        train.lm <- function(formula,to.model,opts) {
            cat1(opts,filename,": Train LM ...\n")
            flush.console();
            res.rf <- lm( formula=formula, data=to.model)
        }
        
        #
        # TODO: add lm
        #

        ptm <- proc.time()
        cat1(opts, "Run ",ifelse(opts$the.nfold>1,paste(opts$i,".",opts$k,sep="")           ,opts$i)    ,"/",
                          ifelse(opts$the.nfold>1,paste(opts$NRUN,".",opts$the.nfold,sep=""),opts$NRUN) ,":\n"); 
        res.rf = switch(opts$method
          ,"RF"  =  train.rf(formula,to.model,opts)
          ,"SVM" =  train.svm(formula,to.model,opts) 
          ,"LM" =  train.lm(formula,to.model,opts) 
          );
        cat1(opts,"Proc time: ",(proc.time()-ptm)[1],"\n");
        
        #print(res.rf)
        #print(res.rf$importance)

        #======================================================
        # PART 4.4: APPLY RANDOM FOREST (OR OTHER METHOD)
        #======================================================
        apply.rf <- function(res.rf,to.model,to.test,opts) {
            cat1(opts,opts$filename,": Apply RF ...\n")
            #opts$RF.p.all = TRUE;          # TEST---------------
            app = list()
            app$test.predict <- predict(res.rf, newdata=to.test, predict.all=opts$RF.p.all)
            if (opts$RF.p.all) {
                train.p <- predict(res.rf, newdata=to.model, predict.all=opts$RF.p.all)
                app$train.indiv <- train.p$individual;
                app$train.predict <- train.p$aggregate;
                app$test.indiv <- app$test.predict$individual;
                app$test.predict <- app$test.predict$aggregate;
            } else {
                app$train.predict <- res.rf$predicted
                # (res.rf$predicted is the *OOB-prediction* on the training set)
                # Think about this! Why is it WRONG (or too optimistic) to use here
                #      app$train.predict <- predict(res.rf, newdata=d_train)   
                # as the prediction for the training set?
                #
            }
            app;
        } 
        apply.other <- function(res.rf,to.model,to.test,opts) {
            cat1(opts,opts$filename,": Apply",opts$method,"...\n")
            app = list()
            app$train.predict <- predict(res.rf, newdata=to.model)
            app$test.predict <- predict(res.rf, newdata=to.test)
            app;
        }
        
        ptm <- proc.time()
        app = switch(opts$method
          ,"RF"  =  apply.rf(res.rf,to.model,to.test,opts)
          ,"SVM" =, "LM" =  apply.other(res.rf,to.model,to.test,opts) 
          );
        train.predict <- app$train.predict;
        test.predict <- app$test.predict;
        cat1(opts,"Proc time: ",(proc.time()-ptm)[1],"\n");

        
        #=============================================
        # PART 4.5: POSTPROCESSING
        #=============================================
        if (opts$OUTTRAFO=="log") {        
            cat1(opts,filename,": Inverting log-transform to response.variable ...\n")
            d_train[,response.variable] <- exp(d_train[,response.variable])-1
            d_test[,response.variable] <- exp(d_test[,response.variable])-1
            train.predict <- exp(train.predict)-1
            test.predict <- exp(test.predict)-1
        }
        if (opts$OUTTRAFO=="mean.shift") {        
            cat1(opts,filename,": Inverting mean.shift-transform to response.variable ...\n")
            d_train[,response.variable] <- d_train[,response.variable]+response.mean
            d_test[,response.variable] <- d_test[,response.variable]+response.mean
            train.predict <- train.predict+response.mean
            test.predict <- test.predict+response.mean
        }
      	if (!is.null(opts$fct.postproc)) {
      		cat1(opts,filename,": User-defined postprocessing: Applying opts$fct.postproc ...\n")
      		train.predict <- opts$fct.postproc(train.predict,opts)
      		test.predict <- opts$fct.postproc(test.predict,opts)
      	}
        # bind the predicted class pred_... as last column to the data frames
        name.of.prediction <- paste("pred_", response.variable, sep="")
        d_train <- bind_response(d_train, name.of.prediction, train.predict)
        d_test  <- bind_response(d_test, name.of.prediction, test.predict)

        #=============================================
        # PART 4.6: EVAL: RMSE, RMAE on TRAIN/OOB/TEST
        #=============================================
        cat1(opts,filename,": Calc RMSE ...\n")
        naive.predict=mean(d_train[,response.variable])
        rmse=list()
        rmse$train <- sqrt(mean((train.predict-d_train[,response.variable])^2)) # this is the OOB-error
        if (opts$method=="RF") rmse$OOB <- sqrt(res.rf$mse[res.rf$ntree])
        else rmse$OOB <- 0;
        rmse$Theil.train <- rmse$train/sqrt(mean((naive.predict-d_train[,response.variable])^2))
        #naive.predict2=d_train[,opts$old.response.variable]
        #rmse$Theil2.train <- rmse$train/sqrt(mean((naive.predict2-d_train[,response.variable])^2))
        rmae=list()
        madtr <- mean(abs(d_train[,response.variable]))
        rmae$train <- mean(abs(train.predict-d_train[,response.variable]))/madtr
        rmae$Theil.train <- rmae$train/(mean(abs(naive.predict-d_train[,response.variable]))/madtr)

        cat2(opts,"\nTraining cases (",length(train.predict),"):\n")
        cat2(opts,"rmse$train (OOB):", rmse$train, "\n")
        cat2(opts,"RMAE$train (OOB):", rmae$train,"\n")
        cat2(opts,"Theils U1 (train, RMSE):", rmse$Theil.train,"\n")#, U2 (train):",rmse$Theil2.train,"\n")    # <1: better than naive forecast
        cat2(opts,"Theils U3 (train, RMAE):", rmae$Theil.train,"\n")    # based on RMAE instead of RMSE
      
        rmse$test <- sqrt(mean((test.predict-d_test[,response.variable])^2))
        rmse$Theil.test <- rmse$test/sqrt(mean((naive.predict-d_test[,response.variable])^2))
        #naive.predict2=d_test[,opts$old.response.variable]
        #rmse$Theil2.test <- rmse$test/sqrt(mean((naive.predict2-d_test[,response.variable])^2))
        madt <- mean(abs(d_test[,response.variable]))
        rmae$test <- mean(abs(test.predict-d_test[,response.variable]))/madt
        rmae$Theil.test <- rmae$test/(mean(abs(naive.predict-d_test[,response.variable]))/madt)
        cat2(opts,"\nTest cases (",length(test.predict),"):\n")
      	cat2(opts,"rmse$test:", rmse$test, ", rmse$train (OOB):", rmse$train,"\n")
        cat2(opts,"RMAE$test:", rmae$test, "\n")
        cat2(opts,"Theils U1 (test, RMSE):", rmse$Theil.test,"\n")#, U2 (test):",rmse$Theil2.test,"\n")    # <1: better than naive forecast
        cat2(opts,"Theils U3 (test, RMAE):", rmae$Theil.test,"\n")    # based on RMAE instead of RMSE
     
        if (!is.na(match("SRF",ls()))) {
        	rmae$SRF.perc = SRF$perc;			# just to report these
        	rmae$SRF.lsd = SRF$lsd;				# three numbers below in allRMAE
        	rmae$SRF.lsi = length(SRF$input.variables)	#
       	}

        #=============================================
        # PART 4.7: GRAPHICS
        #=============================================
        if (opts$GRAPHDEV!="non") {
          # point plot: true response (x) vs. predicted response (y)
          tdmGraphicNewWin(opts);
          if (opts$gr.log) {
            gr_trans <- function(x) { log(x[!is.na(x)]+1, base=10); }
          } else {
            gr_trans <- function(x) { x; }
          }
          true_out = gr_trans(d_train[,response.variable])
          r = c(d_train[,response.variable],d_test[,response.variable]);
          if (is.null(opts$lim)) lim=c(min(r),max(r)) else lim=opts$lim;

          plot(c(0,max(true_out)),c(0,max(true_out)),col='blue',type="l"
               ,xlab=ifelse(opts$gr.log,"lg(true_out+1)","true_out")
               ,ylab=ifelse(opts$gr.log,"lg(predict+1)","predict")
               ,xlim=gr_trans(lim)
               ,ylim=gr_trans(lim)
               )
          #points(true_out[true_out<opts$OCUT],train.predict[true_out<opts$OCUT],col='green')
          #points(true_out[true_out<opts$OCUT],train.predict[true_out<opts$OCUT],col='blue')
          points(true_out,gr_trans(train.predict),col='blue')
          true_out = gr_trans(d_test[,response.variable])
          points(true_out,gr_trans(test.predict),col='red')
          title(response.variable)
          legend("topleft",legend=c("train", "test"),
                 col=c("blue","red"), lty=c(1,1), lwd=c(1,1),
                 pch=c(5,5), pt.bg=c("white","white"), pt.cex=1.0,
                 text.col="blue4", bg="gray95")
        } # if (opts$GRAPHDEV!="non")
        
        if (first) {
            RMSE=lapply(rmse,sum);
            RMAE=lapply(rmae,sum);
            allRMAE=as.data.frame(rmae);
            first=F;
        } else {
            RMSE<- as.list(mapply(sum,rmse,RMSE));
            RMAE<- as.list(mapply(sum,rmae,RMAE));
            allRMAE <- rbind(allRMAE,as.data.frame(rmae));
        }
        
          cat1(opts,sprintf("  %s: rmae$test =%8.3f, RMAE$test =%8.3f\n",response.variable,rmae$test,RMAE$test));
          cat1(opts,sprintf("  %s: rmae$train=%8.3f, RMAE$train=%8.3f\n",response.variable,rmae$train,RMAE$train));

    } # for (response.variable)
               
    #=============================================
    # PART 4.8: WRITE RESULTS ON TEST SET TO FILE
    #=============================================
    #outfile = paste(opts$dir.output,sub(".csv", "", filename), "_predictions.csv", sep="")
    #write.table(d_test, file=outfile, quote=F, sep=";", dec=".", row.names=F, col.names=TRUE)

    res =   list(rmse=RMSE          # root mean square error, summed over all response.variables
                ,rmae=RMAE          # relative mean absolute error, summed over all response.variables
                ,allRMAE=allRMAE	 	# RMAE for each response.variable 
                ,d_train=d_train
                ,d_test=d_test 
                ,opts=opts          # some defaults might have been added  
               )
    if (opts$method=="RF" & opts$RF.p.all) res$train.indiv=app$train.indiv;
    
    res;
} # tdmRegress
        
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
#'       \item{last_res}{ last run, last fold: result from \code{\link{tdmRegress}}} 
#'       \item{R_train}{ RMAE / RMSE on training set (vector of length NRUN), depending on opts$rgain.type=="rmae" or "rmse"} 
#'       \item{S_train}{ RMSE on training set (vector of length NRUN)} 
#'       \item{T_train}{ Theil's U for RMAE on training set (vector of length NRUN)} 
#'       \item{*_test}{ --- similar, with test set instead of training set} 
#    } 
#'     
#' @references   \code{\link{tdmRegress}}, \code{\link{tdmClassifyLoop}}, \code{\link{tdmClassify}}
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
        if (!is.null(opts$TST.SEED)) set.seed(opts$TST.SEED+2*i)  # if you want reproducably the same training/test sets,
                                                                  # but different for each run i
        # NEW: when called via SPOT, the RNG might be at (different but) fixed seed in each call.
        #      But we want different seeds (for test set selection) to see the variability                                                                   
        if (is.null(opts$TST.SEED)) set.seed(as.integer(Sys.time()));                                                                  
        cvi <- tdmModCreateCVindex(dset,response.variables,opts)
        nfold = max(cvi)
        
        # if the seed was fixed above, set it here again to a random value:
        # (use different random numbers for each RF run)
        if (!is.null(opts$TST.SEED)) set.seed(as.integer(Sys.time()));
  
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
        if (opts$GRAPHDEV!="non" & !is.null(opts$gr.fctcall)) {
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
              	, last_res = res     # last run, last fold: result from tdmRegress
              	, R_train = R_train
              	, R_test = R_test
              	, T_train = T_train
              	, T_test = T_test
              	, S_train = S_train
              	, S_test = S_test
              	);
    class(result) <- c("TDMregressor", "TDM")     # this causes > result; or > print(result);
                                                  # NOT to print out the whole list (might be very long!!)
                                                  # but to call instead the function  printTDMregressor
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
#' @references   \code{\link{tdmRegress}}, \code{\link{tdmRegressLoop}}, \code{\link{tdmClassifySummary}}
#' @author Wolfgang Konen, FHK, Sep'2010 - Oct'2011
#' @export
######################################################################################
tdmRegressSummary <- function(result,opts,dset=NULL)     
{
    res <- result$last_res;  
    #print2(opts,res$allRMAE);		   # RMAE for each response variable T1,...,T8
    y = mean(result$R_test);       # RMAE, average of opts$NRUN runs
    ytr = mean(result$R_train);
    if (opts$method %in% c("RF","MC.RF")) {
      cat1(opts,sprintf("\nTrain OOB RMAE: %7.3f",ytr));  
      cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_train)),""));
      cat1(opts,sprintf("   (on %d records)",nrow(res$d_train)));
      result$y=ytr;           # the score (to be minimized by SPOT) is "RMAE OOB"
      result$sd.y=sd(result$R_train);      
    } else {
      result$y=y;             # the score (to be minimized by SPOT) is "RMAE test set"
      result$sd.y=sd(result$R_test);
    }    
    cat1(opts,sprintf("\n Test     RMAE: %7.3f",y));  
    cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_test)),""));
    cat1(opts,sprintf("   (on %d records)",nrow(res$d_test)));
    
    if (!is.null(dset)) result$dset=dset;          # might cost a lot of memory
    
    result;
    
} # tdmRegressSummary

######################################################################################
# print.TDMregressor
#
#'   Print an overview for a \code{TDMregressor} object. 
#'
#'   @param x  return value from a prior call to \code{\link{tdmClassifyLoop}}, an object of class \code{TDMclassifier}.
#'   @param ... e.g. 'type'    which information to print:
#'      \describe{
#'      \item{\code{"overview"}}{ (def.) RMAE on training/test set, number of records, see \code{\link{tdmRegressSummary}}}
#'      \item{\code{"..."}}{ ... other choices, TODO ...}
#'      }
#' @export
######################################################################################
print.TDMregressor <- function(x,...) {
  internalPrintR <- function(result,type) {
    opts = result$opts;
    opts$VERBOSE = 2;
    z <- switch(type
      , "overview"= tdmRegressSummary(result,opts)
      , "?"={cat("Help for print(<TDMregressor>,type=t). Possible values for 't' are:\n"
               ,"\"overview\": see tdmRegressSummary\n"
               ,"\"?\" : display this help message\n"
               ); 1;}
      , "invalid type"
      );
    if (z[1]=="invalid type") warning("Invalid type = ",type,". Allowed types are: overview.");  
    cat("\n");
  }
  
  vaarg <- list(...)
  #alternative: vavalues <- c(...) 

  if (is.null(vaarg$type)) vaarg$type="overview";
  internalPrintR(x,vaarg$type);
}

