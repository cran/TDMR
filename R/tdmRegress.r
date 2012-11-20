######################################################################################
# tdmRegress
#
#'       Core regression function of TDMR. 
#'
#'  tdmRegress is called by \code{\link{tdmRegressLoop}} and returns an object of class \code{tdmRegre}. \cr
#'  It trains a model on training set \code{d_train} and evaluates it on test set \code{d_test}.
#'  If this function is used for tuning, the test set \code{d_test} plays the role of a validation set.
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
#'     \item{\code{filename}}{ }
#'     \item{\code{data.title}}{ }
#'     \item{\code{MOD.method}}{ ["RF"] the main training method
#'                   ["RF"|"SVM"|"LM"]: use [Random forest|  SVM| linear model] for the main model}
#'     \item{\code{MOD.SEED}}{ =NULL: set the RNG to system time as seed (different RF trainings)
#'                   =any value: set the random number seed to this value (+i) to get reproducible random
#'                   numbers. In this way, the model training part (RF, NNET, ...) gets always a fixed seed.
#'                   (see also TST.SEED in tdmRegressLoop) }
#'     \item{\code{OUTTRAFO}}{ [NULL] string, apply a transformation to the output variable}
#'     \item{\code{fct.postproc}}{ [NULL] name of a user-def'd function for postprocessing of predicted output  }
#'     \item{\code{gr.log}}{ =FALSE (def): make scatter plot as-is, 
#'                           =TRUE: transform output x with log(x+1) (x should be nonnegative) }
#'     \item{\code{GRAPHDEV}}{ if !="non", then make a pairs-plot of the 5 most important variables
#'                   and make a true-false bar plot }
#'     \item{\code{VERBOSE}}{ [2] =2: most printed output, =1: less, =0: no output }
#'     }
#'         
#'   @return  \code{res}, an object of class \code{tdmRegre}, this is a list containing
#'       \item{\code{d_train}}{ training set + predicted class column(s) }
#'       \item{\code{d_test}}{ test set + predicted target output }
#'       \item{\code{rmse}}{ root mean square error (on test + train set) + Theil's U (on test + train set) }
#'       \item{\code{rmae}}{ relative mean absolute error (on test + train set) + Theil's U  (on test + train set).
#'                   rmse and rmae are lists. If there is more than one response variable, then rmse and rmae 
#'                   contain the *average* over response.variables for each list-entry. }
#'       \item{\code{allRMAE}}{ data frame with columns = list-entries in rmae and rows = response variables  }
#'       \item{\code{lastModel}}{       the last model built (e.g. the last Random Forest in the case of MOD.method=="RF") }
#'       \item{\code{opts}}{ parameter list from input, some default values might have been added }
#'
#'    The item \code{lastModel} is 
#'    specific for the *last* model (the one built for the last response variable in the last run and last fold) 
#'
#' @seealso  \code{\link{print.tdmRegre}} \code{\link{tdmRegressLoop}} \code{\link{tdmClassifyLoop}}
#' @author Wolfgang Konen, FHK, Sep'2009 - Jun'2012
#'
#' @export
######################################################################################
tdmRegress <- function(d_train,d_test,response.variables,input.variables,opts)
{    
    filename <- opts$filename;
    input.variables_0 <- input.variables;      # save copy for response.variable loop

    if (is.null(opts$RF.samp)) opts$RF.samp=3000;
    if (is.null(opts$SVM.gamma)) opts$SVM.gamma=0.01;
    if (is.null(opts$SVM.epsilon)) opts$SVM.epsilon=0.01;
    if (is.null(opts$SVM.tolerance)) opts$SVM.tolerance=0.001;
    if (is.null(opts$gr.log)) opts$gr.log=F;
    
    allRMAE <- allRMSE <- allMADE <- NULL;    
    for (response.variable in response.variables) {    
        input.variables <- input.variables_0;
        
        if (!is.null(opts$OUTTRAFO)) {
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
        }
        #=============================================
        # PART 4.1: SUMMARY OF DATA
        #=============================================
        #cat1(opts,filename,": Summary of data ...\n")
        #cat1(opts,"Summary of d_train:\n")
        #print(summary(d_train))            # most columns are of numeric type
                                           # -> summary min,max,quantiles...
        cat1(opts,"\n")

        # NEW (May 2012): if opts$MOD.SEED is set, also the importance selection 
        # starts with a deterministic seed
        #
        if (is.null(opts$MOD.SEED)) {
          # NEW: when called via SPOT, the RNG might be at (different but) fixed seed in each call.
          #      But if MOD.SEED==NULL we want different seeds (for RF training) to see the variability       
          set.seed(tdmRandomSeed());                                                                  
        } else if (opts$MOD.SEED=="algSeed") {  # use the seed from SPOT
          newseed=opts$ALG.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed); 
        } else {
          newseed=opts$MOD.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed) # if you want reproducably the same model training,
        }                   # but different for each run i

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
          opts <- SRF$opts;       # some defaults might have been added, some opts$SRF.* values changed  
          SRF$opts <- NULL;       # simplify list result, which will contain both, SRF and opts
         
        }  else {
          if (opts$i==1) {
            cat1(opts,filename,": Using all input variables: \n");
            if (opts$VERBOSE>=1) print(input.variables);
          }
        }

        # We set here the random number generator (RNG) seed again such that the subsequent RF training 
        # starts from the same seed, regardless whether opts$SRF.kind=="none" or !="none" (the latter
        # means extra calls to RNG in tdmModSortedRFimport)
        if (is.null(opts$MOD.SEED)) {
          # NEW: when called via SPOT, the RNG might be at (different but) fixed seed in each call.
          #      But if MOD.SEED==NULL we want different seeds (for RF training) to see the variability       
          set.seed(tdmRandomSeed());                                                                  
        } else if (opts$MOD.SEED=="algSeed") {  # use the seed from SPOT
          newseed=opts$ALG.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed); 
        } else {
          newseed=opts$MOD.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed) # if you want reproducably the same model training,
        }                   # but different for each run i

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
            if (!is.null(opts$CLS.cutoff)) rf.options = paste(rf.options,"cutoff=opts$CLS.cutoff",sep=",")
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
        
        ptm <- proc.time()
        cat1(opts, "Run ",ifelse(opts$the.nfold>1,paste(opts$i,".",opts$k,sep="")           ,opts$i)    ,"/",
                          ifelse(opts$the.nfold>1,paste(opts$NRUN,".",opts$the.nfold,sep=""),opts$NRUN) ,":\n"); 
        res.rf = switch(opts$MOD.method
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
            cat1(opts,opts$filename,": Apply",opts$MOD.method,"...\n")
            app = list()
            app$train.predict <- predict(res.rf, newdata=to.model)
            app$test.predict <- predict(res.rf, newdata=to.test)
            app;
        }
        
        ptm <- proc.time()
        app = switch(opts$MOD.method
          ,"RF"  =  apply.rf(res.rf,to.model,to.test,opts)
          ,"SVM" =, "LM" =  apply.other(res.rf,to.model,to.test,opts) 
          );
        train.predict <- app$train.predict;
        test.predict <- app$test.predict;
        cat1(opts,"Proc time: ",(proc.time()-ptm)[1],"\n");

        
        #=============================================
        # PART 4.5: POSTPROCESSING
        #=============================================
        if (!is.null(opts$OUTTRAFO)) {
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
        }
      	if (!is.null(opts$fct.postproc)) {
      		cat1(opts,filename,": User-defined postprocessing: Applying function",opts$fct.postproc,"...\n");
      		train.predict <- eval(parse(text=paste(opts$fct.postproc,"(train.predict,d_train,opts)",sep="")));
      		test.predict <- eval(parse(text=paste(opts$fct.postproc,"(test.predict,d_test,opts)",sep="")));
      	}
        # bind the predicted class pred_... as last column to the data frames
        name.of.prediction <- paste("pred_", response.variable, sep="")
        d_train <- tdmBindResponse(d_train, name.of.prediction, train.predict)
        d_test  <- tdmBindResponse(d_test, name.of.prediction, test.predict)

        #=============================================
        # PART 4.6: EVAL: RMSE, RMAE on TRAIN/OOB/TEST
        #=============================================
        cat1(opts,filename,": Calc RMSE ...\n")
        naive.predict=mean(d_train[,response.variable])
        rmse=list()
        rmse$train <- sqrt(mean((train.predict-d_train[,response.variable])^2)) # this is the OOB-error in case of RF
        if (opts$MOD.method=="RF") rmse$OOB <- sqrt(res.rf$mse[res.rf$ntree])
        else rmse$OOB <- 0;
        rmse$Theil.train <- rmse$train/sqrt(mean((naive.predict-d_train[,response.variable])^2))
        #naive.predict2=d_train[,opts$old.response.variable]
        #rmse$Theil2.train <- rmse$train/sqrt(mean((naive.predict2-d_train[,response.variable])^2))
        rmae=list()
        rmae$made.tr <- mean(abs(train.predict-d_train[,response.variable]))
        rmae$ma.train <- mean(abs(d_train[,response.variable]))
        rmae$train <- rmae$made.tr/rmae$ma.train;
        rmae$Theil.train <- rmae$train/(mean(abs(naive.predict-d_train[,response.variable]))/rmae$ma.train)

        cat2(opts,"\nTraining cases (",length(train.predict),"):\n")
        cat2(opts,"rmse$train", ifelse(opts$MOD.method=="RF","(OOB)",""),":", rmse$train, "\n")
        cat2(opts,"RMAE$train", ifelse(opts$MOD.method=="RF","(OOB)",""),":", rmae$train,"\n")
        cat2(opts,"Theils U1 (train, RMSE):", rmse$Theil.train,"\n")#, U2 (train):",rmse$Theil2.train,"\n")    # <1: better than naive forecast
        cat2(opts,"Theils U3 (train, RMAE):", rmae$Theil.train,"\n")    # based on RMAE instead of RMSE
 
        rmse$test <- sqrt(mean((test.predict-d_test[,response.variable])^2))
        rmse$Theil.test <- rmse$test/sqrt(mean((naive.predict-d_test[,response.variable])^2))
        #naive.predict2=d_test[,opts$old.response.variable]
        #rmse$Theil2.test <- rmse$test/sqrt(mean((naive.predict2-d_test[,response.variable])^2))
        rmae$made.te <- mean(abs(test.predict-d_test[,response.variable]));
        rmae$ma.test <- mean(abs(d_test[,response.variable]))
        rmae$test <- rmae$made.te/rmae$ma.test
        rmae$Theil.test <- rmae$test/(mean(abs(naive.predict-d_test[,response.variable]))/rmae$ma.test)
        cat2(opts,"\nVali cases (",length(test.predict),"):\n")
      	cat2(opts,"rmse$test:", rmse$test, ", rmse$train", ifelse(opts$MOD.method=="RF","(OOB)",""),":", rmse$train,"\n")
        cat2(opts,"RMAE$test:", rmae$test, "\n")
        cat2(opts,"Theils U1 (test, RMSE):", rmse$Theil.test,"\n")#, U2 (test):",rmse$Theil2.test,"\n")    # <1: better than naive forecast
        cat2(opts,"Theils U3 (test, RMAE):", rmae$Theil.test,"\n")    # based on RMAE instead of RMSE
     
        if (!is.na(match("SRF",ls()))) {
        	rmae$SRF.perc = SRF$perc;			               # just to report these
        	rmae$SRF.lsd = SRF$lsd;				               # three numbers below  
        	rmae$SRF.lsi = length(SRF$input.variables)   # in allRMAE
       	}

        #=============================================
        # PART 4.7: GRAPHICS
        #=============================================
        if (opts$GD.DEVICE!="non") {
          # point plot: true response (x) vs. predicted response (y)
          tdmGraphicNewWin(opts);
          if (opts$gr.log) {
            gr_trans <- function(x,xmin) { log(x[!is.na(x)]+1-xmin, base=10); }
            min_trans <- function(x) { min(min(x[!is.na(x)]),0); }            
          } else {
            gr_trans <- function(x,xmin) { x; }
            min_trans <- function(x) { 0; }            
          }
          r = c(d_train[,response.variable],d_test[,response.variable]);
          rmin = min_trans(r);
          if (is.null(opts$lim)) lim=c(min(r),max(r)) else lim=opts$lim;
          true_out = gr_trans(d_train[,response.variable],rmin)

          plot(gr_trans(lim,rmin),gr_trans(lim,rmin),col='blue',type="l"
               ,xlab=ifelse(opts$gr.log,"lg(true_out+1)","true_out")
               ,ylab=ifelse(opts$gr.log,"lg(predict+1)","predict")
               ,xlim=gr_trans(lim,rmin)
               ,ylim=gr_trans(lim,rmin)
               )
          #points(true_out[true_out<opts$OCUT],train.predict[true_out<opts$OCUT],col='green')
          #points(true_out[true_out<opts$OCUT],train.predict[true_out<opts$OCUT],col='blue')
          points(true_out,gr_trans(train.predict,rmin),col='blue')
          true_out = gr_trans(d_test[,response.variable],rmin)
          points(true_out,gr_trans(test.predict,rmin),col='red')
          title(response.variable)
          legend("topleft",legend=c("train", "test"),
                 col=c("blue","red"), lty=c(1,1), lwd=c(1,1),
                 pch=c(5,5), pt.bg=c("white","white"), pt.cex=1.0,
                 text.col="blue4", bg="gray95")
          tdmGraphicCloseWin(opts);
        } # if (opts$GD.DEVICE!="non")
        
        allRMAE <- rbind(allRMAE,as.data.frame(rmae));
        allRMSE <- rbind(allRMSE,as.data.frame(rmse));
        
        cat1(opts,sprintf("  %s: rmae$test =%8.3f, RMAE$test =%8.3f\n",response.variable,rmae$test,mean(allRMAE$test)));
        cat1(opts,sprintf("  %s: rmae$train=%8.3f, RMAE$train=%8.3f\n",response.variable,rmae$train,mean(allRMAE$train)));

    } # for (response.variable)
    
    RMAE <- as.list(mapply(mean,allRMAE));
    RMSE <- as.list(mapply(mean,allRMSE));
               
    #=============================================
    # PART 4.8: WRITE RESULTS ON TEST SET TO FILE
    #=============================================
    dir.output <- paste(dirname(opts$dir.output),basename(opts$dir.output),sep="/")  # remove trailing "/", if it exists
    if (!file.exists(dir.output)) {
      success = dir.create(dir.output);     
      if (!success) stop(sprintf("Could not create dir.output=%s",dir.output));
    }
    #outfile = paste(opts$dir.output,sub(".csv", "", filename), "_predictions.csv", sep="")
    #write.table(d_test, file=outfile, quote=F, sep=";", dec=".", row.names=F, col.names=TRUE)

    res =   list(rmse=RMSE          # root mean square error, averaged over all response.variables
                ,rmae=RMAE          # relative mean absolute error, Theil's U and mean abs deviation, averaged over all response.variables
                ,allRMAE=allRMAE	 	# RMAE, Theil's U and mean abs deviation, separately for each response.variable 
                ,lastModel = res.rf # model from last response.variable 
                ,d_train=d_train
                ,d_test=d_test 
                ,SRF=SRF            # output from tdmModSortedRFimport or NULL
                ,opts=opts          # some defaults might have been added  
               )
    if (opts$MOD.method=="RF" & opts$RF.p.all) res$train.indiv=app$train.indiv;
    
    class(res) <- c("tdmRegre","TDM")     # this causes > res; or > print(res);
                                          # NOT to print out the whole list (might be very long!!)
                                          # but to call instead the function  print.tdmRegre
    res;
} # tdmRegress
        
