######################################################################################
# tdmClassify
# TODO: make code also applicable if d_test is a 0-row data frame.
#
#'      Core classification function of TDMR. It is called by \code{\link{tdmClassifyLoop}}.
#'      Train a model on training set \code{d_train} and evaluate it on test set \code{d_test}.
#'
#' Currently  d_dis is allowed to be a 0-row data frame, but d_train and d_test must have at least one record. \cr
#' 
#' If this function is used for tuning, the test set \code{d_test} plays the role of a validation set.
#'
#'   @param d_train     training set
#'   @param d_test      test set, same columns as training set
#'   @param d_dis       'disregard set', i.e. everything what is neither train nor test. The model is 
#'                   applied to all records in d_dis (needed for active learning, see ssl_methods.r)
#'   @param response.variables   name of column which carries the target variable - or - 
#'                   vector of names specifying multiple target columns
#'                   (these columns are not used during prediction, only for evaluation)
#'   @param input.variables     vector with names of input columns 
#'   @param opts     additional parameters [defaults in brackets]
#'     \describe{
#'     \item{\code{SRF.*}}{ several parameters for \code{\link{tdmModSortedRFimport}} }
#'     \item{\code{RF.*}}{ several parameters for RF (Random Forest, defaults are set, if omitted)  }
#'     \item{\code{SVM.*}}{ several parameters for SVM (Support Vector Machines, defaults are set, if omitted)}
#'     \item{\code{filename}}{ }
#'     \item{\code{data.title}}{ }
#'     \item{\code{MOD.method}}{ ["RF"] the main training method
#'                   ["RF"|"MC.RF"|"SVM"|"NB"]: use [Random forest| MetaCost-RF| SVM| Naive Bayes] for the main model}
#'     \item{\code{MOD.SEED}}{ =NULL: get a new random number seed with \code{\link{tdmRandomSeed}} (different RF trainings). \cr
#'                   =any value: set the random number seed to this value (+i) to get reproducible random
#'                   numbers. In this way, the model training part (RF, NNET, ...) gets always a fixed seed
#'                   (see also TST.SEED in \code{\link{tdmClassifyLoop}}) }
#'     \item{\code{CLASSWT}}{ class weights (NULL, if all classes should have the same weight)
#'                   (currently used only by methods RF, MC.RF and by \code{\link{tdmModSortedRFimport}})  }
#'     \item{\code{fct.postproc}}{ user-def'd function for postprocessing of predicted output  }
#'     \item{\code{GD.DEVICE}}{ if !="non", then make a pairs-plot of the 5 most important variables
#'                   and make a true-false bar plot }
#'     \item{\code{VERBOSE}}{ [2] =2: most printed output, =1: less, =0: no output }
#'     }
#'         
#'   @return  \code{res}, an object of class \code{tdmClass}, this is a list containing
#'       \item{\code{d_train}}{ training set + predicted class column(s) }
#'       \item{\code{d_test}}{ test set + predicted class column(s) }
#'       \item{\code{d_dis}}{  disregard set + predicted class column(s)  }
#'       \item{\code{sumEVAL}}{ list with evaluation measures, summed over all response variables }
#'       \item{\code{allEVAL}}{ data frame with evaluation measures, one row for each response variable }
#'       \item{\code{lastCmTrain}}{ a list with evaluation info for training set (confusion matrix, gain, class errors, ...)  }
#'       \item{\code{lastCmTest}}{  a list with evaluation info for test set (confusion matrix, gain, class errors, ...) }
#'       \item{\code{lastModel}}{       the last model built (e.g. the last Random Forest in the case of MOD.method=="RF") }
#'       \item{\code{lastProbs}}{    a list with three probability matrices (row: records, col: classes) v_train, v_test, v_dis, if the model provides probabilities; NULL else. }
#'       \item{\code{lastPred}}{     name of the colum where the prediction of the last model is appended to the datasets d_train, d_test and d_dis }
#'       \item{\code{opts}}{ parameter list from input, some default values might have been added }
#'
#'    The 9 evaluation measures in sumEVAL and allEVAL are
#'         cerr.*    (misclassification errror),
#'         gain.*    (total gain) and
#'         rgain.*   (relative gain, i.e. total gain divided by max. achievable gain in *)
#'    where * = [trn | tst | tst2 ] stands for [ training set | test set | test set with special treatment ]
#'    and the special treatment is either opts$test2.string = "no postproc" or = "default cutoff".
#'       \cr\cr
#'    The five items \code{lastCmTrain}, \code{lastCmTest}, \code{lastModel}, \code{lastProbs}, \code{lastPred} are 
#'    specific for the *last* model (the one built for the last response variable in the last run and last fold) 
#'
#' @seealso  \code{\link{print.tdmClass}}, \code{\link{tdmClassifyLoop}}
#' @author Wolfgang Konen, FHK, Sep'2009 - Dec'2011
#'
#' @export
######################################################################################
tdmClassify <- function(d_train,d_test,d_dis,response.variables,input.variables,opts)
{
    first <- TRUE; 
    filename <- opts$filename
    saved.input.variables <- input.variables;      # save copy for response.variable loop
   
    # be sure that all necessary  defaults are set:
    opts <- tdmOptsDefaultsFill(opts);

    if (!is.null(opts$RF.mtry)) opts$RF.mtry=min(length(input.variables),opts$RF.mtry);
    # this is to avoid the "invalid mtry" warning (but it does not eliminate all cases)

    if (opts$i==opts$NRUN & opts$k==opts$the.nfold) {
      if (opts$GD.RESTART) {
        tdmGraphicCloseDev(opts); 
        tdmGraphicInit(opts);
      }
    }   
    
    
    for (response.variable in response.variables) {   
        input.variables <- saved.input.variables;
        if (!is.factor(d_train[,response.variable]))   {
          warning(paste("Column",response.variable," of d_train is not a factor >> we change it to factor!"));
          d_train[,response.variable] <- as.factor(d_train[,response.variable]);
        }
        lev.resp <- levels(d_train[,response.variable]);
        n.class <- length(lev.resp);
        if (!is.null(opts$CLS.CLASSWT)) names(opts$CLS.CLASSWT) <- lev.resp;
        opts$CLS.cutoff <- tdmModAdjustCutoff(opts$CLS.cutoff,n.class);
        opts$SRF.cutoff <- tdmModAdjustCutoff(opts$SRF.cutoff,n.class);
  
        if (is.null(opts$CLS.gainmat)) {
          # default (generic) gain matrix:
          opts$CLS.gainmat <- matrix ( diag(n.class),nrow=n.class,ncol=n.class,
                                  dimnames=list(lev.resp,   # row names    (true levels)
                                                lev.resp    # column names (predicted levels)
                                                ))
          # instead of diag(n.class) an existing opts$CLS.gainmat can specify a user def'd gain matrix, e.g. for n.class=2:
          #     gainmat <- matrix(c(+1,-2,
          #                         0,+1), byrow=TRUE,nrow=2,ncol=2, ...
        }
                
        #=============================================
        # PART 4.1: SUMMARY OF TRAINING DATA
        #=============================================
        #cat1(opts,filename,": Summary of training data ...\n")
        #print(summary(d_train))           # most columns are of numeric type
                                           # -> summary min,max,quantiles...
             
        #=============================================
        # PART 4.2: IMPORTANCE SELECTION (BY USING RF)
        #=============================================
        # determine the importance of all input var's by constructing a test RF
        # --- this step is skipped if SRF.kind=="none", then you use all     ---
        # --- input variables and you do not see the importance of variables ---
        if (opts$SRF.kind!="none") {
          cat1(opts,filename,": Importance check ...\n");
          opts$RF.sampsize <- tdmModAdjustSampsize(opts$SRF.samp, d_train, response.variable, opts);
          SRF <- tdmModSortedRFimport(d_train,response.variable,
                                      input.variables,opts)
          input.variables <- SRF$input.variables;  
          opts <- SRF$opts;       # some defaults might have been added  
        }  else {
          SRF=NULL;
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
        } else {
          newseed=opts$MOD.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed) # if you want reproducably the same model training,
        }                   # but different for each run i

    		# Boruta: Calc variable importance
    		#library(Boruta)
    		#Boruta(formula, data=to.model, doTrace=2)->Bor.son
    		#print(Bor.son)
    		#stats<-attStats(Bor.son);
    		#print(stats);
    		#plot(normHits~meanZ,col=stats$decision,data=stats);
              
    
        res.rf <- res.nb <- train.predict <- test.predict <- NULL
        #================================================================
        # PART 4.3: MODELING: TRAIN RANDOM FOREST (OR OTHER METHOD)
        #================================================================
        to.model <- d_train[,c(input.variables,response.variable)]
        to.test <- d_test[,c(input.variables,response.variable)]
        if (opts$MOD.method %in% c("RF","MC.RF"))
          opts$RF.sampsize <- tdmModAdjustSampsize(opts$RF.samp, to.model, response.variable, opts);
        train.rf <- function(response.variable,to.model,opts) {
            cat1(opts,opts$filename,": Train RF with sampsize =", opts$RF.sampsize,"...\n")
            if (!is.null(opts$CLS.CLASSWT)) {
                cat1(opts,"Class weights: ", opts$CLS.CLASSWT,"\n")
                cwt = opts$CLS.CLASSWT*1.0;   # strange, but necessary: if we omit '*1' then cwt seems to be a copy-by-reference of
                                          # opts$CLS.CLASSWT. After randomForest call  cwt is changed and also opts$CLS.CLASSWT would be changed (!)
            } else { cwt=NULL; }
            if (!is.null(opts$CLS.cutoff)) cat1(opts,"Cutoff: ", opts$CLS.cutoff,"\n")
            formul <- formula(paste(response.variable, "~ ."))   # use all possible input variables
            # we work here with a command text string and eval(parse(...)) to allow for the presence or
            # absence of certain options like "mtry" or "cutoff" which are not allowed to be NULL.
            # The individual "eval(...)" on the following lines are for clarity of res.rf$call 
            # (it should read "..., ntree=400, ..." and not "..., ntree=opts$RF.ntree, ...") 
            # BUT: we cannot use "eval(...)" for the lists cutoff and cwt, therefore we use here
            # the 'non-speaking' variables cwt, opts$CLS.cutoff and add below res.rf$cutoff, res.rf$classwt 
            # for optional later reference or user inspection.
            rf.options = paste(" ntree=",eval(opts$RF.ntree));
            rf.options = paste(rf.options,paste(" sampsize=",eval(opts$RF.sampsize)),sep=",")
            rf.options = paste(rf.options," classwt=cwt"," na.action=na.roughfix"," proximity=FALSE",sep=",")
            #if (!is.null(cwt))  paste(rf.options,paste("classwt=",eval(cwt)),sep=",")    # not run
            if (!is.null(opts$RF.mtry)) rf.options = paste(rf.options,paste(" mtry=",eval(opts$RF.mtry)),sep=",")
            if (!is.null(opts$CLS.cutoff)) rf.options = paste(rf.options," cutoff=opts$CLS.cutoff",sep=",")
            if (!is.null(opts$RF.nodesize)) rf.options = paste(rf.options,paste(" nodesize=",eval(opts$RF.nodesize)),sep=",")
            #dbg_chase_cutoff_bug(formul,to.model,d_train,response.variable,rf.options,opts);
            flush.console();          
            eval(parse(text=paste("res.rf <- randomForest( formul, data=to.model,",rf.options,")"))); 
            res.rf$HasVotes = TRUE; 
            res.rf$HasProbs = TRUE; 
            res.rf$cutoff = opts$CLS.cutoff;
            res.rf$classwt = opts$CLS.CLASSWT;
            res.rf;
        } 
        train.mc.rf <- function(response.variable,to.model,opts) {
            cat1(opts,opts$filename,": Train tdmMetacostRf ...\n")
            flush.console();
            res.rf <- tdmMetacostRf(response.variable,to.model,opts)
            res.rf$HasVotes = TRUE; 
            res.rf$HasProbs = TRUE; 
            res.rf;
        }
#        train.kSVM <- function(response.variable,to.model,opts) {
#            cat1(opts,filename,": Train kSVM ...\n")
#		        res.rf = list()
#      		  class(res.rf) <- "TDMres_c"
#            formul <- formula(paste(response.variable, "~ ."))   # use all possible input variables
#  	        flush.console();
#            res.rf$model <- ksvm(formul
#                    	           , data=to.model
#          						           , kernel="rbfdot"
#	                               , kpar=list(sigma=opts$SVM.sigma)
#                        	       , cost=opts$SVM.cost
#                         	       , type="spoc-svc"  ### type=spoc-svc   #type="C-svc"
#            							       , class.weights=opts$CLS.CLASSWT 
#            							       , na.action=na.roughfix)
#            res.rf$HasVotes = FALSE;            
#            res.rf$HasProbs = FALSE; 
#	        res.rf;
#        } # train.kSVM
        train.svm <- function(response.variable,to.model,opts) {
            cat1(opts,filename,": Train SVM ...\n")
	          require(e1071)			
            if (!is.null(opts$CLS.CLASSWT)) cat1(opts,"Class weights: ", opts$CLS.CLASSWT,"\n")
            flush.console();
            formul <- formula(paste(response.variable, "~ ."))   # use all possible input variables
            res.rf <- svm(formul 
            							, data=to.model 
            							, kernel="radial"
                          , gamma=opts$SVM.gamma
                 	        , cost=opts$SVM.cost
                          , type="C-classification"
            							, class.weights = opts$CLS.CLASSWT
            							, na.action=na.roughfix
            							, probability=TRUE)
            res.rf$HasVotes = FALSE;            
            res.rf$HasProbs = TRUE; 
      			res.rf;
        }# train.SVM
        train.naiveBayes <- function(response.variable,to.model,opts) {
            cat1(opts,filename,": Train NB ...\n")
            flush.console();
            formul <- formula(paste(response.variable, "~ ."))   # use all possible input variables
            # CAUTION 1: Be careful, that the column ordering in 'naiveBayes(...,data=..)' is 
            # the same as below in 'predict(res.nb,newdata=..)'. Therefore we need here
            #     to.model=d_train[,c(input.variables,response.variable)]
            # instead of a simple 'data=d_train'
            res.rf <- naiveBayes(formula=formul, data=to.model)
            res.rf$HasVotes = FALSE;                          
            res.rf$HasProbs = FALSE; 
            res.rf;
        }
        train.CMB <- function(opts) {
            res.rf <- list();
            res.rf$HasVotes = FALSE;                          
            res.rf$HasProbs = TRUE;
            res.rf;
        }

        ptm <- proc.time()
        cat1(opts, "Run ",ifelse(opts$the.nfold>1,paste(opts$i,".",opts$k,sep="")           ,opts$i)    ,"/",
                          ifelse(opts$the.nfold>1,paste(opts$NRUN,".",opts$the.nfold,sep=""),opts$NRUN) ,":\n"); 
        res.rf = switch(opts$MOD.method
          ,"RF"  =  train.rf(response.variable,to.model,opts)
          ,"MC.RF"  =  train.mc.rf(response.variable,to.model,opts)
          ,"SVM" =  train.svm(response.variable,to.model,opts)
#          ,"kSVM" =  train.kSVM(response.variable,to.model,opts)
          ,"NB"  =  train.naiveBayes(response.variable,to.model,opts)
          ,"CMB" = train.CMB(opts)
          ,"INVALID"
          );

        if (res.rf[1]=="INVALID") {
          cat1(opts,sprintf("*** Invalid opts$MOD.method=%s ***\n",opts$MOD.method));
        }
        #print(res.rf)
        cat1(opts,"Proc time: ",(proc.time()-ptm)[1],"\n");       
		#browser()
    
        #=====================================================================
        # PART 4.4: APPLY RANDOM FOREST  (OR OTHER METHOD)
        #=====================================================================
        apply.rf <- function(res.rf,to.model,to.test,to.dis,opts) {        
            cat1(opts,filename,": Apply",opts$MOD.method,"...\n")
            app = list()            
            app$train.predict <- res.rf$predicted
            # (res.rf$predicted is the *OOB-prediction* on the training set)
            # Think about this! Why is it WRONG (or too optimistic) to use here
            #      app$train.predict <- predict(res.rf, newdata=d_train)   
            # as the prediction for the training set?
            
            app$test.predict <- predict(res.rf, newdata=to.test)
            app$test.votes <- predict(res.rf, newdata=to.test, type="vote")
            #app$test.predict <- factor(rep(0,nrow(to.test)),levels=c(0,1)) # dummy for DMC2010 to predict the naive model
            
            app$dis.predict <- predict(res.rf, newdata=to.dis)
            app$dis.votes <- predict(res.rf, newdata=to.dis, type="vote")
            
            app;
        }
        apply.other <- function(res.rf,to.model,to.test,to.dis,opts) {
            predict.TDMres_c <- function(res.rf, newdata) {predict(res.rf$model, newdata=newdata);}#required for S4 models
            cat1(opts,opts$filename,": Apply",opts$MOD.method,"...\n")
            app = list()  
            app$train.predict <- predict(res.rf, newdata=to.model)
            app$test.predict <- predict(res.rf, newdata=to.test)
            if (nrow(to.dis)>0) app$dis.predict <- predict(res.rf, newdata=to.dis)
            app;
        }
        apply.SVM <- function(res.rf,to.model,to.test,to.dis,opts) {
            cat1(opts,opts$filename,": Apply",opts$MOD.method,"...\n")
            app = list()  

            app$train.prob <- attr(predict(res.rf, newdata=to.model, probability=TRUE),"probabilities");
            if (is.null(opts$CLS.cutoff)) {
              app$train.predict <- predict(res.rf, newdata=to.model)
            } else {
              cat1(opts,"Cutoff: ", opts$CLS.cutoff,"\n")
  			      # Apply new weighting scheme for training set            
     			    app$train.predict <- apply(app$train.prob/(matrix(1,nrow(to.model),1) %*% opts$CLS.cutoff),1,which.max);
     			    app$train.predict <- lev.resp[app$train.predict];

            }
  
      			if (nrow(to.test)>0) {
         			app$test.prob <- ifelse(nrow(to.test)==0,NULL,attr(predict(res.rf, newdata=to.test, probability=TRUE),"probabilities"));
              if (is.null(opts$CLS.cutoff)) {
              app$test.predict <- predict(res.rf, newdata=to.test)
              } else {
        			# Apply new weighting scheme for test set            
     			    app$test.predict <- apply(app$test.prob/(matrix(1,nrow(to.test),1) %*% opts$CLS.cutoff),1,which.max)
     			    app$test.predict <- lev.resp[app$test.predict];
    			    }
  			    }
  			    
      			if (nrow(to.dis)>0) {
        			app$dis.prob <- attr(predict(res.rf, newdata=to.dis, probability=TRUE),"probabilities");
              if (is.null(opts$CLS.cutoff)) {
                app$dis.predict <- predict(res.rf, newdata=to.dis)
              } else {
          			# Apply new weighting scheme for disregard set (unknown label data in active learning)
       			    app$dis.predict <- apply(app$dis.prob/(matrix(1,nrow(to.dis),1) %*% opts$CLS.cutoff),1,which.max)
       			    app$dis.predict <- lev.resp[app$dis.predict];
    			    }
  			    }
            
            app;
        }
        apply.CMB <- function(to.model,to.test,to.dis,opts) {
            cat1(opts,opts$filename,": Apply",opts$MOD.method,"...\n")
            app = list()  
  
            app$train.prob <- opts$result1$lastRes$lastProbs$v_train * opts$result2$lastRes$lastProbs$v_train;
      			app$test.prob <- opts$result1$lastRes$lastProbs$v_test * opts$result2$lastRes$lastProbs$v_test;
      			app$dis.prob <- opts$result1$lastRes$lastProbs$v_dis * opts$result2$lastRes$lastProbs$v_dis;

   			    app$train.predict <- lev.resp[apply(app$train.prob,1,which.max)];
   			    app$test.predict <- lev.resp[apply(app$test.prob,1,which.max)];
   			    app$dis.predict <- lev.resp[apply(app$dis.prob,1,which.max)];   			    
   			    app;
        }
          
        ptm <- proc.time()
        opts$response.variable <- response.variable;
        #if (opts$MOD.method=="NB") {
          # CAUTION 2: predict.naiveBayes requires that only the input variables
          # enter via 'newdata', so we have to exclude column response.variable
          # Otherwise a strange error message occurs:
          #     Fehler in FUN(1:6[[1L]], ...) : Indizierung außerhalb der Grenzen
          to.model <- data.frame(a=d_train[,input.variables]); 
          to.test <- data.frame(a=d_test[,input.variables]);
          to.dis <- data.frame(a=d_dis[,input.variables]);
          names(to.model) <- names(to.test) <- names(to.dis) <- input.variables;
          # Why 'data.frame', 'a=' and 'names...'? - In this way it works also
          # for length(input.variables)==1. If we had instead the simple 
          #     to.model <- d_test[,input.variables]
          # this would lead to an unnamed vector.
        #}

        app = switch(opts$MOD.method
          ,"RF" =,"MC.RF" =  apply.rf(res.rf,to.model,to.test,to.dis,opts)
          ,"NB" =, "kSVM" = apply.other(res.rf,to.model,to.test,to.dis,opts) 
    		  ,"SVM" = apply.SVM(res.rf,to.model,to.test,to.dis,opts)
    		  ,"CMB" = apply.CMB(to.model,to.test,to.dis,opts)
          ,"INVALID"
          );
        if (app[1]=="INVALID") {
          cat1(opts,sprintf("*** Invalid opts$MOD.method=%s ***\n",opts$MOD.method));
        }
        train.predict <- app$train.predict;
        test.predict <- app$test.predict;
        dis.predict <- app$dis.predict;

        #
        # column "votes" is beneficial for opts$fct.postproc (e.g. in the case of DMC2010)
        if (res.rf$HasVotes) {  # column "votes" is the fraction of trees voting for the majority class
          name.of.votes <- "votes";
          d_train <- bind_response(d_train, name.of.votes, res.rf$votes[,1])
          d_test  <- bind_response(d_test, name.of.votes, app$test.votes[,1])
          d_dis   <- bind_response(d_dis, name.of.votes, app$dis.votes[,1])
          if (opts$test2.string == "default cutoff") {
            #-- choice 1: test2 is the prediction with default cutoff -------------------------------
            cutoff <- rep(1/n.class, n.class);
            test2.predict <- predict(res.rf, newdata=to.test, cutoff=cutoff)
            test2.votes <- predict(res.rf, newdata=to.test, cutoff=cutoff, type="vote")
          } else { # i.e. opts$test2.string == "no postproc";
            #-- choice 2: test2 is a mere copy of test for which we later skip the postprocessing --- 
            test2.predict <- test.predict;
            test2.votes <- app$test.votes[,1]; 
          }
        }
        if (res.rf$HasProbs) {
          lastProbs = NULL;                                        # lastProbs is used by ssl_methods.r
          if (opts$MOD.method %in% c("RF","MC.RF")) {
            lastProbs = list(  v_train = res.rf$votes                
                              , v_test = app$test.votes
                              , v_dis = app$dis.votes
                              );
          }
          if (opts$MOD.method %in% c("SVM","CMB")) {
            lastProbs = list(  v_train = app$train.prob             
                              , v_test = app$test.prob
                              , v_dis = app$dis.prob
                              );
          }
        } else {
          lastProbs = NULL;
        } 
        cat1(opts,"Proc time: ",(proc.time()-ptm)[1],"\n");

    ######################################################################################
    # this part is specific for the DMC2010-task and method = "RF" (only needed for human analysis)
    ######################################################################################   
    # just for comparision: with default cutoff (and w/o postprocessing) test2.predict should  
    # be strictly worse in comparision to test.predict:
    VOTE2TARGET.CHK = FALSE
    if ( VOTE2TARGET.CHK ) {
      vres.train <- tdmModVote2Target(res.rf$votes[,1],res.rf$predicted,d_train[,response.variable]);
      print(vres.train);
      #vres.test <- tdmModVote2Target(app$test.votes[,1],test.predict,d_test[,response.variable]);
      #print(vres.test);
      #vres.test2 <- tdmModVote2Target(test2.votes[,1],test2.predict,d_test[,response.variable]);
      #print(vres.test2);
    }
    ######################################################################################
                  
            if (opts$APPLY_TIME) {
              xstart = proc.time()
              for (t in 1:100) {
                dummy <- predict(res.rf, newdata=to.model)
              }
              xend = proc.time()
              cat1(opts,"\nElapsed time for 100x APPLY on train set:\n")
              print(xend-xstart)
              print(nrow(to.model))
            }
    
        #=============================================
        # PART 4.5: POSTPROCESSING
        #=============================================
      	if (opts$DO.POSTPROC) {
      		cat1(opts,filename,": User-defined postprocessing: Applying opts$fct.postproc ...\n")
      		train.predict <- opts$fct.postproc(train.predict,d_train,opts)
      		test.predict <- opts$fct.postproc(test.predict,d_test,opts)
      		dis.predict <- opts$fct.postproc(dis.predict,d_dis,opts)
      		if (res.rf$HasVotes) {
            opts2 <- opts; 
            opts2$DO.POSTPROC=FALSE; # i.e. no postprocessing for test2
            if (opts2$DO.POSTPROC)                                   
          		 test2.predict <- opts$fct.postproc(test2.predict,d_test,opts2)
      		}
      	}
        # bind the predicted class pred_... as last column to the data frames
        name.of.prediction <- paste("pred_", response.variable, sep="")
        name2.of.prediction <- paste("pred2_", response.variable, sep="")
        d_train <- bind_response(d_train, name.of.prediction, train.predict)
        d_test <- bind_response(d_test, name.of.prediction, test.predict)
        d_dis <- bind_response(d_dis, name.of.prediction, dis.predict)
        if (res.rf$HasVotes) d_test  <- bind_response(d_test, name2.of.prediction, test2.predict)

    
        #=============================================
        # PART 4.6: EVAL: CALC CONFUSION MATRIX + GAIN
        #=============================================
        cat1(opts,filename,": Calc confusion matrix + gain ...\n")
            
        cat1(opts,"\nTraining cases (",length(train.predict),"):\n")
        cm.train <- tdmModConfmat(d_train,response.variable,name.of.prediction,opts);
        # the contents of cm.train$rgain depends on opts$rgain.type
        
        #cat1(opts,"   --- predicted ---\n")
        if (opts$VERBOSE>=1) print(cm.train$mat)                     # confusion matrix on training set
        cat1(opts,sprintf("total gain: %7.1f (is %7.3f%% of max. gain = %7.1f)\n", 
                          cm.train$gain,cm.train$gain/cm.train$gainmax*100,cm.train$gainmax));

        if (nrow(d_test)>0) {
          cat1(opts,"\nTest cases (",length(test.predict),"):\n")
          cm.test <- tdmModConfmat(d_test,response.variable,name.of.prediction,opts);
          print1(opts,cm.test$mat)                      # confusion matrix on test set
          print1(opts,cm.test$gain.vector)
          cat1(opts,sprintf("total gain : %7.1f (is %7.3f%% of max. gain = %7.1f)\n", 
                            cm.test$gain,cm.test$gain/cm.test$gainmax*100,cm.test$gainmax));
          if (res.rf$HasVotes) {                          
            cat1(opts,"\nTest2 cases [",opts$test2.string,"] (",length(test.predict),"):\n")
            cm.test2 <- tdmModConfmat(d_test,response.variable,name2.of.prediction,opts);
            print1(opts,cm.test2$mat)                      # confusion matrix on test set, test2-prediction
            cat1(opts,sprintf("total gain2: %7.1f (is %7.3f%% of max. gain = %7.1f)\n", 
                              cm.test2$gain,cm.test2$gain/cm.test2$gainmax*100,cm.test2$gainmax));
            # DMC2010:                          
            # we usually get (w/o postprocessing): total.gain: 27.4%, total.gain2: 21.5%   
            # but with postprocessing only marginal diff: total.gain: 28.0% +- 0.3%, total.gain2: 27.7% +- 0.03% 
          }  else {
            cm.test2 = cm.test;
          }
        } else {
          cm.test <- cm.test2 <- NULL;
        }
        
        EVAL =  list(        # a list of eval-quantities which can be summed
                     cerr.trn=cm.train$cerr[1,"Total"]       # misclassification error 
                    ,gain.trn=cm.train$gain
                    ,rgain.trn=cm.train$rgain
                    ,cerr.tst=cm.test$cerr[1,"Total"]
                    ,gain.tst=cm.test$gain
                    ,rgain.tst=cm.test$rgain
                    ,cerr.tst2=cm.test2$cerr[1,"Total"]      #
                    ,gain.tst2=cm.test2$gain                 # for comparision in method="RF": results with
                    ,rgain.tst2=cm.test2$rgain               # default cutoff 1/n.class or with no postproc (see TEST2.MODE)
                    #,gainmax=cm.train$gainmax
                    );
        EVALa = EVAL;
        EVALa$test2.string=opts$test2.string;
        EVALa$resp = response.variable;
        EVALa$i = opts$i;
        EVALa$k = opts$k;         

        if (first) {
            sumEVAL=lapply(EVAL,sum);
            allEVAL=as.data.frame(EVALa);
            first=FALSE;
        } else {
            sumEVAL<- as.list(mapply(sum,EVAL,sumEVAL));
            allEVAL <- rbind(allEVAL,as.data.frame(EVALa));
        }
        rownames(allEVAL)[nrow(allEVAL)] <- response.variable;
                    
        #=============================================
        # PART 4.7: GRAPHICS
        #=============================================
        if (!(opts$i==opts$NRUN & opts$k==opts$the.nfold)) {
          if (opts$GD.RESTART) {
            tdmGraphicCloseDev(opts); 
            tdmGraphicInit(opts);
          } 
        } 
        if (opts$GD.DEVICE!="non" & length(input.variables)>1) {
            tdmGraphicNewWin(opts);
            opts$gr.points=500;
            p=sample(nrow(d_train));
            ind <- p[1:opts$gr.points];
            # scatter plot for each  pair of the 5 most important input variables, colored with class levels
            # (this can become very large, if #records is high)
            mycolors <-  sample(colors(),n.class,replace=FALSE)    # select n.class random colors
            important.variables=input.variables
            maxi = min(5,length(important.variables));
            pairs(d_train[ind,important.variables[1:maxi]], cex=0.6, gap=0,
                  col=mycolors[as.numeric(d_train[ind,response.variable])],
                  main=paste(opts$data.title,": The five most important inputs", sep=""))
            tdmGraphicCloseWin(opts);
        }
        if (opts$GD.DEVICE!="non") {
            # bar plot of true/false test cases for all classes
            tdmGraphicNewWin(opts);
            cmt<-cm.test$mat
            height<-t(matrix(c(diag(cmt),rowSums(cmt)-diag(cmt)),nrow=n.class,ncol=2))
            # 'height' is a 2*n.class matrix containing in its first row the number of
            # correct classifications for each class level (diagonal of confusion matrix)
            # and in its 2nd row the number of wrong classifications for each class level
            # (sum of off-diagonal elements in each row of the confusion matrix)
            barplot(height,beside=TRUE,col=c("blue","red"),legend=c("true","false"),
                    names.arg=colnames(cmt),
                    main="True/false classification on test set")
            tdmGraphicCloseWin(opts);
        } # if (opts$GD.DEVICE!="non")
 
    } # for (response.variable)
    
    #=============================================
    # PART 4.8: WRITE RESULTS ON TEST SET TO FILE
    #=============================================
    dir.output <- paste(dirname(opts$dir.output),basename(opts$dir.output),sep="/")  # remove trailing "/", if it exists
    if (!file.exists(dir.output)) dir.create(dir.output);     
    #outfile = paste(opts$dir.output,sub(".csv", "", filename), "_predictions.csv", sep="")
    #write.table(d_test, file=outfile, quote=FALSE, sep=";", dec=".", row.names=FALSE, col.names=TRUE)

    if (!is.null(opts$EVALFILE)) {
      colNames = FALSE
      if (opts$i==1 & opts$k==1) colNames = TRUE
    	write.table(data.frame(allEVAL)
    			, file = paste(opts$dir.output,opts$EVALFILE, sep="")
    			, col.names= colNames
    			, row.names= FALSE
    			, append = !colNames         
    			, sep = ";",
    			, quote = FALSE
    			, eol = "\n"
    	);
   	}

    res =   list(lastCmTrain=cm.train   # from last response.variable 
                ,lastCmTest=cm.test     #   "     "     "       "
                ,lastModel = res.rf         #   "     "     "       "
                ,lastPred = name.of.prediction #   "     "       "
                ,lastProbs = lastProbs  # NULL or list with 3 probability matrices (row:records, col: classes) v_train, v_test, v_dis
                ,sumEVAL=sumEVAL
                ,allEVAL=allEVAL
                ,d_train=d_train
                ,d_test=d_test 
                ,d_dis=d_dis 
                ,SRF=SRF                  # output from tdmModSortedRFimport or NULL
                ,opts=opts                # some defaults might have been added  
                );
               
    class(res) <- c("tdmClass","TDM")     # this causes > res; or > print(res);
                                          # NOT to print out the whole list (might be very long!!)
                                          # but to call instead the function  print.tdmClass
    res;
}  # tdmClassify

######################################################################################
# tdmClassifyLoop:
#
#'    Core classification double loop of TDMR. It contains a doublee loop (opts$NRUN and CV-folds)
#'    and calls \code{\link{tdmClassify}}. It is called  by all R-functions main_*
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
#'       \item{\code{TST.SEED}}{ =NULL: get a new random number seed with \code{\link{tdmRandomSeed}}. =any value: set the random number seed 
#'               to this value to get reproducible random numbers and thus reproducible training-test-set-selection.
#'               (only relevant in case TST.kind=="cv" or "rand") (see also MOD.SEED in \code{\link{tdmClassify}})}
#'       \item{\code{TST.kind}}{ how to create cvi, handed over to \code{\link{tdmModCreateCVindex}}. If TST.kind="col", then cvi is taken from dset[,opts$TST.col].}
#'       \item{\code{GD.RESTART}}{ [TRUE] =TRUE/FALSE: do/don't restart graphic devices}
#'       \item{\code{GRAPHDEV}}{ ["non"| other ]}
#'     }
#' 
#'   @return \code{result},  an object of class TDMclassifier, this is a list with results, containing
#'       \item{opts}{ the res$opts from \code{\link{tdmClassify}}} 
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
#' @author Wolfgang Konen, FHK, Sep'2010 - Oct'2011
#' @export
######################################################################################
tdmClassifyLoop <- function(dset,response.variables,input.variables,opts) {
    
    C_train <- C_test <- C_test2 <- G_train <- G_test <- NULL # reserve names (dynamic extension of 
    R_train <- R_test <- R_test2 <- G_test2 <- NULL           # these vectors at and of for-i-loop)

    if (opts$READ.TST==TRUE & opts$TST.kind!="col") 
      warning(sprintf("Are you sure you want opts$READ.TST=TRUE, but opts$TST.kind!='col'? Actual value is opts$TST.kind='%s'.",opts$TST.kind));
    
    for (i in 1:opts$NRUN) {  
        opts$i = i;
        
        #=============================================================================
        # PART 3: CREATE NFOLD CROSSVALIDATION INDEX  OR DIVIDE IN TRAINING / TEST SET
        #=============================================================================
        if (is.null(opts$TST.SEED)) {
          # NEW: when called via SPOT, the RNG might be at (different but) fixed seed in each call.
          #      But we want different seeds (for test set selection) to see the variability                                                                   
          set.seed(tdmRandomSeed());  
        } else {
          newseed=opts$TST.SEED+(opts$i-1)+opts$NRUN*(opts$rep-1);
          set.seed(newseed);  # if you want reproducably the same training/test sets,
        }                     # but different for each run i and each repeat (opts$rep)
                                               
        cvi <- tdmModCreateCVindex(dset,response.variables,opts,stratified=TRUE);
        nfold = max(cvi);
        
        #=============================================
        # PART 4: MODELING, EVALUATION, VISUALIZATION
        #=============================================
        allerr = NULL;
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
            
            if (opts$ncopies>0) {
                cat1(opts,opts$filename,": Adding", opts$ncopies, "parametric bootstap patterns to training set ...\n");
                d_train <- tdmParaBootstrap(d_train,response.variables,input.variables,opts);
            }

            res <- tdmClassify(d_train,d_test,d_dis,response.variables,input.variables,opts)

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
        }  # for (k)
        Err = colSums(allerr)/nfold     # assumes that each fold has the same (or nearly the same) size
            
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
    } # for (i)    

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
                	);
    if (!is.null(opts$TST.COL))         # needed when result is input for coTraining  (see ssl_methods.r)
      if (opts$TST.COL %in% names(dset))  result$TST = dset[,opts$TST.COL]  

    class(result) <- c("TDMclassifier","TDM")     # this causes > result; or > print(result);
                                                  # NOT to print out the whole list (might be very long!!)
                                                  # but to call instead the function  print.TDMclassifier
                                                  # (which in turn calls tdmClassifySummary)
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
      cat1(opts,sprintf("   (on %d records)",nrow(res$d_train)));
      result$y=-ytr;           # the score (to be minimized by SPOT) is "minus OOB score"
      result$sd.y=sd(result$R_train);      
    } else {
      result$y=-y;             # the score (to be minimized by SPOT) is "minus test score"
      result$sd.y=sd(result$R_test);
    }    
    cat1(opts,sprintf("\n%s  Test    relative gain: %7.3f",ifelse(opts$TST.kind=="cv","CV ",""),y));  
    cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_test)),""));
    cat1(opts,ifelse(opts$TST.kind=="cv"
              ,  sprintf("   (on %d records in %d folds)",nrow(res$d_train)+nrow(res$d_test),opts$TST.NFOLD)
              ,  sprintf("   (on %d records)",nrow(res$d_test))
              ),"\n");

    cat1(opts,sprintf("  Test2   relative gain (predict always with %s): %7.3f", res$allEVAL$test2.string, mean(result$R_test2)));  
    cat1(opts,ifelse(opts$NRUN>1,sprintf(" +-%7.3f",sd(result$R_test2)),""));
    cat1(opts,sprintf("   (on %d records)\n",nrow(res$d_test)));
    
    if (!is.null(dset)) result$dset=dset;          # might cost a lot of memory
    
    result;
    
} # tdmClassifySummary

#summary.TDMclassifier <- function(result,...) {
#  cat("This is the TDMclassifier summary\n");
#}

predict.TDMclassifier <- function(result,newdata,...) {
  predict(result$lastRes$lastModel,newdata,...);
}

RGainTST <- function(result) {
  if (!inherits(result, "TDMclassifier"))
        stop("This function is permitted only for objects of class `TDMclassifier'");
  mean(result$R_test);
}

RGainTRN <- function(result) {
  if (!inherits(result, "TDMclassifier"))
        stop("This function is permitted only for objects of class `TDMclassifier'");
  mean(result$R_train);
}

SRF <- function(result) {
  if (!inherits(result, "TDMclassifier"))
        stop("This function is permitted only for objects of class `TDMclassifier'");
  result$lastRes$SRF;
}

Opts <- function(x, ...)  UseMethod("Opts");
  
Opts.default <- function(x, ...)  cat("This is Opts.default\n");

Opts.TDMclassifier  <- function(x, ...) x$lastRes$opts;
Opts.TDMregressor  <- function(x, ...) x$lastRes$opts;
