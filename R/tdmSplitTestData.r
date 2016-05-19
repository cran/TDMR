######################################################################################
# tdmSplitTestData
#
#'   Read and split the task data.
#'
#'   Read the task data using \code{\link{tdmReadData2}} and split them into a test part and 
#'   a training/validation-part and return a \code{\link{TDMdata}} object.
#'
# Detail
#'   If \code{dset} is NULL, the files specified in \code{opts} are read into dset, see 
#'   \code{\link{tdmReadData2}} for details. Then, depending on the value of \code{tdm$umode}
#'   \itemize{
#'      \item \code{"SP_T"}: split the data randomly into training and test data with test 
#'        set fraction according to \code{opts$TST.testFrac}. Make use of \code{tdm$SPLIT.SEED}
#'        and \code{tdm$stratified}, if given. Set TST.COL to \code{"tdmSplit"}.
#'      \item \code{"RSUB", "CV"}: use all data for training/validation. That is, the 
#'        training-validation split is done later in \code{\link{tdmClassifyLoop}} or 
#'        \code{\link{tdmRegressLoop}}.\cr
#'      \item \code{"TST"}: split the data into training and test data according to column.
#'        \code{opts$TST.COL} (usually \code{"TST.COL"}), which carries a 1 for each test record and a 0 else. 
#'        If \code{opts$filetest} is specified, then all records from this file will 
#'        carry a 1 in \code{opts$TST.COL}. All records from \code{opts$filename} carry a 0.
#'   }
#' 
#' @param opts a list from which we need here the elements
#'     \itemize{
#'       \item \code{READ.INI}:  [T] =T: do read and split, =F: return NULL   
#'       \item \code{READ.*}:  other settings for \code{\link{tdmReadData2}}  
#'       \item \code{filename}:  needed for \code{\link{tdmReadData2}}  
#'       \item \code{filetest}:  needed for \code{\link{tdmReadData2}}  
#'       \item \code{TST.testFrac}:  [0.1] set this fraction of the daa aside for testing 
#'       \item \code{TST.COL}:   string with name for the partitioning column, if tdm$umode is not "SP_T".
#'                      (If tdm$umode=="SP_T", then TST.COL="tdmSplit" is used.)
#'     }
#' @param tdm a list from  which we need here the elements 
#'     \itemize{
#'       \item \code{mainFile}:  if not NULL, set working dir to \code{dir(mainFile)} before executing  \code{\link{tdmReadData2}} 
#'       \item \code{umode}:  [ "RSUB" | "CV" | "TST" | "SP_T" ], how to divide in training/validation data for tuning
#'                      and test data for the unbiased runs  
#'       \item \code{SPLIT.SEED}:  if NULL, set random number generator (RNG) to \code{\link{tdmRandomSeed}} when constructing.
#'                      \code{dataObj}. If not NULL, set RNG to SPLIT.SEED + nExp --> deterministic test set split
#'       \item \code{stratified}: [NULL] string specifying the column with the response variable for classification.
#'                      If not NULL, do the split by stratified sampling (at least one record of each class level
#'                      found in \code{dset[,tdm$stratified]} shall appear in the train-vali-set). Recommended for classification
#'     }
#' @param nExp  [0] experiment counter, used to select a reproducible different seed, if \code{tdm$SPLIT.SEED!=NULL}
#' @param dset  [NULL] if non-NULL, reading of dset is skipped and the given data frame dset is used.
#'
#' @return \code{dataObj}, either NULL (if \code{opts$READ.INI==FALSE}) or an object of class \code{\link{TDMdata}} containing
#'      \item{dset}{ a data frame with the complete data set}
#'      \item{TST.COL}{ string, the name of the column in \code{dset} which has a 1 for 
#'                      records belonging to the test set and a 0 for train/vali records. If tdm$umode=="SP_T", then 
#'                      TST.COL="tdmSplit", else TST.COL=opts$TST.COL. }
#'      \item{filename}{ \code{opts$filename}, from where the data were read}
#'    Use the accessor functions  \code{\link{dsetTrnVa.TDMdata}} and \code{\link{dsetTest.TDMdata}} to extract the train/vali and 
#'    the test data, resp., from \code{dataObj}.
#'   
#'    Known caller: \code{\link{tdmBigLoop}}
#'
#' @seealso   \code{\link{dsetTrnVa.TDMdata}}, \code{\link{dsetTest.TDMdata}}, \code{\link{tdmReadData2}}, \code{\link{tdmBigLoop}}
#' @author Wolfgang Konen (\email{wolfgang.konen@@th-koeln.de}), THK
#' @aliases TDMdata 
#' @export
######################################################################################
tdmSplitTestData <- function(opts,tdm,nExp=0,dset=NULL) {
  tdm<-tdmDefaultsFill(tdm)
	if (opts$READ.INI) {
		if (is.null(dset)) {
    		oldwd = getwd();                                              #
        if (!is.null(tdm$mainFile)) setwd(dirname(tdm$mainFile));     # save & change working dir

    		#dset <- tdmReadData(opts); # OLD version (before July'2015)
    		dset <- tdmReadData2(opts);
    
    		setwd(oldwd);                   # restore working dir
    }     
	  testit::assert ("tdmReadData2 does not return a data frame. Check opts$READ.TrnFn", is.data.frame(dset));

		if (is.null(tdm$SPLIT.SEED)) {       
		  theSeed=tdmRandomSeed();
		} else {
		  theSeed=tdm$SPLIT.SEED;
		}
		
		if (tdm$umode=="SP_T") {  
		  #TST.COL = "tdmSplit";
		  #if (any(names(dset)==TST.COL)) 
		  #  warning(sprintf("Name clash in dset, which has already a column \"%s\", will be overwritten. Please consider renaming it.",TST.COL));            
      #dset[,TST.COL] = rep(0,nrow(dset));
      
      ### This is now deferred to dsetTrnVa.TDMdata and dsetTest.TDMdata
      ### (because each nExp requires a different split)
		  cvi <- splitTestTrnVa(opts,tdm,dset,theSeed,nExp); 
		  #dset[,TST.COL] = cvi;  		        # add a new column "tdmSplit" with 1 for test data, 0 else
		  
		  
		} else if (tdm$umode=="TST") {      # take the partition delivered by tdmReadData2     
        if (!any(names(dset)==opts$TST.COL)) {
          stop(sprintf("Data frame dset does not contain a column opts$TST.COL=\"%s\". \n%s",
                        opts$TST.COL,"This might be due to a missing opts$READ.TstFn when using tdm$umode==\"TST\".")); 
        }
        cvi=dset[,opts$TST.COL];
		} else {  # i.e. if tdm$umode=="RSUB" or =="CV"
		    cvi = rep(0,nrow(dset));         # all data in dset used for training and validation
    		#TST.COL = opts$TST.COL;          
        #if (any(names(dset)==TST.COL)) warning(sprintf("Name clash in dset, which has already a column \"%s\", will be overwritten. Please consider renaming it.",TST.COL));            
        #dset[,TST.COL] = cvi;			       # add a new column with only 0's   --> dsetTest(dataObj) will return NULL
		}
    
		dataObj <- list(
			  dset=dset
			, cvi=cvi
			#, TST.COL=TST.COL
			, filename=opts$filename
      , theSeed=theSeed
      , opts = opts
			, tdm = tdm
			);
		class(dataObj) <- "TDMdata";

  	checkData(dataObj,nExp,opts);
	} else {   # i.e. if opts$READ.INI==FALSE
		dataObj <- NULL;
	}	# if (opts$READ.INI)	
	
	dataObj;
}

# checkData:
#     Helper fct for tdmSplitTestData:
#     Perform some plausibility checks on the data read
#
checkData <- function(dataObj,nExp,opts) {
  checkFactor <- function(set,txt,fact) {
    for (i in which(fact==TRUE)) {
      if (nlevels(set[,i])>32) {
        strng = sprintf("Column %s of %s has %d levels. Consider to use tdmPreGroupLevels() or as.numeric().",names(dset)[i], txt,nlevels(set[,i]));
        cat("NOTE:",strng);
        warning(strng); 
      }
    }
  }
  dset <- dsetTrnVa(dataObj,nExp);
  tset <- dsetTest(dataObj,nExp);
  testit::assert ("dsetTrnVa does not return a data frame. Check opts$READ.TrnFn", is.data.frame(dset));
  testit::assert ("dsetTest does not return a data frame. Check opts$READ.TrnFn", 
                  if (!is.null(tset)) {is.data.frame(tset)} else {TRUE});
  
  dfactor <- sapply(dset,is.factor);
  tfactor <- sapply(tset,is.factor);
  if (any(dfactor!=tfactor)) {
    w = which(dfactor!=tfactor);
    strng = paste("dataObj has columns with different mode in train-vali- and test part:",sprintf("%s,",names(dset)[w]));
    cat("NOTE:",strng);
    warning(strng);
  }
  checkFactor(dset,"train-validation set",dfactor);
  checkFactor(tset,"test set",tfactor);

  if (!is.null(tset)) {
    # check whether the first 100 rows of dset and tset are equal. If so, issue a warning 
    # that both data sets may be identical (usually due to an error in the read functions)
    firstRows <- min(100,nrow(dset),nrow(tset))
    activeCols <- setdiff(names(dset),opts$TST.COL)
    if (all(dset[1:firstRows,activeCols]==tset[1:firstRows,activeCols])) {
      warning(sprintf("Data sets dset and tset might be identical, since the first %d rows %s"
                      ,firstRows,"are identical. \n  Check the reading functions opts$READ.TrnFn and opts$READ.TstFn."))
    }
  }
}

# splitTestTrnVa
#     Helper fct for tdmSplitTestData, dsetTrnVa.TDMdata, dsetTest.TDMdata:
#
splitTestTrnVa <- function(opts,tdm,dset,theSeed,nExp) {
  if (exists(".Random.seed")) SAVESEED<-.Random.seed     #save the Random Number Generator RNG status
  if (is.null(tdm$TST.testFrac)) stop("tdm$TST.testFrac is NULL. Consider using 'tdm <- tdmDefaultsFill(tdm);'")
  
  set.seed(theSeed+nExp); # if you want reproducably the same training/test sets,
                          # but different for each experiment nExp  
  
  L = nrow(dset);
  if (is.null(L)) stop("No data");    		if (L==0) stop("Empty data frame");
  
  if (!is.null(tdm$stratified)) {
    # the division is done by ***stratified*** random sampling (recommended for classification):
    cat1(opts,opts$filename,": Stratified random test-trainVali-index w.r.t. variable",tdm$stratified
         ,"and with tdm$TST.testFrac = ",tdm$TST.testFrac*100,"%\n");
    if (!any(names(dset)==tdm$stratified)) stop("The value of tdm$stratified does not match any column name in dset!");
    rv <- dset[,tdm$stratified];
    #lrv = length(response.variables);
    #if (lrv>1) warning(sprintf("Stratified sampling is only done w.r.t. 1st response variable. It is not guaranteed to work for all %d response variables",lrv))  
    urv <- unique(rv);    # the class levels in rv
    # calculate tfr, the number of training set records for each level of variable tdm$stratified
    tfr <- sapply(urv,function(x) { round((1-tdm$TST.testFrac)*length(which(rv==x))) });
    # (this seems complicated, but the simpler command: tfr <- round((1-opts$TST.valiFrac)*table(rv));
    # does not work, because 'table' orders the levels alphabetically but 'unique'  requires them in the 
    # order they appear in column rv.) 
    #
    tfr[tfr<1] <- 1;      # ensure that there is at least one record for each class level in the trainVali-set
    cvi <- rep(1,L);
    for (i in 1:length(urv))  cvi[ sample(which(rv==urv[i]), tfr[i]) ] <- 0;
  } else {  # i.e. tdm$stratified is NULL
    # simple random sampling (recommended for regression):
    p <- sample(L)                  # random permutation of indices 1:L  
    # calculate tfr, the record where the test set starts (TST.testFrac)
    tfr <- (1-tdm$TST.testFrac)*L;
    cat1(opts,opts$filename,": Setting data aside for testing with tdm$TST.testFrac = ",tdm$TST.testFrac*100,"%\n");
    cvi <- rep(0,L);
    cvi[p[(tfr+1):L]] <- 1;          # test set index ( TST.testFrac  percent of the data)
  }
  
  cat1(opts,"*** from tdmSplitTestData: ***  \n");
  wI <- which(cvi==1);
  cat1(opts,"dset contains",L,"records; we put ",length(wI),"records aside into test set\n")
  if (length(wI)<300) print1(opts,wI);
  
  if (exists("SAVESEED")) assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
  return(cvi)
}

######################################################################################
# dsetTrnVa
#
#' Return train-validataion data
#' 
#' Return train-validataion data for an object.
#' See \code{\link{dsetTrnVa.TDMdata}} for details.
#'
#' @param x  return value from a prior call to \code{\link{tdmSplitTestData}}, an object of class \code{\link{TDMdata}}.
#' @param ... may contain nExp, experiment number
#' @return \code{dset}, a data frame with all train-validation records
#' 
#' @seealso   \code{\link{dsetTrnVa.TDMdata}}   \code{\link{dsetTest.TDMdata}}
#' @export
#' @keywords internal
dsetTrnVa <- function(x,...)  UseMethod("dsetTrnVa");
dsetTrnVa.default <- function(x,...)  stop("Method dsetTrnVa only allowed for objects of class TDMdata. Consider opts$READ.INI=TRUE");
######################################################################################
# dsetTrnVa.TDMdata
#
#' Return train-validation data of \code{\link{TDMdata}} object
#'
#' Return the train-validation part of a \code{\link{TDMdata}} object containing the task data.
#'
#' @method dsetTrnVa TDMdata
#' @param x   return value from a prior call to \code{\link{tdmSplitTestData}}, an object of class \code{\link{TDMdata}}.
#' @param ... may contain nExp, experiment number, needed only if \code{x$tdm$umode=="SP_T"}: 
#'            add nExp to seed when randomly splitting in train and test data [default: nExp=0]
#' @return \code{dset}, a data frame with all train-validation records
#'
#' @seealso   \code{\link{dsetTest.TDMdata}} \code{\link{tdmSplitTestData}}
#' @export
#' @author Wolfgang Konen, THK
dsetTrnVa.TDMdata <- function(x,...)  {
  #if (!any(names(x$dset)==x$TST.COL)) {
  #  stop(sprintf("Data frame dset does not contain a column x$TST.COL=\"%s\". \n%s",
  #               x$TST.COL,"Are you sure to have opts$READ.TstFn!=NULL when using tdm$umode==\"TST\"?")); 
  #}
  dots = list(...)
  if (is.null(dots$nExp)) dots$nExp=0
  if (x$tdm$umode=="SP_T") {      # do train-test-split anew, based on actual nExp
    x$cvi <- splitTestTrnVa(x$opts,x$tdm,x$dset,x$theSeed,dots$nExp);  
    #x$dset[,x$TST.COL] = cvi;
  }
  # return the training-validation  part of x$dset
  ind=which(x$cvi==0);
  # OLD: excluding the column x$TST.COL (="tdmSplit" usually)
  #if (x$tdm$umode == "SP_T") {
  #  x$dset[ind, -which(names(x$dset)==x$TST.COL) ];	
  #} else {
    x$dset[ind,];
  #}
  
}

######################################################################################
# dsetTest
#
#' Return test data
#' 
#' Return test data for an object.
#' See \code{\link{dsetTest.TDMdata}} for details.
#'
#' @param x  return value from a prior call to \code{\link{tdmSplitTestData}}, an object of class \code{\link{TDMdata}}.
#' @param ... may contain nExp, experiment number
#' @return \code{dset}, a data frame with all train-validation records
#' 
#' @seealso   \code{\link{dsetTest.TDMdata}}   \code{\link{dsetTrnVa.TDMdata}}
#' @export
#' @keywords internal
dsetTest <- function(x,...)  UseMethod("dsetTest");
dsetTest.default <- function(x,...)  stop("Method dsetTest only allowed for objects of class TDMdata");
######################################################################################
# dsetTest.TDMdata
#
#' Return test data of \code{\link{TDMdata}} object
#'
#' Return the test part of a \code{\link{TDMdata}} object containing the task data.
#'
#' @method dsetTest TDMdata
#' @param x   return value from a prior call to \code{\link{tdmSplitTestData}}, an object of class \code{\link{TDMdata}}.
#' @param ... may contain nExp, experiment number, needed only if \code{x$tdm$umode=="SP_T"}: 
#'            add nExp to seed when randomly splitting in train and test data [default: nExp=0]
#' @return \code{tset}, a data frame with all test records. If there are 0 test records, return NULL.
#'
#' @seealso   \code{\link{unbiasedRun}} \code{\link{dsetTrnVa.TDMdata}} \code{\link{tdmSplitTestData}}
#' @export
#' @author Wolfgang Konen, THK
dsetTest.TDMdata <- function(x,...)  {
  #if (!any(names(x$dset)==x$TST.COL)) {
  #  stop(sprintf("Data frame dset does not contain a column x$TST.COL=\"%s\". \n%s",
  #               x$TST.COL,"Are you sure to have opts$READ.TstFn!=NULL when using tdm$umode==\"TST\"?")); 
  #}
  dots = list(...)
  if (is.null(dots$nExp)) dots$nExp=0
  if (x$tdm$umode=="SP_T") {      # do train-test-split anew, based on actual nExp
    x$cvi <- splitTestTrnVa(x$opts,x$tdm,x$dset,x$theSeed,dots$nExp);
    #x$dset[,x$TST.COL] = cvi;
  }
  ind = which(x$cvi>0);
  #ind = which(x$dset[,x$TST.COL]==1);
  if (length(ind)==0) {
    NULL;
  } else {
    # return the test part of data frame dset
    # OLD: excluding the column x$TST.COL (="tdmSplit" usually)
    #if (x$tdm$umode == "SP_T") {
    #  x$dset[ind, -which(names(x$dset)==x$TST.COL) ];  
    #} else {
      x$dset[ind,];
    #}
  }
}

######################################################################################
# print.TDMdata
#
#' Print an overview for a \code{\link{TDMdata}} object.
#'
#' Print number of rows and number of columns of the data frame \code{dset} contained in the \code{\link{TDMdata}} object.
#'
#' @method print TDMdata
#' @param x  return value from a prior call to \code{\link{tdmSplitTestData}}, an object of class \code{\link{TDMdata}}.
#' @param ... currently not used
#'
#' @seealso   \code{\link{tdmSplitTestData}}
#' @author Wolfgang Konen, FHK
#' @export
print.TDMdata <- function(x,...) {
  nTrn = length(which(x$cvi==0))
  nTst = length(which(x$cvi==1))
  cat(sprintf("TDMdata object with %d records (%d test, %d train-vali records).\n",nrow(x$dset),nTst,nTrn));
  cat(sprintf("TDMdata object with %d variables:\n",length(x$dset)));
  print(names(x$dset));
}