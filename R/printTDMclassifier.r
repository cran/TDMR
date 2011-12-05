######################################################################################
# print.tdmClass
#
#'   Print an overview for a \code{tdmClass} object.
#'
#'   @method print tdmClass
#'   @param x  return value from a prior call to \code{\link{tdmClassify}}, an object of class \code{tdmClass}.
#'   @param ... e.g. 'type'    which information to print:
#'      \describe{
#'      \item{\code{"overview"}}{ (def.) relative gain on training/test set, number of records, see \code{\link{tdmClassifySummary}}}
#'      \item{\code{"cm.train"}}{ confusion matrix on train set}
#'      \item{\code{"cm.test"}}{ confusion matrix on test set}
#'      \item{\code{"?"}}{ help on this method}
#'      }
#'
#' @seealso   \code{\link{tdmClassify}}, \code{\link{print.TDMclassifier}}
#' @author Wolfgang Konen, FHK, Oct'2011 - Dec'2011
#' @export
######################################################################################
print.tdmClass <- function(x,...) {
  internalPrintC <- function(res,type) {
    opts = res$opts;
    opts$VERBOSE = 2;
    z <- switch(type
      , "overview"= {show.tdmClass(res);
              cat("\nUse > print(res,type=\"?\") for more info on tdmClass object res.\n");
              1;    # a value for z
              }
      , "cm.train"= show.cm.train(res)
      , "cm.test"= show.cm.test(res)
      , "?"={cat("Help for print(<tdmClass>,type=t). Possible values for 't' are:\n"
               ,"\"overview\": [def.] info on model and datasets in tdmClass object\n"
               ,"\"cm.train\": confusion matrix on training data\n"
               ,"\"cm.test\" : confusion matrix on test data\n"
               ,"\"?\" : display this help message"
               ,"\n\nThe commands > res or > print(res) invoke the default print(res,type=\"overview\") for tdmClass object res\n"
               ); 1;   # a value for z
            }
      , "invalid type"
      );
    if (z[1]=="invalid type") warning("Invalid type = ",type,". Allowed types are: overview, cm.train, cm.test, ?.");
  }

  vaarg <- list(...)
  if (is.null(vaarg$type)) vaarg$type="overview";
  internalPrintC(x,vaarg$type);
}

show.tdmClass <- function(res) {
    opts = res$opts;
    cat("Model (for last response variable) of tdmClass object:")
    print(res$lastModel);
    cat(sprintf("\nDatasets (# rows) of tdmClass object:\n  d_train (%d), d_test (%d), d_dis (%d)\n",
                      nrow(res$d_train),nrow(res$d_test),nrow(res$d_dis)));
}


######################################################################################
# print.TDMclassifier
#
#'   Print an overview for a \code{TDMclassifier} object.
#'
#'   @method print TDMclassifier
#'   @param x  return value from a prior call to \code{\link{tdmClassifyLoop}}, an object of class \code{TDMclassifier}.
#'   @param ... e.g. 'type'    which information to print:
#'      \describe{
#'      \item{\code{"overview"}}{ (def.) relative gain on training/test set, number of records, see \code{\link{tdmClassifySummary}}}
#'      \item{\code{"cm.train"}}{ confusion matrix on train set}
#'      \item{\code{"cm.test"}}{ confusion matrix on test set}
#'      \item{\code{"?"}}{ help on this method}
#'      }
#' @seealso   \code{\link{tdmClassifyLoop}}, \code{\link{print.tdmClass}}
#' @export
######################################################################################
print.TDMclassifier <- function(x,...) {
  internalPrintC <- function(result,type) {
    opts = result$lastRes$opts;
    opts$VERBOSE = 2;
    z <- switch(type
      , "overview"= {cat("Filename of task:",opts$filename);
              tdmClassifySummary(result,opts);
              cat("\nUse > print(result,type=\"?\") or > result$lastRes   for more info on TDMclassifier object result."); 1;
              }
      , "cm.train"= show.cm.train(result)
      , "cm.test"= show.cm.test(result)
      , "?"={cat("Help for print(<TDMclassifier>,type=t). Possible values for 't' are:\n"
               ,"\"overview\": [def.] see help(tdmClassifySummary)\n"
               ,"\"cm.train\": confusion matrix on training data\n"
               ,"\"cm.test\" : confusion matrix on test data\n"
               ,"\"?\" : display this help message"
               ,"\nThe commands > result or > print(result) invoke the default print(result,type=\"overview\") for TDMclassifier object result"
               ); 1;}
      , "invalid type"
      );
    if (z[1]=="invalid type") warning("Invalid type = ",type,". Allowed types are: overview, cm.train, cm.test, ?.");
    cat("\n");
  }

  vaarg <- list(...)
  #alternatively:
  # vavalues <- c(...)

  if (is.null(vaarg$type)) vaarg$type="overview";
  internalPrintC(x,vaarg$type);
}

# this helper fct works for both classes tdmClass and TDMClassifier in object result:
show.cm.train <- function(result) {
  cls <- class(result)[1];
  z <- switch(cls
    , "TDMclassifier"= {show.cm.train(result$lastRes); 1; }
    , "tdmClass"={
        opts = result$lastRes$opts;
        opts$VERBOSE = 2;
        cm.train <- result$lastCmTrain;
        cat1(opts,"Training cases (",nrow(result$d_train),"):\n")
        if (opts$VERBOSE>=1) print(cm.train$mat)                     # confusion matrix on training set
        print1(opts,cm.train$gain.vector)
        cat1(opts,sprintf("total gain: %7.1f (is %7.3f%% of max. gain = %7.1f)\n",
                          cm.train$gain,cm.train$gain/cm.train$gainmax*100,cm.train$gainmax));
        cat1(opts,sprintf("Relative gain (rgain.type='%s') is %7.2f%%\n",opts$rgain.type,cm.train$rgain));
        1;    # a value for z
       }
    , "invalid class"
    );
  if (z[1]=="invalid class") warning("Invalid class = ",cls,". Allowed classes are: TDMclassifier, tdmClass.");
  result;
}

# this helper fct works for both classes tdmClass and TDMClassifier in object result:
show.cm.test <- function(result) {
  cls <- class(result)[1];
  z <- switch(cls
    , "TDMclassifier"= {show.cm.test(result$lastRes); 1; }
    , "tdmClass"={
        opts = result$lastRes$opts;
        opts$VERBOSE = 2;
        cm.test <- result$lastCmTest;
        n.test <- nrow(result$d_test);
        if (n.test>0) {
          cat1(opts,"Test cases (",n.test,"):\n")
          print1(opts,cm.test$mat)                      # confusion matrix on test set
          print1(opts,cm.test$gain.vector)
          cat1(opts,sprintf("total gain : %7.1f (is %7.3f%% of max. gain = %7.1f)\n",
                            cm.test$gain,cm.test$gain/cm.test$gainmax*100,cm.test$gainmax));
          cat1(opts,sprintf("Relative gain (rgain.type='%s') is %7.2f%%\n",opts$rgain.type,cm.test$rgain));
        } else {
          cat1(opts,"There are no test cases in data set!\n");
        }
        1;    # a value for z
       }
    , "invalid class"
    );
  if (z[1]=="invalid class") warning("Invalid class = ",cls,". Allowed classes are: TDMclassifier, tdmClass.");
  result;
}

