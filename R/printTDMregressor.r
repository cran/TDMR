######################################################################################
# print.tdmRegre
#
#'   Print an overview for a \code{tdmRegre} object.
#'
#'   @method print tdmRegre
#'   @param x  return value from a prior call to \code{\link{tdmRegress}}, an object of class \code{tdmRegre}.
#'   @param ... e.g. 'type'    which information to print:
#'      \describe{
#'      \item{\code{"overview"}}{ (def.) RMAE on training/test set, number of records, see \code{\link{tdmRegressSummary}}}
#'      \item{\code{"..."}}{ ... other choices, TODO ...}
#'      \item{\code{"?"}}{ help on this method}
#'      }
#'
#' @seealso   \code{\link{tdmRegress}}, \code{\link{print.TDMregressor}}
#' @author Wolfgang Konen, FHK, Oct'2011 - Dec'2011
#' @export
######################################################################################
print.tdmRegre <- function(x,...) {
  internalPrintC <- function(res,type) {
    opts = res$opts;
    opts$VERBOSE = 2;
    z <- switch(type
      , "overview"= {show.tdmRegre(res);
              cat("\nUse print(res,type=\"?\") for more info on tdmRegre object res.\n");
              1;    # a value for z
              }
      , "?"={cat("Help for print(<tdmRegre>,type=t). Possible values for 't' are:\n"
               ,"\"overview\": [def.] info on model and datasets in tdmRegre object\n"
               ,"\"?\" : display this help message"
               ,"\n\nThe commands > res or > print(res) invoke the default print(res,type=\"overview\") for tdmRegre object res\n"
               ); 1;   # a value for z
            }
      , "invalid type"
      );
    if (z[1]=="invalid type") warning("Invalid type = ",type,". Allowed types are: overview, ?.");
  }

  vaarg <- list(...)
  if (is.null(vaarg$type)) vaarg$type="overview";
  internalPrintC(x,vaarg$type);
}

show.tdmRegre <- function(res) {
    opts = res$opts;
    cat("Model (for last response variable) of tdmRegre object:")
    print(res$lastModel);
    cat(sprintf("\nDatasets (# rows) of tdmRegre object:\n  d_train (%d), d_test (%d)\n",
                      nrow(res$d_train),nrow(res$d_test)));
}

######################################################################################
# print.TDMregressor
#
#'   Print an overview for a \code{TDMregressor} object.
#'
#'   @method print TDMregressor
#'   @param x  return value from a prior call to \code{\link{tdmRegressLoop}}, an object of class \code{TDMregressor}.
#'   @param ... e.g. 'type'    which information to print:
#'      \describe{
#'      \item{\code{"overview"}}{ (def.) RMAE on training/test set, number of records, see \code{\link{tdmRegressSummary}}}
#'      \item{\code{"..."}}{ ... other choices, TODO ...}
#'      }
#' @seealso   \code{\link{tdmRegressLoop}}, \code{\link{print.tdmRegre}}
#' @export
######################################################################################
print.TDMregressor <- function(x,...) {
  internalPrintR <- function(result,type) {
    opts = result$lastRes$opts;
    opts$VERBOSE = 2;
    z <- switch(type
      , "overview"= { tdmRegressSummary(result,opts);
              cat("\nUse print(result,type=\"?\") for more info on TDMregressor object result."); 1;
              }
      , "?"={cat("Help for print(<TDMregressor>,type=t). Possible values for 't' are:\n"
               ,"\"overview\": see tdmRegressSummary\n"
               ,"\"?\" : display this help message\n"
               ); 1;}
      , "invalid type"
      );
    if (z[1]=="invalid type") warning("Invalid type = ",type,". Allowed types are: overview, ?.");
    cat("\n");
  }

  vaarg <- list(...)
  #alternative: vavalues <- c(...)

  if (is.null(vaarg$type)) vaarg$type="overview";
  internalPrintR(x,vaarg$type);
}

