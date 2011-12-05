######################################################################################
######################################################################################
#
# GENERAL UTILITY FUNCTIONS
#
######################################################################################
######################################################################################

######################################################################################
#' Bind the column with name \code{response.predict} and contents \code{vec} as last column
#'     to data frame d
#' @param d dataframe
#' @param response.predict name of column \code{vec}
#' @param vec the last column bound to data frame d
#' @return dataframe d with column
#' @export
######################################################################################
bind_response <- function(d,response.predict,vec)
{
    if (is.na(match(response.predict,names(d)))) {
      # bind column response.predict as last column to data frame d
      eval(parse(text=paste("d <- cbind(d, ",response.predict,"=vec)")))
    } else {
      # replace contents of existing column response.predict
      d[,response.predict] <- vec;
    }

    return(d)
}
#
# this older version does the same, but requires three copy-replacements of
# data frame d (instead of one):
bind_response_OLD <- function(d,response.predict,vec)
{
    # drop column response.predict if there, do nothing if not there
    d <- d[,setdiff(names(d),response.predict)]
    # bind column response.predict as last column to data frame d
    d <- cbind(d, prediction=vec)
    names(d)[ncol(d)] <- response.predict

    return(d)
}

######################################################################################
# printout functions for different verbosity levels
######################################################################################
#' Output the objects to \code{cat} if \code{opts$VERBOSE>=1}
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{cat}}
#' @export
cat1 <- function(opts, ...) {  if (opts$VERBOSE>=1) cat(...); }
######################################################################################
#' Output the objects to \code{cat} if \code{opts$VERBOSE>=2}
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{cat}}
#' @export
cat2 <- function(opts, ...) {  if (opts$VERBOSE>=2) cat(...); }
######################################################################################
#' Prints the objects using \code{print} if \code{opts$VERBOSE>=1}
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{print}}
#' @export
print1 <- function(opts, ...) {  if (opts$VERBOSE>=1) print(...); }
######################################################################################
#' Prints the objects using \code{print} if \code{opts$VERBOSE>=2}
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{print}}
#' @export
print2 <- function(opts, ...) {  if (opts$VERBOSE>=2) print(...); }
