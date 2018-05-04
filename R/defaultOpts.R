#defaultOpts.R
#' 
#' Default settings for the data mining part of TDMR (list \code{opts}).
#' 
#' Sets suitable defaults for the data mining part of TDMR. 
#' 
#' With the call \code{\link{setParams}(myOpts,defaultOpts())} it is possible to extend a partial list 
#' \code{myOpts} to a list containing all \code{opts}-elements (the missing ones are taken from 
#' \code{defaultOpts()}).  If \code{myOpts} has an element not present in \code{defaultOpts()}, 
#' this element is not taken and a warning is issued. \cr
#' With \code{\link{setParams}(myOpts,defaultOpts(),keepNotMatching=TRUE)} also elements
#' of \code{myOpts} not present in \code{defaultOpts()} are taken (no warnings).
#'
#' @return a list with the elements according to \code{\link{tdmOptsDefaultsSet}}
#'
#' @seealso   \code{\link{setParams}}, \code{\link{defaultSC}}
#' @author Wolfgang Konen (\email{wolfgang.konen@@th-koeln.de}), Samineh Bagheri, THK, 2018
#' @export
#'  
defaultOpts <- function() {
  # --- keep it a flat list (no other list as members), so that the user can set 
  # --- individual members before calling setParams(mySC,defaultSC())
  tdmOptsDefaultsSet();
}