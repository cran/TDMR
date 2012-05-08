######################################################################################
######################################################################################
#
# GRAPHICS DEVICE FUNCTIONS
#
######################################################################################
#' Initialize graphics and log file. 
#'
#' The log file is opened in \code{opts$dir.output/opts$LOGFILE}.
#'
#' @param opts  with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]
#' @return \code{gdObj}, an object of class TDMgdev. Pass this object on when calling tdmGraAndLogFinalize(opts,gdObj)
#'         (if not, a warning is issued before the sink-closing-error occurs)
#' @export
tdmGraAndLogInitialize <- function(opts) {

    dir.output <- paste(dirname(opts$dir.output),basename(opts$dir.output),sep="/")  # remove trailing "/", if it exists
    if (!file.exists(dir.output)) dir.create(dir.output);     
    if (opts$GD.DEVICE!="non") {
      if (opts$GD.RESTART==T) graphics.off()          # close all graphics windows
      tdmGraphicInit(opts);                        
    }  
    sink(paste(dir.output,opts$LOGFILE,sep="/"),split=T,append=F);	  
    
    gdObj = list();
    class(gdObj) <- c("TDMgdev","TDM");
    gdObj;
}

#' Finalize graphics and log file
#'
#' @param opts  with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]
#' @param gdObj object of class TDMgdev, the return value from tdmGraAndLogInitialize, to ensure that tdmGraAndLogInitialize was called before 
#'              (and the sink on opts$LOGFILE can be closed)
#' @export
tdmGraAndLogFinalize <- function(opts,gdObj=NULL) {
    if (opts$GD.CLOSE & opts$GD.DEVICE!="win") tdmGraphicCloseDev(opts);
    cat1(opts,sprintf("\n%s: All done.\n\n",opts$filename));
    if (is.null(gdObj)) warning("Deprecated: No gdObj (return value from tdmGraAndLogInitialize) passed"); 
    sink();       # close the LOGFILE
}
######################################################################################
#
#' Initialize graphic device. Open multipage PDF or (create and) clear opts$GD.PNGDIR.
#'
#' @param opts  with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]
#' @param ...   optional arguments to hand over to \code{\link{pdf}} (the other devices require no further arguments) 
#' @export
tdmGraphicInit <- function(opts,...) {
  png.init <- function(opts) {
     if (!any(dir(opts$dir.output)==basename(opts$GD.PNGDIR))) dir.create(paste(opts$dir.output,opts$GD.PNGDIR,sep=""));
     d=dir(paste(opts$dir.output,opts$GD.PNGDIR,sep=""));
     if (length(d)>0)
        file.remove(paste(opts$dir.output,opts$GD.PNGDIR,"/",d,sep=""));
  }
  dir.output <- paste(dirname(opts$dir.output),basename(opts$dir.output),sep="/")  # remove trailing "/", if it exists
  if (!file.exists(dir.output)) dir.create(dir.output);     
  if (is.null(opts$GD.DEVICE)) opts$GD.DEVICE="win";
  switch (opts$GD.DEVICE
    , "pdf" = pdf(paste(opts$dir.output,opts$PDFFILE,sep=""),onefile=T,paper="a4r",...)
    , "png" = png.init(opts)
    , "win" = {}
    , "non" = {}
    , "invalid switch"
    );
}
#
#' Initialize a new window. 
#' 
#' Initialize a new window ("win") / a new file ("png") for current graphic device.
#'
#' @param opts  with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]
#' @param ...   optional arguments to hand over to \code{\link{png}} or \code{windows} or \code{X11} in 
#'              package grDevices (the other devices require no further arguments)
#' @export
tdmGraphicNewWin <- function(opts,...) {
  png.newFile <- function(opts,...) {
    pngdir <- paste(opts$dir.output,opts$GD.PNGDIR,"/",sep="");
    pngfile = "000.png"
    d <- dir(pngdir)
    if (length(d)>0)
      pngfile <- paste(sprintf("%03d",max(as.numeric(sub(".png","",d))+1)),".png",sep="");
    png(paste(pngdir,pngfile,sep=""), width=730, height=730, ...)
  }
  if (is.null(opts$GD.DEVICE)) opts$GD.DEVICE="win";
  switch (opts$GD.DEVICE
    , "pdf" = {}
    , "png" = png.newFile(opts,...)
    , "win" = dev.new(...)   # former:   ifelse(.Platform$OS.type=="windows", {windows(...);1}, {X11(...);1})
    , "non" = {}
    , "invalid switch"
    );    
    # note for switch "win": X11(), which worked fine up to R2.12 leads now to strange warnings on my Win-XP machine
    # when plotting anything on this device. 
    # Therefore we now use windows(...) on all Windows platforms, X11(...) for other OS (e.g. Unix, Linux).
    # Or, simpler, we use dev.new(...), which works allways
}
#
#
#
tdmGraphicToTop <- function(opts,...) {
  if (is.null(opts$GD.DEVICE)) opts$GD.DEVICE="win";
  switch (opts$GD.DEVICE
    , "pdf" = {}
    , "png" = {}
    , "win" = {bringToTop();}
    , "non" = {}
    , "invalid switch"
    );
}
#
#' Close active file ("png").
#'
#' @param opts  with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]
#' @param ...   optional arguments (currently not used)
#' @export
tdmGraphicCloseWin <- function(opts,...) {
  if (is.null(opts$GD.DEVICE)) opts$GD.DEVICE="win";
  switch (opts$GD.DEVICE
    , "pdf" = {}
    , "png" = dev.off()
    , "win" = {}
    , "non" = {}
    , "invalid switch"
    );
}
#
#' Close all open graphic devices.
#'
#' @param opts  with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]
#' @param ...   optional arguments (currently not used)
#' @export
tdmGraphicCloseDev <- function(opts,...) {
  pdf.dev.off <- function() {
    pdfdev = dev.list()[names(dev.list())=="pdf"]
    if (length(pdfdev)>0) for (i in 1:length(pdfdev)) dev.off(which=pdfdev[i])
  }
  png.dev.off <- function() {
    pngdev = dev.list()[grep("png",names(dev.list()))]
    if (length(pngdev)>0) for (i in 1:length(pngdev)) dev.off(which=pngdev[i])
  }
  if (is.null(opts$GD.DEVICE)) opts$GD.DEVICE="win";
  switch (opts$GD.DEVICE
    , "pdf" = pdf.dev.off() # close all pdf devices
    , "png" = png.dev.off() # close all png devices ('dev.off()[grep("png",names(dev.list())]' has problems, if no png device is open)
    , "win" = graphics.off()# close all devices 
    , "non" = {}
    , "invalid switch"
    );
}
