# tdmMapDesLoad
#
#'    Load the mapping files. Load the map files \code{"tdmMapDesign.csv"} and optionally 
#'    also \code{"userMapDesign.csv"} and store them in \code{envT$map} and \code{envT$mapUser},
#'    resp. These maps are used by \code{\link{tdmMapDesApply}}.
#'
#'    \code{"tdmMapDesign.csv"} is searched in the TDMR library path \code{.find.package("TDMR")}.
#'    (For the developer version: \code{<tdm$tdmPath>/inst}).
#'    \code{"userMapDesign.csv"} is searched in the path \code{dirname(tdm$mainFile)}.
#' @param envT  environment
#' @param tdm   list 
#' @export
tdmMapDesLoad <- function(envT,tdm) {
      if (is.null(tdm$tdmPath)) {
        mapPath <- .find.package("TDMR");   # this is for the package version
      } else {
        mapPath <- paste(tdm$tdmPath,"inst",sep="/");
      }
      tdmMapFile=paste(mapPath,"tdmMapDesign.csv",sep="/")
      if (!file.exists(tdmMapFile)) stop(sprintf("Could not find map file %s",tdmMapFile));
      envT$map <- read.table(tdmMapFile,sep=";",header=T)
      userMapFile=paste(dirname(tdm$mainFile),"userMapDesign.csv",sep="/");
      if (file.exists(userMapFile)) envT$mapUser <- read.table(userMapFile,sep=";",header=T)
}    

# tdmMapDesApply
#
#'    Apply the mapping from \code{des} to \code{opts}.     
#'    For each variable which appears in .roi (and thus in .des file): set its counterpart in list \code{opts}.
#'    For each variable not appearing: leave its counterpart in \code{opts} at its default value from .apd file.
#'
#' @param des   design points data frame
#' @param opts  list of options
#' @param k     apply mapping for the \code{k}th design point
#' @param envT  environment
#' @param tdm   list
#' @return opts modified list of options
#' @export
tdmMapDesApply <- function(des,opts,k,envT,tdm) {
    cRound <- function(n,map,x) {
      x <- ifelse(map$isInt[n]==1,round(x),x);
      x; 
    }
    setMapValue <- function(n,map,des,opts,k) {
    	cmd = paste("if(!is.null(des$",map$roiValue[n],")) ",map$optsValue[n],"=cRound(n,map,des$",map$roiValue[n],"[k])",sep="");
      # (The check "if (!is.null...)" assures that tdmMapDes will work for all different .roi files.)
    	eval(parse(text=cmd));
    	opts;
    }
    
    for (n in 1:nrow(envT$map)) {
      opts <- setMapValue(n,envT$map,des,opts,k);
    }
    if (!is.null(envT$mapUser)) {
      for (n in 1:nrow(envT$mapUser)) {
        opts <- setMapValue(n,envT$mapUser,des,opts,k);
      }
    }
    if (!is.null(opts$CLASSWT) && is.na(opts$CLASSWT[1])) opts$CLASSWT[1]=10;
    
    opts;
}

######################################################################################
# makeTdmMapDesSpot is a helper fct for tdmMapDesSpot:
#     function to make tdmMapDesSpot, which contains two functions load and apply (see below)
#     This is necessary to have private storage (map, mapUser) which allows to  read the 
#     csv files at one point (e.g. before parallel execution) and map the elements later
#     (e.g. on the individual parallel cores) 
#
# Author: Wolfgang Konen, FHK, Sep'2010 - Nov'2010
#
makeTdmMapDesSpot <- function() {
    # For each variable which appears in .roi (and thus in .des file): set its counterpart in list opts.
    # For each variable not appearing: leave its counterpart in opts at its default value from .apd file.
    # (The check "if (!is.null...)" assures that tdmMapDes will work for all different .roi files.)
    #
    map = NULL;
    mapUser = NULL; 
    loadFunc <- function(tdm) {
      if (is.null(tdm$tdmPath)) {
        mapPath <- .find.package("TDMR");   # this is for the package version
      } else {
        mapPath <- paste(tdm$tdmPath,"inst",sep="/");
      }
      tdmMapFile=paste(mapPath,"tdmMapDesign.csv",sep="/")
      if (!file.exists(tdmMapFile)) stop(sprintf("Could not find map file %s",tdmMapFile));
      map <<- read.table(tdmMapFile,sep=";",header=T)
      userMapFile=paste(dirname(tdm$mainFile),"userMapDesign.csv",sep="/");
      mapUser <<- NULL;
      if (file.exists(userMapFile)) mapUser <<- read.table(userMapFile,sep=";",header=T)
    }    
    
    applyFunc <- function(des,opts,k,tdm) {
      cRound <- function(n,map,x) {
        x <- ifelse(map$isInt[n]==1,round(x),x);
        x; 
      }
      setMapValue <- function(n,map,des,opts,k) {
      	cmd = paste("if(!is.null(des$",map$roiValue[n],")) ",map$optsValue[n],"=cRound(n,map,des$",map$roiValue[n],"[k])",sep="");
      	eval(parse(text=cmd));
      	opts;
      }
      
      for (n in 1:nrow(map)) {
        opts <- setMapValue(n,map,des,opts,k);
      }
      if (!is.null(mapUser)) {
        for (n in 1:nrow(mapUser)) {
          opts <- setMapValue(n,mapUser,des,opts,k);
        }
      }
      if (!is.null(opts$CLASSWT) && is.na(opts$CLASSWT[1])) opts$CLASSWT[1]=10;
      
      opts;
    }
    list(load=loadFunc, apply=applyFunc);
}

######################################################################################
# tdmMapDesSpot:
#'   Helper fct for \code{\link{tdmStartSpot}}.
#'   Map the parameters from \code{des} to \code{opts} for the \code{k}th line of \code{des}.
#'
#' @export
tdmMapDesSpot <- makeTdmMapDesSpot();

######################################################################################
tdmMapDesInt <- function(des,printSummary=T,spotConfig=NULL)
{
	# the following is a fix for the fact that the current SPOT version does not
	# generate INTs but FLOATs for each INT in the .roi file.
	# We hard-code a round() for certain names we know that they only can be INT:
	if (!is.null(des$MTRY)) des$MTRY <- round(des$MTRY);
	if (!is.null(des$NTREE)) des$NTREE <- round(des$NTREE);
	if (!is.null(des$NODESIZE)) des$NODESIZE <- round(des$NODESIZE);
	if (!is.null(des$SAMPSIZE)) des$SAMPSIZE <- round(des$SAMPSIZE);
  if (!is.null(des$PRE_NPC)) des$PRE_NPC = round(des$PRE_NPC);
  #
  # --- TODO: infer the INT columns automatic from spotConfig's ROI
  # --- (when the interface to tdmStart* is clear everywhere)

	if (printSummary) print(summary(des));
	des;
}

######################################################################################
# tdmMapOpts:
#   helper fct for unbiasedBestRun_*.R, map certain parameters of opts for the unbiased
#   runs, depending on parameter umode, which controls the mode of the unbiased run
# INPUT
#   umode       # one of { "RSUB" | "CV" | "TST" }
#               # ="RSUB": activate random subsampling with 20% test data
#               # ="CV": activate cross validation with tdm$nfold [10] folds
#               # ="TST": activate test on unseen test data
#   opts        # current state of parameter settings
#   tdm         # list, here we use the elements
#     nfold         # [10] value for opts$TST.NFOLD during unbiased run with umode="CV"
#     nrun          # [5] value for opts$NRUN during each unbiased run
#     test2.string  # ["default.cutoff"] value for opts$test2.string during each 
#                   # unbiased run
# OUTPUT
#   opts        #  enhanced state of parameter settings
######################################################################################
tdmMapOpts <- function(umode,opts,tdm)
{
    if (is.null(tdm$test2.string)) tdm$test2.string="default cutoff";
    if (is.null(tdm$tstCol)) tdm$tstCol="TST";
    if (is.null(tdm$nfold)) tdm$nfold=10;
    if (is.null(tdm$nrun)) tdm$nrun=5;
    if (is.null(tdm$optsVerbosity)) tdm$optsVerbosity <- 0;   # the verbosity for the unbiased runs
    
    setOpts.RSUB <- function(opts) {
      opts$TST.kind <- "rand" # select test set by random subsampling, see tdmModCreateCVindex in tdmModelingUtils.r
      opts$TST.FRAC = 0.20    # set this fraction of data aside for testing (only for TST.kind=="rand")
      opts;
    } 
    setOpts.TST <- function(opts) {
      opts$TST.kind <- "col"  # select test set from column TST.COL, see tdmModCreateCVindex in tdmModelingUtils.r
      opts$READ.TST = T       # =T: read in extra unseen test data 
                              # and fill column dset[,opts$TST.COL] accordingly (1 for test records) 
      opts$TST.COL=tdm$tstCol                              
      opts;
    } 
    setOpts.CV <- function(opts) {
      opts$TST.kind <- "cv"       # select test data by CV, see tdmModCreateCVindex in tdmModelingUtils.r
      opts$TST.NFOLD = tdm$nfold  # number of CV-folds (only for TST.kind=="cv")
      opts$READ.TST = F           # =F: do not read in extra unseen test data 
      opts;
    } 
    opts <- switch(umode
      , "RSUB" = setOpts.RSUB(opts)
      , "TST" = setOpts.TST(opts)
      , "CV" = setOpts.CV(opts)
      , "INVALID"
      );
    if (opts[1]=="INVALID") 
      stop(sprintf("*** Invalid umode=%s ***\n",umode));

    opts$NRUN = tdm$nrun    # how many runs with different train & test samples  - or -
                            # how many CV-runs, if TST.kind=="cv"
    opts$test2.string=tdm$test2.string   # test2, another copy of the test set, receives a special treatment and the
                            # special treatment is either opts$test2.string = "no postproc" or = "default cutoff"
#    opts$READ.TXT = T       # =T: read data from .csv and save as .Rdata, =F: read from .Rdata

    opts$VERBOSE <- opts$SRF.verbose <- tdm$optsVerbosity;
    
    opts;
}

#--- DEPRECATED, just for compatibility with older versions unbiasedBestRun_appStorm.R, unbiasedBestRun_DMC2007.R -----------------
#
tdmMapOpts.OLD <- function(umode,opts,test2.string="no postproc",nfold=10) {
  tdm = list();
  tdm$test2.string=test2.string;
  tdm$nfold=nfold;
  opts <- tdmMapOpts(umode,opts,tdm);
}

######################################################################################
# tdmMapCutoff:
#   helper fct for tdmStart*.r in the classification task case
#   enforce parameter constraint sum(CUTOFFi)=1 if CUTOFF2,3,4 appears in .des-file.
#   The following code works up to N.c=5  [N.c = number of classes]
#
#   Bug fix 01/2011: the constraint fix is refined in such a way that all constrained will 
#   be enforced simultaneously: 
#       1) sum(cutoffs)==1
#       2) any(cutoffs) <= high ROI constraint (from spotConfig)
#       3) any(cutoffs) >= low ROI constraint (from spotConfig)
#   We assume here for simplicity that the high ROI and low ROI are the same for all cutoffs.
######################################################################################
tdmMapCutoff <- function(des,k,spotConfig) {   
    # note that nothing needs to be done here if only des$CUTOFF1 appears (two-class problem):
    # then the dependent parameter opts$RF.cutoff[2] = 1-opts$RF.cutoff[1] is set in tdmModAdjustCutoff
    #
  	if (!is.null(des$CUTOFF2)) {        # enforce parameter constraint if CUTOFF1,2,3,4 appears in .des-file:
      hig=spotConfig$alg.roi["CUTOFF1","high"];
      low=spotConfig$alg.roi["CUTOFF1","low"];    
      OLD.VER=F;	 # if OLD.VER==T: restore the previous version which had the low/high ROI constraint
			             # violation bug (but gave better results on appAcid)	 
      constr=ifelse(OLD.VER,0,1);
  	  # If sum(cutoff[1:(N.c-1)])>=1, we clip cutoff[1:(N.c-1] in such a way that the sum is below 1. 
      # Why eps? - Because RF does not allow cutoff[N.c]=0, it has to be positive (see main_DMC2007.r)
      eps <- runif(1, min=0.0001, max=0.03);
      csum <- des$CUTOFF1[k]+des$CUTOFF2[k];
      if (!is.null(des$CUTOFF3)) csum <- csum + des$CUTOFF3[k];
      if (!is.null(des$CUTOFF4)) csum <- csum + des$CUTOFF4[k];
      if (csum+eps>constr) {
        # If constr==1: CUTOFF1..4 sum up to something larger than 1-eps.
        # There is no room left for CUTOFF[N.c], so we scale CUTOFF1..4 down.
        # If constr==0 (i.e. OLD.VER==T): this if-branch is always true, we scale always. 
        # This means that CUTOFF[N.c]=eps and that CUTOFF1..4 will be scaled up, if csum+eps<1; 
        # in this case the high ROI constraint can be violated.        
        des$CUTOFF1[k] <- des$CUTOFF1[k]/(csum+eps);
        des$CUTOFF2[k] <- des$CUTOFF2[k]/(csum+eps);
        if (!is.null(des$CUTOFF3)) des$CUTOFF3[k] <- des$CUTOFF3[k]/(csum+eps);
        if (!is.null(des$CUTOFF4)) des$CUTOFF4[k] <- des$CUTOFF4[k]/(csum+eps);
      }
      # CUTOFF[N.c] will be set in main_TASK.r
      # Attention: it can (and will) happen, that 1-csum>hig, if hig is the high ROI 
      # constraint for CUTOFF. In this case CUTOFF[N.c]=1-csum will be larger than hig.
      # We currently tolerate this; CUTOFF[N.c]=1-csum will still obey the constraint "<1". 
      
      cutoffs=c(des$CUTOFF1[k],des$CUTOFF2[k],des$CUTOFF3[k],des$CUTOFF4[k]);
      # cutoffs will be shorter than 4 elements if some of the des$CUTOFF*[k] are NULL
      
      if (!OLD.VER) {
        while (any(cutoffs<low)) {
          # repair violations of the low ROI constraint, if any:
          # add the missing amount delta to the lowest cutoff, subtract it from the highest
          cutoffs = c(cutoffs,1-sum(cutoffs));
          wmin = which(cutoffs==min(cutoffs));
          wmax = which(cutoffs==max(cutoffs));
          delta = low-cutoffs[wmin];
          cutoffs[wmin] = cutoffs[wmin]+delta;
          cutoffs[wmax] = cutoffs[wmax]-delta;
          cutoffs=cutoffs[1:(length(cutoffs)-1)];
        }
        if (!is.null(des$CUTOFF1[k])) des$CUTOFF1[k]=cutoffs[1];
        if (!is.null(des$CUTOFF2[k])) des$CUTOFF2[k]=cutoffs[2];
        if (!is.null(des$CUTOFF3[k])) des$CUTOFF3[k]=cutoffs[3];
        if (!is.null(des$CUTOFF4[k])) des$CUTOFF4[k]=cutoffs[4];
      }
      
      if (any(cutoffs>hig)) {
        msg=sprintf("Some cutoffs violate high ROI constraint %f (for k=%d)",hig,k);
        warning(msg);
        print(cutoffs);
      }
      if (any(cutoffs<low)) {
        msg=sprintf("Some cutoffs violate low ROI constraint %f (for k=%d)",low,k);
        warning(msg);
        print(cutoffs);
      }
    } # if (!is.null(des$CUTOFF2))
#print(c(cutoffs,1-sum(cutoffs)));
#browser()   
    des;
}
