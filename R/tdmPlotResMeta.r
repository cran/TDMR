######################################################################################
# tdmPlotResMeta:
#
#' Interactive plots of RES data frames and their metamodels.
#'
#' Makes interactive plots for any of the result data frames contained in \code{envT} together with fitted metamodels.
#' \code{tdmPlotResMeta} creates a \code{\link{twiddle}} interface which allows to select
#     \tabular{ll}{
#'    \itemize{
#'      \item tuner:      one of the tuners
#'      \item nExperim:   a knob to select one of the experiments in \code{envT} (only if  \code{envT$tdm$nExperim > 1})
#'      \item reportFunc: one of \{"spotReport3d", "spotReportContour"\}, see \code{\link{SPOT}} 
#'      \item modelFit:   one of \{"spotPredictGausspr", "spotPredictRandomForest", "spotPredictMlegp"\}, see \code{\link{SPOT}} 
#'     } 
#'
#'
#'   @param envT   environment with results as returned from \code{\link{tdmCompleteEval}} or as loaded from an appropriate .RData file
#'
#'
#' @note Side Effects:
#'   One or several RGL plot windows are created which can be manipulated interactively. A certain RGL window \code{n} can be selected with 
#'   \code{rgl.set(n)}. An interactively manipulated RGL window can be saved with \code{rgl.snapshot("myplot.png")}.
#' 
#' @examples
#'    ##
#'    ## Read previous tuning results 'envT' from demo02sonar/demoSonar.RData (relative to the TDMR package directory). 
#'    ## Then, dmPlotResMeta lets you explore interactively the RES data frame(s):
#'    \dontrun{ 
#'      load(paste(.find.package("TDMR"), "demo02sonar","demoSonar.RData",sep="/"));
#'      tdmPlotResMeta(envT);
#'    }
#'
#' @seealso   \code{\link{tdmCompleteEval}}
#' @author Wolfgang Konen
#' @export
######################################################################################
tdmPlotResMeta <- function(envT) {
  require(twiddler)
#  reportFunc = "spotReport3d" # "spotReportContour" #"spotReportSens" #
#  modelFit = "spotPredictGausspr" # "spotPredictRandomForest"

  # getInd: private helper fct for picking the right element from envT$resGrid
  getInd <- function(confFile,nExp,theTuner) {
    nTuner = length(envT$tdm$tuneMethod);
    indTuner = which(envT$tdm$tuneMethod==theTuner);
    if (length(indTuner)==0) stop(paste("Could not find tuner ",theTuner,"in envT$tdm$tuneMethod"));
    nConf = which(envT$runList==confFile);
    if (length(nConf)==0) stop(paste("Could not find conf file ",confFile,"in envT$runList"));
    if (nExp<1 | nExp>envT$tdm$nExperim) stop(paste("nExp is not in range {1,...,",envT$tdm$nExperim,"}",sep=""));
    ind = indTuner + nTuner*((nExp-1) + envT$tdm$nExperim*(nConf-1));
  }
  showMeta <- function(theTuner,nExp,confFile,reportFunc,modelFit,nSkip,y_10Exp,xAxis,yAxis) {
        res = envT$resGrid[[getInd(confFile,nExp,theTuner)]];        
    
        # TODO: add function to skip incomplete CONFIGs
    
        if (nSkip>0) {
          ind=order(res$Y);
          nrw=nrow(res);
          res=res[ind[1:(nrw-nSkip)],];
        }

        sC <- envT$tunerVal;
        names(sC$alg.roi) <- c("lower","upper","type");
        sC$alg.currentResult = res;  
        sC$spot.fileMode = FALSE;
        sC$seq.predictionModel.func = modelFit; # override the confFile setting
        sC$report.func <- reportFunc;           # override the confFile setting
        sC$seq.modelFit <- NULL;          # NEW: needed to force spot to use sC$seq.predictionModel.func
        
        sC$report.main <- paste(theTuner,", nExp=",as.character(nExp),sep="");
        if (nrow(sC$alg.roi)==2) {   # only 2 design variables
          sC$report.aIndex=1; sC$report.bIndex=2; 
          sC$report.interactive=FALSE;
        }
        if (xAxis!="<none>" | yAxis!="<none>") {
          vars = rownames(sC$alg.roi);
          sC$report.aIndex=which(vars==xAxis); sC$report.bIndex=which(vars==yAxis); 
          sC$report.interactive=FALSE;
        }
        # if there are more than 2 design variables or if xAxis and yAxis have values different from "<none>", 
        # then SPOT will open a 2nd twiddle-interface to select two of the design variables

        spotConfFile="NULL";  # "NULL" means: don't read any file, take all values from envT$spotConfig
        sC$alg.currentResult[1] = sC$alg.currentResult[1]*10^y_10Exp;
        sC <- spot(spotConfFile,"rep",NA,sC);
        return(sC);
  }
  
  buildTwidCmd <- function(envT) {
    vars = rownames(envT$tunerVal$alg.roi);
    tve = length(vars);
    tne = envT$tdm$nExperim;
    tce = length(envT$runList);
    res = envT$resGrid[[1]];
    twiddleCmd <- paste("twiddle(showMeta(tuner,nExp",sep="");
    if (tne==1)   twiddleCmd <- paste(twiddleCmd,"=1",sep="");
    twiddleCmd <- paste(twiddleCmd,",confFile",sep="");
    if (tce==1)   twiddleCmd <- paste(twiddleCmd,"=envT$runList[1]",sep="");
    twiddleCmd <- paste(twiddleCmd,",reportFunc,modelFit,nSkip,y_10Exp,xAxis",sep="");
    if (tve==2)   twiddleCmd <- paste(twiddleCmd,"=vars[1]",sep="");
    twiddleCmd <- paste(twiddleCmd,",yAxis",sep="");
    if (tve==2)   twiddleCmd <- paste(twiddleCmd,"=vars[2]",sep="");
    twiddleCmd <- paste(twiddleCmd,"), eval=FALSE",sep="");
                  # eval=FALSE triggers two buttons "EVAL" and "CLOSE"  and inhibits auto-evaluation in twiddle
    tm =  envT$tdm$tuneMethod;
    if (length(tm)>1) {
      twiddleCmd <- paste(twiddleCmd,", tuner=combo(\"",tm[1],"\"",sep="");
      for (i in 2:length(tm)) twiddleCmd <- paste(twiddleCmd,",\"",tm[i],"\"",sep="");
      twiddleCmd <- paste(twiddleCmd,",label=as.character(\"tuner\"))",sep="");
      # the lines above construct a command which looks e.g. like
      #       twiddle(showMeta(tuner), eval=FALSE, tuner=combo("spot","lhd",label=as.character("tuner")))   
      # if tm = c("spot","lhd"). We need to do it this (complicated) way, including eval(parse(...))
      # below, to get the right text strings into the combo boxes.
      # --- It would be nicer, if we could just say ' twiddle(..., tuner=combo(tm,label="tuner"),...)
      # --- if tm is a string vector like c("spot","lhd")
    } else { # i.e. only one tuneMethod
      twiddleCmd <- paste(twiddleCmd,", tuner=combo(\"",tm[1],"\",\"",tm[1],"\",label=as.character(\"tuner\"))",sep="");
      # A bit awkward, but combo needs at least two entries --> we put twice the first and only entry into the combo box.
      # It does also NOT work to set 'tuner <<- "spot"', because then a knob-interface is created 
      # (unclear why, it must have to do s.th. with "spot" being of type string, because a setting 'tuner <<- 2' would 
      # work in twiddler, but is of course meaningless in the context of TDMR)
    }
    if (tne>1) {
      twiddleCmd <- paste(twiddleCmd,", nExp=knob(c(1,",tne,"), res=1, label=\"nExper\")",sep="");
    } 
    
    if (tce>1) {
      twiddleCmd <- paste(twiddleCmd,", confFile=combo(\"",envT$runList[1],"\"",sep="");
      for (i in 2:tce) twiddleCmd <- paste(twiddleCmd,",\"",envT$runList[i],"\"",sep="");
      twiddleCmd <- paste(twiddleCmd,",label=as.character(\"confFile\"))",sep="");
    } 
    
    # if there are more than 2 design variables, build two combo boxes to select two of them: 
    if (tve>2) {
      twiddleCmd <- paste(twiddleCmd,", xAxis=combo(\"<none>\"",sep="");
      for (i in 1:length(vars)) twiddleCmd <- paste(twiddleCmd,",\"",vars[i],"\"",sep="");
      twiddleCmd <- paste(twiddleCmd,",label=as.character(\"xAxis\"))",sep="");
      twiddleCmd <- paste(twiddleCmd,", yAxis=combo(\"<none>\"",sep="");
      for (i in 1:length(vars)) twiddleCmd <- paste(twiddleCmd,",\"",vars[i],"\"",sep="");
      twiddleCmd <- paste(twiddleCmd,",label=as.character(\"yAxis\"))",sep="");
    } else {
      if (length(vars)<2) stop("Need at least two design variables!");
    }
      
    twiddleCmd <- paste(twiddleCmd,", reportFunc = combo(\"spotReport3d\",\"spotReportContour\",label=\"reportFunc\")",sep="");
    twiddleCmd <- paste(twiddleCmd,", modelFit = combo(\"spotPredictGausspr\",\"spotPredictRandomForest\",\"spotPredictMlegp\",label=\"modelFit\")",sep="");
    twiddleCmd <- paste(twiddleCmd,", nSkip=knob(c(0,",min(10,nrow(res)),"), res=1, label=\"nSkip\")",sep="");
    twiddleCmd <- paste(twiddleCmd,", y_10Exp=knob(c(0,3), res=1, label=\"y_10Exp\")",sep="");
    twiddleCmd <- paste(twiddleCmd,", chkSkip=toggle(default=FALSE,label=\"Skip incomplete CONFIGs\")",sep="");
    twiddleCmd <- paste(twiddleCmd,")",sep="");
    browser()
    eval(parse(text=twiddleCmd));
  }
  buildTwidCmd(envT);
    
  return("tdmPlotResMeta finished");
}

#tdmPlotResMeta(envT);
