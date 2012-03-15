######################################################################################
# tdmPlotResMeta:
#
#' Interactive plots of RES data frames and their metamodels.
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
#'    # Read previous tuning results 'envT' from demo02sonar/sonar_04.RData (relative to the TDMR package directory). 
#'    # Then, \code{tdmPlotResMeta} lets you explore interactively the RES data frame(s):
#'    \dontrun{ 
#'      load(paste(.find.package("TDMR"), "demo02sonar","sonar_04.RData",sep="/"));
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

  nExp=1;  
  # just a dummy statement. It is needed, so that function buildTwidCmd can set the variable
  # in its OUTER environment with 'nExp <<- 1'  (see below; this is the way twiddler needs it)

  # getInd: private helper fct for picking the right element from envT$resGrid
  getInd <- function(confFile,nExp,theTuner) {
    indTuner = which(envT$tdm$tuneMethod==theTuner);
    if (length(indTuner)==0) stop(paste("Could not find tuner ",theTuner,"in envT$tdm$tuneMethod"));
    nConf = which(envT$runList==confFile);
    if (length(nConf)==0) stop(paste("Could not find conf file ",confFile,"in envT$runList"));
    if (nExp<1 | nExp>envT$tdm$nExperim) stop(paste("nExp is not in range {1,...,",envT$tdm$nExperim,"}",sep=""));
    ind = indTuner + envT$nTuner*((nExp-1) + envT$tdm$nExperim*(nConf-1));
  }
  showMeta <- function(theTuner,nExp,reportFunc,modelFit) {
        # TODO: extend for various runList elements
        res = envT$resGrid[[getInd(envT$runList[1],nExp,theTuner)]];        

        sC <- envT$tunerVal;
        names(sC$alg.roi) <- c("lower","upper","type");
        sC$alg.currentResult = res;  
        sC$spot.fileMode = FALSE;
        sC$seq.predictionModel.func = modelFit; # override the confFile setting
        sC$report.func <- reportFunc;           # override the confFile setting
        
        sC$report.main <- paste(theTuner,", nExp=",as.character(nExp),sep="");
        if (nrow(sC$alg.roi)==2) {   # only 2 design variables
          sC$report.aIndex=1; sC$report.bIndex=2; 
          sC$report.interactive=FALSE;
        }
        # if there are more than 2 design variables, SPOT will open a 2nd twiddle-interface to select two of them

        confFile="NULL";  # "NULL" means: don't read any file, take all values from envT$spotConfig
        sC <- spot(confFile,"rep",NA,sC);
        return("twiddle done");
  }
  
  buildTwidCmd <- function(envT) {
    twiddleCmd <- paste("twiddle(showMeta(tuner,nExp,reportFunc,modelFit), eval=FALSE",sep="");
                  # eval=FALSE triggers two buttons "EVAL" and "CLOSE"  and inhibits auto-evaluation in twiddle
    tm =  envT$tdm$tuneMethod;
    if (length(tm)>1) {
      twiddleCmd <- paste(twiddleCmd,", tuner=combo(\"",tm[1],"\"",sep="");
      for (i in 2:length(tm)) twiddleCmd <- paste(twiddleCmd,",\"",tm[i],"\"",sep="");
      twiddleCmd <- paste(twiddleCmd,",label=as.character(\"tuner\"))",sep="");
      # the lines above construct a command which looks e.g. like
      #       twiddle(showMeta(tuner), eval=FALSE, tuner=combo("spot","lhd",label=as.character("Tuner")))   
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
    tne = envT$tdm$nExperim;
    if (tne>1) {
      twiddleCmd <- paste(twiddleCmd,", nExp=knob(c(1,",tne,"), res=1, label=\"nExper\")",sep="");
    } else { # i.e. only one tuneMethod
      nExp <<- 1;     
      # a bit awkward, but twiddle requires a bound variable to be in the environment parent.frame() == environment(tdmPlotResMeta)
      # --- It would be nicer, if twiddler would allow a binding just by nExp <- 1;
    }
    twiddleCmd <- paste(twiddleCmd,", reportFunc = combo(\"spotReport3d\",\"spotReportContour\",label=\"reportFunc\")",sep="");
    twiddleCmd <- paste(twiddleCmd,", modelFit = combo(\"spotPredictGausspr\",\"spotPredictRandomForest\",\"spotPredictMlegp\",label=\"modelFit\")",sep="");
    twiddleCmd <- paste(twiddleCmd,")",sep="");
    #browser()
    eval(parse(text=twiddleCmd));
  }
  
  buildTwidCmd(envT);
    
  return("tdmPlotResMeta finished");
}

#tdmPlotResMeta(envT);
