#defaultSC.R
#' 
#' Default settings for the spotConfig part of TDMR.
#' 
#' Sets suitable defaults for the spotConfig part of TDMR. 
#' 
#' With the call \code{\link{setParams}(mySC,defaultSC())} it is possible to extend a partial list 
#' \code{mySC} to a list containing all \code{sC}-elements (the missing ones are taken from 
#' \code{defaultSC()}). If \code{mySC} has an element not present in \code{defaultSC()}, 
#' this element is not taken and a warning is issued. \cr
#' With \code{\link{setParams}(mySC,defaultSC(),keepNotMatching=TRUE)} also elements
#' of \code{mySC} not present in \code{defaultSC()} are taken (no warnings).
#'
#' @return a list with the following elements (the values in parantheses \code{[]} are the defaults):
#'      \item{alg.roi}{  ["NEEDS_TO_BE_SET"] a data frame with columns \code{lower}, 
#'             \code{upper}, \code{type}, \code{row.names}, each a vector with as many 
#'             entries as there are parameter to be tuned }
#'      \item{opts}{  ["NEEDS_TO_BE_SET"]  }
#'      \item{sCName}{  ["NEEDS_TO_BE_SET.conf"] a string ending on ".conf", 
#'                      the configuration name }
#'      \item{OCBA}{  [FALSE] see \code{\link[SPOT]{spotControl}}  }
#'      \item{plot}{  [FALSE] TRUE: make a line plot showing progress  }
#'      \item{seedSPOT}{  [1] see \code{\link[SPOT]{spotControl}}  }
#'      \item{funEvals}{  [50] the budget, max number of algo evaluations   }
#'      \item{design}{  [designLHD] function that creates initial design, 
#'                      see \code{\link[SPOT]{spotControl}}  }
#'      \item{designControl.size}{  [10] number of initial design points (former init.design.size)  }
#'      \item{designControl.replicates}{  [2] number of initial repeats (former init.design.repeats)  }
#'      \item{replicates}{  [2] number of repeats for the same model design point  }
#'      \item{noise}{  [TRUE] whether the object function has noise or not
#'                     (Note: TRUE is required if \code{replicates>1} (!)) } 
#'      \item{seq.merge.func}{  [mean] how to merge Y over replicates: mean or min  }
#'      \item{model}{  [buildKriging] function that builds the surrogate model, 
#'                     see \code{\link[SPOT]{spotControl}}  }
#'      \item{optimizer}{  [optimLHD] function that optimizes on surrrogate, 
#'                         see \code{\link[SPOT]{spotControl}}  }
#'      \item{optimizerControl.funEvals}{  [100] optimizer budget (former seq.design.size)  }
#'      \item{optimizerControl.retries}{  [2] optimLHD retries  (former seq.design.retries)  }
#'
#' 
#' @seealso   \code{\link{setParams}}, \code{\link{defaultOpts}}
#'
#' @author Wolfgang Konen (\email{wolfgang.konen@@th-koeln.de}) THK, 2018
#' @export
#'
defaultSC<-function(){
    sC <-list(# SPOT global parameter
              # --- keep it a flat list (no other list as members), so that the user can set 
              # --- individual members before calling setParams(mySC,defaultSC())
              ###########################################################
               alg.roi = "NEEDS_TO_BE_SET"
              ,opts = "NEEDS_TO_BE_SET"
              ,sCName = "NEEDS_TO_BE_SET.conf"
              ,alg.resultColumn = "Y"   # Column name containing results
              ,alg.seed = 1235          # seed for user algorithm
              ,OCBA = FALSE
              ,plots = FALSE            # TRUE: make a line plot showing progress
              ,seedSPOT = 1
              #
              # SPOT steps related
              ###########################################################
              ,funEvals = 50            # max number of algo evaluations 
              ,design = designLHD
              ,designControl.size = 10       # number of initial design points (former init.design.size)
              ,designControl.replicates = 2  # number of initial repeats (former init.design.repeats)
              ,replicates = 2           # number of repeats for the same model design point
              ,noise = TRUE             # whether the object function has noise or not
                                        # TRUE required if replicates>1 (!) 
              ,seq.merge.func = mean        # how to merge Y over replicates: mean or min
              ,model = buildKriging
              ,modelControl.useLambda = TRUE    # test WK
              ,optimizer = optimLHD     # which optimizer to use on model
              ,optimizerControl.funEvals = 100 # optimizer budget (former seq.design.size)
              ,optimizerControl.retries = 2    # optimLHD retries  (former seq.design.retries)
              #
              # --- DEPRECATED ---
              #auto.loop.steps = 50,     # NOW DEPRECATED (number of spot metamodels to be generated)
              #io.verbosity = 1,         # =3 print much & do updating graphics, =0: print (nearly) nothing
              #seq.design.oldBest.size=1,# how many old (best) points shall be repeated 
              #seq.design.new.size=3     # how many new points shall be added 
    );
    return(sC);  
}

defaultSCList<-function(){
  sCList <- list();
  sCList[[1]] <- defaultSC();
  sCList;
}

# setParams
#' 
#' Merge the parameters from a partial list and the default list
#'
#' @param opts       a partial list of parameters
#' @param defaultOpt a list with default values for every element
#' @param keepNotMatching [FALSE] if TRUE, copy the elements appearing in \code{opts}, but not in 
#'      \code{defaultOpt} to the return value. If FALSE, do not copy them, but issue a warning.
#'
#' @return a list combined from \code{opts} and \code{defaultOpt} where every available element 
#'    in \code{opts} overrides the default. For the rest of the elements the value from \code{defaultOpt}
#'    is taken. \cr
#'    A warning is issued for every element appearing in \code{opts} but not in \code{defaultOpt} 
#'    (only if \code{keepNotMatching==FALSE}).
#'    
#' @seealso   \code{\link{defaultSC}}, \code{\link{defaultOpts}}
#'
#' @author Wolfgang Konen (\email{wolfgang.konen@@th-koeln.de}), Samineh Bagheri
#' @export
#'
setParams<-function(opts,defaultOpt,keepNotMatching=FALSE){
  
  setting<-defaultOpt

  if(methods::hasArg(opts)){
    matching<-intersect(names(opts),names(defaultOpt))
    setting[matching] <- opts[matching]
    
    notMatching <- setdiff(names(opts),
                           names(defaultOpt))
 
    if (keepNotMatching) {
      for (x in notMatching) setting[x] = opts[x]
      # a for-loop is necessary here to create the non-existing elements in 'setting'
    } else {
      if(length(notMatching)!=0) warning(paste("
            The following arguments are ignored: ", notMatching))
    }
      
    
  }
  
  return(setting)
}