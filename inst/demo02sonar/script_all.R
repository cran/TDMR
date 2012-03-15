tdm <- list(tdmPath= "../../../tdm" # NULL  #  source R-files for TDMR from this dir. If NULL, load instead the library TDMR 
            , unbiasedFunc="unbiasedRun"
            , umode=c("CV")     # ,"RSUB"
            , mainFile="main_sonar.r"
            , mainFunction="main_sonar"
            , tuneMethod=c("spot")   #   ,  "spot"   "cmaes"   "bfgs"
            , finalFile="sonar.fin"
            , experFile=NULL # "sonar.exp"
            , nrun=5, nfold=2          # repeats and CV-folds for the unbiased runs
            , optsVerbosity=0           # the verbosity for the unbiased runs
            , withParams=TRUE
            , nExperim=1
            , parallelCPUs = 1         # [1] 1: sequential, >1: parallel with snowFall and this many cpus
            );
            
#--
#-- the following 3 lines are only needed if you want to initialize for parallel execution OR if you want to 
#-- run the developer sources in tdm$tdmPath; otherwise a simple  "require(TDMR);"  will do.
tdm$theSpotPath <- "USE.SOURCE"; # NA;
start.tdm.path <- ifelse(is.null(tdm$tdmPath),.find.package("TDMR"),paste(tdm$tdmPath,"inst",sep="/")); 
source(paste(start.tdm.path,"start.tdm.r",sep="/"),local=T); 
#require(TDMR);

runList = c("sonar_04.conf"); # ,"sonar_01.conf","sonar_02.conf","sonar_03.conf"); #
spotList = NULL # list() #       #  =NULL: all in runList; =list(): none
spotStep = "auto"

envT <- tdmCompleteEval(runList,spotList,spotStep,tdm);
