#
# This script produces with tdm$tuneMethod="cmaes" an error after CONFIG=4
#
tdm <- list(tdmPath=NULL # from where to load TDMR: if NULL, load package TDMR, else: source R-files from this dir
            , unbiasedFunc="unbiasedBestRun_R"
            , umode=c("RSUB")     # ,"CV"
            , mainFile="main_cpu.r"
            , mainCommand="result <- main_cpu(opts)"
            , tuneMethod=c("spot")   #   "spot",,"lhd",    "cmaes"   "bfgs"
            , finalFile="cpu.fin"
            , experFile=NULL # "cpu.exp"
            , nrun=2, nfold=2          # repeats and CV-folds for the unbiased runs
            , optsVerbosity=2           # the verbosity for the unbiased runs
            , withParams=TRUE
            , nExperim=1
            , parallelCPUs = 1         # [1] 1: sequential, >1: parallel execution with snowFall using this many cpus
            );

#
# the following 3 lines are only needed if you want to initialize for parallel execution or from developper sources in tdm$tdmPath, 
# otherwise a simple  "require(TDMR);"  will do
tdm$theSpotPath <- NA;
start.tdm.path <- ifelse(is.null(tdm$tdmPath),.find.package("TDMR"),paste(tdm$tdmPath,"..",sep="/")); 
source(paste(start.tdm.path,"start.tdm.r",sep="/"),local=T); 

runList = c("cpu_01.conf") #,"cpu_02.conf"); 
spotList = NULL # list() #       #  =NULL: all in runList; =list(): none
spotStep = "auto"

envT <- tdmCompleteEval(runList,spotList,spotStep,tdm);
