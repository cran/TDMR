#
# This script produces with tdm$tuneMethod="cmaes" an error after CONFIG=4
#
tdm <- list(tdmPath=NULL # from where to load TDMR: if NULL, load package TDMR, else: source R-files from this dir
            , unbiasedFunc="unbiasedRun"
            , umode=c("RSUB")     # ,"CV"
            , mainFile="main_cpu.r"
            , mainFunction="main_cpu"
            , tuneMethod=c("spot")   #   "spot","lhd",    "cmaes"   "bfgs"
            , finalFile="cpu.fin"
            , experFile=NULL # "cpu.exp"
            , nrun=2, nfold=2          # repeats and CV-folds for the unbiased runs
            , optsVerbosity=2           # the verbosity for the unbiased runs
            , withParams=TRUE
            , nExperim=1
            , parallelCPUs = 1         # [1] 1: sequential, >1: parallel execution with snowFall using this many cpus
            );


tdm$runList = c("cpu_01.conf") #,"cpu_02.conf"); 
tdm$spotList = NULL # list() #       #  =NULL: all in runList; =list(): none
spotStep = "auto"

envT <- tdmEnvTMakeNew(tdm);
envT <- tdmBigLoop(envT,spotStep);
#deprecated: envT <- tdmCompleteEval(runList,spotList,spotStep,tdm);
