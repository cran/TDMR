###############################################################################################
# start.tdm.r: 
#     Script to load TDMR software. The list tdm should be def'd by the user beforehand.
#     If it does not exist, a minimal list tdm will be def'd here. 
#     (TDMR is either sourced from R files in tdm$tdmPath or its library is loaded, if is.null(tdm$tdmPath))
#
#     Additionally, parallel execution (snowfall) is initialized according to the settings in tdm
#
###############################################################################################

#
# script part to initialize parallel execution if tdm$parallelCPUs>1
#
#if (is.na(match("USE.SPOT.PACKAGE",ls()))) USE.SPOT.PACKAGE <<- TRUE;   # note "<<-" ! USE.SPOT.PACKAGE needs to be def'd in .GlobalEnv
if (is.na(match("tdm",ls()))) tdm <- list();
if (is.null(tdm$theSpotPath)) tdm$theSpotPath <- NA;
if (is.null(tdm$parallelCPUs)) tdm$parallelCPUs=1;
if (tdm$parallelCPUs>1) {
  require(snow)
  require(snowfall)
  parallelCPUs = tdm$parallelCPUs;
  sfInit(parallel=TRUE, cpus=parallelCPUs, type="SOCK" )
#  sfExport(list=c("USE.SPOT.PACKAGE"))
  sfExport(list=c("tdm"))
}

#
# function part to load TDMR functions or library and SPOT functions or library
#
if (is.null(tdm$tdmPath)) {
    cat("Loading installed library TDMR \n");
    require(TDMR)
} else {
    cat("Sourcing TDMR from R files in",tdm$tdmPath,"\n");
    source(paste(tdm$tdmPath,"source.tdm.r",sep="/"),local=TRUE);
    source.tdm(tdm$tdmPath,tdmParallelCPUs=tdm$parallelCPUs,theSpotPath=tdm$theSpotPath);
}
#tdmRandomSeed <- makeTdmRandomSeed();      # this is now in makeTdmRandomSeed.r
if (tdm$parallelCPUs>1) sfExport(list=c("tdmRandomSeed"));
