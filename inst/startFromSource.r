#
#  script to load all developer sources for TDMR
# (  only if no 'tdm' is yet def'd, otherwise use > source("start.tdm.r") directly  )
#
tdm <- list(tdmPath="../../TDMR"  # NULL or source TDMR-files from the R subdir of this dir. If NULL, load instead the TDMR library.
        , theSpotPath=NULL # "USE.SOURCE"  # NULL or source R-files for SPOT from the dir def'd in source.tdm.r. If NULL, load SPOT library.
        , theRsfaPath=NULL # "USE.SOURCE"  # NULL or source R-files for rSFA from the dir def'd in source.tdm.r. If NULL, load rSFA library.
        , parallelCPUs=1
        );
#tdm$theSpotPath="C:/user/datasets/Vorlesungen/R-Project/SPOT_2.0.1/SPOT/R";
source("start.tdm.r");
