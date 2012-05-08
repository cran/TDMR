#
# script to load all developer sources for TDMR
#
tdm <- list(tdmPath="../../TDMR"  # NULL or source R-files for TDMR from this dir. If NULL, load instead the library TDMR
        , theSpotPath="USE.SOURCE"  # NULL or source R-files for SPOT from the dir def'd in source.tdm.r. If NULL, load SPOT library.
        , theRsfaPath="USE.SOURCE"  # NULL or source R-files for rSFA from the dir def'd in source.tdm.r. If NULL, load rSFA library.
        );

source("start.tdm.r");
