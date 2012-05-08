#*# This demo shows a phase-2 example (SPOT tuning on task SONAR)

## load package and set working directory (dir with .apd, .conf and main_*.r file)
#library(TDMR);
path <- paste(.find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");
oldwd <- getwd(); 
setwd(path);

tdm=tdmDefaultsFill(mainFile="main_sonar.r");
tdmMapDesSpot$load(tdm);     # load the mapping from design variables to opts variables

source("sonar_01.apd");      # read in settings for opts
source(tdm$mainFile);
spotUserConfig = list(tdm=tdm,spot.fileMode=F, opts=opts);
spotConfig = spot("sonar_01.conf","auto",spotConfig=spotUserConfig);

## restore old working directory
setwd(oldwd);

