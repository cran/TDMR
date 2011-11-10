confFile = "sonar_01.conf";
apdFile = "sonar_01.apd";
tdm=list(mainCommand="result<-main_sonar(opts)",mainFile="main_sonar.r");

opts <- NULL;
source(apdFile);      # read in opts-settings
source(tdm$mainFile);
spotUserConfig = list(tdm=tdm,spot.fileMode=F, opts=opts);
spotConfig = spot(confFile,"auto",spotConfig=spotUserConfig);
