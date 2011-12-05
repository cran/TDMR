######################################################################################
######################################################################################
#
# PREPROCESSING FUNCTIONS
#
######################################################################################
######################################################################################

######################################################################################
#' Find constant columns. Find all those columns in data frame \code{dset} which are completely constant or
#' completely NA and return a vector with their names.
#' @param dset  data frame
#' @return name vector of constant columns
#' @export
######################################################################################
tdmPreFindConstVar <- function(dset)
{
    const.variables = NULL
    for (n in names(dset)) {
      if (is.factor(dset[,n])) {
        if (length(levels(dset[,n]))==1)  const.variables = c(const.variables,n);
      }
      if (is.numeric(dset[,n])) {
        if (max(dset[,n],na.rm=T)==min(dset[,n],na.rm=T)) const.variables = c(const.variables,n);
      }
    }
    return(const.variables);
}

######################################################################################
#tdmPreGroupLevels
#
#' Group the levels of factor variable in \code{dset[,colname]}
#'
#' This function reduces the number of levels for factor variables with too many levels.
#' It counts the cases in each level and orders them decreasingly. Binds the least
#' frequent levels together in a new level "OTHER" such that the remaining untouched
#' levels have more than opts$Xpgroup percent of all cases. OR binds the least
#' important levels together in "OTHER" such that the total number of new levels
#' is opts$MaxLevel, whichever number is smaller.
#' INPUT:
#' @param  dset      data frame
#' @param  colname   name of column to be re-grouped
#' @param  opts      list, here we need \itemize{
#'      \item  Xpgroup   [0.99]
#'      \item  MaxLevel  [32]  (32 is the maximum number of levels allowed for RF)
#'      }
#' @return dset      data frame dset with dset[,colname] re-grouped
#'     
#' @export
######################################################################################
tdmPreGroupLevels <- function(dset,colname,opts)
{
    if (is.null(opts$Xpgroup)) opts$Xpgroup=0.99;
    if (is.null(opts$MaxLevel)) opts$MaxLevel=32;

    thisCol =  dset[,colname];
    z <- split(thisCol,thisCol);       # be aware that empty values ("") are dropped
    bz <- sapply(z,length);
    bz <- bz[order(bz,decreasing=T)];
    perc <- cumsum(bz)/sum(bz);
    w1 <- which(perc>opts$Xpgroup);
    L <- length(bz); w2 <- NULL;
    if(L>opts$MaxLevel) w2 <- (opts$MaxLevel:L);
    if(length(w1)>length(w2)) {
      othernames <- names(perc)[w1];
    } else {
      othernames <- names(perc)[w2];
    }
    othernames <- setdiff(othernames,"");

    # some 'factor-arithmetic' to give newCol exactly the right level names:
    newCol <- factor(thisCol,levels=c(levels(factor(thisCol)),"OTHER"));
    newCol[which(thisCol %in% othernames)] <- "OTHER";
    newCol <- factor(newCol, levels=setdiff(levels(newCol),othernames));
    dset[,colname] <- newCol;

    dset;
}

######################################################################################
# tdmPreLevel2Target
#
#' Relate levels of a column with a target (column). Print for each level of factor variable f which ratio 0 / 1 of the binary target
#' variable it contains and how many cases are in each level
#'
#' @param  dset    data frame
#' @param  target  name of target column
#' @param  f       number of column with factor variable
#' @param  opts    list, here we need \itemize{
#'    \item opts$thresh_pR
#'    \item opts$verbose
#' }
#'
#' @note SIDE EFFECTS:
#'     some printed output
#' @export
######################################################################################
tdmPreLevel2Target <- function(dset,target,f,opts)
{
  if (is.null(opts$thresh_pR))
    opts$thresh_pR=0 #23.08      # 0.0: print every level, >0: print only levels with %reorderer > thresh_pR

  z = split(as.numeric(dset[,target])-1,dset[,f]);        # why "-1"? - because as.numeric converts factors 0/1 to numbers 1/2
  bz = as.data.frame(sapply(z,length));
  bz = cbind(bz,sapply(z,mean)*100);                      # sapply(..,mean) gives the fraction of "1"'s (%reorderer)
  colnames(bz) <- c("cases","%reorderer");
  ind <- which(bz[,"%reorderer"]>=opts$thresh_pR);
  if (length(ind)>0) {
    cat1(opts,"column",names(dset)[f],":\n")
    print(bz[ind,]);
  }
}

######################################################################################
# tdmPreAddMonomials
#
#' Add monomials of degree 2 to a data frame
#'
#' Given the data frame \code{dset} and a data frame \code{rx} with the same number of rows,
#' add monomials of degree 2 to dset for all quadratic combinations of the columns
#' in rx. The naming of these new columns is "R1x2" for the combination of cols
#' 1 and 2 and so on (if prefix="R").
#'
#' @param   dset  the target data frame
#' @param   rx    a data frame where to draw the monomials from
#' @param   degree (currently only 2 is supported)
#' @param   prefix  character prefix for the monomial column names
#'
#' @return  data frame \code{dset} with the new monomial columns appended
#'
#' @note CAVEAT: The double for-loop costs some time (e.g. 2-4 sec for ncol(rx)=8 or 10)
#' How to fix: make a version w/o for-loop and w/o frequent assigns to dset (**TODO**)
#' @export
#'#####################################################################################
tdmPreAddMonomials <- function(dset,rx,degree=2,prefix="R") {
  if (degree!=2) stop("other degrees than 2 not yet supported");
  for (i in 1:ncol(rx)) {
    for (j in i:ncol(rx)) {
      dset <- cbind(dset, rx[,i]*rx[,j]/mean(rx[,i])/mean(rx[,j]));
      names(dset)[ncol(dset)] <- paste(prefix,i,"x",j,sep="");
    }
  }
  dset;
}

######################################################################################
# tdmPrePCA
#
#'     PCA (Principal Component Analysis) for numeric columns in a data frame 
#'
#'     tdmPrePCA is capable of linear PCA, based on prcomp (which uses SVD), and 
#'     of kernel PCA (either KPCA, KHA or KFA).
#'
#' @param   dset    the data frame with training (and test) data. 
#' @param   numeric.variables   vector with all column names in dset for which PCA is performed.
#'             These columns may contain *numeric* values only (at least for linear PCA).
#' @param   opts    a list from which we need here the following entries: \itemize{
#'     \item    PRE.PCA:   ["linear" | "kernel" | "none" ]
#'     \item    PRE.knum:  if >0 and if PRE.PCA="kernel", take only a subset of PRE.knum records from dset 
#'     \item    PRE.npc:   if >0, then add for the first PRE.npc PCs the monomials of
#'                   degree 2 (see tdmPreAddMonomials)
#'     \item    READ.TST:   if =T, then use only training records (those with dset[,opts$TST.COL]==0) during 
#'                   PC calculation, but perform the rotation to PC-space for all records.
#'     }
#' @return    pca     a list with entries: \itemize{
#'     \item    dset: the input data frame dset with columns numeric.variables replaced
#'                    by the PCs with names PC1, PC2, ... (in case PRE.PCA=="linear")
#'                    or with names KP1, KP2, ... (in case PRE.PCA=="kernel")
#'                    and optional with monomial columns added, if PRE.npc>0
#'     \item    numeric.variables:   the new column names for PCs and for the monomials
#'     \item    eigval:   the eigenvalues for the PCs
#'     \item    time:     the processing time
#'     }
#' @note CAUTION: Kernel PCA (opts$PRE.PCA=="kernel") is currently only experimental, it *crashes*
#'       for large number of records or large number of columns.
#' @export
######################################################################################
tdmPrePCA <- function(dset,numeric.variables,opts)  {
    makeX <- function(dset,numeric.variables,fname,strg,opts) {
      cat1(opts,fname,strg,length(numeric.variables),"numeric variables...\n")
      if(opts$READ.TST) x <- dset[dset[,opts$TST.COL]==0,numeric.variables]
      else x <- dset[,numeric.variables];
      x;
    }
    adjustDSet <- function(dset,numeric.variables,rx,prefix) {
      if (length(intersect(names(dset),names(rx)))>0) {
        # try "PC_1" instead of "PC1" or "KP_1" instead of "KP1"                             
        names(rx) <- sub(prefix,paste(prefix,"_",sep=""),names(rx));  
      }
      if (length(intersect(names(dset),names(rx)))>0) {
        cat("Conflicting names are:", intersect(names(dset),names(rx)), "\n");
        stop("dset contains some columns with the same name as names(rx)");
      }  
      # replace columns numeric.variables with the columns from rx (PCA columns)
      dset <- cbind(dset,rx);
      dset <- dset[,setdiff(names(dset),numeric.variables) ];
    }
    ptm <- proc.time()
    fname <-  opts$filename;
    other.vars <- setdiff(names(dset),numeric.variables);
    
    if (opts$PRE.PCA=="linear") {
      x <- makeX(dset,numeric.variables,fname,": linear PCA with",opts);

      P <- prcomp(x);
      # P$rotation[,i]: ith eigenvector, i.e. its components from original space (x)
      # P$sdev[i]^2: the corresponding ith eigenvalue
      eigval = P$sdev^2;
      rx = as.data.frame( as.matrix(dset[,numeric.variables]) %*% P$rotation);         # names are PC1, PC2, ...
      # rx contains in its rows the PC-rotated x-vectors for each case: The 1st component
      # in each row is the projection of this case on PC1, the 2nd component in each row is
      # the projection on PC2 and so on. Thus column rx[,1] contains the PC1-values of all cases,
      # column rx[,2] the PC2-values and so on.

      dset <- adjustDSet(dset,numeric.variables,rx,"PC");
    }
    
#    if (opts$PRE.PCA=="kernel") {
#      x <- makeX(dset,numeric.variables,fname,": Kernel PCA (KHA) with",opts);
#      
#      require(kernlab);
#      if (opts$PRE.knum>0) x <- x[1:opts$PRE.knum, ];
#      # *** big mem/other problems with kpca: R crashes when PRE.knum = 3000; error code from kpca when PRE.knum >190;
#      # *** runs for PRE.knum<=150 on appAcid, but with (of course) terrible result (rgain=45%)
#      #kpc <- kpca(~.,data=x,kernel="rbfdot",kpar=list(sigma=0.2),features=40);
#      # *** use instead kfa, much faster than kpca and no crash
#      #kpc <- kfa(~.,data=x,kernel="rbfdot",kpar=list(sigma=0.2),features=0);
#      # *** another option is kha, takes longer than kfa, but perhaps more reliable numerical results
#      kpc <- kha(~.,data=x,kernel="rbfdot",kpar=list(sigma=0.2),features=20, maxiter=100);
#      # pcv(kpc)[,i]: the ith eigenvector, i.e. its components from original space (x)  (not for kfa)
#      # eig(kpc)[i]: the corresponding ith eigenvalue (not for kfa)
#      eigval <- NULL # eig(kpc);
#      rx <- as.data.frame(predict(kpc,dset[,numeric.variables]));     
#      # rx contains in its rows the PC-rotated x-vectors for each case: The 1st component
#      # in each row is the projection of this case on PC1, the 2nd component in each row is
#      # the projection on PC2 and so on. Thus column rx[,1] contains the PC1-values of all cases,
#      # column rx[,2] the PC2-values and so on.
#      names(rx) <- sub("V","KP",names(rx));                               # names are KP1, KP2, ...
#
#      dset <- adjustDSet(dset,numeric.variables,rx,"KP");
#    }
#    
    if (opts$PRE.PCA=="none") {
      rx = dset[,numeric.variables];
      eigval = NULL;
    }
  
    # adding nonlinear input combinations (monomials of degree 2 for the first PRE.npc PCs)
    old.names <- names(dset);
    if (opts$PRE.npc>0) {
      npc = min(opts$PRE.npc,ncol(rx));
      dset <- tdmPreAddMonomials(dset,rx[,1:npc],degree=2);
      Nmono <- length(setdiff(names(dset),old.names));
      cat1(opts,fname,": adding", Nmono,"monomials of degree 2 for the first",npc,"PCs ...\n")
    }
    numeric.variables = setdiff(names(dset),other.vars);   #  = union{ PC-names , monomial names }
                      
    cat1(opts,"Proc time for PCA: ",(proc.time()-ptm)[1],"\n");
    
    pca <- list(dset=dset
               ,numeric.variables=numeric.variables
               ,eigval=eigval
               );
}
