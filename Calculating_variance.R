VarDefExpress <- function(x){ 
  ######################################Defintional expression of the sample variance ###########
  ##################################
  
  n <- length(x) # number of observations
  
  ##Initialize loop sums
  meanLoop <- 0
  varLoop <- 0
  
  ##Calculate mean
  for(i in 1:n){
    meanLoop <- meanLoop + x[i] 
  }
  
  xMean <- meanLoop/n  #The mean of the x values 
  
  ## Calculate variance
  for(i in 1:n){
    varLoop <- varLoop + (x[i] - xMean)^2
  }
  
  xVar <- varLoop/(n-1)
  return(xVar)
} #end::VarDefExpress


VarCompExpress <- function(x){
  ######################################Computational expression of the sample variance ###########
  ##################################
  
  n <- length(x) # number of observations
  
  #initialize sum_xsq and sum_xsq2
  sum_xsq <- 0
  sum_xsq2 <- 0
  
  
  for(i in 1:n){
    sum_xsq <- (sum_xsq) + (x[i])^2 
    
    sum_xsq2 <- (sum_xsq2) + x[i]  
  }
  
  #Calculate variance
  xVar <- ((sum_xsq)-((sum_xsq2)^2/(n)))/(n-1)
  return(xVar)
} #end::VarComExpress