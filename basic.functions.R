GG.correction <- function(ezResult) {

  # a function to obtain the Greenghouse-Geisser corrected DF
  # the function takes the output of the ezANOVA function
  # note that this function is not production ready and does
  # check that the input is valid
 
  GGe <- ezResult$`Sphericity Corrections`$GGe
  DFn <- ezResult$ANOVA$DFn
  DFd <- ezResult$ANOVA$DFd
  
  A<-ezResult$ANOVA$Effect
  B<-ezResult$`Sphericity Corrections`$Effect
  
  ind <- A %in% B
  
  adj.DFn <- GGe * DFn[ind]
  adj.DFd <- GGe * DFd[ind]
  
  result <- data.frame(B, adj.DFn, adj.DFd)
  colnames(result) <- c("Effect","Adj.DFn","Adj.DFd")
  
  return(result)

  }
  

critical_t_values <- function(df , p = 0.95 , SILENT = FALSE) {
  
  # a function to find critical t values for a predefined 
  # probability (p) value (or alpha level)
  #
  # the function takes two inputs:
  # df == degrees of freedom
  # p == your probability level (the default level is 95%)
  # 
  # if you save the output to a variable you will get a list
  # containing the two critical values
  #
  # you can silence the output by including a variable SILENT = TRUE
  
  if (p<0.5 | p>=1){
    stop("p must be between 0.5 and 1 (one not included) : 0.5 <= x < 1")
  }
  
  one_sided <- abs(qt((1-p) , df))
  two_sided <- abs(qt((1-p)/2 , df))
  
  if (SILENT == FALSE){
    cat(paste("The" , toString(100*p) , "% critical t values at" , toString(df) , "degrees of freedom:\n"))
    cat(paste("One sided critical t ==" , toString(one_sided)), "\n")
    cat(paste("Two sided critical t ==", toString(two_sided)), "\n")
    cat("\n")
    cat("You can calculate your confidence interval by multiplying\n")
    cat("the estimated standard error of the mean by the two sided\n")
    cat(paste("critical t value ==" , toString(two_sided) , "* SEM\n"))
    cat(" \n")
  }
  
  return(c(one_sided , two_sided))
}
