pope <- function(catage, surv, M=0.2)
{
  nyears <- dim(catage)[1]
  nages <- dim(catage)[2]

  Nmat <- matrix(nrow=nyears+1, ncol=nages)
  Nmat[nyears+1,] <- surv
  Nmat[-(nyears+1),nages] <- exp(M/2) * catage[,nages]

  for(j in (nages-1):1)
    Nmat[1:nyears,j] <- (Nmat[2:(nyears+1),j+1]*exp(M/2) + catage[1:nyears,j]) * exp(M/2)

  if(!is.null(dimnames(catage)))
  {
    yearRange <- as.numeric(dimnames(catage)[[1]])
    yearRange <- c(yearRange, max(yearRange)+1)
    dimnames(Nmat)[[1]] <- yearRange
    dimnames(Nmat)[[2]] <- dimnames(catage)[[2]]
  }

  return(Nmat)
}
