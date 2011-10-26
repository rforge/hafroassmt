popes.approx <-
function(catage, surv, natMor = 0.2)
{
  nyears <- dim(catage)[1]
  nages <- dim(catage)[2]
  natage <- matrix(nrow = nyears + 1, ncol = nages)
  natage[nyears + 1,  ] <- surv
  natage[ - (nyears + 1), nages] <- exp(natMor/2) * catage[, nages]
  for(j in (nages - 1):1)
    natage[1:nyears, j] <- (natage[2:(nyears + 1), j + 1] * exp(
      natMor/2) + catage[1:nyears, j]) * exp(natMor/2)
  if(!is.null(dimnames(catage))) {
    yearRange <- as.numeric(dimnames(catage)[[1]])
    yearRange <- c(yearRange, max(yearRange) + 1)
    dimnames(natage)[[1]] <- yearRange
    dimnames(natage)[[2]] <- dimnames(catage)[[2]]
  }
  natage
}

