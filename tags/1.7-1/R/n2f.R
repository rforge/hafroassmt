n2f <- function(Nmat, M=0.2)
{
  Fmat <- log(Nmat[-nrow(Nmat),-ncol(Nmat)] / Nmat[-1,-1]) - M

  if(!is.null(dimnames(Nmat)))
  {
    yearAgeRanges <- dim(Nmat)
    dimnames(Fmat)[[1]] <- dimnames(Nmat)[[1]][-yearAgeRanges[1]]
    dimnames(Fmat)[[2]] <- dimnames(Nmat)[[2]][-yearAgeRanges[2]]
  }

  return(Fmat)
}
