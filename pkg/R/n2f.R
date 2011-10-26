n2f <-
function(natage,natMor=0.2) {
  fatage <- log(natage[-nrow(natage), -ncol(natage)]/natage[-1, -1]) - natMor
  if(!is.null(dimnames(natage))) {
    yearAgeRanges <- dim(natage)
    dimnames(fatage)[[1]] <- dimnames(natage)[[1]][-yearAgeRanges[1]]
    dimnames(fatage)[[2]] <- dimnames(natage)[[2]][-yearAgeRanges[2]]
  }
  fatage
}
