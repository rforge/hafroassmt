zlines <- function(slope, by=1, ...)
{
  if(slope == 0)
    stop("Lines are supposed to be oblique, use grid() or abline() instead")
  else if(slope < 0)
    intercepts <- seq(floor(par("usr")[3]-slope*par("usr")[1]), ceiling(par("usr")[4]-slope*par("usr")[2]), by=by)
  else
    intercepts <- seq(ceiling(par("usr")[4]-slope*par("usr")[1]), floor(par("usr")[3]-slope*par("usr")[2]), by=-by)

  sapply(intercepts, abline(b=slope, ...))

  invisible(intercepts)
}
