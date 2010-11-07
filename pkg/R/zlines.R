zlines <-
function(slope, by = 1, ...)
{
  if (slope == 0)
    stop("lines are supposed to be oblique, use 'grid' or 'abline' instead")
  if (slope < 0)
    intercepts <- seq(floor(par("usr")[3] - slope*par("usr")[1]),
      ceiling(par("usr")[4] - slope*par("usr")[2]), by = by)
  else
    intercepts <- seq(ceiling(par("usr")[4] - slope*par("usr")[1]),
      floor(par("usr")[3] - slope*par("usr")[2]), by = -by)
  for(i in seq(along = intercepts))
    abline(a = intercepts[i], b = slope, ...)
}

