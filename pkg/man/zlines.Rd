\name{zlines}
\alias{zlines}
\title{Adds oblique lines to an existing plot}
\description{Adds oblique/slanting lines to a plot, spaced by 'by'
  x-units. This can be of aid in the interpretion of catch curves. The 'z' in 'zlines' refers to total mortality rate usually denoted 'Z'.}
\usage{
zlines(slope, by = 1, ...)
}
\arguments{
  \item{slope}{Slope of oblibque lines sent to argument 'b' in 'abline'}
  \item{by}{Spacing of oblique lines in x-units.}
  \item{...}{Arguments to be passed to methods through 'abline', such as 'col', 'lty', 'lwd'.}
}
\note{Simplifies the code previously used in scripts for the same effect.}
\author{Sigurdur Thor Jonsson}
\examples{
  ## on any plot, slanting up or down
  plot(rnorm(10,10,10),rnorm(10,10,10))
  zlines(1)
  zlines(1, by = 2, col = "blue")
  zlines(-1, col = "red")

  ## on a catch curve
  data(catage)
  slope <- 0.6
  yclasses <- unique(catage$ycl)
  mycol <- rainbow(length(yclasses))
  tmp <-tapply(catage$ObsCno, list(catage$year, catage$age), sum)
  matplot(1980:2009, log(tmp), type = "n",
    ylab = "log(catage)", xlab = "Year", asp = 2)
    axis(side = 1, tck = 1, lty = 2, lwd = 0.5, col = "grey", labels = FALSE)
    axis(side = 2, tck = 1, lty = 2, lwd = 0.5, col = "grey", labels = FALSE)
  zlines(slope = slope, col = "grey", lty = 2)
  for (i in seq(along = yclasses)) {
    tmp <- catage[catage$ycl == yclasses[i], ]
    lines(tmp$year, log(tmp$ObsCno), col = mycol[i])
    text(tmp$year, log(tmp$ObsCno), tmp$age, col = mycol[i])
  }
  text(par("usr")[1]+par("cxy")[1],par("usr")[4]-par("cxy")[2],
    paste("slope =", slope), adj = 0)

}
% Graphics
\keyword{aplot}
