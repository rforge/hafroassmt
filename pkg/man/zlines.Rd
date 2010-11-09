\name{zlines}
\alias{zlines}
\title{Add Diagonal Lines to Existing Plot}
\description{
  Adds diagonal lines to a plot, spaced by \code{by} x-units. This can
  be of aid in the interpretion of catch curves. The \sQuote{\samp{z}}
  in \code{zlines} refers to total mortality rate, usually denoted
  \eqn{Z}.
}
\usage{
zlines(slope, by=1, \ldots)
}
\arguments{
  \item{slope}{slope of diagonal lines sent to argument \code{b} in
    \code{abline}.}
  \item{by}{spacing of diagonal lines in x-units.}
  \item{\ldots}{passed to \code{abline}.}
}
\value{Invisible \code{NULL}.}
\note{
  Simplifies the code previously used in scripts for the same effect.
}
\author{Sigurdur Thor Jonsson.}
\seealso{
  \code{\link{abline}} is called iteratively to draw the lines.
}
\examples{
## Any plot, slanting up or down
plot(rnorm(10,10,10), rnorm(10,10,10))
zlines(1)
zlines(1, by=2, col="blue")
zlines(-1, col="red")

## Catch curve
slope <- 0.6
yclasses <- unique(catage$ycl)
mycol <- rainbow(length(yclasses))
tmp <- tapply(catage$ObsCno, list(catage$year,catage$age), sum)
matplot(1980:2009, log(tmp), type="n", ylab="log(catage)", xlab="Year",
        asp=2)
grid(col="grey", lty=2)
zlines(slope=slope, col="grey", lty=2)
for(i in seq(along=yclasses))
{
  tmp <- catage[catage$ycl==yclasses[i],]
  lines(tmp$year, log(tmp$ObsCno), col=mycol[i])
  text(tmp$year, log(tmp$ObsCno), tmp$age, col=mycol[i])
}
text(par("usr")[1]+par("cxy")[1], par("usr")[4]-par("cxy")[2],
     paste("slope =",slope), adj=0)
}
% Graphics
\keyword{aplot}
