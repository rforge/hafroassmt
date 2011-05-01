\name{residplot}
\alias{residplot}
\title{Plot Catch-at-Age Log Residuals}
\description{
  Plot log-transformed residuals from a model fit to catch-at-age data.
  The default is to draw positive residuals as red bubbles and negative
  residuals as hollow bubbles.
}
\usage{
residplot(residuals, xpos, ypos, maxsize=0.2, poscol=2, linecol=1,
          lwd=1, n=50, maxn, negcol, txt=FALSE, csi=0.1, xlab="",
          ylab="", axes=TRUE, arg=TRUE, argcol=20, arglty=2,
          cn=c("x","y","z"), append=FALSE)
}
\arguments{
  \item{residuals}{not documented.}
  \item{xpos}{not documented.}
  \item{ypos}{not documented.}
  \item{maxsize}{not documented.}
  \item{poscol}{not documented.}
  \item{linecol}{not documented.}
  \item{lwd}{not documented.}
  \item{n}{not documented.}
  \item{maxn}{not documented.}
  \item{negcol}{not documented.}
  \item{txt}{not documented.}
  \item{csi}{not documented.}
  \item{xlab}{not documented.}
  \item{ylab}{not documented.}
  \item{axes}{not documented.}
  \item{arg}{not documented.}
  \item{argcol}{not documented.}
  \item{arglty}{not documented.}
  \item{cn}{not documented.}
  \item{append}{not documented.}
}
\value{\code{NULL}, but a plot is drawn.}
\author{Hoskuldur Bjornsson.}
\seealso{
  \code{\link{plot}}, \code{\link{polygon}}, and \code{\link{lines}} are
  used to generate the plot.
}
\examples{
catage$CalcCno <- round(catage$ObsCno*exp(rnorm(nrow(catage),s=0.2)), 1)
catage$CatchDiff <- log(catage$ObsCno) - log(catage$CalcCno)

residplot(catage, cn=c("year","age","CatchDiff"), maxn=1)
}
% Graphics
\keyword{dplot}
\keyword{hplot}
