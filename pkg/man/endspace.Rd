\name{endspace}
\alias{endspace}
\title{Find Appropriate Barplot Margins}
\description{
  Calculate bounds that can be used as \code{xlim} in a \code{barplot}.
}
\usage{
endspace(x, space=0.2)
}
\arguments{
  \item{x}{matrix returned by \code{barplot}.}
  \item{space}{space at ends of barplot, as a fraction of bar width.}
}
\value{Vector of two values.}
\author{Birkir Bardarson.}
\seealso{
  \code{\link{barplot}}.

  \code{\link{hafroAssmt-package}} gives an overview of the package.
}
\examples{
out <- endspace(barplot(stack.loss,plot=FALSE))

barplot(stack.loss, xaxs="i", xlim=out, ylim=c(0,45))
box()
}
\keyword{dplot}
