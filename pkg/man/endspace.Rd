\name{endspace}
\alias{endspace}
\title{Make space at ends of bars in barplot()}
\description{Make space at ends of bars in barplot()}
\usage{
endspace(x, space = 0.2)
}
\arguments{
  \item{x}{matrix returned by barplot()}
  \item{space}{space at ends of barplot as fraction of barwidth}
}
\value{vector of two values, to be used as xlim in barplot()}
\author{Birkir Bardarson}
\seealso{\code{\link{barplot}}}
\examples{
out <- endspace(barplot(stack.loss))
barplot(stack.loss, xaxs="i", xlim=out, ylim=c(0,45))
box()
}
\keyword{dplot}
