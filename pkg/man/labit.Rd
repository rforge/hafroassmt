\name{labit}
\alias{labit}
\title{Create Label with Italic}
\description{
  Paste two strings and create a plot label, where the second string is
  \emph{italic}.}
\usage{
labit(x, y, bold=FALSE, sep=" ")
}
\arguments{
  \item{x}{a string to render in roman.}
  \item{y}{a string to render in italic.}
  \item{bold}{whether the first string should be in boldface.}
  \item{sep}{a string to separate the text strings.}
}
\value{
  \code{expression} that can be passed to plot functions as annotation.
}
\note{
  Designed for plots in the annual stock assessment summary report (e.g.
  Marine Research Institute 2010).
}
\author{Arni Magnusson and Hoskuldur Bjornsson.}
\references{
  Marine Research Institute. 2010. \cite{State of marine stocks in
    Icelandic waters 2009/2010.} Available at
  \url{http://www.hafro.is/Astand/2010/ASTAND-10.pdf}.
}
\seealso{
  \code{\link{plotmath}} provides the mechanism to combine roman,
  italic, and boldface strings in one plot label.
}
\examples{
  ## Plain and italic:
  plot(1, xlab=labit("Aldur", "Age"))
  ## equivalent to
  plot(1, xlab=expression(paste("Aldur ", italic("Age"))))

  ## Bold and italic:
  plot(1, xlab=labit("Aldur", "Age", bold=TRUE))
  ## equivalent to
  plot(1, xlab=expression(paste(bold("Aldur "), italic("Age"))))
}
% Graphics
\keyword{aplot}
