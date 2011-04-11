\name{veidarfaeri}
\alias{veidarfaeri}
\title{Look Up Gear Codes}
\description{
  Look up gear codes, as defined in \samp{orri.veidarfaeri} in Oracle.
}
\usage{
veidarfaeri(n=NULL, names=FALSE)
}
\arguments{
  \item{n}{vector of integer gear codes, or \code{NULL} (default).}
  \item{names}{whether to include gear code in the output.}
}
\value{
  String vector of gear descriptions, or data frame with all gears if
  \samp{n} is \samp{NULL}.
}
\note{
  The lookup table contains 100 gears numbered from 0 to 174,
  synchronized to \samp{orri.veidarfaeri} on 11 Apr 2011.
}
\author{Arni Magnusson.}
\references{\url{http://afladagbok.fiskistofa.is}.}
\seealso{
  \code{\link{match}} is used to look up entries in the gear table.

  \code{\link{hafroAssmt-package}} gives an overview of the package.
}
\examples{
veidarfaeri()
veidarfaeri(6)
veidarfaeri(c(2,6), T)
}
% Basics
\keyword{list}
\keyword{manip}
