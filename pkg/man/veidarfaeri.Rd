\name{veidarfaeri}
\alias{veidarfaeri}
\title{Look Up Fishing Gear}
\description{
  Look up fishing gear codes, as defined in \samp{orri.veidarfaeri} in
  Oracle.
}
\usage{
veidarfaeri(x=NULL, names=FALSE)
}
\arguments{
  \item{x}{vector of integers (gear codes), strings (gear names), or
    \code{NULL} to show all gears.}
  \item{names}{whether to assign descriptive names to vector elements.}
}
\value{
  Vector of integers (gear codes), strings (gear names), or a data frame
  if \samp{n=NULL}, with named rows and the following columns:
  \item{id}{gear code \emph{(optional)}.}
  \item{lysing}{gear description.}
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

veidarfaeri(c(2,6), TRUE)

veidarfaeri("Flotvarpa")
}
% Basics
\keyword{list}
\keyword{manip}
