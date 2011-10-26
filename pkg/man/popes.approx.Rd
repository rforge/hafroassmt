\name{popes.approx}
\alias{popes.approx}

\title{
  Pope's approximation for catch-at-age matrices.
}

\description{
Performs Pope's approxmate cohort analysis on a matrix of catch-at-age numbers.
}

\usage{
popes.approx(catage, surv, natMor = 0.2)
}

\arguments{
  \item{catage}{
A matrix of catch at age given, year x age, often with year and age
  vectors as dimnames dimnames   
}
  \item{surv}{
A vector of survivors, of length equal to columns in \code{catage}), to the year following the newest catch.
}
  \item{natMor}{
Natural mortality, constant.
}
}

\details{The currently implemented assumption is that the whole catch is
taken mid-year and the backwards calculation is initiated assuming that
catch-at-oldest-age is the cohort's remainder. Natural mortality, in
this simple model from all causes other than fishing, is assumed
constant, defaults to a value once derived from a question mark. The
vector of survivors has to be taken from other sources (a simple one
would be from some type of adapt tuning).}

\value{
A matrix of numbers at age.
}

\references{Pope, J. G. 1972. An investigation of the accuracy of
virtual population analysis using cohort
analysis. Res. Bull. Int. Comm. Northwest Atl. Fish. 9: 65-74.}

\author{
Sigurdur Thor Jonsson
}

\seealso{
\code{\link{\n2f}}, \code{\link{catage}}
}

\examples{

# prepare a matrix of catch-at-age from the data.frame 'catage'.

catageMat <- tapply(catage$ObsCno, list(catage$year, catage$age), sum)

# supply a vecor of survivors

surv <- c(33845, 31405, 12389, 4199, 5001, 5306,
  1141, 2179, 1244, 629, 213, 111)

# estimate numbers

N <- popes.approx(catageMat, surv)

# Time-series of numbers-at-youngest-age
N[, 1]

# estimate fishing mortalities

F <- n2f(N)

# arithmetic mean F for a reference age range,
# note matrices have named dimensions

apply(F[ , dimnames(F)[[2]] %in% 4:9], 1, mean)

}

\keyword{arith}
