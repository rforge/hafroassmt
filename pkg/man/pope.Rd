\name{pope}
\alias{pope}
\title{Pope's Cohort Analysis}
\description{
  Perform Pope's approximate cohort analysis on a matrix of catch-at-age
  numbers.
}
\usage{
pope(catage, surv, M=0.2)
}
\arguments{
  \item{catage}{Catch-at-age matrix, years in rows and ages in columns.}
  \item{surv}{Vector of survivors, of length equal to columns in
    \code{catage}), to the year following the newest catch.}
  \item{M}{Natural mortality rate, constant.}
}
\details{
  The currently implemented assumption is that the whole catch is taken
  mid-year and the backwards calculation is initiated assuming that
  catch-at-oldest-age is the cohort's remainder. Natural mortality rate, in
  this simple model from all causes other than fishing, is assumed
  constant, defaults to a value once derived from a question mark. The
  vector of survivors has to be taken from other sources (a simple one
  would be from some type of ADAPT tuning).
}
\value{A matrix of numbers at age.}
\references{
  Pope, J.G. 1972. An investigation of the accuracy of virtual
  population analysis using cohort analysis. \cite{ICNAF Research
    Bulletin} 9:65--74.
}
\author{Sigurdur Thor Jonsson.}
\seealso{
  \code{\link{n2f}} evaluates F from numbers at age.

  \code{\link{hafroAssmt-package}} gives an overview of the package.
}
\examples{
# Prepare a matrix of catch-at-age from the data.frame 'catage'
catageMat <- tapply(catage$ObsCno, list(catage$year,catage$age), sum)

# Supply a vector of survivors
surv <- c(33845, 31405, 12389, 4199, 5001, 5306, 1141, 2179, 1244, 629,
          213, 111)

# Estimate numbers
Nmat <- pope(catageMat, surv)

# Time series of numbers-at-youngest-age
Nmat[,1]

# Estimate fishing mortalities
Fmat <- n2f(Nmat)

# Arithmetic mean F for a reference age range
# Note matrices have named dimensions
apply(Fmat[,colnames(Fmat) \%in\% 4:9], 1, mean)
}
\keyword{models}
\keyword{ts}
