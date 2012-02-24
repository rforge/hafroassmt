\encoding{latin1}
\name{n2f}
\alias{n2f}
\title{Evaluate F from Numbers at Age}
\description{
  Evaluate fishing mortality rates \eqn{F_{t,a}} from numbers at age
  \eqn{N_{t,a}} with an assumed natural mortality rate \eqn{M}.
}
\usage{
n2f(Nmat, M=0.2)
}
\arguments{
  \item{Nmat}{Numbers-at-age matrix, possibly with dimnames of years and
    ages.}
  \item{M}{Natural mortality rate, constant.}

}
\details{
  Calculated from \eqn{N_{t+1,a+1}=N_{t,a}\exp(-Z)}, where \eqn{Z=F+M}.
}
\value{
  A matrix of \eqn{F_{t,a}} one row and one column shorter than
  \code{Nmat}.
}
\references{
  \enc{Stefánsson}{Stefansson}, G. 1994. \enc{Undirstöðuatriði}{Basics
    of} \enc{stærðfræðilegrar}{mathematical} \enc{fiskifræði}{fisheries
    biology}. \enc{Útg.}{Version} 4.4.11/4/94, 60p.
}
\author{
  \enc{Sigurður}{Sigurdur} \enc{Þór}{Thor} \enc{Jónsson}{Jonsson}
}
\seealso{
  \code{\link{pope}} is a simple assessment model to estimate stock
  size.

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
