\encoding{latin1}

\name{n2f}
\alias{n2f}

\title{
Numbers to fishing mortalities
}
\description{
Calculates fishing mortalities from numbers with an assumption about
'natural' mortality, with the stock equation solved for F.
}
\usage{
n2f(N, natMor = 0.2)
}

\arguments{
  \item{N}{
Matrix of numbers-at-age, possibly with numeric dimnames of year and age vectors. 
}
  \item{natMor}{
Natural mortality, constant.
}

}
\details{
Calculated from Ny+1,a+1=exp(-Z)*Ny,a where Z = F + M
}
\value{
A matrix of Fs-at-age one row and column short of that of the N-at-age
used as input.
}

\references{ \enc{Stefánsson}{Stefansson},
G. 1994. \enc{Undirstöðuatriði}{Basics of}
\enc{stærðfræðilegrar}{mathematical} \enc{fiskifræði}{fisheries
biology}. \enc{Útg}{Version}. 4.4.11/4/94, 60p.  }

\author{
\enc{Sigurður}{Sigurdur} \enc{Þór}{Thor} \enc{Jónsson}{Jonsson}
}

\seealso{
\code{\link{popes.approx}}, \code{\link{catage}}
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
