\name{catage}
\alias{catage}
\docType{data}
\title{Saithe Catch and Weight at Age}
\description{
  This dataset contains catch at age and weight at age for the Icelandic
  saithe stock in a 5-column data frame, indexed by year, age, and year
  class for the period 1980--2009.
}
\usage{catage}
\format{
  A data frame containing 360 rows and 5 columns:
  \tabular{ll}{
    \code{year}    \tab calendar year                        \cr
    \code{age}     \tab age                                  \cr
    \code{ObsCno}  \tab catch in numbers (thousands)         \cr
    \code{CatchWts}\tab mean weight at age in the catches (g)\cr
    \code{ycl}     \tab year class                           \cr
  }
}
\source{
  Tables 8.2 and 8.3 in: ICES (International Council for the Exploration
  of the Sea). 2010. Report of the North-Western Working Group (NWWG).
  ICES CM 2010/ACOM:07. Available at
  \url{http://ices.dk/products/CMdocs/CM-2010/ACOM/ACOM0710.pdf}.
}
\examples{
## Catch curve
slope <- 0.6
yclasses <- unique(catage$ycl)
mycol <- rainbow(length(yclasses))
tmp <- tapply(catage$ObsCno, list(catage$year,catage$age), sum)
matplot(1980:2009, log(tmp), type="n", ylab="log(catage)", xlab="Year",
        asp=2)
grid(col="grey", lty=2)
zlines(slope=slope, col="grey", lty=2)
for(i in seq(along=yclasses))
{
  tmp <- catage[catage$ycl==yclasses[i],]
  lines(tmp$year, log(tmp$ObsCno), col=mycol[i])
  text(tmp$year, log(tmp$ObsCno), tmp$age, col=mycol[i])
}
text(par("usr")[1]+par("cxy")[1], par("usr")[4]-par("cxy")[2],
     paste("slope =",slope), adj=0)
}
\keyword{datasets}
