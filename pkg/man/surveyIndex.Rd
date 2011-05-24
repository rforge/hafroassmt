\name{surveyIndex}
\alias{surveyIndex}
\title{Get Survey Biomass Index}
\description{
  Extract survey biomass index series from \code{aggrVisit} dataframe,
  for a given species, size group, and survey.
}
\usage{
surveyIndex(tegund, lengd=NULL, sur="smb", data=aggrVisit)
}
\arguments{
  \item{tegund}{species name, like \code{"ufsi"}.}
  \item{lengd}{minimum length group to include in the biomass index, or
    \code{NULL} to include all lengths.}
  \item{sur}{survey name, either \code{"smb"} or \code{"smh"}.}
  \item{data}{main dataframe, like \code{aggrVisit}.}
}
\value{
  Dataframe with eight columns:
  \item{ar}{year}
  \item{bio.staerri}{biomass index}
  \item{cv.bio.staerri}{cv}
  \item{svaedi}{area}
  \item{svaedisnr}{area code}
  \item{lengd}{length}
  \item{tegund}{species}
  \item{sur}{survey name}
}
\author{Arni Magnusson.}
\seealso{
  \code{\link{[}} is used to extract the relevant rows and columns.

  \code{\link{hafroAssmt-package}} gives an overview of the package.
}
\examples{
aggrDemo <- data.frame(fj=0, cv.fj=NA,
                       bio.staerri=round(1e5*exp(rnorm(36,sd=0.3)),1),
                       cv.bio.staerri=round(runif(36,0.05,0.5),3),
                       fj.minni=0, cv.fj.minni=NA, fj.haenga=0,
                       cv.fj.haenga=NA, fj.hrygna=0, cv.fj.hrygna=NA,
                       svaedi=c("N","S","Heild"), svaedisnr=13:15,
                       lengd=rep(7:9,each=3), ar=rep(2001:2004,each=9),
                       bio=0, tegund="daemi", sur="smb")
surveyIndex("daemi", data=aggrDemo)
surveyIndex("daemi", data=aggrDemo, div=1e3)

## Typical usage requires aggrVisit:
##
## surveyIndex("ufsi", 55)
}
\keyword{manip}
\keyword{list}
