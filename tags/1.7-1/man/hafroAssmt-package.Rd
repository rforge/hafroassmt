\name{hafroAssmt-package}
\alias{hafroAssmt-package}
\alias{hafroAssmt}
\docType{package}
\title{Hafro Assessment Toolbox}
\description{
  Functions used in fisheries stock assessment at Hafro, the Marine
  Research Institute in Iceland.
}
\details{
  \emph{Look up codes:}
  \tabular{ll}{
    \code{\link{veidarfaeri}}\tab look up fishing gear\cr
  }
  \emph{Survey data:}
  \tabular{ll}{
    \code{\link{surveyIndex}}\tab get survey biomass index\cr
  }
  \emph{Models:}
  \tabular{ll}{
    \code{\link{n2f}}     \tab \eqn{F} from numbers at age\cr
    \code{\link{pope}}    \tab Pope's cohort analysis     \cr
    \code{\link{shepherd}}\tab Shepherd-Nicholson sigma   \cr
  }
  \emph{Plot functions:}
  \tabular{ll}{
    \code{\link{endspace}} \tab barplot margins   \cr
    \code{\link{labit}}    \tab create label      \cr
    \code{\link{residplot}}\tab plot CA residuals \cr
    \code{\link{zlines}}   \tab add diagonal lines\cr
  }
  \emph{Example datasets:}
  \tabular{ll}{
    \code{\link{catage}} \tab saithe catch and weight at age\cr
  }
}
\note{
  \code{browseVignettes()} shows the vignette, found in the
  \file{hafroAssmt/doc} directory.
}
\author{
  Arni Magnusson, Sigurdur Thor Jonsson, Hoskuldur Bjornsson, and Birkir
  Bardarson.
}
\references{
  \url{http://hafroassmt.r-forge.r-project.org}.

  \url{http://www.hafro.is}.
}
% Programming
\keyword{utilities}
% Statistics
\keyword{models}
