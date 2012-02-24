\name{shepherd}
\alias{shepherd}
\title{Shepherd-Nicholson Sigma}
\description{
  Estimate Shepherd-Nicholson sigma for a given catch-at-age matrix.
}
\usage{
shepherd(x, plot=FALSE)
}
\arguments{
  \item{x}{Catch at age, as a \code{data.frame} (year, age, frequency in
    columns 1--3), \code{matrix} (years in rows, ages in columns), or
    \code{table} (years in rows, ages in columns).}
  \item{plot}{Whether to plot model fit to data.}
}
\value{
  List containing
  \item{model}{\code{lm} model.}
  \item{fit}{\code{data.frame} containing \code{Year, Age, Series,
      Freq}.}
  \item{res}{\code{matrix} of residuals.}
  \item{age.sigma}{standard deviation of log residuals within each age.}
  \item{year.sigma}{standard deviation of log residuals within each
    year.}
  \item{sigma}{standard deviation of all log residuals.}
}
\note{
  A common purpose of fitting the Shepherd-Nicholson model is to
  evaluate a simple diagnostic of commercial and survey catch-at-age,
  describing how well the data conform to a set of basic assumptions.
  The scale-free sigmas can be compared between ages, years, datasets,
  and species.

  The resulting sigmas are maximum likelihood estimates, with the usual
  negative bias. They combine observation noise, process variability,
  and model misspecification.
}
\author{Sigurdur Thor Jonsson and Arni Magnusson.}
\references{
  Shepherd, J.G. and M.D. Nicholson. 1991. Multiplicative modelling of
  catch-at-age data, and its application to catch forecasts.
  \cite{Journal du Conseil international pour l'Exploration de la Mer}
  47:284--294.
}
\seealso{
  \code{\link{pope}} is a simple assessment model to estimate stock
  size.

  \code{\link{hafroAssmt-package}} gives an overview of the package.
}
\examples{
shepherd(catage)
shepherd(catage, plot=TRUE)

shepherd(catage)$sigma                 # MLE
summary(shepherd(catage)$model)$sigma  # unbiased estimate
}
% Graphics
\keyword{models}
