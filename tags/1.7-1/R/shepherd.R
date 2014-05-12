shepherd <- function(x, plot=FALSE)
{
  if(is.matrix(x))
  {
    years <- as.integer(rownames(x))
    ages <- as.integer(colnames(x))
    x <- data.frame(rep(years,each=length(ages)), rep(ages,length(years)), as.vector(t(x)))
  }
  else
  {
    years <- unique(x[[1]])
    ages <- unique(x[[2]])
    x <- x[order(x[[1]],x[[2]]),]
  }
  names(x)[1:3] <- c("Year", "Age", "Freq")
  x$Cohort <- x$Year - x$Age

  model <- lm(log(Freq)~factor(Year)+factor(Age)+factor(Cohort), data=x)
  fit <- data.frame(Year=x$Year, Age=x$Age, Series=rep(c("Obs","Fit"),each=nrow(x)), Freq=c(x$Freq,exp(model$fit)))
  res <- matrix(model$residuals, ncol=length(ages), dimnames=list(years,ages))
  age.sigma <- apply(res, 2, function(x) sqrt(sum(x^2)/length(x)))
  year.sigma <- apply(res, 1, function(x) sqrt(sum(x^2)/length(x)))
  sigma <- sqrt(sum(res^2)/length(res))

  output <- list(model=model, fit=fit, res=res, age.sigma=age.sigma, year.sigma=year.sigma, sigma=sigma)

  if(plot)
  {
    print(xyplot(Freq~Age|factor(Year), data=fit, panel=panel.superpose.2, groups=Series, type=c("l","p"),
                 as.table=TRUE, strip=strip.custom(par.strip.text=list(cex=0.8),bg="gray95"), col.symbol="black",
                 col.line="gray50", lwd=3))
  }

  print(list(age.sigma=age.sigma, year.sigma=year.sigma, sigma=sigma))
  invisible(output)
}
