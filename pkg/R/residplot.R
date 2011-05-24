residplot <- function(residuals, xpos, ypos, maxsize=0.2, poscol=2, linecol=1, lwd=1, n=50, maxn, negcol, txt=FALSE,
                      csi=0.1, xlab="", ylab="", axes=TRUE, arg=TRUE, argcol=20, arglty=2, cn=c("x","y","z"),
                      append=FALSE)
{
  par(err=-1)

  if(is.data.frame(residuals))
  {
    x <- residuals[,cn[1]]
    y <- residuals[,cn[2]]
    residuals <- residuals[,cn[3]]
  }
  else
  {
    residuals <- t(residuals)
    if(missing(maxn))
      maxn <- max(abs(residuals), na.rm=TRUE)
    if(missing(xpos))
      xpos <- 1:nrow(residuals)
    if(missing(ypos))
      ypos <- 1:ncol(residuals)
    x <- matrix(xpos, length(xpos), length(ypos))
    y <- matrix(ypos, length(xpos), length(ypos), byrow=TRUE)
  }

  if(!append)
    plot(x, y, type="n", xlab=xlab, ylab=ylab, axes=axes)

  x.bck <- x
  y.bck <- y

  if(arg)
  {
    r <- x.bck-y.bck
    tmp <- unique(r)
    for(i in 1:length(tmp))
    {
      j <- r==tmp[i]
      lines(x.bck[j], y.bck[j], col=argcol, lty=arglty)
    }
  }

  plt <- par()$pin
  xscale <- (par()$usr[2]-par()$usr[1]) / plt[1] * maxsize
  yscale <- (par()$usr[4]-par()$usr[3]) / plt[2] * maxsize
  rx <- c(unlist(sqrt(abs(residuals)/maxn)*xscale))
  ry <- c(unlist(sqrt(abs(residuals)/maxn)*yscale))
  theta <- seq(0, 2*pi, length=n)
  n1 <- length(rx)
  theta <- matrix(theta, n1, n, byrow=TRUE)
  x <- matrix(x, n1, n)
  y <- matrix(y, n1, n)
  rx <- matrix(rx, n1, n)
  ry <- matrix(ry, n1, n)
  x <- x + rx*cos(theta)
  y <- y + ry*sin(theta)
  x <- cbind(x, rep(NA, nrow(x)))
  y <- cbind(y, rep(NA, nrow(y)))

  i <- residuals>0
  if(any(i))
  {
    polygon(c(t(x[i,])), c(t(y[i,])), col=poscol)
    lines(c(t(x[i,])), c(t(y[i,])), col=linecol, lwd=lwd)
  }

  i <- residuals<0
  if(any(i))
  {
    if(!missing(negcol))
      polygon(c(t(x[i,])), c(t(y[i,])), col=negcol)
    lines(c(t(x[i,])), c(t(y[i,])), col=linecol, lwd=lwd)
  }

  if(txt)
    text(x.bck, y.bck, as.character(round(residuals)), csi=csi)

  return(invisible())
}
