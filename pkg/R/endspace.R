endspace <- function(x,space=0.2)
{
  barwidth <- ((max(x)-min(x))/length(x))
  x1 <- min(x)-(barwidth/2)-(barwidth*space)
  x2 <- max(x)+(barwidth/2)+(barwidth*space)
  out <- c(x1,x2)
  return(out)
}
