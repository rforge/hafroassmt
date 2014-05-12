labit <- function(x, y, bold=FALSE, sep=" ")
{
  if(bold)
    output <- bquote(paste(bold(.(x)), .(sep), italic(.(y))))
  else
    output <- bquote(paste(.(x), .(sep), italic(.(y))))
  output <- as.expression(output)

  return(output)
}
