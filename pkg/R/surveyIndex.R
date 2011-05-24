surveyIndex <- function(tegund, lengd=NULL, sur="smb", data=aggrVisit)
{
  output <- data[data$tegund==tegund & data$svaedi=="Heild" & data$sur==sur,]

  if(is.null(lengd))
    lengd <- min(output$lengd)
  output <- output[output$lengd==lengd,]
  output <- output[c("ar","bio.staerri","cv.bio.staerri","svaedi","svaedisnr","lengd","tegund","sur")]

  row.names(output) <- seq_along(row.names(output))

  return(output)
}
