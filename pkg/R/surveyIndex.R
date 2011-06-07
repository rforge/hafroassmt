surveyIndex <- function(tegund, lengd=NULL, sur="smb", div=1, data=aggrVisit)
{
  ## 1  Filter species and survey, select total area
  output <- data[data$tegund==tegund & data$sur==sur & data$svaedi=="Heild",]

  ## 2  Filter size group, select columns
  if(is.null(lengd))
    lengd <- min(output$lengd)
  output <- output[output$lengd==lengd,]
  output <- output[c("ar","bio.staerri","cv.bio.staerri","svaedi","svaedisnr","lengd","tegund","sur")]

  ## 3  Scale values, rename rows
  output$bio.staerri <- output$bio.staerri / div
  row.names(output) <- seq_along(row.names(output))

  return(output)
}
