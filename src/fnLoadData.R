fnLoadData <- function(vstrFileName) {
  vTDados <- read_excel(vstrFileName, col_types = c("text", 
                                                  "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric"))
  return(vTDados)
}