#' @title A .csv file reader
#'
#' @param csv The filename to be read in
#'
#' @return A dataframe populated by the data from the .csv file
#' @export
#'
#' @examples
#' \dontrun{file <- "SPRUCE.csv"; myread(file)}
myread=function(csv){
  dird="D:\\Libraries\\Documents\\Stats\\Labs\\DATAxls\\"
  fl=paste(dird,csv,sep="")
  utils::read.table(fl,header=TRUE,sep=",")
}
