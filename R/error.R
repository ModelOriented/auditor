checkDataConsistency <- function(data, y){
  if (nrow(data) != length(y)) stop("Data and y are not consistent.", call. = F)
}
