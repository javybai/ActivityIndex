#' @title \eqn{\bar{\sigma}_i} computing using raw accelerometry data
#' @description \code{Sigma0} computes \eqn{\bar{\sigma}_i}, which is needed for
#'  the Activity Index computing in \code{\link{computeActivityIndex}}
#' @param x A 4-column data frame containing the raw accelerometry
#'  data when the device is not worn. The 1st column has the record/index
#'  number. The 2nd to 4th columns contain the tri-axial raw acceleration. The
#'  data will be used to calculate \eqn{\bar{\sigma}_i}.
#' @param hertz The sample rate of the data.
#' @return \eqn{\bar{\sigma}_i}, a numeric vector of length one.
#' @export
#' @examples
#' filename = system.file("extdata","sample_GT3X+.csv.gz",package="ActivityIndex")
#' res = ReadGT3XPlus(filename)
#' hertz = res$Hertz
#' x = res$Raw[ 1:1000, c("Time", "X", "Y", "Z")]
#' res = Sigma0(x, hertz = hertz)
#' testthat::expect_true(res ==  0.1843216371355338723)
Sigma0 = function(x, hertz = 30)
{
  # stopifnot(is.data.table(x))
  x = as.data.table(x)
  X = Y = Z = NULL
  rm(list = c("X", "Y", "Z"))
  n = nrow(x) %/% hertz*hertz
  result = mean(x[1:n,
                sqrt((rowVars(matrix(X, ncol = hertz, byrow=TRUE))+
                        rowVars(matrix(Y, ncol = hertz, byrow=TRUE))+
                        rowVars(matrix(Z, ncol = hertz, byrow=TRUE)))/3)]
                )
  names(result) = c("SD")
  return(result)
}
