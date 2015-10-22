#' @title \eqn{\bar{\sigma}_i} computing using raw accelerometry data
#' @description \code{Sigma0} computes \eqn{\bar{\sigma}_i}, which is needed for
#'  the Activity Index computing in \code{\link{computeActivityIndex}}
#' @param \code{x} A 4-column data frame containing the raw accelerometry
#'  data when the device is not worn. The 1st column has the record/index 
#'  number. The 2nd to 4th columns contain the tri-axial raw acceleration. The 
#'  data will be used to calculate \eqn{\bar{\sigma}_i}.
#' @param \code{hertz} The sample rate of the data.
#' @return \eqn{\bar{\sigma}_i}.
#' @export

Sigma0=function(x,hertz=30)
{
  n=nrow(x)%/%hertz*hertz
  result=mean(x[1:n,
                sqrt((rowSds(matrix(X,ncol=hertz,byrow=TRUE))^2+
                      rowSds(matrix(Y,ncol=hertz,byrow=TRUE))^2+
                      rowSds(matrix(Z,ncol=hertz,byrow=TRUE))^2)/3)])
  names(result)=c("SD")
  return(result)
}