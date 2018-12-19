#' @title Compute Activity Index
#' @description \code{computeActivityIndex} computes the Activity Index using raw
#' accelerometry data, based on user specified parameters such as sample rate
#' and epoch length.
#'
#' @param x An object containing raw accelerometry data, which could
#' either be a 4-column data frame or "\code{GT3XPlus}" object. See "Details".
#' @param x_sigma0 A 4-column data frame containing the raw accelerometry
#'  data when the device is not worn. The 1st column has the record/index
#'  number. The 2nd to 4th columns contain the tri-axial raw acceleration. The
#'  data will be used to calculate \eqn{\bar{\sigma}_i}.
#' @param sigma0 Specify \eqn{\bar{\sigma}_i} directly. At least one of
#' \code{x_sigma0} and \code{sigma0} must be specified. If both existed,
#' \code{sigma0} will be used.
#' @param epoch The epoch length (in second) of the Activity Index. Must
#' be a positive integer.
#' @param hertz The sample rate of the data.
#' @return A data frame with two columns. The first column has the "record
#' number" associated with each entry of Activity Index, while the second column
#'  has the actual value of Activity Index.
#' @details \code{x} could be either of the following two types of objects:
#' \enumerate{
#'    \item A 4-column data frame containing the tri-axial raw accelerometry
#'    data in the 2nd to 4th column, and the associated record number (could be
#'    time) in the 1st column. \code{\link{ReadTable}} can be used to generate
#'    such data frame.
#'    \item An "\code{GT3XPlus}" object given by function
#'    \code{\link{ReadGT3XPlus}}.
#'    }
#' @examples
#' library(graphics)
#' fname = system.file("extdata", "sample_table.csv.gz",
#' package = "ActivityIndex")
#' sampleTable = ReadTable(fname)
#' AI_sampleTable_x = computeActivityIndex(
#'   sampleTable,
#'   x_sigma0 = sampleTable[1004700:1005600, ],
#'   epoch = 1,
#'   hertz = 30)
#' AI_sampleTable_x
#' plot(AI ~ RecordNo, data = AI_sampleTable_x, type = "l")
#' @export
computeActivityIndex=function(x,x_sigma0=NULL,sigma0=NULL,epoch=1,hertz)
{
  if (epoch<1) stop("epoch must not be less than 1!")
  if (abs(round(epoch)-epoch)>0) stop("epoch must be an integer!")
  UseMethod("computeActivityIndex",x)
}


#' @rdname computeActivityIndex
#' @export
computeActivityIndex.default=function(x,x_sigma0=NULL,sigma0=NULL,epoch=1,hertz)
{
  Index = NULL
  rm("Index")
  if (is.null(x_sigma0)&&is.null(sigma0)) stop("Either x_sigma0 or sigma0 needs to be specified!")
  if (is.null(sigma0))
  {
    sigma0=Sigma0(x_sigma0,hertz)
  }
  x = as.data.table(x)
  n=nrow(x)%/%hertz*hertz
  result=array(0,c(n%/%hertz,2))
  if (sigma0!=0)
  {
    result[,2]=x[1:n,(rowSds(matrix(x$X,ncol=hertz,byrow=TRUE))^2-sigma0^2)/sigma0^2+
                      (rowSds(matrix(x$Y,ncol=hertz,byrow=TRUE))^2-sigma0^2)/sigma0^2+
                      (rowSds(matrix(x$Z,ncol=hertz,byrow=TRUE))^2-sigma0^2)/sigma0^2]
  } else
  {
    result[,2]=x[1:n,rowSds(matrix(x$X,ncol=hertz,byrow=TRUE))^2+
                        rowSds(matrix(x$Y,ncol=hertz,byrow=TRUE))^2+
                        rowSds(matrix(x$Z,ncol=hertz,byrow=TRUE))^2]
  }
  result[which(result[,2]<0),2]=0
  result[,2]=sqrt(result[,2]/3)
  result=as.data.frame(result,stringsAsFactors=FALSE)
  # is Index a column name or some other thing?
  result[,1]=x[(1:(n%/%hertz)-1)*hertz+1,Index] # fetch "Index"
  if (epoch>1)
  {
    L_AI=length(result[,2])
    result0=as.data.frame(array(0,c(L_AI%/%epoch,2)),stringsAsFactors=FALSE)
    result0[,2]=as.numeric(rowSums(matrix(result[1:(L_AI-L_AI%%epoch),2],ncol=epoch,byrow=TRUE)))
    result0[,1]=result[(1:(L_AI%/%epoch)-1)*epoch+1,1]
    result=result0
  }
  colnames(result)=c("RecordNo","AI")
  class(result) = c("ActivityIndex", class(result))
  return(result)
}

#' @rdname computeActivityIndex
#' @export
computeActivityIndex.GT3XPlus=function(x,x_sigma0=NULL,sigma0=NULL,epoch=1,hertz)
{
  Time = NULL
  rm("Time")
  X = Y = Z = NULL
  rm(list = c("X", "Y", "Z"))

  if (x$Hertz!=hertz) stop("hertz must be equal to the sample rate of GT3XPlus!")
  if (is.null(x_sigma0)&&is.null(sigma0)) stop("Either x_sigma0 or sigma0 needs to be specified!")
  if (is.null(sigma0))
  {
    sigma0=Sigma0(x_sigma0,hertz)
  }
  # Note that x here is a GT3XPlus subject, while x$Raw is our data table
  # x$Raw has five columns: Date, Time, X, Y, Z
  n=nrow(x$Raw)%/%hertz*hertz
  result=array(0,c(n%/%hertz,2))
  if (sigma0!=0)
  {
    result[,2]=x$Raw[1:n,(rowSds(matrix(X,ncol=hertz,byrow=TRUE))^2-sigma0^2)/sigma0^2+
                   (rowSds(matrix(Y,ncol=hertz,byrow=TRUE))^2-sigma0^2)/sigma0^2+
                   (rowSds(matrix(Z,ncol=hertz,byrow=TRUE))^2-sigma0^2)/sigma0^2]
  } else
  {
    result[,2]=x$Raw[1:n,rowSds(matrix(X,ncol=hertz,byrow=TRUE))^2+
                   rowSds(matrix(Y,ncol=hertz,byrow=TRUE))^2+
                   rowSds(matrix(Z,ncol=hertz,byrow=TRUE))^2]
  }
  result[which(result[,2]<0),2]=0
  result[,2]=sqrt(result[,2]/3)
  result=as.data.frame(result,stringsAsFactors=FALSE)
  result[,1]=x$Raw[(1:(n%/%hertz)-1)*hertz+1,Time] # Fetch "Time"
  if (epoch>1)
  {
    L_AI=length(result[,2])
    result0=as.data.frame(array(0,c(L_AI%/%epoch,2)),stringsAsFactors=FALSE)
    result0[,2]=as.numeric(rowSums(matrix(result[1:(L_AI-L_AI%%epoch),2],ncol=epoch,byrow=TRUE)))
    result0[,1]=result[(1:(L_AI%/%epoch)-1)*epoch+1,1]
    result=result0
  }
  colnames(result) = c("RecordNo","AI")
  class(result) = c("ActivityIndex", class(result))
  return(result)
}
