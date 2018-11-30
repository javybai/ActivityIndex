#' @title Read the raw tri-axial accelerometry data csv file.
#'
#' @description
#' \code{ReadTable} reads the raw tri-axial accelerometry data in csv files.
#'
#' @details
#' The function reads csv files containing only three columns: acceleration time series
#' of x-, y- and z-axis.
#'
#' @param filename
#' The name of the csv file.
#'
#' @return The \code{ReadTable} returns a data frame with 4 columns: Index, X, Y and Z.
#' Index is the column for the indices of acceleration. X, Y and Z are for the acceleration
#' time series in each direction.
#' @export
#' @importFrom utils read.csv
#' @examples
#' filename = system.file("extdata","sample_table.csv.gz",package="ActivityIndex")
#' res = ReadTable(filename)
#'
ReadTable = function(filename) {
  if (ncol(read.csv(file = filename,
                    stringsAsFactors = FALSE,
                    header = FALSE,
                    nrows = 1)) != 3)
  {
    stop(paste0(filename, " is not an appropriate 3-column data file!"))
  }
  result = fread(
    filename,
    sep = ",",
    stringsAsFactors = FALSE,
    colClasses = rep("numeric", 3),
    header = FALSE,
    showProgress = FALSE
  )
  result = cbind(1:nrow(result), result)
  colnames(result) = c("Index", "X", "Y", "Z")
  return(result)
}
