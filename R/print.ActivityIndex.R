#' Print method for token
#'
#' @return NULL
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods
#' @export
#'
#' @examples
#' x = data.frame(RecordNo = rnorm(100), AI = rnorm(100))
#' class(x) = c("ActivityIndex", class(x))
#' print(x)
#' @importFrom utils head tail
#' @method print ActivityIndex
print.ActivityIndex = function(x, ...) {
  x = as.data.frame(x)
  cat("Showing head and tail rows\n")
  print(head(x), ...)
  print(tail(x), ...)
}
