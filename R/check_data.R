#' Data checker
#'
#' Throws an error message if data is not matrix format, checks names attribute of the matrix, assign new ones if not defined
#'
#' @param x numeric values containing the data of direct relationship decision matrix.
#'
#' @return This function checks whether \code{data} is matrix or not. Returns a matrix and assign new names if not defined.
#'
#' @author Muhlis Ozdemir <muhlisoz@gmail.com>


check_data = function(x) {

  # check data whether it is matrix format
  if (!is.matrix(x)) {
    message("Data converted to matrix format. Row and column names are renamed as C*")
    x = as.matrix(x)
  }

  rownames(x) = paste("C",1:nrow(x), sep = "")

  colnames(x) = paste("C",1:ncol(x), sep = "")

  return(x)
}
