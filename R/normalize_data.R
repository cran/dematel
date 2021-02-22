#' Normalize Data
#'
#' Normalizes matrix format data
#'
#' @param x a matrix containing the values of direct relationship decision matrix.
#' @param data_control is a pre-defined logical parameter that whether data should checked.
#'
#' @return This function returns a \code{list} of data, and normalized matrix.
#'
#' @export
#'
normalize_data <- function(x , data_control = TRUE) {

  if (data_control == TRUE) {
    x <- check_data(x)
  }

  sum_of_rows <- apply(x, 1, sum)
  sum_of_columns <- apply(x, 2, sum)
  maximum_of_rows_and_column <- max(sum_of_rows, sum_of_columns)
  return(normalized_matrix = round(x/maximum_of_rows_and_column, 5))
}

