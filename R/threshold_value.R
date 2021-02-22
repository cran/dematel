#' Threshold value
#'
#' Returns threshold value of direct relationship decision matrix
#'
#' @param x a matrix containing the values of direct relationship decision matrix.
#' @param data_control is a pre-defined logical parameter that whether data should checked.
#'
#' @return This function returns a \code{num}
#'
#' @author Muhlis Ozdemir <muhlisoz@gmail.com>
#'
#' @export
#' @examples
#' threshold_value(dematel::hospitaldata)
#' threshold_value(dematel::nurseselection)
#' threshold_value(dematel::medicaldevice)
threshold_value <- function(x, data_control = TRUE) {

  if (data_control == TRUE) {
    x <- check_data(x)
  }

  return(mean(total_relationship_matrix(x, data_control = data_control)))
}
