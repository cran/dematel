#' Relationship matrix
#'
#' Returns total relationship matrix of direct relationship decision matrix
#'
#' @param x a matrix containing the values of direct relationship decision matrix.
#' @param data_control is a pre-defined logical parameter that whether data should checked.
#'
#' @return This function returns a \code{matrix}
#'
#' @author Muhlis Ozdemir <muhlisoz@gmail.com>
#'
#' @seealso \code{\link{apply}} function.
#'
#' @export
#'
#' @examples
#' total_relationship_matrix(dematel::hospitaldata)
#' total_relationship_matrix(dematel::nurseselection)
#' total_relationship_matrix(dematel::medicaldevice)
total_relationship_matrix <- function(x, data_control = TRUE) {

  if (data_control == TRUE) {
    x <- check_data(x)
  }

  get_normalized_matrix <- normalize_data(x, data_control = data_control)
  return(get_normalized_matrix %*% solve((diag(nrow(x)) - get_normalized_matrix)))
}

