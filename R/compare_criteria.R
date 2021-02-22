#' Relation results
#'
#' Returns relation results that exceed threshold value of direct relationship decision matrix
#'
#' @param x a matrix containing the values of direct relationship decision matrix.
#' @param data_control is a pre-defined logical parameter that whether data should checked.
#'
#' @return This function returns a \code{num} matrix.
#'
#' @author Muhlis Ozdemir <muhlisoz@gmail.com>
#'
#' @export
#'
#' @seealso \link[base]{apply} function.
#'
#' @examples
#' compare_criteria(dematel::hospitaldata)
#' compare_criteria(dematel::nurseselection)
#' compare_criteria(dematel::medicaldevice)
compare_criteria <- function(x, data_control = TRUE) {

  if (data_control == TRUE) {
    x <- check_data(x)
  }

  get_total_relationship_matrix <- total_relationship_matrix(x, data_control = data_control)
  get_threshold_value <- threshold_value(x, data_control = data_control)

  compare <- function(x, get_threshold_value){
    names(which(x > get_threshold_value))
    x[which(x > get_threshold_value)]
  }

  return(apply(get_total_relationship_matrix, 1, compare, get_threshold_value))

}
