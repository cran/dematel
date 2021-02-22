#' Relationships between criteria
#'
#' Returns total relationships between criteria data.frame
#'
#' @param x a matrix containing the values of direct relationship decision matrix.
#' @param data_control is a pre-defined logical parameter that whether data should checked.
#'
#' @return This function returns a \code{data.frame}
#'
#' @author Muhlis Ozdemir <muhlisoz@gmail.com>
#'
#' @export
#'
#' @seealso \link[base]{apply}
#'
#' @examples
#' relationships_between_criteria(dematel::hospitaldata)
#' relationships_between_criteria(dematel::nurseselection)
#' relationships_between_criteria(dematel::medicaldevice)

relationships_between_criteria <- function(x, data_control = TRUE) {

  if (data_control == TRUE) {
    x <- check_data(x)
  }

  get_relationship_matrix <- total_relationship_matrix(x, data_control = data_control)

  ci <- apply(get_relationship_matrix, 1, sum)
  ri <- apply(get_relationship_matrix, 2, sum)
  determination_of_relationships_between_criteria_matrix <- data.frame(ci, ri, ci + ri, ci - ri)
  colnames(determination_of_relationships_between_criteria_matrix) <- c("ci", "ri", "c+r", "c-r")
  return(determination_of_relationships_between_criteria_matrix)
}
