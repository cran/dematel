#' Complete Dematel Analysis
#'
#' Executes all functions and conducts dematel analysis at once
#'
#' @param x a matrix containing the values of normalized initial direct-relation decision matrix.
#'
#' @return This function executes all functions, conducts dematel analysis at once and returns
#' a \code{matrix} that contains data,
#' a \code{matrix} that contains normalized data,
#' a \code{matrix} that contains normalized initial direct-relation matrix,
#' a \code{data.frame} that contains relationships between criteria,
#' a \code{graph},
#' a \code{num} that contains threshold value,
#' a \code{list} of criteria comparisons.
#'
#' @export
#'

execute_dematel <- function(x) {
  x <- check_data(x)
  return(list(
    data = x,
    normalized_data = normalize_data(x, data_control = FALSE),
    normalized_initial_direct_relation_matrix = total_relationship_matrix(x, data_control = FALSE),
    relationships_between_criteria = relationships_between_criteria(x, data_control = FALSE),
    graph = visualize(x, data_control = FALSE),
    threshold_value = threshold_value(x, data_control = FALSE),
    comparison_of_criteria = compare_criteria(x, data_control = FALSE)
  ))
}

