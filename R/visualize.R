#' Causal Diagram
#'
#' Returns Causal Diagram of criteria
#'
#' @param x a matrix containing the values of direct relationship decision matrix.
#' @param data_control is a pre-defined logical parameter that whether data should checked.
#'
#' @return This function returns a graph
#'
#' @author Muhlis Ozdemir <muhlisoz@gmail.com>
#'
#' @import ggplot2
#' @export
#' @examples
#' visualize(dematel::hospitaldata)
#' visualize(dematel::nurseselection)
#' visualize(dematel::medicaldevice)

visualize <- function(x, data_control = TRUE) {

  if (data_control == TRUE) {
    x <- check_data(x)
  }

  inputs <- relationships_between_criteria(x, data_control = data_control)

  ggplot(data = inputs, aes(x = inputs[,3], y = inputs[,4])) +
    geom_point() +
    geom_text(label = rownames(inputs), nudge_y = 0.05) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    xlab("ci+ri") +
    ylab("ci-ri") +
    ggtitle("Causal Relations among the Criteria Diagram") +
    theme_light()
}
