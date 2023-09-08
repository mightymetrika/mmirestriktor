#' Convert Newline Characters to HTML Line Breaks
#'
#' This function takes a string and replaces all newline characters (`\n`) with
#' HTML line break tags (`<br/>`), which can be useful when you want to render
#' text with preserved line breaks in a web interface, such as a Shiny app.
#'
#' @param x A character vector where each element is a string in which newline
#'   characters should be replaced with `<br/>` tags. The function vectorizes
#'   over the character vector, replacing newline characters in each string.
#'
#' @return A character vector of the same length as `x`, where each string has
#'   newline characters replaced with `<br/>` tags.
#'
#' @keywords internal
nl2br <- function(x) {
  gsub("\n", "<br/>", x)
}
