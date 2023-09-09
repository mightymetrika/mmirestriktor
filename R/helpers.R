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

#' Convert a Character String to a List
#'
#' This internal function converts a character string representing R arguments
#' into a list of arguments. It is primarily used to facilitate the passing of
#' additional arguments from the Shiny UI to internal functions within the app.
#'
#' @param arg_str A character string representing R arguments.
#'
#' @return A list containing the arguments represented by \code{arg_str}. If
#'   \code{arg_str} is not a valid representation of R arguments, the function
#'   will throw an error.
#'
#' @keywords internal
str2list <- function(arg_str) {
  # Evaluate the string in a new environment to convert it to a list
  eval(parse(text = paste0("list(", arg_str, ")")), envir = new.env())
}
