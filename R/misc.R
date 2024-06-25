#' Determine is well name is in A01 format
#'
#' This is just guessing by looking for the A:N format.  It can be fooled.
#'
#' @export
#' @param x string, one or more well names
#' @return logical, TRUE if the input is in A01 format
is_A01 = function(x = c("A01", "A:1", "P:24")){
  pattern = "[[:alpha:]][[:punct:]]([[:digit:]]+)"
  grepl(pattern, x)
}