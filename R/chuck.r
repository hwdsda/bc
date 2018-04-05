#' @title A function to divide a data frame (x) into equal row number (n) of data frames (by row)
#'
#' @description A function to divide a data frame (x) into equal row number (n) of data frames (by row).
#'
#' @param x, n
#'
#' @return a list of data frames
#'
#' @examples chunk(df, 3)
#'
#' @export

chunk <- function(x,n) split(x, factor(sort(seq_len(nrow(x)) %% n)))
