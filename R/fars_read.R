#' read in a csv file as a tibble "data"
#' @description The fars_read function wraps the read_csv function.
#' This function takes a file in csv format reads it and returns
#' a tibble data frame if the file exists. Otherwise the error
#' message filename does not exist will be displayed.
#'
#' @param filename Name of the csv file as a string.
#' @return tibble data frame
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom readr read_csv function
#' @importFrom dplyr tbl_df function
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
