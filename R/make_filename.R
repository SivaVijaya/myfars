#' Makes a filename formatted with the given year input
#' @description The function make_filename creates a filename
#' with the given year make_filename takes string year as input and appends that year to the
#' filename
#' @param year as a string.
#' @return file name with the year attached
#' @examples
#' make_filename("2013")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
