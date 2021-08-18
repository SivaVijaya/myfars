#' Reads the years and gives fatalities by month and year
#' @description The function fars_read_years list the fatality data for each
#' month in the years given
#' fars_read_years function takes vector of years as input
#' and selects the MONTH and year columns and lists tibble data
#' frame for each year
#' @param vector of years
#'
#' @return lists of tibble dataframes selecting the fatal accident
#' data for each month in the given years. If the year is not matching
#' it gives the warning 'invalid year'
#' @inheritParams make_filename
#' @inheritParams fars_read
#' @examples
#' fars_read_years(2013)
#'
#' @importFrom mutate and select from dplyr package
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
