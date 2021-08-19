#' Summarises the fatalities by each month of the years
#' @description The fars_summarize_years function gives the total number
#' of fatalities for each month of given years as input
#' fars_summarize_years summarises the fatalities each month for
#' all the given years
#' @param vector of years
#' @return the summary of fatalities by month and year
#' @examples
#' \dontrun{
#' fars_summarize_years <- function(years = c(2013, 2014, 2015))
#' }
#' @inheritParams fars_read_years
#' @importFrom bind  group_by summarize from dplyr package
#' @importFrom spread from tidyr package
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
