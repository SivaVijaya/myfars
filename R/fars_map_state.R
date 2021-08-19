#' Plots the fatal accidents by location on the state
#' @description The fars_map_state function maps the fatality of the state
#' for a given year
#' fars_map_state function takes the state number and year as input
#' and maps the fatality data based on the location
#' @param state number and year
#' @return plot of fatalities for the given state and year
#' if the state number is invalid it gives error message
#' "invalid STATE number: "
#'
#'  @example
#'  \dontrun{
#'   fars_map_state(25,2013)}
#'
#' @importFrom filter from dplyr package
#' @importFrom map from maps package
#' @importFrom points from braphics package
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })

}
