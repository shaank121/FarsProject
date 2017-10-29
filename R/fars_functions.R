#' Read file of Fatality Analysis and Reporting System
#'
#' This function reads the file as csv as a dataframe and prompt error if file does not existi
#'
#' @param filename to be printed
#'
#' @return returns the dataframe from read file.
#'
#' @examples
#' setwd(system.file("extdata", package = "FarsProject"))
#' fars_read('accident_2013.csv.bz2')
#'
#' @importFrom dplyr tbl_df
#'
#' @importFrom readr read_csv
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create the FARS filename using year
#'
#' It will make the filename with year embeded in its name
#'
#' @param year to be embeded in the filename
#'
#' @return print the new file name with year from parameter added to it.
#'
#'
#'

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' Read years data from Fatality Analysis and Reporting system
#'
#' Reads the month and year data from the file
#'
#' @param years - year for which data need to be extracted
#'
#' @return returns the dataframe from read file.
#'
#'
#'
#' @importFrom dplyr select mutate
#' @importFrom readr read_csv
#' @importFrom tidyr %>%
#'
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


#' Summarizes the FARS data for a given year
#'
#' It summarizes the monthly accident in a given year
#'
#' @param year -year for which summarization need to be done
#' @return return N X 2 dataframe with month and number of accident
#'
#' @examples
#' setwd(system.file("extdata", package = "FarsProject"))
#' fars_summarize_years(2013)
#'
#' @importFrom dplyr summarize bind_rows group_by summarise
#'
#' @importFrom readr read_csv
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot accident for given state in a given year
#' This function plots the accident in a given state for a given year.
#' @param unique state number - numeric
#' @param valid year - numeric
#'
#' @return
#' 1. plotted map of accident for the year
#' 2. "invalid state" if state number not found
#' 3. "No accident to plot " if for a given year and given state no accident occurred.
#'
#' @examples
#' setwd(system.file("extdata", package = "FarsProject"))
#' fars_map_state(050,2014)
#'
#' @importFrom dplyr filter
#' @importFrom readr read_csv
#' @importFrom graphics  points
#' @importFrom maps  map
#' @export
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
