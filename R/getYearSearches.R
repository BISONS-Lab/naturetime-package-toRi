#' Extract relative search interest from Google Trends for a target year and a temporal horizon around it
#'
#' @param focus.year A numeric value indicating the focus year to scan for temporal search interest. This parameter is bounded between 2004 (first year of available data) and the previous year from the current calendar year to ensure complete search data for the whole year.
#' @param search.horizon A numeric value indicating the the number of years around the focus year to scope for interest. Accepts values equal or higher than 1, up to 25 (due to character limits imposed by Google trends). Defaults to 1 indicating a search horizon of one year to scope before and after the target year (i.e., assessing interest in the years immediately before and after the focus year). When 'search.horizon' > 1, search interest metrics are calculated separately for the years immediately adjacent to the focus year (i.e., one year before and after the focus year), and for the remaining years in the defined time horizon (two or more years before and after the focus year).
#' @param geo Applies a geographical restriction to data query. Input should be a string indicating the target geography (e.g, country, region). Please refer to ISO-3166-2 for the supported values. Defaults to a worldwide query when unspecified.
#' @param api.key Google Trends API access key.
#' @returns This function returns a data frame containing the following relative search interest metrics:
#'
#' - present.yr.sv: A vector of relative search volume values for the focus year of the analysis (e.g., 2010, see example).
#' - past.yr.sv: A vector of relative search volume values for the year before the focus year of the analysis (e.g., 2009).
#' - future.yr.sv: A vector of relative search volume values for the year before the focus year of the analysis (e.g., 2011).
#'
#' @export
#' @examples
#' \dontrun{
#' getYearSearches(focus.year = 2010, search.horizon = 5, geo = "US", api.key = "YOURAPIKEY")
#' }
getYearSearches <- function(focus.year, search.horizon = 1, geo = NULL, api.key){
  # Check year length and class
  if(length(focus.year)!=1 | !is.numeric(focus.year)){
    stop("Parameter focus.year should be a single number indicating a target focus year")
  }

  # Check year range
  if(focus.year<2004 | focus.year>(lubridate::year(Sys.Date())-1)){
    stop(paste0("Parameter focus.year can only take values between 2004 and ", (lubridate::year(Sys.Date())-1)))
  }

  # Check time.horizon length and class
  if(length(search.horizon)!=1 | !is.numeric(search.horizon)){
    stop("Parameter time.horizon should be a single number indicating the time horizon of interest in years")
  }

  # Check time.horizon size
  if(search.horizon<1 | search.horizon>20){
    stop("Parameter time.horizon should be a single number between 1 and 20 (due to character limits imposed by the Google trends platform)")
  }

  # Get search years
  if(search.horizon==1){
    search.years <- c(focus.year, focus.year-1, focus.year+1)
  } else {
    past.th <- paste(seq((focus.year-1), (focus.year-search.horizon), -1), collapse = "%2B")
    future.th <- paste(seq((focus.year+1), (focus.year+search.horizon), 1), collapse = "%2B")
    search.years <- c(focus.year, past.th, future.th)
  }

  # Extract search volume for target years
  yr_sv <- gtrendsAPI::getGraph(terms = as.character(search.years),
                                geo = geo,
                                startDate = paste0(focus.year,"-01"),
                                endDate = paste0(focus.year,"-12"),
                                api.key = api.key)

  # Add search year
  yr_sv$focus_year <- focus.year

  # Widen search data
  yr_sv_wide <- tidyr::pivot_wider(data = yr_sv,
                                   names_from = keyword,
                                   values_from = value)

  # Correct column names
  # Present
  colnames(yr_sv_wide)[ncol(yr_sv_wide)-2] <- paste0("present.yr.sv (", focus.year, ")")
  colnames(yr_sv_wide)[ncol(yr_sv_wide)-1] <- paste0("past.yr.sv (", focus.year-search.horizon, "-", focus.year-1, ")")
  colnames(yr_sv_wide)[ncol(yr_sv_wide)] <- paste0("future.yr.sv (", focus.year+1, "-", focus.year+search.horizon, ")")

  # Return data
  return(yr_sv_wide)
}
