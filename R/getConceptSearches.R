#' Extract relative search interest from Google Trends for past, present and future concepts in a given calendar year
#'
#' @param focus.year A numeric value indicating the focus year to scan for temporal search interest. This parameter is bounded between 2004 (first year of available data) and the previous year from the current calendar year to ensure complete search data for the whole year.
#' @param geo Applies a geographical restriction to data query. Input should be a string indicating the target geography (e.g, country, region). Please refer to ISO-3166-2 for the supported values. Defaults to a worldwide query when unspecified.
#' @param api.key Google Trends API access key.
#' @returns This function returns a data frame containing the following relative search interest metrics:
#'
#' - present.sv: A vector of relative search interest values for the focus year of the analysis (e.g., 2010, see example).
#' - past.sv: A vector of relative search interest values for the year before the focus year of the analysis (e.g., 2009).
#' - future.sv: A vector of relative search interest values for the year before the focus year of the analysis (e.g., 2011).
#'
#' @export
#' @examples
#' \dontrun{
#' getConceptSearches(focus.year = 2010, geo = "US", api.key = "YOURAPIKEY")
#' }
getConceptSearches <- function(focus.year, geo = NULL, api.key){
  # Check year length and class
  if(length(focus.year)!=1 | !is.numeric(focus.year)){
    stop("Parameter focus.year should be a single number indicating a target focus year")
  }

  # Check year range
  if(focus.year<2004 | focus.year>(lubridate::year(Sys.Date())-1)){
    stop(paste0("Parameter focus.year can only take values between 2004 and ", (lubridate::year(Sys.Date())-1)))
  }

  # Extract search volume for target years
  # Temporal concept ids
  # Present: /m/03khhv
  # Past: /m/038yj7
  # Future: /m/015n_w
  yr_sv <- gtrendsAPI::getGraph(terms = c("/m/03khhv", "/m/038yj7", "/m/015n_w"),
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
  colnames(yr_sv_wide)[ncol(yr_sv_wide)-2] <- "present.sv"
  colnames(yr_sv_wide)[ncol(yr_sv_wide)-1] <- "past.sv"
  colnames(yr_sv_wide)[ncol(yr_sv_wide)] <- "future.sv"

  # Return data
  return(yr_sv_wide)
}
