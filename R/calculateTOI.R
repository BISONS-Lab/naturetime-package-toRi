#' Calculate Temporal Orientation Indices from relative yearly interest data
#'
#' @param data A data table containing the necessary information to calculate the temporal orientation indices. The data.frame should contain at least four columns indicating the focus year, and present, past and future search interest, which the functio ntries to identify autocatially. Alternatively, this information can be assigned manually using the other function parameters.
#' @param focus.year (optional) A numeric value indicating the focus year of the analysis, for which time orientation metrics will be calculated.
#' @param present.int (optional) A vector of relative search interest values for the focus year of the analysis.
#' @param past.int (optional) A vector of relative search interest values for the year before the focus year of the analysis.
#' @param future.int (optional) A vector of relative search interest values for the year after the focus year of the analysis.
#' @returns This function returns a vector containing the following time orientation metrics calculated for a one year time horizon (past year + focus year + future year):
#'
#' - prop.pres: Proportion of interest (between 0 and 1) for the “present” (focus year in year-based searches, or “present” concept in concept-based searches).
#' - prop.past: Proportion of interest (between 0 and 1) for the “past” (year(s) before the focus year in year-based searches, or “past” concept in concept-based searches).
#' - prop.fut: Proportion of interest (between 0 and 1) for the “future” (year(s) after the focus year in year-based searches, or “future” concept in concept-based searches).
#' - fut.ori.ind: The future-oriention index is a metric indicative of future temporal orientation. Values higher than 1 indicate future orientation, whereas lower values reflect a past orientation. Replicates the future orientation index from Preis et al. 2012 (doi: 10.1038/srep00350).
#' - temp.ori.ind: The time orientation index is a metric indicative of a past or future temporal orientation. It is calculated as (prop.fut - prop.past)/(prop.fut + prop.past). Values range between -1 and 1, with (larger) positive values indicating a (higher) future orientation and (larger) negative values indicating a (higher) past orientation.
#' - past.focus: The past-focus index is a Metric indicative of the degree of past focus in relation to the present. Values higher than 1 indicate past focus, whereas values lower than 1 reflect a present focus. Replicates the past-focus index from Noguchi et al. 2014 (doi: 10.1371/journal.pone.0095209).
#' - future.focus: The future-focus index is a metric indicative of the degree of future focus in relation to the present. Values higher than 1 indicate future focus, whereas values lower than 1 reflect a present focus. Replicates the future-focus index from Noguchi et al. 2014 (doi: 10.1371/journal.pone.0095209).
#' - past.horizon: The past-horizon index is a metric indicating the rate of decay in past interest throughout the focus year. An index of 1 represents constant decay of past interest throughout the year. Values smaller than 1 indicate early decay in past interest during the year; values larger than 1 indicate late-year decay in search interest. Replicates the past-horizon index from Noguchi et al. 2014 (doi: 10.1371/journal.pone.0095209).
#' - future.horizon: The future-horizon index is a Metric indicating the rate of growth in future interest throughout the focus year. An index of 1 represents constant accumulation of future interest throughout the year. Values smaller than 1 indicate accumulation of future interest towards the end of the year; values larger than 1 indicate accumulation of search interest early during the year. Replicates the future-horizon index from Noguchi et al. 2014 (doi: 10.1371/journal.pone.0095209).
#'
#' @export
#' @examples
#' data(sample_data)
#'
#' calculateTOI(focus.year = sample_data$focus_year,
#'              present.int = sample_data$'2025',
#'              past.int = sample_data$'2024',
#'              future.int = sample_data$'2026')
#'
#' names(sample_data) <- c("focus_year", "present", "past", "future")
#' calculateTOI(sample_data)
#'
#' \dontrun{
#' test.sv <- getYearSearches(focus.year = 2010, search.horizon = 5, geo = "US", api.key)
#'
#' test.toi <- calculateTOI(test.sv)
#' }
calculateTOI <- function(data, focus.year, present.int, past.int, future.int){

  # Check if focus.year is missing
  if(missing(focus.year)){

    # Check if data.frame or individual vectors
    if(!missing(data)){

      if(!is.data.frame(data)){
        data <- as.data.frame(data)
      }

      # Check if focus year is in data
      if(sum(grepl("year", colnames(data), ignore.case = TRUE))==0){

        stop("No columns match the word 'year', please assign focus.year manually")

      } else if(sum(grepl("year", colnames(data), ignore.case = TRUE))>1){

        stop("Multiple columns match the word 'year', please assign focus.year manually")

      } else {

        if(length(unique(data[,grepl("year", colnames(data), ignore.case = TRUE)]))!=1){

          stop("Multiple values found in focus.year, please input focus.year manually")

        } else {

          focus.year <- unlist(unique(data[,grepl("year", colnames(data), ignore.case = TRUE)]), use.names = F)

        }

      }

      # Check if present data is found
      if(sum(grepl("present", colnames(data), ignore.case = TRUE))==0){

        stop("No columns match the word 'present', please assign search interest data manually")

      } else if(sum(grepl("present", colnames(data), ignore.case = TRUE))>1){

        stop("Multiple columns match the word 'present', please assign search interest data manually")

      } else {

        present.int <- unlist(data[,grepl("present", colnames(data), ignore.case = TRUE)], use.names = F)

      }


      # Check if past data is found
      if(sum(grepl("past", colnames(data), ignore.case = TRUE))==0){

        stop("No columns match the word 'past', please assign search interest data manually")

      } else if(sum(grepl("past", colnames(data), ignore.case = TRUE))>1){

        stop("Multiple columns match the word 'past', please assign search interest data manually")

      } else {

        past.int <- unlist(data[,grepl("past", colnames(data), ignore.case = TRUE)], use.names = F)

      }

      # Check if future data is found
      if(sum(grepl("future", colnames(data), ignore.case = TRUE))==0){

        stop("No columns match the word 'future', please assign search interest data manually")

      } else if(sum(grepl("future", colnames(data), ignore.case = TRUE))>1){

        stop("Multiple columns match the word 'future', please assign search interest data manually")

      } else {

        future.int <- unlist(data[,grepl("future", colnames(data), ignore.case = TRUE)], use.names = F)

      }

    } else {

      stop("Please provide either a data.frame with search interest data or assign search interest data manually to each dimension")

    }

  } else {

    #Check if temporal parameter is missing
    if(any(missing(present.int)|missing(past.int)|missing(future.int))){
      stop("Please provide search interest data for all temporal dimensions")
    }

    #Check if temporal parameters are numeric
    if(any(!is.numeric(present.int)|!is.numeric(past.int)|!is.numeric(future.int))){
      stop("Please ensure all temporal dimensions contain numeric data")
    }

    # Check focus year
    focus.yr <- unique(focus.year)
    if(length(focus.yr)!=1 | !is.numeric(focus.yr)){
      stop("Parameter focus.year contain a single number indicating a target focus year")
    }

  }

  # Calculate proportion of present interest
  prop.pres <- sum(present.int)/sum(present.int, past.int, future.int)

  # Calculate proportion of past interest
  prop.past <- sum(past.int)/sum(present.int, past.int, future.int)

  # Calculate proportion of future interest
  prop.fut <- sum(future.int)/sum(present.int, past.int, future.int)

  # Calculate Future orientation index
  fut.ori.ind <- prop.fut/prop.past

  # Calculate Temporal Orientation Index
  temp.ori.ind <- (prop.fut-prop.past)/(prop.fut+prop.past)

  # Calculate past focus index
  past.focus <- prop.past/prop.pres

  # Calculate future focus index
  future.focus <- prop.fut/prop.pres

  # Calculate ref time-horizon
  ref.decay <- 1/length(present.int)
  past.ref.decay <- rev(cumsum(rep(ref.decay,length(present.int))))
  future.ref.decay <- cumsum(rep(ref.decay,length(present.int)))

  # Calculate past-horizon index
  rel.past <- past.int/ifelse(sum(past.int) == 0, 1, sum(past.int))
  cum.past <- rev(cumsum(rev(rel.past)))
  past.horizon <- sum(cum.past)/sum(past.ref.decay)

  # Calculate future-horizon index
  rel.future <- future.int/ifelse(sum(future.int) == 0, 1, sum(future.int))
  cum.future <- cumsum(rel.future)
  future.horizon <- sum(cum.future)/sum(future.ref.decay)

  # Prepare data.frame for output
  toi.df <- data.frame(focus.year,
                       prop.pres,
                       prop.past,
                       prop.fut,
                       fut.ori.ind,
                       temp.ori.ind,
                       past.focus,
                       future.focus,
                       past.horizon,
                       future.horizon)

  # Return output
  return(toi.df)

}
