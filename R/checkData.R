#' Implement a set of basic data checks for conditions needed to calculate time orintation indices
#'
#' @param data A data table containing the necessary information to calculate the temporal orientation indices. The data.frame should contain at least four columns indicating the focus year, and present, past and future search interest, which the functio ntries to identify autocatially. Alternatively, this information can be assigned manually using the other function parameters.
#' @returns Returns a "Check complete, no warnings" message if all required conditions are met, or a set of warnings for conditions to be checked by the user before using the calculateTOI function.
#'
#' @export
#' @examples
#' data(sample_data)
#' checkData(sample_data)
#'
#' names(sample_data) <- c("focus_year", "present", "past", "future")
#' checkData(sample_data)
checkData <- function(data){

  # Convert to data.frame
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }

  #Check number of data columns
  checkCol <- ifelse(ncol(data)>=3, FALSE, TRUE)

  # Check number of data rows
  checkRow <- ifelse(nrow(data)>=12, FALSE, TRUE)

  # Check if any column in the data mentions year
  checkNoYear <- ifelse(sum(grepl("year", colnames(data), ignore.case = TRUE))==0, TRUE, FALSE)

  # Check if there are multiple columns in the data mentioning year
  checkMultYear <- ifelse(sum(grepl("year", colnames(data), ignore.case = TRUE))>1, TRUE, FALSE)

  # Check if tentative year column includes more than one year
  if(sum(grepl("year", colnames(data), ignore.case = TRUE))==1){
    checkYears <- length(unique(data[,grepl("year", colnames(data), ignore.case = TRUE)]))!=1
  } else {
    checkYears <- FALSE
  }

  # Check if any column in the data mentions present
  checkNoPresent <- ifelse(sum(grepl("present", colnames(data), ignore.case = TRUE))==0, TRUE, FALSE)

  # Check if there are multiple columns in the data mentioning present
  checkMultPresent <- ifelse(sum(grepl("present", colnames(data), ignore.case = TRUE))>1, TRUE, FALSE)

  # Check if any column in the data mentions past
  checkNoPast <- ifelse(sum(grepl("past", colnames(data), ignore.case = TRUE))==0, TRUE, FALSE)

  # Check if there are multiple columns in the data mentioning past
  checkMultPast <- ifelse(sum(grepl("past", colnames(data), ignore.case = TRUE))>1, TRUE, FALSE)

  # Check if any column in the data mentions future
  checkNoFuture <- ifelse(sum(grepl("future", colnames(data), ignore.case = TRUE))==0, TRUE, FALSE)

  # Check if there are multiple columns in the data mentioning future
  checkMultFuture <- ifelse(sum(grepl("future", colnames(data), ignore.case = TRUE))>1, TRUE, FALSE)

  # Return message
  if(any(checkCol, checkRow, checkNoYear, checkMultYear, checkYears, checkNoPresent, checkMultPresent, checkNoPast, checkMultPast, checkNoFuture, checkMultFuture)==TRUE){
    print("Check complete, issues identified.")

    if(checkCol){
      warning("Data has insufficient data columns to calculate temporal orientation indices. Confirm if past, present and future interest data are included", call. = FALSE)
    }

    if(checkRow){
      warning("Monthly (or higher) resolution in the data is recommended to calculate temporal orientation indices.", call. = FALSE)
    }

    if(checkNoYear){
      warning("No focus year column was identified in the data. Consider assigning focus year manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkMultYear){
      warning("Multiple columns were identified possibly containing focus year information. Consider assigning focus year manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkYears){
      warning("Multiple years were identified in the column likely containing focus year information. Check the data and consider assigning focus year manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkNoPresent){
      warning("No column identified that is likely to contain data on interest in the present was identified. Consider assigning present interest data manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkMultPresent){
      warning("Multiple columns likely to contain data on interest in the present were identified. Consider assigning present interest data manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkNoPast){
      warning("No column identified that is likely to contain data on interest in the past was identified. Consider assigning past interest data manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkMultPast){
      warning("Multiple columns likely to contain data on interest in the past were identified. Consider assigning past interest data manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkNoFuture){
      warning("No column identified that is likely to contain data on interest in the future was identified. Consider assigning future interest data manually for calculating temporal orientation indices.", call. = FALSE)
    }

    if(checkMultFuture){
      warning("Multiple columns likely to contain data on interest in the future were identified. Consider assigning future interest data manually for calculating temporal orientation indices.", call. = FALSE)
    }

  } else {

    print("Check complete, no warnings")

  }

}
