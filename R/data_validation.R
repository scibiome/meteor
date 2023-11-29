#' Check Data for MeTEor
#'
#' This function performs several checks on the input data to ensure it meets the expected
#' requirements for analysis. It checks column names, data types, and performs additional
#' validations.
#'
#' @param data A data frame containing the input data.
#' @return No explicit return value. The function stops with an error if any check fails.
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' # Assuming covid_data is your data frame
#' data_check(covid_data)
#' }
#'
#' @export
#'
data_check <- function(data) {
  expected_column_names <- c("id", "time", "metabolites", "values")

  # Check if the first four column names match the expected names
  if (!all(names(data)[1:4] == expected_column_names)) {
    stop("Error: Column names do not match the expected names.
          The names of the first four columns should be: 'id', 'time', 'metabolites', 'values'")
  } else {
    message("Column names passed the check!")
  }

  # Check if 'time' column is numeric or integer
  if (!is.numeric(data$time) && !is.integer(data$time)) {
    stop("Error: 'time' column should be either numeric or integer.")
  } else {
    message("'time' column passed the check!")
  }

  # Check if a fifth column exists at index 5
  if (ncol(data) >= 5) {
    message("Fifth column exists!")

    # Check if the fifth column is one of the allowed types
    fifth_column_index <- 5
    if (!(is.factor(data[[fifth_column_index]]) ||
          is.numeric(data[[fifth_column_index]]) ||
          is.integer(data[[fifth_column_index]]) ||
          is.double(data[[fifth_column_index]]) ||
          is.character(data[[fifth_column_index]]))) {
      stop("Error: The fifth column should be a factor, numeric, integer, double, or character.")
    } else {
      message("Fifth column passed the check!")
    }
  } else {
    message("Warning: The fifth column is missing. To add a categorical variable, use: data[, 5] <- factor(rep(c('CategoryA', 'CategoryB', 'CategoryC'), each = nrow(data)/3))")
    message("To add a variable of any allowed type, adjust the code accordingly.")
  }

  # Check if metabolites column names are syntactically valid
  illegal_metabolites <- grep("[^A-Za-z0-9_.]", data$metabolites)
  if (length(illegal_metabolites) > 0 || grepl("^[0-9._]", data$metabolites[1])) {
    stop("Error: Metabolite names contain illegal characters or do not start with a letter or dot not followed by a number. Names should consist of letters, numbers, dots, or underscores only, and should start with a letter or a dot not followed by a number.")
  } else {
    message("Metabolite names passed the check!")
  }

  # Pivot the data to wide format by metabolites and time
  data_wide <- data %>%
    pivot_wider(
      id_cols = c(id),
      names_from = c(metabolites, time),
      values_from = values
    )

  # Check for duplicate IDs in the wide-format data
  duplicate_ids <- data_wide[duplicated(data_wide$id), "id"]

  # Print the duplicate IDs
  if (any(duplicated(data_wide$id))) {
    message("Duplicate IDs found:")
    print(duplicate_ids)
    stop("Error: Duplicate IDs found in the wide-format data.")
  } else {
    message("No duplicate IDs found in the wide-format data.")
  }
}


# data_validation(covid_data)
#
# head(covid_data)
