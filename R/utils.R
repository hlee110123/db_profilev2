#' Validate Database Connection
#'
#' Internal function to validate database connection object
#'
#' @param conn A DBI connection object
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_connection <- function(conn) {
  if (!inherits(conn, "DBIConnection")) {
    stop("Invalid connection object. Must be a DBI connection.", call. = FALSE)
  }
  if (!DBI::dbIsValid(conn)) {
    stop("Database connection is not valid or has been closed.", call. = FALSE)
  }
  TRUE
}

#' Validate DatabaseConnector Connection
#'
#' Internal function to validate DatabaseConnector connection object
#'
#' @param conn A DatabaseConnector connection object
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_connection_db_connector <- function(conn) {
  if (is.null(conn) || !inherits(conn, "connection")) {
    stop("Invalid connection object. Please provide a valid DatabaseConnector connection", call. = FALSE)
  }
  TRUE
}

#' Validate Schema Name
#'
#' Internal function to validate schema name
#'
#' @param schema Character string of schema name
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_schema <- function(schema) {
  if (!is.character(schema) || length(schema) != 1 || nchar(schema) == 0) {
    stop("Schema name must be a non-empty character string.", call. = FALSE)
  }
  TRUE
}

#' Validate Category Name
#'
#' Internal function to validate disease category name
#'
#' @param category Character string of category name
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_category <- function(category) {
  if (!category %in% names(DISEASE_CATEGORIES)) {
    stop(paste("Invalid category. Must be one of:",
               paste(names(DISEASE_CATEGORIES), collapse = ", ")),
         call. = FALSE)
  }
  TRUE
}

#' Validate ATC Code
#'
#' Internal function to validate ATC code
#'
#' @param atc_code Character string of ATC code
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_atc_code <- function(atc_code) {
  if (!atc_code %in% names(ATC_CATEGORIES)) {
    stop(paste("Invalid ATC code. Must be one of:",
               paste(names(ATC_CATEGORIES), collapse = ", ")),
         call. = FALSE)
  }
  TRUE
}

#' Validate Date Format
#'
#' Internal function to validate date format
#'
#' @param date_str Character string representing a date in YYYY-MM-DD format
#' @param date_name Character string with the name of the date parameter for error messages
#' @return Date object if valid, throws error if invalid
#' @keywords internal
validate_date <- function(date_str, date_name = "date") {
  tryCatch({
    date <- as.Date(date_str)
    if (is.na(date)) {
      stop(paste0("Invalid ", date_name, ". Please use YYYY-MM-DD format."), call. = FALSE)
    }
    return(date)
  }, error = function(e) {
    stop(paste0("Invalid ", date_name, ". Please use YYYY-MM-DD format."), call. = FALSE)
  })
}

#' Validate Domain Name
#'
#' Internal function to validate domain name
#'
#' @param domain Character string of domain name
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_domain <- function(domain) {
  if (!domain %in% names(DOMAIN_CONFIGS)) {
    stop(paste("Invalid domain. Must be one of:",
               paste(names(DOMAIN_CONFIGS), collapse = ", ")),
         call. = FALSE)
  }
  TRUE
}
