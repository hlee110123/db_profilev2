#' Get Patient Count for Disease Category
#'
#' Calculates the number of unique patients in a specific disease category
#' based on ICD-10 code ranges.
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param category Character string specifying the disease category
#'
#' @return Integer representing the count of unique patients
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' count <- get_category_count(conn, "cdm_schema", "infectious")
#' }
get_category_count <- function(conn, cdm_schema, category) {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)
  validate_category(category)

  # Get code range for category
  codes <- DISEASE_CATEGORIES[[category]]$code_range

  tryCatch({
    query <- paste0(
      "WITH standard_concepts AS (
        SELECT DISTINCT
          cr.concept_id_2 as standard_concept_id
        FROM ",
      cdm_schema, ".concept c
        INNER JOIN ", cdm_schema, ".concept_relationship cr
          ON c.concept_id = cr.concept_id_1
          AND cr.relationship_id = 'Maps to'
          AND cr.invalid_reason IS NULL
        WHERE
          c.vocabulary_id = 'ICD10CM'
          AND c.concept_code >= '", codes[1], "'
          AND c.concept_code <= '", codes[2], "'
          AND c.invalid_reason IS NULL
      )
      SELECT
        COUNT(DISTINCT co.person_id) as total_unique_patients
      FROM
        standard_concepts sc
        INNER JOIN ", cdm_schema, ".condition_occurrence co
          ON sc.standard_concept_id = co.condition_concept_id
      WHERE
        co.condition_start_date >= '2016-01-01'
        AND (co.condition_end_date IS NULL OR co.condition_end_date <= '2024-12-31')")

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(0)
    }

    return(result$total_unique_patients[1])

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Get Total Patient Count
#'
#' Calculates the total number of unique patients in the database
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#'
#' @return Integer representing the total count of patients
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' total <- get_total_patients(conn, "cdm_schema")
#' }
get_total_patients <- function(conn, cdm_schema) {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)

  tryCatch({
    query <- paste0(
      "SELECT COUNT(DISTINCT person_id) as total_patients
       FROM ", cdm_schema, ".person")

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(0)
    }

    return(result$total_patients[1])

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Calculate Disease Prevalence Rates
#'
#' Calculates prevalence rates for all disease categories defined in DISEASE_CATEGORIES
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param categories Optional character vector of specific categories to calculate.
#'        If NULL (default), calculates for all categories.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{category}{Category name}
#'   \item{category_name}{Full descriptive name of the category}
#'   \item{code_start}{Starting ICD-10 code}
#'   \item{code_end}{Ending ICD-10 code}
#'   \item{patient_count}{Number of unique patients}
#'   \item{total_patients}{Total patients in database}
#'   \item{prevalence_rate}{Prevalence as percentage}
#'   \item{prevalence_per_100k}{Prevalence per 100,000 patients}
#'   \item{date_range}{Time period for the analysis}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' rates <- get_prevalence_rates(conn, "cdm_schema")
#' rates_subset <- get_prevalence_rates(conn, "cdm_schema",
#'                                     categories = c("infectious", "neoplasms"))
#' }
get_prevalence_rates <- function(conn, cdm_schema, categories = NULL) {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)

  # Validate categories if provided
  if (!is.null(categories)) {
    sapply(categories, validate_category)
  } else {
    categories <- names(DISEASE_CATEGORIES)
  }

  # Get total patients first
  total_patients <- get_total_patients(conn, cdm_schema)

  if (total_patients == 0) {
    stop("No patients found in the database", call. = FALSE)
  }

  # Calculate for each category
  results <- lapply(categories, function(category) {
    count <- get_category_count(conn, cdm_schema, category)
    prevalence <- (count / total_patients) * 100
    prevalence_per_100k <- (count / total_patients) * 100000

    data.frame(
      category = category,
      category_name = DISEASE_CATEGORIES[[category]]$name,
      code_start = DISEASE_CATEGORIES[[category]]$code_range[1],
      code_end = DISEASE_CATEGORIES[[category]]$code_range[2],
      patient_count = count,
      total_patients = total_patients,
      prevalence_rate = round(prevalence, 2),
      prevalence_per_100k = round(prevalence_per_100k, 2),
      date_range = "2016-01-01 to 2024-12-31",
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  final_results <- do.call(rbind, results)

  # Order by prevalence rate descending
  final_results[order(-final_results$prevalence_rate), ]
}
