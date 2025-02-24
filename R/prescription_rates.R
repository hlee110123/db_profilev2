#' Get Prescription Count for ATC Category
#'
#' Calculates the number of prescriptions in a specific ATC category
#' based on the first letter of the ATC code.
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param atc_code Character string specifying the ATC category (first letter)
#'
#' @return Integer representing the count of prescriptions
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' count <- get_atc_prescription_count(conn, "cdm_schema", "C")
#' }
get_atc_prescription_count <- function(conn, cdm_schema, atc_code) {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)
  validate_atc_code(atc_code)

  tryCatch({
    query <- paste0(
      "WITH atc_concepts AS (
        SELECT
          concept_id,
          concept_name,
          concept_code
        FROM ", cdm_schema, ".concept
        WHERE vocabulary_id = 'ATC'
        AND concept_code LIKE '", atc_code, "%'
      ),
      drug_mappings AS (
        SELECT DISTINCT
          c1.concept_id as atc_concept_id,
          c2.concept_id as rx_concept_id
        FROM atc_concepts c1
        JOIN ", cdm_schema, ".concept_relationship cr
          ON c1.concept_id = cr.concept_id_1
        JOIN ", cdm_schema, ".concept c2
          ON cr.concept_id_2 = c2.concept_id
        WHERE c2.vocabulary_id IN ('RxNorm', 'RxNorm Extension')
        AND cr.relationship_id = 'Maps to'
        AND c2.invalid_reason IS NULL
        AND cr.invalid_reason IS NULL
      ),
      all_drugs AS (
        SELECT DISTINCT
          dm.rx_concept_id,
          ca.descendant_concept_id
        FROM drug_mappings dm
        JOIN ", cdm_schema, ".concept_ancestor ca
          ON dm.rx_concept_id = ca.ancestor_concept_id
      )
      SELECT
        COUNT(*) as prescription_count
      FROM
        ", cdm_schema, ".drug_exposure de
        JOIN all_drugs ad
          ON de.drug_concept_id = ad.descendant_concept_id
      WHERE
        de.drug_exposure_start_date >= '2016-01-01'
        AND (de.drug_exposure_end_date IS NULL OR de.drug_exposure_end_date <= '2024-12-31')")

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(0)
    }

    return(result$prescription_count[1])

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Get Total Prescription Count
#'
#' Calculates the total number of prescriptions in the database
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#'
#' @return Integer representing the total count of prescriptions
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' total <- get_total_prescriptions(conn, "cdm_schema")
#' }
get_total_prescriptions <- function(conn, cdm_schema) {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)

  tryCatch({
    query <- paste0(
      "SELECT COUNT(*) as total_prescriptions
       FROM ", cdm_schema, ".drug_exposure
       WHERE drug_concept_id IS NOT NULL
       AND drug_exposure_start_date >= '2016-01-01'
       AND (drug_exposure_end_date IS NULL OR drug_exposure_end_date <= '2024-12-31')")

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(0)
    }

    return(result$total_prescriptions[1])

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Get Patient Count for ATC Category
#'
#' Calculates the number of unique patients who received prescriptions in a specific ATC category
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param atc_code Character string specifying the ATC category (first letter)
#'
#' @return Integer representing the count of unique patients
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' count <- get_atc_patient_count(conn, "cdm_schema", "C")
#' }
get_atc_patient_count <- function(conn, cdm_schema, atc_code) {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)
  validate_atc_code(atc_code)

  tryCatch({
    query <- paste0(
      "WITH atc_concepts AS (
        SELECT
          concept_id,
          concept_name,
          concept_code
        FROM ", cdm_schema, ".concept
        WHERE vocabulary_id = 'ATC'
        AND concept_code LIKE '", atc_code, "%'
      ),
      drug_mappings AS (
        SELECT DISTINCT
          c1.concept_id as atc_concept_id,
          c2.concept_id as rx_concept_id
        FROM atc_concepts c1
        JOIN ", cdm_schema, ".concept_relationship cr
          ON c1.concept_id = cr.concept_id_1
        JOIN ", cdm_schema, ".concept c2
          ON cr.concept_id_2 = c2.concept_id
        WHERE c2.vocabulary_id IN ('RxNorm', 'RxNorm Extension')
        AND cr.relationship_id = 'Maps to'
        AND c2.invalid_reason IS NULL
        AND cr.invalid_reason IS NULL
      ),
      all_drugs AS (
        SELECT DISTINCT
          dm.rx_concept_id,
          ca.descendant_concept_id
        FROM drug_mappings dm
        JOIN ", cdm_schema, ".concept_ancestor ca
          ON dm.rx_concept_id = ca.ancestor_concept_id
      )
      SELECT
        COUNT(DISTINCT de.person_id) as patient_count
      FROM
        ", cdm_schema, ".drug_exposure de
        JOIN all_drugs ad
          ON de.drug_concept_id = ad.descendant_concept_id
      WHERE
        de.drug_exposure_start_date >= '2016-01-01'
        AND (de.drug_exposure_end_date IS NULL OR de.drug_exposure_end_date <= '2024-12-31')")

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(0)
    }

    return(result$patient_count[1])

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Calculate ATC Prescription Rates
#'
#' Calculates prescription rates for all ATC categories defined in ATC_CATEGORIES
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param categories Optional character vector of specific ATC categories to calculate.
#'        If NULL (default), calculates for all categories.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{atc_code}{ATC category code (first letter)}
#'   \item{category_name}{Full descriptive name of the category}
#'   \item{prescription_count}{Number of prescriptions}
#'   \item{total_prescriptions}{Total prescriptions in database}
#'   \item{prescription_rate}{Prescription rate as percentage of all prescriptions}
#'   \item{patient_count}{Number of unique patients with prescriptions in this category}
#'   \item{total_patients}{Total patients in database}
#'   \item{patient_rate}{Percentage of patients receiving drugs in this category}
#'   \item{prescriptions_per_100k}{Prescriptions per 100,000 patients}
#'   \item{date_range}{Time period for the analysis}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' rates <- get_atc_prescription_rates(conn, "cdm_schema")
#' rates_subset <- get_atc_prescription_rates(conn, "cdm_schema",
#'                                           categories = c("C", "N"))
#' }
get_atc_prescription_rates <- function(conn, cdm_schema, categories = NULL) {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)

  # Validate categories if provided
  if (!is.null(categories)) {
    sapply(categories, validate_atc_code)
  } else {
    categories <- names(ATC_CATEGORIES)
  }

  # Get total prescriptions and patients first
  total_prescriptions <- get_total_prescriptions(conn, cdm_schema)
  total_patients <- get_total_patients(conn, cdm_schema)

  if (total_prescriptions == 0) {
    stop("No prescriptions found in the database", call. = FALSE)
  }

  if (total_patients == 0) {
    stop("No patients found in the database", call. = FALSE)
  }

  # Calculate for each category
  results <- lapply(categories, function(atc_code) {
    prescription_count <- get_atc_prescription_count(conn, cdm_schema, atc_code)
    patient_count <- get_atc_patient_count(conn, cdm_schema, atc_code)

    prescription_rate <- (prescription_count / total_prescriptions) * 100
    patient_rate <- (patient_count / total_patients) * 100
    prescriptions_per_100k <- (prescription_count / total_patients) * 100000

    data.frame(
      atc_code = atc_code,
      category_name = ATC_CATEGORIES[[atc_code]]$name,
      prescription_count = prescription_count,
      total_prescriptions = total_prescriptions,
      prescription_rate = round(prescription_rate, 2),
      patient_count = patient_count,
      total_patients = total_patients,
      patient_rate = round(patient_rate, 2),
      prescriptions_per_100k = round(prescriptions_per_100k, 2),
      date_range = "2016-01-01 to 2024-12-31",
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  final_results <- do.call(rbind, results)

  # Order by prescription rate descending
  final_results[order(-final_results$prescription_rate), ]
}
