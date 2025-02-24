#' Get Count of Individuals in a Domain
#'
#' Calculates the number of unique individuals (person_id) in a specific OMOP CDM domain
#' within a specified date range.
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param domain Character string specifying the domain name (e.g., "condition", "drug")
#' @param start_date Character string in YYYY-MM-DD format for analysis start date
#' @param end_date Character string in YYYY-MM-DD format for analysis end date
#'
#' @return Integer representing the count of unique individuals
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' count <- get_domain_individual_count(conn, "cdm_schema", "condition")
#' }
get_domain_individual_count <- function(conn, cdm_schema, domain,
                                        start_date = "2016-01-01",
                                        end_date = "2024-12-31") {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)
  validate_domain(domain)
  validate_date(start_date, "start_date")
  validate_date(end_date, "end_date")

  # Get domain configuration
  domain_config <- DOMAIN_CONFIGS[[domain]]

  tryCatch({
    # For person domain, just count all people
    if (domain == "person") {
      query <- paste0("SELECT COUNT(DISTINCT person_id) as count FROM ",
                      cdm_schema, ".person")
    } else {
      # Build date filter clause
      date_clause <- ""
      if (!is.null(domain_config$date_field)) {
        date_clause <- paste0(" WHERE ", domain_config$date_field, " >= '", start_date,
                              "' AND ", domain_config$date_field, " <= '", end_date, "'")
      } else if (!is.null(domain_config$start_field)) {
        date_clause <- paste0(" WHERE ", domain_config$start_field, " >= '", start_date, "'")
        if (!is.null(domain_config$end_field)) {
          date_clause <- paste0(date_clause, " AND (", domain_config$end_field, " IS NULL OR ",
                                domain_config$end_field, " <= '", end_date, "')")
        }
      }

      # Build and execute query
      query <- paste0("SELECT COUNT(DISTINCT person_id) as count FROM ",
                      cdm_schema, ".", domain_config$table, date_clause)
    }

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(0)
    }

    return(result$count[1])

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Get Count of Records in a Domain
#'
#' Calculates the total number of records in a specific OMOP CDM domain
#' within a specified date range.
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param domain Character string specifying the domain name (e.g., "condition", "drug")
#' @param start_date Character string in YYYY-MM-DD format for analysis start date
#' @param end_date Character string in YYYY-MM-DD format for analysis end date
#'
#' @return Integer representing the count of records
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' count <- get_domain_record_count(conn, "cdm_schema", "condition")
#' }
get_domain_record_count <- function(conn, cdm_schema, domain,
                                    start_date = "2016-01-01",
                                    end_date = "2024-12-31") {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)
  validate_domain(domain)
  validate_date(start_date, "start_date")
  validate_date(end_date, "end_date")

  # Get domain configuration
  domain_config <- DOMAIN_CONFIGS[[domain]]

  tryCatch({
    # Build date filter clause
    date_clause <- ""
    if (!is.null(domain_config$date_field)) {
      date_clause <- paste0(" WHERE ", domain_config$date_field, " >= '", start_date,
                            "' AND ", domain_config$date_field, " <= '", end_date, "'")
    } else if (!is.null(domain_config$start_field)) {
      date_clause <- paste0(" WHERE ", domain_config$start_field, " >= '", start_date, "'")
      if (!is.null(domain_config$end_field)) {
        date_clause <- paste0(date_clause, " AND (", domain_config$end_field, " IS NULL OR ",
                              domain_config$end_field, " <= '", end_date, "')")
      }
    }

    # Build and execute query
    query <- paste0("SELECT COUNT(*) as count FROM ",
                    cdm_schema, ".", domain_config$table, date_clause)

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(0)
    }

    return(result$count[1])

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Get Records Per Individual and Per Visit Ratio for a Domain
#'
#' Calculates the average number of records per person and per visit for a specific domain.
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param domain Character string specifying the domain name (e.g., "condition", "drug")
#' @param start_date Character string in YYYY-MM-DD format for analysis start date
#' @param end_date Character string in YYYY-MM-DD format for analysis end date
#'
#' @return A list containing avg_records_per_person and avg_records_per_visit
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' ratios <- get_domain_density_ratio(conn, "cdm_schema", "condition")
#' }
get_domain_density_ratio <- function(conn, cdm_schema, domain,
                                     start_date = "2016-01-01",
                                     end_date = "2024-12-31") {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)
  validate_domain(domain)
  validate_date(start_date, "start_date")
  validate_date(end_date, "end_date")

  # Check if domain is one we can analyze (only certain domains have ratios)
  if (!domain %in% c("condition", "drug", "procedure", "measurement", "observation")) {
    stop(paste("Domain density ratios are not available for domain:", domain), call. = FALSE)
  }

  # Get domain configuration
  domain_config <- DOMAIN_CONFIGS[[domain]]

  tryCatch({
    # Determine which date field to use for filtering
    date_field <- domain_config$date_field
    if (is.null(date_field)) {
      date_field <- domain_config$start_field
    }

    # Build and execute query
    query <- paste0("
      SELECT
          COUNT(*) * 1.0 / (SELECT COUNT(DISTINCT person_id) FROM ", cdm_schema, ".person) AS avg_records_per_person,
          COUNT(*) * 1.0 / (SELECT COUNT(DISTINCT visit_occurrence_id) FROM ", cdm_schema, ".visit_occurrence
                           WHERE visit_start_date BETWEEN '", start_date, "' AND '", end_date, "') AS avg_records_per_visit
      FROM ", cdm_schema, ".", domain_config$table, "
      WHERE ", date_field, " BETWEEN '", start_date, "' AND '", end_date, "'")

    result <- DBI::dbGetQuery(conn, query)

    if (nrow(result) == 0) {
      return(list(avg_records_per_person = 0, avg_records_per_visit = 0))
    }

    # Return as a list
    return(list(
      avg_records_per_person = round(result$avg_records_per_person[1], 2),
      avg_records_per_visit = round(result$avg_records_per_visit[1], 2)
    ))

  }, error = function(e) {
    stop(paste("Error executing query:", e$message), call. = FALSE)
  })
}

#' Calculate Domain Statistics
#'
#' Calculates statistics for all domains defined in DOMAIN_CONFIGS
#'
#' @param conn A DBI connection object to the database
#' @param cdm_schema Character string specifying the CDM schema name
#' @param domains Optional character vector of specific domains to calculate.
#'        If NULL (default), calculates for all domains.
#' @param start_date Character string in YYYY-MM-DD format for analysis start date
#' @param end_date Character string in YYYY-MM-DD format for analysis end date
#'
#' @return A data frame containing:
#' \describe{
#'   \item{domain}{Domain name}
#'   \item{table_name}{Table name in OMOP CDM}
#'   \item{individual_count}{Number of unique individuals}
#'   \item{record_count}{Total number of records}
#'   \item{records_per_person}{Average records per person}
#'   \item{records_per_visit}{Average records per visit (for applicable domains)}
#'   \item{date_range}{Time period for the analysis}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' stats <- get_domain_statistics(conn, "cdm_schema")
#' stats_subset <- get_domain_statistics(conn, "cdm_schema",
#'                                       domains = c("condition", "drug"))
#' }
get_domain_statistics <- function(conn, cdm_schema, domains = NULL,
                                  start_date = "2016-01-01",
                                  end_date = "2024-12-31") {
  # Validate inputs
  validate_connection(conn)
  validate_schema(cdm_schema)
  validate_date(start_date, "start_date")
  validate_date(end_date, "end_date")

  # Validate domains if provided
  if (!is.null(domains)) {
    sapply(domains, validate_domain)
  } else {
    domains <- names(DOMAIN_CONFIGS)
  }

  # Calculate for each domain
  results <- lapply(domains, function(domain) {
    individual_count <- get_domain_individual_count(conn, cdm_schema, domain, start_date, end_date)
    record_count <- get_domain_record_count(conn, cdm_schema, domain, start_date, end_date)

    # Calculate records per person
    records_per_person <- if (individual_count > 0) round(record_count / individual_count, 2) else 0

    # Get density ratios for applicable domains
    records_per_visit <- NA
    if (domain %in% c("condition", "drug", "procedure", "measurement", "observation")) {
      ratio <- tryCatch({
        get_domain_density_ratio(conn, cdm_schema, domain, start_date, end_date)
      }, error = function(e) {
        list(avg_records_per_person = NA, avg_records_per_visit = NA)
      })
      records_per_visit <- ratio$avg_records_per_visit
    }

    data.frame(
      domain = domain,
      table_name = DOMAIN_CONFIGS[[domain]]$table,
      individual_count = individual_count,
      record_count = record_count,
      records_per_person = records_per_person,
      records_per_visit = records_per_visit,
      date_range = paste(start_date, "to", end_date),
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  final_results <- do.call(rbind, results)

  # Order by record count descending
  final_results[order(-final_results$record_count), ]
}
