#' Domain Configurations Based on OMOP CDM Tables
#'
#' A list containing configurations for each domain in the OMOP CDM, including
#' table names and relevant date fields.
#'
#' @format A list of lists where each sublist contains:
#' \describe{
#'   \item{table}{Table name in the OMOP CDM}
#'   \item{date_field}{Single date field name, if applicable}
#'   \item{start_field}{Start date field name, if applicable}
#'   \item{end_field}{End date field name, if applicable}
#' }
#' @export
DOMAIN_CONFIGS <- list(
  person = list(
    table = "person",
    date_field = NULL,
    start_field = NULL,
    end_field = NULL
  ),
  condition = list(
    table = "condition_occurrence",
    date_field = NULL,
    start_field = "condition_start_date",
    end_field = "condition_end_date"
  ),
  procedure = list(
    table = "procedure_occurrence",
    date_field = "procedure_date",
    start_field = NULL,
    end_field = NULL
  ),
  drug = list(
    table = "drug_exposure",
    date_field = NULL,
    start_field = "drug_exposure_start_date",
    end_field = "drug_exposure_end_date"
  ),
  device = list(
    table = "device_exposure",
    date_field = NULL,
    start_field = "device_exposure_start_date",
    end_field = "device_exposure_end_date"
  ),
  measurement = list(
    table = "measurement",
    date_field = "measurement_date",
    start_field = NULL,
    end_field = NULL
  ),
  observation = list(
    table = "observation",
    date_field = "observation_date",
    start_field = NULL,
    end_field = NULL
  ),
  visit = list(
    table = "visit_occurrence",
    date_field = NULL,
    start_field = "visit_start_date",
    end_field = "visit_end_date"
  ),
  death = list(
    table = "death",
    date_field = "death_date",
    start_field = NULL,
    end_field = NULL
  )
)
