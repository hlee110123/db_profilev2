#' diseasestats: Analysis Tools for Disease Prevalence and Medication Prescription Rates
#'
#' A package for analyzing healthcare data in OMOP Common Data Model (CDM) format.
#' Provides functions for calculating disease prevalence rates based on ICD-10 codes
#' and medication prescription rates based on ATC classification.
#'
#' @section Disease Prevalence Functions:
#' \describe{
#'   \item{\code{\link{get_category_count}}}{Calculate patient count for a disease category}
#'   \item{\code{\link{get_total_patients}}}{Calculate total patient count}
#'   \item{\code{\link{get_prevalence_rates}}}{Calculate disease prevalence rates}
#' }
#'
#' @section Prescription Rate Functions:
#' \describe{
#'   \item{\code{\link{get_atc_prescription_count}}}{Calculate prescription count for an ATC category}
#'   \item{\code{\link{get_atc_patient_count}}}{Calculate patient count for an ATC category}
#'   \item{\code{\link{get_total_prescriptions}}}{Calculate total prescription count}
#'   \item{\code{\link{get_atc_prescription_rates}}}{Calculate prescription rates for ATC categories}
#' }
#'
#' @docType package
#' @name diseasestats
NULL
