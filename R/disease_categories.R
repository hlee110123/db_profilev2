#' Disease Categories Based on ICD-10 Codes
#'
#' A list containing standardized disease categories with their ICD-10 code ranges
#' and descriptive names.
#'
#' @format A list of lists where each sublist contains:
#' \describe{
#'   \item{name}{Full name of the disease category}
#'   \item{code_range}{Vector of two elements indicating start and end ICD-10 codes}
#' }
#' @export
DISEASE_CATEGORIES <- list(
  infectious = list(name = "Certain infectious and parasitic disease",
                    code_range = c("A00", "B99")),
  neoplasms = list(name = "Neoplasm",
                   code_range = c("C00", "D49")),
  blood_immune = list(name = "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                      code_range = c("D50", "D89")),
  endocrine = list(name = "Endocrine, nutritional and metabolic diseases",
                   code_range = c("E00", "E89")),
  mental = list(name = "Mental, Behavioral and Neurodevelopmental disorders",
                code_range = c("F01", "F99")),
  nervous = list(name = "Diseases of the nervous system",
                 code_range = c("G00", "G99")),
  eye = list(name = "Disease of the eye and adnexa",
             code_range = c("H00", "H59")),
  ear = list(name = "Diseases of the ear and mastoid process",
             code_range = c("H60", "H95")),
  circulatory = list(name = "Diseases of the circulatory system",
                     code_range = c("I00", "I99")),
  respiratory = list(name = "Diseases of the respiratory system",
                     code_range = c("J00", "J99")),
  digestive = list(name = "Diseases of the digestive system",
                   code_range = c("K00", "K95")),
  skin = list(name = "Diseases of the skin and subcutaneous tissue",
              code_range = c("L00", "L99")),
  musculoskeletal = list(name = "Diseases of the musculoskeletal system and connective tissue",
                         code_range = c("M00", "M99")),
  genitourinary = list(name = "Diseases of the genitourinary system",
                       code_range = c("N00", "N99")),
  pregnancy = list(name = "Pregnancy, childbirth and the puerperium",
                   code_range = c("O00", "O9A")),
  perinatal = list(name = "Certain conditions originating in the perinatal period",
                   code_range = c("P00", "P96")),
  congenital = list(name = "Congenital malformations, deformations and chromosomal abnormalities",
                    code_range = c("Q00", "Q99")),
  symptoms = list(name = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
                  code_range = c("R00", "R99")),
  injury = list(name = "Injury, poisoning and certain other consequences of external causes",
                code_range = c("S00", "T88")),
  external_causes = list(name = "External causes of morbidity and mortality",
                         code_range = c("V00", "Y99")),
  health_status = list(name = "Factors influencing health status and contact with health services",
                       code_range = c("Z00", "Z99")),
  special = list(name = "Codes for special purposes",
                 code_range = c("U00", "U85"))
)
