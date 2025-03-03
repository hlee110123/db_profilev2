# dbprofile analysis Package

This R package provides tools for analyzing healthcare utilization in OMOP CDM databases.

Analysis of Healthcare Data (2016–2024):

1. Prevalence Rates of Disease-Related Groups (DRG) (2016–2024)
2. Prescription Rates of Anatomical Therapeutic Chemical (ATC) Classifications (2016–2024)
3. Number of Individuals Across Domains (2016–2024)
4. Number of Occurrence Records Across Domains (2016–2024)
5. Prevalence Rates per 100,000 Patients (2016–2024)
6. Prescription Rates per 100,000 Patients (2016–2024)
7. Average Number of Records per Person in Each Table (2016-2024)
8. Average Number of Records per Visit in Each Table (2016 - 2024)
9. Distribution of Chronic vs. Acute Conditions
10. Utilization of Intensive Care Services
11. Prevalence of Comorbidity-Indicating Codes
12. Severity Scores (Elixhauser Comorbidity Index or Charlson Comorbidity Index)
13. Primary payer types (Medicare, Medicaid, Private, Self-pay, Other)
14. Hospital region (Northeast, North central/Midwest, South, West)
15. Hospital type (Government, non-federal, prviate, non-profit, private, for-profit)
16. Hospital factors (number of beds: S/M/L, teaching status, percentage of hospital beds designated as ICU beds, and ICU occupancy)


# Installation: diseasestats package from GITHUB 

```r
remotes::install_github("hlee110123/dbprofile_v1")
```
# How to run

```r
#install packages 
install.packages("DBI")
install.packages("DatabaseConnector")
install.packages("rJava")

#Load packages
library(rJava)
library(DBI)
library(DatabaseConnector)
library(dbprofile)

#Verify change of Java setting in R Environment
Sys.getenv('JAVA_OPTIONS')

#Set your JAVA_HOME environment variable (set to path where Java was installed)
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = "#your JAR FOLDER directory") 

#Setup the connection details for your OMOP instance
connectionDetails <- createConnectionDetails(   
  dbms = #your dbms,   
  server = #your server,   
  user = #your username,   
  password = #your password,   
  port = #your port number,   
  pathToDriver = Sys.getenv('DATABASECONNECTOR_JAR_FOLDER')) ## Establish a connection using the DatabaseConnector "connect" function 

#Connect to the database
conn <- connect(connectionDetails)

#Get counts for a specific category
respiratory_count <- get_category_count(conn, "your schema name", "respiratory")
print(paste("Respiratory patients (2016-2024):", respiratory_count))

#Get all prevalence rates
prevalence_rates <- get_prevalence_rates(conn, "your schema name")

#View results sorted by prevalence
sorted_results <- prevalence_rates[order(-prevalence_rates$prevalence_rate), ]
print(sorted_results)

#Get all ATC prescription rates
atc_prescription_rates <- get_atc_prescription_rates(conn, "your schema name")

#Export to CSV
write.csv(prevalence_rates, "disease_prevalence_2016_2024.csv", row.names = FALSE)
write.csv(prevalence_rates, "atc_2016_2024.csv", row.names = FALSE)
```

## Notes
Once you've completed the analysis, please export the results to "disease_prevalence_2016_2024.csv" and kindly share it as an email attachment. Thank you! 

Email address: "hlee292@jh.edu"
