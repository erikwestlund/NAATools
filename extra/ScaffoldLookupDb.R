library(NAATools)

fileLocation <- "~/Desktop/lookupDb.duckdb"

createLookupDb(
  c(
    "census",
    "diagnosis",
    "discharge_diagnosis",
    "encounter",
    "geography",
    "hospitalization",
    "insurance",
    "laboratory",
    "medication",
    "mortality",
    "patient",
    "procedure",
    "risk_factor",
    "substance_survey"
  ),
  save_location = fileLocation
)
