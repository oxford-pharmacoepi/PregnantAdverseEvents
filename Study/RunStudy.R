output_folder <- here::here(results)
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# create logger ----
log_file <- here(results, paste0("log", "_", gsub("-", "", Sys.Date()), ".txt"))
if (file.exists(log_file)) {
  unlink(log_file)
}

logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"
info(logger, "CREATE LOGGER")

# STEP 0 Load study parameters and functions ----
info(logger, "STEP 0 INITIAL SETTINGS ----")

# Load study functions and parameters
info(logger, "Load study functions and parameters")
source(here("Analysis", "functions.R"))
dataCutDate <- cdm$observation_period |> dplyr::pull("observation_period_end_date") |> max()
if (grepl("SCIFI-PEARL", cdmName(cdm))) dataCutDate <- as.Date("2024-01-12")
set.seed(123)

if (sensitvitySCIFIPEARL) {
  locations <- readr::read_csv(here::here("Data", "locations_sweden.csv"))
  cdm$person <- cdm$person |>
    inner_join(locations |> select("location_id"), by = "location_id", copy = TRUE) |>
    compute()
  subjects <- cdm$person |> distinct(person_id) |> compute()
  cdm$observation_period <- cdm$observation_period |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$observation <- cdm$observation |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$condition_occurrence <- cdm$condition_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$drug_exposure <- cdm$drug_exposure |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$measurement <- cdm$measurement |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$visit_occurrence <- cdm$visit_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$procedure_occurrence <- cdm$procedure_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
}

if (database_name == "SIDIAP") {
  info(logger, "SIDIAP: Filter drug exposure table")
  cdm$drug_exposure <- cdm$drug_exposure |>
    filter(drug_type_concept_id == 32839) |>
    compute()
}

# Database snapshot:
summariseOmopSnapshot(cdm) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("cdm_snapshot_", cdmName(cdm), ".csv"))

if (runInstantiateCohorts) {
  info(logger, "STEP 1 INSTANTIATE COHORTS ----")
  source(here("Analysis", "01_InstantiateCohorts.R"))
}

if (runBackgroundRates) {
  if (!runInstantiateCohorts) {
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = results_database_schema,
      writePrefix = tolower(table_stem),
      cdmName = database_name,
      cohortTables = c(
        "mother_table", "aesi_90", "aesi_30", "aesi_inf", "mae", "comedications",
        "covariates_inf", "covariates_5", "base", "thrombocytopenia", "aesi_180",
        "pregnancy_denominator", "miscarriage_denominator", "stillbirth_denominator", 
        "preterm_labour_denominator", "postpartum_6_weeks_denominator", 
        "postpartum_12_weeks_denominator", "maternal_death_denominator",
        "dysfunctional_labour_denominator", "antepartum_haemorrhage_denominator",
        "postpartum_12_weeks_denominator_sens"
      ),
      .softValidation = TRUE
    )
  }
  info(logger, "STEP 2 BACKGROUND RATES ----")
  source(here("Analysis", "02_BackgroundRates.R"))
}

if (runBRCharacteristics) {
  if (!runBackgroundRates) {
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = results_database_schema,
      writePrefix = tolower(table_stem),
      cdmName = database_name,
      cohortTables = c(
        "mother_table", "mae", "comedications", "pregnancy_denominator", 
        "covariates_inf", "covariates_5", 
        "aesi_90", "aesi_30", "aesi_inf", "mae", "aesi_180",
        "ir_aesi_30", "ir_aesi_inf", "ir_aesi_90", "ir_aesi_180",
        "ir_mae", "ir_maternal_death", "ir_postpartum_endometritis",
        "ir_postpartum_haemorrhage", "ir_preterm_labour", "ir_miscarriage", 
        "ir_stillbirth", "ir_antepartum_haemorrhage", "ir_dysfunctional_labour",
        "ir_postpartum_haemorrhage_sens"
      ),
      .softValidation = TRUE
    )
  }
  info(logger, "STEP 3 BACKGROUND RATES CHARACTERISTICS ----")
  source(here("Analysis", "03_BRCharacterisation.R"))
}

info(logger, "STEP 4 ZIP RESULTS ----")
output_folder <- basename(output_folder)
zip(
  zipfile = paste0(output_folder, "_", gsub("-", "", Sys.Date()), ".zip"),
  files = list.files(output_folder, full.names = TRUE)
)

dbDisconnect(db)

info(logger, " -- DONE! --")
