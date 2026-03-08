strata <- list("maternal_age_group", "pregnancy_start_period")
if (grepl("SIDIAP", cdmName(cdm))) {
  strata <- c(strata, list("socioeconomic_status", "nationallity"))
}
if (grepl("CPRD", cdmName(cdm))) {
  strata <- c(strata, list("socioeconomic_status", "ethnicity"))
}
if (grepl("NLHR@UiO", cdmName(cdm))) {
  strata <- c(strata, list("birth_continent"))
}
if (grepl("SCIFI-PEARL", cdmName(cdm))) {
  strata <- c(strata, list("socioeconomic_status", "birth_continent"))
}

# Cumulative incidence ----
info(logger, "Cumulative incidence")

# Estimate cumulative incidence ----
info(logger, "- Estimate cumulative incidence")
### AESI
cif_aesi_30 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_30",
  outcomeWashout = 30,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_aesi_90 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_90",
  outcomeWashout = 90,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_aesi_180 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_180",
  outcomeWashout = 180,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_aesi_inf <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "aesi_inf",
  outcomeWashout = Inf,
  censorOnCohortExit = TRUE,
  strata = strata
)
### MAE pregnancy
maePregnancy <- c(
  "miscarriage", "stillbirth", "antepartum_haemorrhage", 
  "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
  "gestational_diabetes", "hellp", "preeclampsia", "preterm_labour"
)
maePregnancy <- maePregnancy[maePregnancy %in% settings(cdm$mae)$cohort_name]
cif_mae_pregnancy <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "pregnancy_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = maePregnancy,
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_mae_postpartum_12 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "postpartum_12_weeks_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = "postpartum_haemorrhage",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_mae_postpartum_12_sens <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "postpartum_12_weeks_denominator_sens",
  outcomeCohortTable = "mae",
  outcomeCohortId = "postpartum_haemorrhage",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_mae_postpartum_6 <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "postpartum_6_weeks_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = "postpartum_endometritis",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)
cif_mae_maternal_death <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "maternal_death_denominator",
  outcomeCohortTable = "mae",
  outcomeCohortId = "maternal_death",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)

cdm$cif_overall_denominator <- cdm$pregnancy_denominator |>
  mutate(cohort_end_date = postpartum_12_end) |>
  dplyr::compute(name = "cif_overall_denominator", temporary = FALSE) |>
  renameCohort(cohortId = 1, newCohortName = "overall_denominator")
cif_mae_overall <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "cif_overall_denominator",
  outcomeCohortTable = "mae",
  outcomeWashout = 0,
  censorOnCohortExit = TRUE,
  strata = strata
)

## Export CIF ----
exportSummarisedResult(
  cif_aesi_30, cif_aesi_90, cif_aesi_180, cif_aesi_inf, cif_mae_pregnancy, 
  cif_mae_postpartum_6, cif_mae_postpartum_12, cif_mae_maternal_death, 
  cif_mae_postpartum_12_sens, cif_mae_overall,
  path = output_folder,
  fileName = paste0("cumulative_incidence_", cdmName(cdm), ".csv")
)

# Incidence Rates ----
info(logger, "Incidence Rates")
## Get time-to-event data ----
info(logger, "- Get time to event")
### AESI 30
cdm$ir_aesi_30 <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_30",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_30"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_30",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-30, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_30"
  ) |>
  getTimeToEvent(washOut = 30, outcomes = settings(cdm$aesi_30)$cohort_name)

### AESI Inf
cdm$ir_aesi_inf <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_inf",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_inf"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_inf",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-Inf, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_inf"
  ) |>
  getTimeToEvent(washOut = 9999, outcomes = settings(cdm$aesi_inf)$cohort_name)

### AESI 90
cdm$ir_aesi_90 <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_90",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_90"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_90",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-90, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_90"
  ) |>
  getTimeToEvent(washOut = 90, outcomes = settings(cdm$aesi_90)$cohort_name)

### AESI 180
cdm$ir_aesi_180 <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_180",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_aesi_180"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = "aesi_180",
    indexDate = "cohort_start_date",
    targetDate = "cohort_start_date",
    order = "last",
    window = c(-180, 0),
    nameStyle = "prior_{cohort_name}",
    name = "ir_aesi_180"
  ) |>
  getTimeToEvent(washOut = 180, outcomes = settings(cdm$aesi_180)$cohort_name)

### MAE 
cdm$ir_mae <- cdm$pregnancy_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_mae"
  ) |>
  getTimeToEvent(
    washOut = 0, 
    outcomes = c("eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia")
  )

### Antepartum haemorrhage 
cdm$ir_antepartum_haemorrhage <- cdm$antepartum_haemorrhage_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = "antepartum_haemorrhage",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_antepartum_haemorrhage"
  ) |>
  getTimeToEvent(
    washOut = 0, 
    outcomes = "antepartum_haemorrhage"
  )

### Dysfunctional labour  
cdm$ir_dysfunctional_labour <- cdm$dysfunctional_labour_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = "dysfunctional_labour",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_dysfunctional_labour"
  ) |>
  getTimeToEvent(
    washOut = 0, 
    outcomes = "dysfunctional_labour"
  )

### Maternal death
cdm$ir_maternal_death <- cdm$maternal_death_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("maternal_death"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_maternal_death"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("maternal_death"))

### Postpartum endometritis
cdm$ir_postpartum_endometritis <- cdm$postpartum_6_weeks_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("postpartum_endometritis"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_postpartum_endometritis"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("postpartum_endometritis"))

### Postpartum haemorrhage
cdm$ir_postpartum_haemorrhage <- cdm$postpartum_12_weeks_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("postpartum_haemorrhage"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_postpartum_haemorrhage"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("postpartum_haemorrhage"))

### Postpartum haemorrhage
cdm$ir_postpartum_haemorrhage_sens <- cdm$postpartum_12_weeks_denominator_sens |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("postpartum_haemorrhage"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}_sens",
    name = "ir_postpartum_haemorrhage_sens"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("postpartum_haemorrhage_sens"))

### Miscarriage
if ("miscarriage" %in% settings(cdm$mae)$cohort_name) {
  cdm$ir_miscarriage <- cdm$miscarriage_denominator |>
    addCohortIntersectDate(
      targetCohortTable = "mae",
      targetCohortId = "miscarriage",
      indexDate = "cohort_start_date",
      censorDate = "cohort_end_date",
      targetDate = "cohort_start_date",
      order = "first",
      window = c(0, Inf),
      nameStyle = "{cohort_name}",
      name = "ir_miscarriage"
    ) |>
    getTimeToEvent(washOut = 0,  outcomes = "miscarriage")
}

### Stillbirth
if ("stillbirth" %in% settings(cdm$mae)$cohort_name) {
  cdm$ir_stillbirth <- cdm$stillbirth_denominator |>
    addCohortIntersectDate(
      targetCohortTable = "mae",
      targetCohortId = c("stillbirth"),
      indexDate = "cohort_start_date",
      censorDate = "cohort_end_date",
      targetDate = "cohort_start_date",
      order = "first",
      window = c(0, Inf),
      nameStyle = "{cohort_name}",
      name = "ir_stillbirth"
    ) |>
    getTimeToEvent(washOut = 0,  outcomes = c("stillbirth"))
}

### Preterm
cdm$ir_preterm_labour <- cdm$preterm_labour_denominator |>
  addCohortIntersectDate(
    targetCohortTable = "mae",
    targetCohortId = c("preterm_labour"),
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = "ir_preterm_labour"
  ) |>
  getTimeToEvent(washOut = 0,  outcomes = c("preterm_labour"))

## Estimates ----
info(logger, "- Get estimates")
ir_aesi_30 <- estimateIncidenceRate(cdm$ir_aesi_30, strata, settings(cdm$aesi_30)$cohort_name)
ir_aesi_inf <- estimateIncidenceRate(cdm$ir_aesi_inf, strata, settings(cdm$aesi_inf)$cohort_name)
ir_aesi_90 <- estimateIncidenceRate(cdm$ir_aesi_90, strata, settings(cdm$aesi_90)$cohort_name)
ir_aesi_180 <- estimateIncidenceRate(cdm$ir_aesi_180, strata, settings(cdm$aesi_180)$cohort_name)
ir_mae <- estimateIncidenceRate(cdm$ir_mae, strata, c("eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia"))
ir_antepartum_haemorrhage <- estimateIncidenceRate(cdm$ir_antepartum_haemorrhage, strata, "antepartum_haemorrhage")
ir_dysfunctional_labour <- estimateIncidenceRate(cdm$ir_dysfunctional_labour, strata, "dysfunctional_labour")
ir_maternal_death <- estimateIncidenceRate(cdm$ir_maternal_death, strata, "maternal_death")
ir_postpartum_endometritis <- estimateIncidenceRate(cdm$ir_postpartum_endometritis, strata, "postpartum_endometritis")
ir_postpartum_haemorrhage <- estimateIncidenceRate(cdm$ir_postpartum_haemorrhage, strata, "postpartum_haemorrhage")
ir_postpartum_haemorrhage_sens <- estimateIncidenceRate(cdm$ir_postpartum_haemorrhage_sens, strata, "postpartum_haemorrhage_sens")
ir_preterm_labour <- estimateIncidenceRate(cdm$ir_preterm_labour, strata, "preterm_labour")
if ("miscarriage" %in% settings(cdm$mae)$cohort_name) {
  ir_miscarriage <- estimateIncidenceRate(cdm$ir_miscarriage, strata, "miscarriage")
} else {
  ir_miscarriage <- NULL
}
if ("stillbirth" %in% settings(cdm$mae)$cohort_name) {
  ir_stillbirth <- estimateIncidenceRate(cdm$ir_stillbirth, strata, "stillbirth")
} else {
  ir_stillbirth <- NULL
}

## Export IR ----
exportSummarisedResult(
  ir_aesi_30, ir_aesi_90, ir_aesi_180, ir_aesi_inf, ir_mae, ir_maternal_death,
  ir_postpartum_endometritis, ir_postpartum_haemorrhage, ir_preterm_labour,
  ir_miscarriage, ir_stillbirth, ir_antepartum_haemorrhage, ir_dysfunctional_labour,
  ir_postpartum_haemorrhage_sens,
  path = output_folder,
  fileName = paste0("incidence_rates_", cdmName(cdm), ".csv")
)
