# Base cohorts subsetted to pregnant people ----
# pregnant table clean
info(logger, "- Mother table")
cdm$mother_table <- getPregnantCohort(db, cdm, mother_table_schema, mother_table_name)

# Prepare denominators ----
info(logger, "- Prepare denominators")
### Pregnancy and postpartum denominator
cdm$pregnancy_denominator <- cdm$mother_table |>
  select(all_of(c(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", 
    "pregnancy_start_date", "pregnancy_end_date", "observation_period_end_date",
    "pregnancy_outcome_study", "pre_pregnancy_smoking"
  ))) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(
      cohort_definition_id = 1, cohort_name = c("pregnancy_episode")
    )
  ) |>
  requireDemographics(
    ageRange = c(12, 55), sex = "Female", minPriorObservation = 365
  ) |>
  # Start in 2018 and end 9 months before end of data
  requireInDateRange(dateRange = c(as.Date("2018-01-01"), dataCutDate - lubridate::month(9))) %>% 
  # Add postpartum dates
  mutate(
    postpartum_6_weeks = !!dateadd("pregnancy_end_date", 6*7),
    postpartum_12_weeks = !!dateadd("pregnancy_end_date", 12*7)
  ) |>
  mutate(
    postpartum_6_weeks = if_else(
      observation_period_end_date < postpartum_6_weeks,
      observation_period_end_date,
      postpartum_6_weeks
    ),
    postpartum_12_weeks = if_else(
      observation_period_end_date < postpartum_12_weeks,
      observation_period_end_date,
      postpartum_12_weeks
    )
  ) |>
  group_by(subject_id) |>
  arrange(cohort_start_date) |>
  mutate(next_pregnancy = lead(pregnancy_start_date)) |>
  ungroup() %>% 
  mutate(
    postpartum_6_weeks = if_else(
      next_pregnancy > postpartum_6_weeks | is.na(next_pregnancy),
      as.Date(postpartum_6_weeks),
      as.Date(!!dateadd("next_pregnancy", -1))
    ),
    postpartum_12_weeks = if_else(
      next_pregnancy > postpartum_12_weeks | is.na(next_pregnancy),
      as.Date(postpartum_12_weeks),
      as.Date(!!dateadd("next_pregnancy", -1))
    )
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) %>% 
  mutate(
    trimester_1_start = cohort_start_date,
    trimester_2_start =  as.Date(!!dateadd("pregnancy_start_date", 91)),
    trimester_3_start = as.Date(!!dateadd("pregnancy_start_date", 181)),
    postpartum_6_start = as.Date(!!dateadd("pregnancy_end_date", 1)),
    postpartum_12_start = as.Date(!!dateadd("pregnancy_end_date", 1)),
    trimester_1_end = as.Date(!!dateadd("pregnancy_start_date", 90)),
    trimester_2_end =  as.Date(!!dateadd("pregnancy_start_date", 180)),
    trimester_3_end = cohort_end_date,
    postpartum_6_end = postpartum_6_weeks,
    postpartum_12_end = postpartum_12_weeks
  ) |>
  mutate(
    trimester_1_end = if_else(trimester_1_end > pregnancy_end_date, pregnancy_end_date, trimester_1_end),
    trimester_2_start = if_else(trimester_2_start <= pregnancy_end_date, trimester_2_start, NA),
    trimester_3_start = if_else(trimester_3_start <= pregnancy_end_date, trimester_3_start, NA),
    trimester_2_end = case_when(
      is.na(trimester_2_start) ~ NA,
      trimester_2_end > pregnancy_end_date ~ pregnancy_end_date,
      .default = trimester_2_end
    ),
    trimester_3_end = if_else(is.na(trimester_3_start), NA, trimester_3_end)
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE) |>
  renameCohort(cohortId = 1, newCohortName = "pregnancy_denominator")

### Add strata
cdm$pregnancy_denominator <- cdm$pregnancy_denominator |>
  addAge(
    ageName = "maternal_age", 
    ageGroup = list("maternal_age_group" = list("12 to 17" = c(12, 17), "18 to 34" = c(18, 34), "35 to 55" = c(35, 55)))
  ) |>
  mutate(
    pregnancy_start_period = case_when(
      year(pregnancy_start_date) %in% 2018:2019 ~ "Pre COVID-19",
      year(pregnancy_start_date) %in% 2020:2021 ~ "COVID-19 main outbreak",
      .default = "Post COVID-19 main outbreak"
    )
  ) |>
  compute(name = "pregnancy_denominator", temporary = FALSE)

cdm$pregnancy_denominator <- cdm$pregnancy_denominator |>
  addSocioeconomicStatus() |>
  addEthnicity() |>
  compute(name = "pregnancy_denominator", temporary = FALSE)

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

## Denominators ----
info(logger, "- Specific denominators")
cdm$miscarriage_denominator <- getMiscarriageDenominator(cdm$pregnancy_denominator)
cdm$stillbirth_denominator <- getStillbirhtDenominator(cdm$pregnancy_denominator)
cdm$antepartum_haemorrhage_denominator <- getAntepartumDenominator(cdm$pregnancy_denominator)
cdm$dysfunctional_labour_denominator <- getDysfunctionalDenominator(cdm$pregnancy_denominator)
cdm$preterm_labour_denominator <- getPretermDenominator(cdm$pregnancy_denominator)
cdm$postpartum_6_weeks_denominator <- getPostpartum6Denominator(cdm$pregnancy_denominator)
cdm$postpartum_12_weeks_denominator <- getPostpartum12Denominator(cdm$pregnancy_denominator)
cdm$postpartum_12_weeks_denominator_sens <- getPostpartum12DenominatorSensitivity(cdm$pregnancy_denominator)
cdm$maternal_death_denominator <- getMaternalDeathDenominator(cdm$pregnancy_denominator)

# Base cohorts ----
# read codes
info(logger, "- Base cohort")
csvs <- list.files(here("Codelists"))
codes <- NULL
for (csv in csvs) {
  codes <- codes |>
    union_all(read_csv(here("Codelists", csv)))
}
# construct codelsit
codelist <- split(codes$concept_id, codes$codelist_name)

# base cohorts subsetted
baseCodelist <- codelist[!names(codelist) %in% c("platelet_measurement")] # if any measurement
cdm$base <- conceptCohort(
  cdm = cdm,
  conceptSet = baseCodelist,
  exit = "event_start_date",
  subsetCohort = "pregnancy_denominator",
  name = "base"
)

## Obesity ----
# diagnostics cohort
cdm$obesity <- cdm$base |>
  subsetCohorts(
    cohortId = "obesity", name = "obesity"
  )

# bmi cohort
cdm$bmi_measurement <- measurementCohort(
  cdm = cdm, conceptSet = codelist["bmi_measurement"], name = "bmi_measurement",
  valueAsNumber = list(c(30, 60))
)
# body weight cohort
cdm$body_weight <- measurementCohort(
  cdm = cdm, conceptSet = codelist["body_weight"], name = "body_weight",
  valueAsNumber = list("9529" = c(120, 200), "3195625" = c(265, 440))
)
cdm <- omopgenerics::bind(cdm$obesity, cdm$bmi_measurement, cdm$body_weight, name = "obesity")
cdm$obesity <- unionCohorts(cdm$obesity, cohortName = "obesity")

## Covariates ----
# covid test, influenza, tdap and smoking apart
covariatesInf <- c(
  "asthma", "diabetes", "essential_hypertension", "hiv", "uterus_malformations",
  "polycystic_ovary_syndrome", "systemic_lupus_erythematosus", "thyroid_disorder",
  "epilepsy", "chronic_viral_hepatitis", "inflammatory_bowel_disease"
)
covariates5 <- c(
  "alcohol_misuse_dependence", "anxiety", "depression"
)
## All history
cdm$covariates_inf <- cdm$base |>
  subsetCohorts(cohortId = covariatesInf, name = "covariates_inf") |>
  requireIsFirstEntry()
## 5 years
cdm$covariates_5 <-  cdm$base |>
  subsetCohorts(cohortId = covariates5, name = "covariates_5")
cdm <- omopgenerics::bind(cdm$obesity, cdm$covariates_5, name = "covariates_5")

## Comedications ----
comedications <- c(
  "nsaids", "antidepressants", "antiepileptics", "antiinflammatory_antirheumatic",
  "diabetes_treatments", "opioids", "treatment_acid_related_disorder", 
  "antithrombotics", "corticosteroids", "immunosupressants"
)
cdm$comedications <- cdm$base |>
  subsetCohorts(cohortId = comedications, name = "comedications")

## AESI ----
info(logger, "- AESI")
info(logger, "  - Thombosis with Thrombocytopenia Syndrome")
## complex thrombocytopenia
cdm$platelet_measurement <- measurementCohort(
  cdm = cdm,
  conceptSet = codelist["platelet_measurement"],
  valueAsNumber  = list(
    "8848" = c(10, 150), "8961" = c(10, 150), "8785" = c(10, 150), "8686" = c(10, 150),
    "9444" = c(10, 150), "9254" = c(10, 150), "8816" = c(10, 150), "8647" = c(10000, 150000)
  ),
  name = "platelet_measurement"
)
cdm$thrombocytopenia <- cdm$base |> subsetCohorts("thrombocytopenia", "thrombocytopenia")
cdm <- bind(cdm$platelet_measurement, cdm$thrombocytopenia, name = "thrombocytopenia")
cdm$thrombocytopenia <- cdm$thrombocytopenia|>
  unionCohorts(cohortName = "thrombocytopenia") 

cdm$thrombocytopenia_90 <- cdm$thrombocytopenia |>
  padCohortEnd(days = 90, name = "thrombocytopenia_90")

cdm$tts <- cdm$base |>
  subsetCohorts(cohortId = "thrombosis", name = "tts") |>
  padCohortEnd(days = 90) |>
  requireCohortIntersect(
    targetCohortTable = "thrombocytopenia_90",
    targetEndDate = NULL,
    window = c(-10, 10)
  )
cdm$tts  <- cdm$tts |>
  newCohortTable(
    cohortSetRef = settings(cdm$tts) |> mutate(cohort_name = "tts")
  )

## AESI acute
info(logger, "  - Acute")
cdm$aesi_90 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "myocardial_infarction", "ischaemic_stroke", "pulmonary_embolism",
      "bells_palsy", "guillain_barre_syndrome", "transverse_myelitis",
      "haemorrhagic_stroke", "encephalitis", "immune_thrombocytopenia",
      "disseminated_intravascular_coagulation", "deep_vein_thrombosis",
      "myocarditis_or_pericarditis"
    ),
    name = "aesi_90"
  ) |>
  padCohortEnd(days = 90)

cdm$cnsi_90 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "bells_palsy", "encephalitis", "guillain_barre_syndrome", "transverse_myelitis"
    ),
    name = "cnsi_90"
  ) |> unionCohorts(cohortName = "central_nervous_system_immune") |>
  padCohortEnd(days = 90)

cdm <- bind(cdm$aesi_90, cdm$cnsi_90, cdm$tts, name = "aesi_90")

## AESI recurrent
info(logger, "  - Recurrent")
cdm$aesi_30 <- cdm$base |>
  subsetCohorts(cohortId = "anaphylaxis", name = "aesi_30") |>
  padCohortEnd(days = 30)

## AESI chronic
info(logger, "  - Chronic")
cdm$aesi_inf <- cdm$base |>
  subsetCohorts(cohortId = "narcolepsy", name = "aesi_inf") |>
  exitAtObservationEnd()

cdm <- bind(cdm$aesi_30, cdm$aesi_90, cdm$aesi_inf, name = "aesi")

## AESI sensitivity ----
cdm$thrombocytopenia_180 <- cdm$thrombocytopenia |>
  padCohortEnd(days = 180, name = "thrombocytopenia_180")

cdm$tts <- cdm$base |>
  subsetCohorts(cohortId = "thrombosis", name = "tts") |>
  padCohortEnd(days = 180) |>
  requireCohortIntersect(
    targetCohortTable = "thrombocytopenia_180",
    targetEndDate = NULL,
    window = c(-10, 10)
  )
cdm$tts  <- cdm$tts |>
  newCohortTable(
    cohortSetRef = settings(cdm$tts) |> mutate(cohort_name = "tts")
  )

cdm$aesi_180 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "myocardial_infarction", "ischaemic_stroke", "pulmonary_embolism",
      "bells_palsy", "guillain_barre_syndrome", "transverse_myelitis",
      "haemorrhagic_stroke", "encephalitis", "immune_thrombocytopenia",
      "disseminated_intravascular_coagulation", "deep_vein_thrombosis",
      "myocarditis_or_pericarditis"
    ),
    name = "aesi_180"
  ) |>
  padCohortEnd(days = 180)

cdm$cnsi_180 <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "bells_palsy", "encephalitis", "guillain_barre_syndrome", "transverse_myelitis"
    ),
    name = "cnsi_180"
  ) |> unionCohorts(cohortName = "central_nervous_system_immune") |>
  padCohortEnd(days = 180)

cdm <- bind(cdm$aesi_180, cdm$cnsi_180, cdm$tts, name = "aesi_180")

cdm$aesi_180 <- cdm$aesi_180 |>
  newCohortTable(
    cohortSetRef = settings(cdm$aesi_180) |> mutate(cohort_name = paste0(cohort_name, "_sens"))
  )

# MAE ----
info(logger, "- MAE from OMOP tables")
## From OMOP table
cdm$mae_omop <- cdm$mother_table |>
  select("subject_id", "cohort_start_date" = "pregnancy_end_date", "cohort_name" = "pregnancy_outcome_study") |>
  mutate("cohort_end_date" = .data$cohort_start_date) |>
  filter(cohort_name %in% c("miscarriage", "stillbirth")) |>
  compute(name = "mae_omop", temporary = FALSE)
settingsSQL <- cdm$mae_omop |>
  distinct(cohort_name) |>
  mutate(cohort_definition_id = row_number() |> as.integer()) |>
  compute()
cdm$mae_omop <- cdm$mae_omop |>
  inner_join(settingsSQL, by = "cohort_name") |>
  select(omopgenerics::cohortColumns("cohort")) |>
  compute(name = "mae_omop", temporary = FALSE) |>
  newCohortTable(cohortSetRef = settingsSQL |> collect(), cohortAttritionRef = NULL)
cdm$preterm_labour <- cdm$mother_table |>
  filter(pregnancy_outcome_study %in% c("livebirth", "stillbirth"), gestational_length <= 37*7) |>
  select("subject_id", "cohort_start_date" = "pregnancy_end_date") |>
  mutate(
    "cohort_definition_id" = 1L,
    "cohort_end_date" = .data$cohort_start_date
  ) |>
  select(omopgenerics::cohortColumns("cohort")) |>
  compute(name = "preterm_labour", temporary = FALSE) |>
  newCohortTable(
    cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "preterm_labour"), 
    cohortAttritionRef = NULL
  )

## Maternal death
cdm$maternal_death <- deathCohort(cdm = cdm, name = "maternal_death") |>
  inner_join(
    cdm$mother_table |>
      select("subject_id", "pregnancy_start_date", "pregnancy_end_date"),
    by = "subject_id"
  ) %>%
  filter(cohort_start_date <= !!dateadd("pregnancy_end_date", 7*6)) |>
  filter(cohort_start_date >= pregnancy_start_date) |>
  select(
    "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
  ) |>
  compute(name = "maternal_death", temporary = FALSE) |>
  newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "maternal_death"), cohortAttritionRef = NULL)

# From phenotype
cdm$mae_pregnancy <- cdm$base |>
  subsetCohorts(
    cohortId = c(
      "eclampsia", "ectopic_pregnancy", "gestational_diabetes", "hellp", "preeclampsia"
    ),
    name = "mae_pregnancy"
  ) |>
  startsInPregnancy()
cdm$mother_table <- cdm$mother_table %>%
  mutate(
    end_6 = !!CDMConnector::dateadd("pregnancy_end_date", 42),
    end_12 = !!CDMConnector::dateadd("pregnancy_end_date", 84),
    start_20 = !!CDMConnector::dateadd("pregnancy_start_date", 20*7),
    start_24 = !!CDMConnector::dateadd("pregnancy_start_date", 24*7)
  )
cdm$mae_dysfunctional_labour <- cdm$base |>
  subsetCohorts(
    cohortId = "dysfunctional_labour",
    name = "mae_dysfunctional_labour"
  ) |>
  startsInPregnancy(start = "start_20", reason = "From week 20 of pregnancy")
cdm$mae_antepartum_haemorrhage <- cdm$base |>
  subsetCohorts(
    cohortId = "antepartum_haemorrhage",
    name = "mae_antepartum_haemorrhage"
  ) |>
  startsInPregnancy(start = "start_24", reason = "From week 24 of pregnancy")
cdm$mae_postpartum_6weeks <- cdm$base |>
  subsetCohorts(
    cohortId = "postpartum_endometritis",
    name = "mae_postpartum_6weeks"
  ) |>
  startsInPregnancy(start = "pregnancy_end_date", end = "end_6", reason = "In the firsts 6 weeks postpartum")
cdm$mae_postpartum_12weeks <- cdm$base |>
  subsetCohorts(
    cohortId = "postpartum_haemorrhage",
    name = "mae_postpartum_12weeks"
  ) |>
  startsInPregnancy(start = "pregnancy_end_date", end = "end_12", reason = "In the firsts 12 weeks postpartum")

# Bind all
cdm <- bind(
  cdm$mae_pregnancy, cdm$mae_dysfunctional_labour, cdm$mae_antepartum_haemorrhage,
  cdm$mae_postpartum_6weeks, cdm$mae_postpartum_12weeks,
  cdm$maternal_death, cdm$mae_omop, cdm$preterm_labour,
  name = "mae"
)

# Summarise cohorts ----
info(logger, "- Summarise cohorts")
cdm <- bind(
  cdm$pregnancy_denominator, 
  cdm$miscarriage_denominator, 
  cdm$stillbirth_denominator, 
  cdm$preterm_labour_denominator, 
  cdm$postpartum_6_weeks_denominator, 
  cdm$postpartum_12_weeks_denominator, 
  cdm$postpartum_12_weeks_denominator_sens,
  cdm$maternal_death_denominator,
  cdm$antepartum_haemorrhage_denominator,
  cdm$dysfunctional_labour_denominator,
  name = "denominator"
)
bind(
  summaryCohort(cdm$denominator), 
  summaryCohort(cdm$mother_table), 
  summaryCohort(cdm$aesi), 
  summaryCohort(cdm$mae), 
  summaryCohort(cdm$comedications), 
  summaryCohort(cdm$covariates_inf),
  summaryCohort(cdm$covariates_5)
) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("cohort_summary_br_", cdmName(cdm), ".csv"))

bind(
  cohortCodeUseFromCohort(cdm$aesi),
  cohortCodeUseFromCohort(cdm$mae),
  cohortCodeUseFromCohort(cdm$comedications), 
  cohortCodeUseFromCohort(cdm$covariates_inf),
  cohortCodeUseFromCohort(cdm$covariates_5)
) |>
  exportSummarisedResult(path = output_folder, fileName = paste0("cohort_code_use_br_", cdmName(cdm), ".csv"))
