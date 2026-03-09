getId <- function(cohort, name) {
  settings(cohort) |>
    filter(cohort_name %in% name) |>
    pull(cohort_definition_id)
}

getPregnantCohort <- function(db, cdm, mother_table_schema, mother_table_name) {
  cdm$mother_table_original <- tbl(
    db, inSchema(schema = mother_table_schema, table = mother_table_name)
  ) |>
    compute(
      name = inSchema(results_database_schema, "mother_table_original"),
      temporary = FALSE,
      overwrite = TRUE
    )
  
  cdm$mother_table <- cdm$mother_table_original |>
    mutate(
      cohort_definition_id = 1L,
      cohort_start_date = pregnancy_start_date,
      cohort_end_date = pregnancy_end_date,
      person_id = as.numeric(person_id),
      pregnancy_id = as.numeric(pregnancy_id)
    ) |>
    rename("subject_id" = "person_id") |>
    compute(name = "mother_table", temporary = FALSE, overwrite = TRUE) |>
    newCohortTable(
      cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = "mother_table"),
      .softValidation = TRUE
    ) |>
    # Only pregnancies in continuous observation from start to end
    left_join(
      cdm$observation_period |>
        select(
          subject_id = person_id, observation_period_start_date, observation_period_end_date
        ),
      by = "subject_id"
    ) |>
    filter(
      pregnancy_start_date >= observation_period_start_date,
      pregnancy_start_date <= observation_period_end_date,
      pregnancy_end_date <= observation_period_end_date,
      pregnancy_end_date >= observation_period_start_date
    ) |>
    recordCohortAttrition(reason = "Pregnancy in observation") |>
    filter(pregnancy_start_date < pregnancy_end_date) |>
    mutate(cohort_definition_id = 1L) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date") %>%
    mutate(gestational_length = !!datediff("pregnancy_start_date", "pregnancy_end_date")) |>
    filter(gestational_length < 308) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Gestational length < 308 days") |>
    filter(gestational_length_in_day != 0) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "Gestational length days != 0")
  cdm$mother_table <- cdm$mother_table |>
    addCohortIntersectCount(
      targetCohortTable = "mother_table",
      window = list(c(0, Inf)),
      indexDate = "pregnancy_start_date",
      censorDate = "pregnancy_end_date",
      targetStartDate = "pregnancy_start_date",
      targetEndDate = NULL,
      nameStyle = "overlap"
    ) |>
    filter(overlap == 1) |>
    select(!c(
      "gestational_length_in_day", "prev_pregnancy_gravidity", "pregnancy_single",
      "overlap", "pregnancy_mode_delivery"
    )) |>
    mutate(
      pregnancy_outcome_study = case_when(
        pregnancy_outcome == 4092289 ~ "livebirth",
        pregnancy_outcome == 4067106 & gestational_length < 20*7 ~ "miscarriage",
        pregnancy_outcome == 4067106 & gestational_length >= 20*7 ~ "stillbirth",
        pregnancy_outcome == 443213 & gestational_length < 20*7 ~ "miscarriage",
        pregnancy_outcome == 443213 & gestational_length >= 20*7 ~ "stillbirth",
        pregnancy_outcome == 4081422 ~ "elective_termination",
        pregnancy_outcome == 4095714 ~ "discordant",
        .default = "unknown"
      )
    ) |>
    compute(name = "mother_table", temporary = FALSE) |>
    recordCohortAttrition(reason = "No overlapping pregnancy records")
  
  if (cdmName(cdm) %in% "CPRD GOLD") {
    smoking_observation <- cdm$observation |>
      filter(observation_concept_id %in% c(903653, 40766579, 903657)) |>
      mutate(
        pre_pregnancy_smoking = case_when(
          observation_concept_id == 40766579 & value_as_number == 0 ~ "No smoker", 
          observation_concept_id == 40766579 & value_as_number > 0 ~ "Smoker", 
          observation_concept_id == 903657 ~ "Smoker", 
          observation_concept_id == 903653 ~ "No smoker", 
          .default = "Missing")
      ) |>
      select(subject_id = person_id, observation_date, pre_pregnancy_smoking) |>
      inner_join(cdm$mother_table)  |>
      filter(observation_date < pregnancy_end_date) %>% 
      filter(observation_date > !!dateadd("pregnancy_start_date", -5, interval = "year")) |>
      compute() |>
      group_by(subject_id, pregnancy_id, pregnancy_start_date) |>
      filter(observation_date == max(observation_date)) |>
      ungroup() |>
      select(subject_id, observation_date, pre_pregnancy_smoking) |>
      compute()
    cdm$mother_table <- cdm$mother_table |>
      left_join(smoking_observation, by = "subject_id") |>
      compute(name = "mother_table", temporary = FALSE) |>
      mutate(pre_pregnancy_smoking = if_else(is.na(pre_pregnancy_smoking), "Missing", pre_pregnancy_smoking)) |>
      select(!observation_date) |>
      distinct() |>
      # check different records in same day
      group_by(across(-pre_pregnancy_smoking)) |>
      summarise(
        pre_pregnancy_smoking = case_when(
          any(pre_pregnancy_smoking == "Smoker") ~ "Smoker", 
          any(pre_pregnancy_smoking == "No smoker") ~ "No smoker", 
          .default = "Missing"
        ),
        .groups = "drop" 
      ) |>
      compute(name = "mother_table", temporary = FALSE) |>
      recordCohortAttrition(reason = "Add smoking status")
    
  } else if (cdmName(cdm) %in% "SIDIAP") {
    smoking_observation <- cdm$observation |>
      filter(value_as_concept_id %in% c(45879404, 45883458, 45884037)) |>
      mutate(
        pre_pregnancy_smoking = case_when(
          value_as_concept_id == 45879404 ~ "Never smoker",
          value_as_concept_id == 45883458 ~ "Former smoker",
          value_as_concept_id == 45884037 ~ "Current some day smoker", 
          .default = "Missing")
      ) |>
      select(subject_id = person_id, observation_date, pre_pregnancy_smoking) |>
      inner_join(cdm$mother_table)  |>
      filter(observation_date < pregnancy_end_date) %>% 
      filter(observation_date > !!dateadd("pregnancy_start_date", -5, interval = "year")) |>
      compute() |>
      group_by(subject_id, pregnancy_id, pregnancy_start_date) |>
      filter(observation_date == max(observation_date)) |>
      ungroup() |>
      select(subject_id, observation_date, pre_pregnancy_smoking) |>
      compute()
    cdm$mother_table <- cdm$mother_table |>
      left_join(smoking_observation, by = "subject_id") |>
      compute(name = "mother_table", temporary = FALSE) |>
      mutate(pre_pregnancy_smoking = if_else(is.na(pre_pregnancy_smoking), "Missing", pre_pregnancy_smoking)) |>
      select(!observation_date) |>
      distinct() |>
      # check different records in same day
      group_by(across(-pre_pregnancy_smoking)) |>
      summarise(
        pre_pregnancy_smoking = case_when(
          any(pre_pregnancy_smoking == "Current some day smoker") ~ "Current some day smoker", 
          any(pre_pregnancy_smoking == "Former smoker") ~ "Former smoker", 
          any(pre_pregnancy_smoking == "Never smoker") ~ "Never smoker", 
          .default = "Missing"
        ),
        .groups = "drop" 
      ) |>
      compute(name = "mother_table", temporary = FALSE) |>
      recordCohortAttrition(reason = "Add smoking status")
    
  } else {
    cdm$mother_table <- cdm$mother_table |>
      mutate(
        pre_pregnancy_smoking = case_when(
          pre_pregnancy_smoking == 4188540 ~ "No",
          pre_pregnancy_smoking == 4188539 ~ "Yes",
          .default = "Missing"
        )
      ) |>
      compute(name = "mother_table", temporary = FALSE)
  }
  
  return(cdm$mother_table)
}

startsInPregnancy <- function(cohort, start = "pregnancy_start_date", end = "pregnancy_end_date", reason = "During pregnancy") {
  cdm <- omopgenerics::cdmReference(cohort)
  name <- omopgenerics::tableName(cohort)
  cdm[[name]]  <- cdm[[name]] |>
    dplyr::inner_join(
      cdm$mother_table |>
        dplyr::select("subject_id", start, end),
      by = "subject_id"
    ) |>
    dplyr::filter(cohort_start_date >= .data[[start]] & cohort_start_date <= .data[[end]]) |>
    dplyr::select(!dplyr::starts_with("pregnancy")) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition(reason = reason)
  return(cdm[[name]])
}

summaryCohort <- function(cohort) {
  bind(summariseCohortCount(cohort), summariseCohortAttrition(cohort))
}

getRegion <- function(x) {
  database_name <- omopgenerics::cdmName(omopgenerics::cdmReference(x))
  name <- omopgenerics::tableName(x)
  if (database_name == "CPRD GOLD") {
    x <- x |>
      left_join(
        cdm$person |> select("subject_id" = "person_id", "care_site_id"),
        by = "subject_id"
      ) |>
      left_join(
        cdm$care_site |>
          select("care_site_id", "location_id") |>
          left_join(cdm$location |> select("location_id", "region" = "location_source_value"), by = "location_id"),
        by = "care_site_id"
      ) |>
      select(!"care_site_id")
  } else if (database_name == "NLHR@UiO") {
    x <- x |>
      left_join(
        cdm$person |> select("subject_id" = "person_id", "location_id"),
        by = "subject_id"
      ) |>
      left_join(
        cdm$location |> select("location_id", "region" = "county"),
        by = "location_id"
      ) |>
      select(!"location_id")
  } else {
    x <- x |>
      left_join(
        cdm$person |> select("subject_id" = "person_id", "location_id"),
        by = "subject_id"
      ) |>
      left_join(
        cdm$location |> select("location_id", "region" = "location_source_value"),
        by = "location_id"
      ) |>
      select(!"location_id")
  }
  x |>
    mutate(region = as.character(region)) |>
    compute(name = name, temporary = FALSE)
}

addSeason <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  cohort |>
    dplyr::mutate(
      season = dplyr::case_when(
        clock::get_month(.data$cohort_start_date) %in% 3:5 ~ "Spring",
        clock::get_month(.data$cohort_start_date) %in% 6:8 ~ "Summer",
        clock::get_month(.data$cohort_start_date) %in% 9:11 ~ "Autumn",
        clock::get_month(.data$cohort_start_date) %in% c(12, 1:2) ~ "Winter"
      )
    ) |>
    dplyr::compute(name = name, temporary = FALSE)
}

addEthnicity <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  database <- omopgenerics::cdmName(omopgenerics::cdmReference(cohort))
  if (grepl("CPRD", database)) {
    cohort <- cohort |>
      inner_join(
        cdm$person |>
          dplyr::select("subject_id" = "person_id", "concept_id" = "race_concept_id"),
        by = "subject_id"
      ) |>
      inner_join(
        cdm$concept |>
          dplyr::select("concept_id", "ethnicity" = "concept_name")
      ) |>
      dplyr::mutate(
        ethnicity = dplyr::if_else(.data$concept_id == 0, "Missing", .data$ethnicity)
      ) |>
      dplyr::select(!"concept_id") |>
      dplyr::compute(name = name, temporary = FALSE)
  } else if (grepl("SIDIAP", database)) {
    cohort <- cohort |>
      left_join(
        cdm$observation |>
          filter(observation_concept_id == 4087925) |>
          mutate(
            nationallity = case_when(
              value_as_string %in% c("Espanya", "Europa meridional", "Europa occidental", "Europa oriental", "Europa septentrional") ~ "Europe",
              value_as_string %in% c("Àsia central", "Àsia meridional", "Àsia occidental", "Àsia oriental", "Àsia sud-oriental") ~ "Asia",
              value_as_string %in% c("Amèrica del Nord") ~ "North America",
              value_as_string %in% c("Austràlia i Nova Zelanda") ~ "Oceania",
              value_as_string %in% c("Amèrica central", "Amèrica del Sud", "Carib") ~ "Central/South America",
              value_as_string %in% c("Àfrica central", "Àfrica meridional", "Àfrica occidental", "Àfrica oriental", "Àfrica septentrional") ~ "Africa",
              .default = "Missing"
            )
          ) |>
          select("subject_id" = "person_id", "nationallity"),
        by = "subject_id"
      ) |>
      mutate(nationallity = if_else(is.na(nationallity), "Missing", nationallity)) |>
      compute(name = name, temporary = FALSE)
    
  } else if (grepl("SCIFI-PEARL", database)) {
    cohort <- cohort |>
      left_join(
        cdm$observation |>
          filter(observation_concept_id == 4197735) |>
          mutate(
            birth_continent = case_when(
              value_as_string %in% c("EU28 utom Norden", "Europa utom EU28 och Norden", "Norden utom Sverige", "Sverige", "Sovjetunionen") ~ "Europe",
              value_as_string %in% c("Asien") ~ "Asia",
              value_as_string %in% c("Nordamerika") ~ "North America",
              value_as_string %in% c("Oceanien") ~ "Oceania",
              value_as_string %in% c("Sydamerika") ~ "Central/South America",
              value_as_string %in% c("Afrika") ~ "Africa",
              .default = "Missing"
            )
          ) |>
          select("subject_id" = "person_id", "birth_continent"),
        by = "subject_id"
      ) |>
      mutate(birth_continent = if_else(is.na(birth_continent), "Missing", birth_continent)) |>
      dplyr::compute(name = name, temporary = FALSE)
    
  } else if (grepl("NLHR@UiO", database)) {
    cohort <- cohort |>
      inner_join(
        cdm$mother_table |> 
          mutate(
            birth_continent = case_when(
              birth_region == "Africa" ~ "Africa",
              birth_region == "America" ~ "America",
              birth_region == "Asia" ~ "Asia",
              birth_region == "Oceania" ~ "Oceania",
              birth_region == "Europe/Norway" ~ "Europe",
              birth_region == "Norway" ~ "Europe",
              .default = "Missing"
            )
          ) |>
          select("subject_id", "pregnancy_start_date", "pregnancy_end_date", "birth_continent")
      ) |>
      mutate(birth_continent = if_else(is.na(birth_continent), "Missing", birth_continent)) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  return(cohort)
}

addSocioeconomicStatus <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  database <- omopgenerics::cdmName(omopgenerics::cdmReference(cohort))
  if (grepl("CPRD", database)) {
    cohort <- cohort |>
      dplyr::left_join(
        cdm$measurement |>
          dplyr::filter(measurement_concept_id == 715996) |>
          dplyr::select("subject_id" = "person_id", "socioeconomic_status" = "value_as_number") |> 
          dplyr::mutate(
            socioeconomic_status = case_when(
              socioeconomic_status %in% 1:2 ~ "Q1",
              socioeconomic_status %in% 3:4 ~ "Q2",
              socioeconomic_status %in% 5:6 ~ "Q3",
              socioeconomic_status %in% 7:8 ~ "Q4",
              socioeconomic_status %in% 9:10 ~ "Q5",
              .default = "Missing"
            )
          ),
        by = "subject_id"
      ) |>
      dplyr::compute(name = name, temporary = FALSE)
    
  } else if (grepl("SIDIAP", database)) {
    cohort <- cohort |>
      left_join(
        cdm$observation |>
          filter(observation_source_value == "qmedea11") |>
          select("subject_id" = "person_id", "socioeconomic_status" = "value_as_string"),
        by = "subject_id"
      ) |>
      mutate(socioeconomic_status = if_else(is.na(socioeconomic_status), "Missing", socioeconomic_status)) |>
      compute(name = name, temporary = FALSE)
    
  } else if (grepl("SCIFI-PEARL", database)) {
    values2020 <- cdm$observation |>
      filter(observation_concept_id == 4076114, year(observation_date) == 2020) |>
      pull(value_as_number)
    quintiles <- quantile(values2020, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
    q1 <- as.numeric(quintiles[[1]])
    q2 <- as.numeric(quintiles[[2]])
    q3 <- as.numeric(quintiles[[3]])
    q4 <- as.numeric(quintiles[[4]])
    
    # people who have both 2020 and 2021
    tableSES <- cdm$observation |>
      filter(
        observation_concept_id == 4076114,
        lubridate::year(observation_date) %in% c(2020, 2021)
      ) |>
      group_by(person_id) |>
      mutate(has2020 = max(if_else(lubridate::year(observation_date) == 2020, 1L, 0L))) |>
      filter(
        (has2020 == 1 & year(observation_date) == 2020) |
          (has2020 == 0 & year(observation_date) == 2021) # if has 2020, drop 2021, otherwise keep 2021
      ) |>
      mutate(
        socioeconomic_status = case_when(
          is.na(value_as_number) ~ "Missing",
          value_as_number <= q1  ~ "Q1",
          value_as_number <= q2  ~ "Q2",
          value_as_number <= q3  ~ "Q3",
          value_as_number <= q4  ~ "Q4",
          value_as_number >  q4  ~ "Q5",
          .default               = "Missing"
        )
      ) |>
      select(subject_id = person_id, socioeconomic_status) |>
      distinct() |>
      compute()
    
    # add column
    cohort <- cohort |>
      left_join(
        tableSES,
        by = "subject_id"
      ) |>
      mutate(socioeconomic_status = if_else(is.na(socioeconomic_status), "Missing", socioeconomic_status)) |>
      compute(name = name, temporary = FALSE)
  }
  
  return(cohort)
}

getBRCharacteristics <- function(cohort, strata) {
  
  # Variables to add: 
  # Pregnancy: previous pregnancy
  # Comorbidities: alchohol, obesity, diabetes, hypertension, asthma, depression/anxiety, epilepsy
  # Medications: omeprazole/antiacids, diabetes treatment/s, nsaids, opioids, antidepressants, antiepilepsy, corticosteroids
  # Outcomes: all other MAE during pregnancy
  # Others: smoking status, socioeconomic status, ethnicity, season and period pregnancy start
  name <- tableName(cohort)
  cdm <- cdmReference(cohort)
  cohort <- cohort |>
    addCohortIntersectFlag(
      targetCohortTable = "mae",
      targetCohortId = c(
        "miscarriage", "stillbirth", "antepartum_haemorrhage", 
        "dysfunctional_labour", "eclampsia", "ectopic_pregnancy", 
        "gestational_diabetes", "hellp", "preeclampsia", "preterm_labour"
      ),
      indexDate = "pregnancy_start_date",
      censorDate = "pregnancy_end_date",
      targetStartDate = "cohort_start_date",
      window = list(c(0, Inf)),
      nameStyle = "{cohort_name}",
      name = name
    ) |>
    addCohortIntersectFlag(
      targetCohortTable = "mae",
      targetCohortId = "postpartum_haemorrhage",
      indexDate = "pregnancy_end_date",
      censorDate = "postpartum_12_weeks",
      targetStartDate = "cohort_start_date",
      window = list(c(0, Inf)),
      nameStyle = "{cohort_name}",
      name = name
    ) |>
    addCohortIntersectFlag(
      targetCohortTable = "mae",
      targetCohortId = "postpartum_endometritis",
      indexDate = "pregnancy_end_date",
      censorDate = "postpartum_6_weeks",
      targetStartDate = "cohort_start_date",
      window = list(c(0, Inf)),
      nameStyle = "{cohort_name}",
      name = name
    ) |>
    addCohortIntersectFlag(
      targetCohortTable = "mae",
      indexDate = "pregnancy_start_date",
      targetStartDate = "cohort_start_date",
      window = list(c(-Inf, -1)),
      nameStyle = "previous_{cohort_name}",
      name = name
    ) 
  
  maeNames <- settings(cdm$mae)$cohort_name
  estimates = c(
    list(
      'season' = c('count', 'percentage'),
      'season_yearly' = c('count', 'percentage'),
      'ethnicity' = c('count', 'percentage'),
      'socioeconomic_status' = c('count', 'percentage'),
      'maternal_age' = c('min', 'max', 'q25', 'q75', 'median', 'sd', 'mean'),
      'maternal_age_group' = c('count', 'percentage'),
      'trimester' = c('count', 'percentage'),
      'nationallity' = c('count', 'percentage'),
      'birth_continent' = c('count', 'percentage'),
      'pregnancy_start_period' = c('count', 'percentage'),
      'pre_pregnancy_smoking' = c('count', 'percentage')
    ),
    rep(list(c('count', 'percentage')), length(maeNames)) |>
      setNames(maeNames),
    rep(list(c('count', 'percentage')), length(maeNames)) |>
      setNames(paste0("previous_", maeNames))
  )
  estimates <- estimates[names(estimates) %in% colnames(cohort)]
  otherVariables = names(estimates)
  cohort |>
    summariseCharacteristics(
      counts = TRUE,
      demographics = TRUE,
      strata = strata,
      cohortIntersectFlag = list(
        # covariatesInf (-Inf, 0)
        "History of comorbidities" = list(
          targetCohortTable = "covariates_inf", window = c(-Inf, 0)
        ),
        # covariates1 (-365, 0)
        "Covariates in the past year" = list(
          targetCohortTable = "covariates_5", window = c(-365, 0)
        ),
        # Comedications
        "Medications in the past 6 months" = list(
          targetCohortTable = "comedications", window = c(-180, 0)
        )
      ),
      otherVariables = otherVariables,
      estimates = estimates
    )
}

getMiscarriageDenominator <- function(cohort) {
  cohort %>%
    mutate(
      cohort_end_date = as.Date(!!dateadd("pregnancy_start_date", 19*7 + 6))
    ) |>
    mutate(
      cohort_end_date = if_else(
        cohort_end_date > pregnancy_end_date, pregnancy_end_date, cohort_end_date
      ), 
      trimester_2_end = if_else(!is.na(trimester_2_end), cohort_end_date, trimester_2_end),
      trimester_3_start = as.Date(NA),
      trimester_3_end = as.Date(NA)
    ) |>
    compute(name = "miscarriage_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("miscarriage_denominator")
      )
    )
}

getStillbirhtDenominator <- function(cohort) {
  cohort %>%
    mutate(
      cohort_start_date = as.Date(!!dateadd("pregnancy_start_date", 20*7))
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "stillbirth_denominator", temporary = FALSE) |>
    recordCohortAttrition("Stillbirth denominator") |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("stillbirth_denominator")
      )
    )
}

getPretermDenominator <- function(cohort) {
  cohort %>%
    mutate(
      cohort_start_date = as.Date(!!dateadd("pregnancy_start_date", 20*7))
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "preterm_labour_denominator", temporary = FALSE) |>
    recordCohortAttrition("Preterm labour denominator") %>%
    mutate(
      cohort_end_date = as.Date(!!dateadd("pregnancy_start_date", 37*7))
    ) |>
    mutate(
      cohort_end_date = if_else(
        cohort_end_date > pregnancy_end_date, pregnancy_end_date, cohort_end_date
      )
    ) |>
    compute(name = "preterm_labour_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("preterm_labour_denominator")
      )
    )
}

getDysfunctionalDenominator <- function(cohort) {
  cohort %>%
    mutate(
      cohort_start_date = as.Date(!!dateadd("pregnancy_start_date", 20*7))
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "dysfunctional_labour_denominator", temporary = FALSE) |>
    recordCohortAttrition("Dysfunctional labour denominator") %>%
    compute(name = "dysfunctional_labour_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("dysfunctional_labour_denominator")
      )
    )
}

getAntepartumDenominator <- function(cohort) {
  cohort %>%
    mutate(cohort_start_date = trimester_2_start) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "antepartum_haemorrhage_denominator", temporary = FALSE) |>
    recordCohortAttrition("Antepartum haemorrhage denominator") %>%
    compute(name = "antepartum_haemorrhage_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("antepartum_haemorrhage_denominator")
      )
    )
}


getPostpartum6Denominator <- function(cohort) {
  cohort |>
    filter(pregnancy_outcome_study != "miscarriage") |>
    mutate(
      cohort_start_date = pregnancy_end_date,
      cohort_end_date = postpartum_6_end
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "postpartum_6_weeks_denominator", temporary = FALSE) |>
    recordCohortAttrition("Postpartum 6 weeks denominator") |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("postpartum_6_weeks_denominator")
      )
    )
}

getPostpartum12DenominatorSensitivity <- function(cohort) {
  cohort |>
    filter(pregnancy_outcome_study != "miscarriage") |>
    mutate(
      cohort_start_date = postpartum_12_start,
      cohort_end_date = postpartum_12_end
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "postpartum_12_weeks_denominator_sens", temporary = FALSE) |>
    recordCohortAttrition("Postpartum 12 weeks denominator") |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("postpartum_12_weeks_denominator_sens")
      )
    )
}

getPostpartum12Denominator <- function(cohort) {
  cohort |>
    filter(pregnancy_outcome_study != "miscarriage") |>
    mutate(
      cohort_start_date = pregnancy_end_date,
      cohort_end_date = postpartum_12_end
    ) |>
    filter(cohort_start_date <= cohort_end_date) |>
    compute(name = "postpartum_12_weeks_denominator", temporary = FALSE) |>
    recordCohortAttrition("Postpartum 12 weeks denominator") |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("postpartum_12_weeks_denominator")
      )
    )
}

getMaternalDeathDenominator <- function(cohort) {
  cohort |>
    mutate(
      cohort_start_date = pregnancy_start_date,
      cohort_end_date = postpartum_6_weeks
    ) |>
    compute(name = "maternal_death_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("maternal_death_denominator")
      )
    )
}

getPregnancyPostpartum12Denominator <- function(cohort) {
  cohort |>
    mutate(
      cohort_end_date = postpartum_12_end
    ) |>
    compute(name = "pregnancy_postpartum_12_denominator", temporary = FALSE) |>
    newCohortTable(
      cohortSetRef = tibble(
        cohort_definition_id = 1, cohort_name = c("pregnancy_postpartum_12_denominator")
      )
    )
}


getTimeToEvent <- function(cohort, washOut, outcomes) {
  name <- omopgenerics::tableName(cohort)
  postpartum <- "maternal_death"
  onlyPostpartum <- c("postpartum_endometritis", "postpartum_haemorrhage")
  
  if (washOut != 0) {
    for (outcome in outcomes) {
      colsExclude <- c("time_t4", "status_t4", "pregnancies_t4")
      if (outcome %in% postpartum) colsExclude <- NULL
      if (outcome %in% onlyPostpartum) colsExclude <- c("time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3")
      colsRename <- c("time", "status", "pregnancies", "time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3", "time_t4", "status_t4", "pregnancies_t4")
      colsRename <- colsRename[!colsRename %in% colsExclude]
      
      postpartumColStart <- "pregnancy_end_date"
      postpartumColEnd <- "postpartum_6_end"
      if (outcome == "postpartum_haemorrhage_sens") {
        postpartumColStart <- "postpartum_12_start"
        postpartumColEnd <- "postpartum_12_end"
      } else if (outcome == "postpartum_haemorrhage") {
        postpartumColEnd <- "postpartum_12_end"
      }
      
      cohort <- cohort %>%
        mutate(prior_outcome_washout = !!dateadd(glue::glue("prior_{outcome}"), washOut)) %>%
        mutate(
          # time at risk considering wash-out
          date_time_at_risk_start = case_when(
            is.na(.data$prior_outcome_washout) ~ cohort_start_date,
            .data$prior_outcome_washout < .data$cohort_start_date ~ cohort_start_date,
            .data$prior_outcome_washout >= .data$cohort_start_date ~ prior_outcome_washout
          ),
          # time-status overall
          status = case_when(
            date_time_at_risk_start > cohort_end_date ~ NA, # don't contribute
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute (either no time at risk or outcome before)
            .data[[outcome]] >= date_time_at_risk_start ~ 1
          ),
          time = case_when(
            status == 1 ~ !!datediff("date_time_at_risk_start", outcome),
            status == 0 ~ !!datediff("date_time_at_risk_start", "cohort_end_date"),
            is.na(status) ~ NA
          ),
          pregnancies = if_else(is.na(status), 0, 1),
          # time-status trimester 1
          status_t1 = case_when(
            date_time_at_risk_start > trimester_1_end ~ NA, # don't contribute
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= date_time_at_risk_start & .data[[outcome]] <= trimester_1_end ~ 1, # outcome during time at risk
            .data[[outcome]] > trimester_1_end ~ 0 # outcome after time at risk
          ),
          time_t1 = case_when(
            status_t1 == 1 ~ !!datediff("date_time_at_risk_start", outcome),
            status_t1 == 0 ~ !!datediff("date_time_at_risk_start", "trimester_1_end"),
            is.na(status_t1) ~ NA
          ),
          pregnancies_t1 = if_else(is.na(status_t1), 0, 1),
          # time-status trimester 2
          status_t2 = case_when(
            is.na(trimester_2_start) | date_time_at_risk_start > trimester_2_end ~ NA, # don't contribute
            .data[[outcome]] < trimester_2_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before trimester start
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= trimester_2_start & .data[[outcome]] <= trimester_2_end ~ 1, # outcome during time at risk
            .data[[outcome]] > trimester_2_end ~ 0 # outcome after time at risk
          ),
          time_t2 = case_when(
            status_t2 == 1 ~ !!datediff("trimester_2_start", outcome) + 1,
            status_t2 == 0 ~ !!datediff("trimester_2_start", "trimester_2_end") + 1,
            is.na(status_t2) ~ NA
          ),
          pregnancies_t2 = if_else(is.na(status_t2), 0, 1),
          # time-status trimester 3
          status_t3 = case_when(
            is.na(trimester_3_start) | date_time_at_risk_start > trimester_3_end ~ NA, # don't contribute
            .data[[outcome]] < trimester_3_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before trimester start
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= trimester_3_start & .data[[outcome]] <= trimester_3_end ~ 1, # outcome during time at risk
            .data[[outcome]] > trimester_3_end ~ 0 # outcome after time at risk
          ),
          time_t3 = case_when(
            status_t3 == 1 ~ !!datediff("trimester_3_start", outcome) + 1,
            status_t3 == 0 ~ !!datediff("trimester_3_start", "trimester_3_end") + 1,
            is.na(status_t3) ~ NA
          ),
          pregnancies_t3 = if_else(is.na(status_t3), 0, 1),
          # time-status postpatum 6 weeks
          status_t4 = case_when(
            is.na(.data[[postpartumColStart]]) | date_time_at_risk_start > .data[[postpartumColEnd]] ~ NA, # don't contribute
            .data[[outcome]] < .data[[postpartumColStart]] & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before trimester start
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] < date_time_at_risk_start ~ NA, # don't contribute
            .data[[outcome]] >= .data[[postpartumColStart]] & .data[[outcome]] <= .data[[postpartumColEnd]] ~ 1, # outcome during time at risk
            .data[[outcome]] > .data[[postpartumColEnd]] ~ 0 # outcome after time at risk
          ),
          time_t4 = case_when(
            status_t4 == 1 ~ !!datediff(postpartumColStart, outcome) + 1,
            status_t4 == 0 ~ !!datediff(postpartumColStart, postpartumColEnd) + 1,
            is.na(status_t4) ~ NA
          ),
          pregnancies_t4 = if_else(is.na(status_t4), 0, 1)
        ) |>
        select(!all_of(c(outcome, "prior_outcome_washout", "date_time_at_risk_start", glue::glue("prior_{outcome}"), colsExclude))) |>
        rename_with(
          .fn = \(x){paste0(outcome, "_", x)},
          .cols = colsRename
        ) |>
        compute()
    }
  } else {
    for (outcome in outcomes) {
      colsExclude <- c("time_t4", "status_t4", "pregnancies_t4")
      if (outcome %in% postpartum) colsExclude <- NULL
      if (outcome %in% onlyPostpartum) colsExclude <- c("time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3")
      colsRename <- c("time", "status", "pregnancies", "time_t1", "status_t1", "pregnancies_t1", "time_t2", "status_t2", "pregnancies_t2", "time_t3", "status_t3", "pregnancies_t3", "time_t4", "status_t4", "pregnancies_t4")
      colsRename <- colsRename[!colsRename %in% colsExclude]
      
      postpartumColStart <- "pregnancy_end_date"
      postpartumColEnd <- "postpartum_6_end"
      if (outcome == "postpartum_haemorrhage_sens") {
        postpartumColStart <- "postpartum_12_start"
        postpartumColEnd <- "postpartum_12_end"
      } else if (outcome == "postpartum_haemorrhage") {
        postpartumColEnd <- "postpartum_12_end"
      }
      
      cohort <- cohort %>%
        mutate(
          # overall
          status = if_else(is.na(.data[[outcome]]), 0, 1),
          time = if_else(is.na(.data[[outcome]]), !!datediff("cohort_start_date", "cohort_end_date"), !!datediff("cohort_start_date", outcome)),
          pregnancies = if_else(is.na(status), 0, 1),
          # trimester 1
          status_t1 = case_when(
            is.na(.data[[outcome]]) ~ 0,
            .data[[outcome]] > trimester_1_end ~ 0, # outcome after trimester 1
            .data[[outcome]] <= trimester_1_end ~ 1
          ),
          time_t1 = if_else(status_t1 == 0, !!datediff("trimester_1_start", "trimester_1_end"), !!datediff("cohort_start_date", outcome)),
          pregnancies_t1 = if_else(is.na(status_t1), 0, 1),
          # trimester 2
          status_t2 = case_when(
            is.na(trimester_2_start) ~ NA, # don't get to trimester 2
            .data[[outcome]] < trimester_2_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before
            is.na(.data[[outcome]]) ~ 0,
            .data[[outcome]] > trimester_2_end ~ 0, # outcome after trimester 2
            .data[[outcome]] <= trimester_2_end | .data[[outcome]] >= trimester_2_start  ~ 1
          ),
          time_t2 = case_when(
            status_t2 == 1 ~ !!datediff("trimester_2_start", outcome) + 1,
            status_t2 == 0 ~ !!datediff("trimester_2_start", "trimester_2_end") + 1,
            is.na(status_t2) ~ NA
          ),
          pregnancies_t2 = if_else(is.na(status_t2), 0, 1),
          # trimester 3
          status_t3 = case_when(
            is.na(trimester_3_start) ~ NA, # don't get to trimester 3
            .data[[outcome]] < trimester_3_start & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before
            is.na(.data[[outcome]]) ~ 0,
            .data[[outcome]] > trimester_3_end ~ 0, # outcome after trimester 3
            .data[[outcome]] >= trimester_3_start  ~ 1
          ),
          time_t3 = case_when(
            status_t3 == 1 ~ !!datediff("trimester_3_start", outcome) + 1,
            status_t3 == 0 ~ !!datediff("trimester_3_start", "trimester_3_end") + 1,
            is.na(status_t3) ~ NA
          ),
          pregnancies_t3 = if_else(is.na(status_t3), 0, 1),
          # time-status postpatum 6 weeks
          status_t4 = case_when(
            is.na(.data[[postpartumColStart]]) ~ NA, # don't get to postpartum 
            .data[[outcome]] < .data[[postpartumColStart]] & !is.na(.data[[outcome]]) ~ NA, # don't contribute: outcome before 
            is.na(.data[[outcome]]) ~ 0, # no outcome
            .data[[outcome]] >= .data[[postpartumColStart]] & .data[[outcome]] <= .data[[postpartumColEnd]] ~ 1, # outcome during time at risk
            .data[[outcome]] > .data[[postpartumColEnd]] ~ 0 # outcome after time at risk
          ),
          time_t4 = case_when(
            status_t4 == 1 ~ !!datediff(postpartumColStart, outcome) + 1,
            status_t4 == 0 ~ !!datediff(postpartumColStart, postpartumColEnd) + 1,
            is.na(status_t4) ~ NA
          ),
          pregnancies_t4 = if_else(is.na(status_t4), 0, 1)
        ) |>
        select(!all_of(c(outcome, colsExclude))) %>% 
        {if (outcome %in% onlyPostpartum) {
          mutate(., time_t4 = time, status_t4 = status, pregnancies_t4 = pregnancies)
        } else .} |>
        rename_with(
          .fn = \(x){paste0(outcome, "_", x)},
          .cols = colsRename
        ) |>
        compute()
    }
  }
  cohort <- cohort |>
    compute(name = name, temporary = FALSE, overwrite = TRUE)
  return(cohort)
}

addBRDenominatorStrata <- function(cohort) {
  name <- omopgenerics::tableName(cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  cohort <- cohort |>
    addAge(
      ageName = "maternal_age",
      ageGroup = list("12 to 17" = c(12, 17), "18 to 34" = c(18, 34), "35 to 55" = c(35, 55))
    ) |>
    mutate(
      pregnancy_start_period = case_when(
        year(pregnancy_start_date) %in% 2018:2019 ~ "Pre COVID-19",
        year(pregnancy_start_date) %in% 2020:2021 ~ "COVID-19 main outbreak",
        .default = "Post COVID-19 main outbreak"
      )
    ) |>
    compute(name = "pregnancy_denominator", temporary = FALSE)
  
  strata <- list("maternal_age", "pregnancy_start_period")
  if (cdmName(cdm) %in% c("CPRD AURUM", "CPRD GOLD", "SIDIAP")) {
    cohort <- cohort |>
      addSocioeconomicStatus() |>
      addEthnicity() |>
      compute(name = "pregnancy_denominator", temporary = FALSE)
    strata <- c(strata, list("socioeconomic_status", "ethnicity"))
  }
  return(cohort)
}

estimateIncidenceRate <- function(cohort, strata, outcomes) {
  variables <- c(
    paste0(outcomes, "_status"), paste0(outcomes, "_time"), paste0(outcomes, "_pregnancies"),
    paste0(outcomes, "_status_t1"), paste0(outcomes, "_time_t1"), paste0(outcomes, "_pregnancies_t1"),
    paste0(outcomes, "_status_t2"), paste0(outcomes, "_time_t2"), paste0(outcomes, "_pregnancies_t2"),
    paste0(outcomes, "_status_t3"), paste0(outcomes, "_time_t3"), paste0(outcomes, "_pregnancies_t3"),
    paste0(outcomes, "_status_t4"), paste0(outcomes, "_time_t4"), paste0(outcomes, "_pregnancies_t4")
  )
  variables <- variables[variables %in% colnames(cohort)]
  summarise_ir <- cohort |>
    summariseResult(
      strata = strata,
      includeOverallStrata = TRUE,
      variables = variables,
      estimates = "sum",
      counts = FALSE,
      weights = NULL
    ) |>
    reformatResult() |>
    addIncidenceRate()
  if ("pregnancy_start_period" %in% unlist(strata)) {
    summarise_ir <- summarise_ir |> addByPeriodEvents(cohort)
  }
  
  if (all(outcomes %in% c(
    "preterm_labour", "miscarriage", "stillbirth", "maternal_death",
    "dysfunctional_labour", "eclampsia", "ectopic_pregnancy",
    "antepartum_haemorrhage", "gestational_diabetes", "hellp", "preeclampsia",
    "postpartum_endometritis", "postpartum_haemorrhage", "miscarriage_codelist"
  ))) {
    summarise_ir <- summarise_ir |>
      mutate(
        additional_name = "outcome_group",
        additional_level = "Maternal Adverse Events"
      ) |>
      omopgenerics::newSummarisedResult()
  } else {
    summarise_ir <- summarise_ir |>
      mutate(
        additional_name = "outcome_group",
        additional_level = "Adverse Events of Special Interest"
      ) |>
      omopgenerics::newSummarisedResult()
  }
  
  return(summarise_ir)
}

reformatResult <- function(x) {
  x |>
    splitStrata() |>
    mutate(
      group_name = "outcome_cohort_name",
      group_level = gsub("_status|_time|_pregnancies|_t1|_t2|_t3|_t4", "", .data$variable_name),
      gestational_trimester = case_when(
        grepl("_t1", .data$variable_name) ~ "Trimester 1",
        grepl("_t2", .data$variable_name) ~ "Trimester 2",
        grepl("_t3", .data$variable_name) ~ "Trimester 3",
        grepl("_t4", .data$variable_name) ~ "Postpartum",
        .default = "overall"
      ),
      estimate_name = case_when(
        grepl("pregnancies", .data$variable_name) ~ "denominator_count",
        grepl("time", .data$variable_name) ~ "person_days_count",
        grepl("status", .data$variable_name) ~ "outcome_count",
      ),
      estimate_type = "integer"
    ) |>
    mutate(
      variable_name = "Incidence Rates"
    ) |>
    uniteStrata(cols = c(strataColumns(x), "gestational_trimester")) |>
    newSummarisedResult(settings = NULL)
}

addIncidenceRate <- function(x) {
  bind_rows(
    x,
    x |>
      select(!variable_name) |>
      mutate(estimate_value = as.numeric(estimate_value)) |>
      pivot_wider(values_from = "estimate_value", names_from = "estimate_name") |>
      mutate(
        person_years = round(.data$person_days_count / 365.25, 3),
        incidence_100000_pys = round(((.data$outcome_count / .data$person_years) * 100000), 3),
        incidence_100000_pys_95CI_lower = round(((stats::qchisq(p = 0.025, df = 2 * .data$outcome_count) / 2) / .data$person_years) * 100000, 3),
        incidence_100000_pys_95CI_upper = round(((stats::qchisq(p = 0.975, df = 2 * (.data$outcome_count + 1)) / 2) / .data$person_years) * 100000, 3)
      ) |>
      select(!c("denominator_count", "outcome_count", "person_days_count")) |>
      pivot_longer(
        cols = c("person_years", "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper"),
        names_to = "estimate_name", values_to = "estimate_value"
      ) |>
      mutate(
        variable_name ="Incidence Rates",
        estimate_type = "numeric",
        estimate_value = as.character(estimate_value)
      )
  )
}

addByPeriodEvents <- function(x, cohort) {
  onlyPostpartum <- c("postpartum_endometritis", "postpartum_haemorrhage")
  postpartum <- "maternal_death"
  outcomes <- x$group_level |> unique()
  overallCounts <- x |>
    filter(estimate_name == "outcome_count") |>
    filterStrata(pregnancy_start_period %in% c("Pre COVID-19", "COVID-19 main outbreak")) |>
    splitStrata() |>
    mutate(
      estimate_value = as.numeric(estimate_value),
      gestational_trimester = omopgenerics::toSnakeCase(gestational_trimester)
    ) |>
    pivot_wider(names_from = "gestational_trimester", values_from = "estimate_value") |>
    select(any_of(c("group_level", "pregnancy_start_period", "overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
  result <- NULL
  for (outcome in outcomes) {
    periodResult <- cohort |>
      filter(.data[[paste0(outcome, "_status")]] == 1, pregnancy_start_period %in% c("Pre COVID-19", "COVID-19 main outbreak")) |>
      mutate(
        # socioeconomic_status = as.character(socioeconomic_status),
        days_to_end_period = case_when(
          pregnancy_start_period == "Pre COVID-19" ~ as.Date("2019-12-31"),
          pregnancy_start_period == "COVID-19 main outbreak" ~ as.Date("2021-12-31"),
          .default = as.Date(NA)
        )
      ) %>%
      mutate(
        days_to_end_period = !!datediff("pregnancy_start_date", "days_to_end_period")
      ) |>
      select(any_of(c("pregnancy_start_period", "days_to_end_period", paste0(outcome, c("_time", "_status", "_time_t1", "_status_t1", "_time_t2", "_status_t2", "_time_t3", "_status_t3", "_time_t4", "_status_t4"))))) |>
      collect()
    
    if (outcome %in% onlyPostpartum) {
      periodResult <- periodResult |>
        mutate(
          outcome_in_period_count = if_else(.data[[paste0(outcome, "_status")]] == 1 & (days_to_end_period >= .data[[paste0(outcome, "_time")]] | pregnancy_start_period != "Post COVID-19 main outbreak"), 1, 0),
          outcome_in_period_t4_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t4")]]  == 1, 1, 0)
        ) |>
        group_by(pregnancy_start_period) |>
        summarise(
          outcome_in_period_count = sum(outcome_in_period_count, na.rm = TRUE),
          outcome_in_period_t4_count = sum(outcome_in_period_t4_count, na.rm = TRUE)
        ) |>
        inner_join(overallCounts |> filter(group_level == outcome), by = "pregnancy_start_period") |>
        mutate(
          outcome_in_period_percentage = if_else(outcome_in_period_count != 0, outcome_in_period_count/overall * 100, 0),
          outcome_in_period_t4_percentage = if_else(outcome_in_period_t4_count != 0, outcome_in_period_t4_count/postpartum * 100, 0)
        ) |>
        select(!any_of(c("overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
    } else if (outcome %in% postpartum) {
      periodResult <- periodResult |>
        mutate(
          outcome_in_period_count = if_else(.data[[paste0(outcome, "_status")]] == 1 & (days_to_end_period >= .data[[paste0(outcome, "_time")]] | pregnancy_start_period != "Post COVID-19 main outbreak"), 1, 0),
          outcome_in_period_t1_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t1")]] == 1, 1, 0),
          outcome_in_period_t2_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t2")]]  == 1, 1, 0),
          outcome_in_period_t3_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t3")]]  == 1, 1, 0),
          outcome_in_period_t4_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t4")]]  == 1, 1, 0)
        ) |>
        group_by(pregnancy_start_period) |>
        summarise(
          outcome_in_period_count = sum(outcome_in_period_count, na.rm = TRUE),
          outcome_in_period_t1_count = sum(outcome_in_period_t1_count, na.rm = TRUE),
          outcome_in_period_t2_count = sum(outcome_in_period_t2_count, na.rm = TRUE),
          outcome_in_period_t3_count = sum(outcome_in_period_t3_count, na.rm = TRUE),
          outcome_in_period_t4_count = sum(outcome_in_period_t4_count, na.rm = TRUE)
        ) |>
        inner_join(overallCounts |> filter(group_level == outcome), by = "pregnancy_start_period") |>
        mutate(
          outcome_in_period_percentage = if_else(outcome_in_period_count != 0, outcome_in_period_count/overall * 100, 0),
          outcome_in_period_t1_percentage = if_else(outcome_in_period_t1_count != 0, outcome_in_period_t1_count/trimester_1 * 100, 0),
          outcome_in_period_t2_percentage = if_else(outcome_in_period_t2_count != 0, outcome_in_period_t2_count/trimester_2 * 100, 0),
          outcome_in_period_t3_percentage = if_else(outcome_in_period_t3_count != 0, outcome_in_period_t3_count/trimester_3 * 100, 0),
          outcome_in_period_t4_percentage = if_else(outcome_in_period_t4_count != 0, outcome_in_period_t4_count/postpartum * 100, 0)
        ) |>
        select(!any_of(c("overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
    } else {
      periodResult <- periodResult |>
        mutate(
          outcome_in_period_count = if_else(.data[[paste0(outcome, "_status")]] == 1 & (days_to_end_period >= .data[[paste0(outcome, "_time")]] | pregnancy_start_period != "Post COVID-19 main outbreak"), 1, 0),
          outcome_in_period_t1_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t1")]] == 1, 1, 0),
          outcome_in_period_t2_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t2")]]  == 1, 1, 0),
          outcome_in_period_t3_count = if_else(outcome_in_period_count == 1 & .data[[paste0(outcome, "_status_t3")]]  == 1, 1, 0)
        ) |>
        group_by(pregnancy_start_period) |>
        summarise(
          outcome_in_period_count = sum(outcome_in_period_count, na.rm = TRUE),
          outcome_in_period_t1_count = sum(outcome_in_period_t1_count, na.rm = TRUE),
          outcome_in_period_t2_count = sum(outcome_in_period_t2_count, na.rm = TRUE),
          outcome_in_period_t3_count = sum(outcome_in_period_t3_count, na.rm = TRUE)
        ) |>
        inner_join(overallCounts |> filter(group_level == outcome), by = "pregnancy_start_period") |>
        mutate(
          outcome_in_period_percentage = if_else(outcome_in_period_count != 0, outcome_in_period_count/overall * 100, 0),
          outcome_in_period_t1_percentage = if_else(outcome_in_period_t1_count != 0, outcome_in_period_t1_count/trimester_1 * 100, 0),
          outcome_in_period_t2_percentage = if_else(outcome_in_period_t2_count != 0, outcome_in_period_t2_count/trimester_2 * 100, 0),
          outcome_in_period_t3_percentage = if_else(outcome_in_period_t3_count != 0, outcome_in_period_t3_count/trimester_3 * 100, 0)
        ) |>
        select(!any_of(c("overall", "trimester_1", "trimester_2", "trimester_3", "postpartum")))
    }
    
    # bind results
    result <- bind_rows(result, periodResult)
  }
  if (nrow(result) == 0) return(x)
  result <- result |>
    pivot_longer(
      cols = any_of(c(
        paste0("outcome_in_period", c("_count", "_percentage")), paste0("outcome_in_period_t1", c("_count", "_percentage")),
        paste0("outcome_in_period_t2", c("_count", "_percentage")), paste0("outcome_in_period_t3", c("_count", "_percentage")),
        paste0("outcome_in_period_t4", c("_count", "_percentage")))
      ),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    mutate(
      result_id = 1L,
      cdm_name = unique(x$cdm_name),
      group_name = "outcome_cohort_name",
      variable_name = "Incidence Rates",
      variable_level = NA_character_,
      gestational_trimester = case_when(
        grepl("t1", .data$estimate_name) ~ "Trimester 1",
        grepl("t2", .data$estimate_name) ~ "Trimester 2",
        grepl("t3", .data$estimate_name) ~ "Trimester 3",
        grepl("t4", .data$estimate_name) ~ "Postpartum",
        .default = "overall"
      ),
      estimate_name = gsub("_t1|_t2|_t3|_t4", "", .data$estimate_name),
      estimate_type = if_else(grepl("count", estimate_name), "integer", "percentage"),
      estimate_value = as.character(estimate_value),
      additional_name = "overall",
      additional_level = "overall"
    ) |>
    uniteStrata(cols = c("pregnancy_start_period", "gestational_trimester")) |>
    distinct()
  
  return(bind_rows(x, result))
}

getMatchedCohort <- function(cohort, outcomes, name) {
  tmp <- tmpPrefix()
  tabName <- paste0(tmp, "matching")
  cdm <- omopgenerics::emptyCohortTable(cdm = cdm, name = tabName)
  
  onlyPostpartum <- c("postpartum_endometritis", "postpartum_haemorrhage")
  onlyPostpartumSens <- c("postpartum_haemorrhage_sens")
  postpartum <- "maternal_death"
  
  strata <- c("maternal_age_group", "pregnancy_start_period")
  if (grepl("SIDIAP", cdmName(cdm))) {
    strata <- c(strata, "socioeconomic_status", "nationallity")
  }
  if (grepl("CPRD", cdmName(cdm))) {
    strata <- c(strata, "socioeconomic_status", "ethnicity")
  }
  if (grepl("NLHR@UiO", cdmName(cdm))) {
    strata <- c(strata, "birth_continent")
  }
  if (grepl("SCIFI-PEARL", cdmName(cdm))) {
    strata <- c(strata, "socioeconomic_status", "birth_continent")
  }
  
  for (outcome in outcomes) {
    nameMatch <- paste0(tmp, "match")
    nameSample <- paste0(tmp, "sample")
    nameOriginal <- paste0(tmp, "original")
    # get pregnancies with the outcome
    outcome_cohort <- cohort |>
      filter(.data[[paste0(outcome, "_status")]] == 1) |>
      mutate(
        cohort_definition_id = 1L,
        cohort_start_date = as.Date(clock::add_days(.data$cohort_start_date, .data[[paste0(outcome, "_time")]])),
        cohort_end_date = cohort_start_date
      ) |>
      select(all_of(c(
        "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "pregnancy_start_date", "pregnancy_end_date", "pre_pregnancy_smoking", "maternal_age", strata
      ))) |>
      compute(name = nameOriginal, temporary = FALSE, overwrite = TRUE) |>
      newCohortTable(
        cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = outcome),
        cohortAttritionRef = NULL,
        cohortCodelistRef = NULL
      )
    # if more than 5 pregnancies --> do matching
    if (outcome_cohort |> tally() |> pull() >= 5) {
      # get outcome free pregnancies (outcome_match)
      outcome_match <- cohort |>
        filter(.data[[paste0(outcome, "_status")]] == 0) |>
        select(all_of(c(
          "subject_id", "pregnancy_start_date", "pregnancy_end_date", "pre_pregnancy_smoking", "maternal_age", "cohort_end_date", strata
        ))) |>
        mutate(
          pregnancy_start_band = if_else(
            day(pregnancy_start_date) <= 15,
            paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
            paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
          ),
          age_group_sample = cut(maternal_age, !!seq(12, 56, 2), include.lowest = TRUE, right = FALSE)
        ) |>
        compute(name = nameMatch, temporary = FALSE, overwrite = TRUE)
      # match by "pregnancy_start_band" and "age_group_sample"
      outcome_sampled <- outcome_cohort |>
        mutate(
          pregnancy_start_band = if_else(
            day(pregnancy_start_date) <= 15,
            paste0("01", month(pregnancy_start_date), year(pregnancy_start_date)),
            paste0("02", month(pregnancy_start_date), year(pregnancy_start_date))
          ),
          age_group_sample = cut(maternal_age, !!seq(12, 56, 2), include.lowest = TRUE, right = FALSE)
        ) |>
        inner_join(
          outcome_match |>
            rename(
              "matched_subject_id" = "subject_id",
              "matched_pregnancy_start_date" = "pregnancy_start_date",
              "matched_pregnancy_end_date" = "pregnancy_end_date",
              "matched_maternal_age" = "maternal_age",
              "matched_cohort_end_date" = "cohort_end_date",
              "matched_pre_pregnancy_smoking" = "pre_pregnancy_smoking"
            ) |>
            rename_with(.fn = \(x){glue("matched_{x}")}, .cols = strata),
          by = c("age_group_sample", "pregnancy_start_band")
        ) |>
        compute(name = nameSample, temporary = FALSE, overwrite = TRUE) 
      # make sure match index date (cohort_start_date) is during pregnancy / postpartum
      if (outcome %in% onlyPostpartum) {
        outcome_sampled <- outcome_sampled |>
          filter(matched_pregnancy_end_date <= cohort_start_date & matched_cohort_end_date >= cohort_start_date)
      } else if (outcome %in% onlyPostpartumSens) {
        outcome_sampled <- outcome_sampled |>
          filter(matched_pregnancy_end_date < cohort_start_date & matched_cohort_end_date >= cohort_start_date)
      } else if (outcome %in% postpartum) {
        outcome_sampled <- outcome_sampled |>
          filter(matched_pregnancy_start_date <= cohort_start_date & matched_cohort_end_date >= cohort_start_date)
      } else {
        outcome_sampled <- outcome_sampled |>
          filter(matched_pregnancy_start_date <= cohort_start_date & matched_pregnancy_end_date >= cohort_start_date) 
      }
      # get 1:1 ratio + formatting
      outcome_sampled <- outcome_sampled |>
        slice_sample(n = 1, by = "matched_subject_id") |>
        slice_sample(n = 1, by = c("subject_id", "cohort_start_date")) |>
        select(!"matched_cohort_end_date") |>
        compute(name = nameSample, temporary = FALSE, overwrite = TRUE) 
      outcome_match <- outcome_sampled  |>
        select(!c(
          "subject_id",
          "pregnancy_start_date",
          "pregnancy_end_date",
          "maternal_age",
          "pre_pregnancy_smoking",
          strata
        )) |>
        rename_with(.fn = \(x){gsub("matched_", "", x)}) |>
        select(c(
          "cohort_definition_id", "subject_id",
          "cohort_start_date", "cohort_end_date",
          "pregnancy_start_date",
          "pregnancy_end_date",
          "maternal_age",
          "pre_pregnancy_smoking",
          strata
        )) |>
        compute(name = nameMatch, temporary = FALSE, overwrite = TRUE) |>
        newCohortTable(
          cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = paste0(outcome, "_matched")),
          cohortAttritionRef = NULL,
          cohortCodelistRef = NULL
        )
      outcome_sampled <- outcome_sampled |>
        select(c(
          "cohort_definition_id", "subject_id",
          "cohort_start_date", "cohort_end_date",
          "pregnancy_start_date",
          "pregnancy_end_date",
          "maternal_age",
          "pre_pregnancy_smoking",
          strata
        )) |>
        compute(name = nameSample, temporary = FALSE, overwrite = TRUE) |>
        newCohortTable(
          cohortSetRef = tibble(cohort_definition_id = 1L, cohort_name = paste0(outcome, "_sampled")),
          cohortAttritionRef = NULL,
          cohortCodelistRef = NULL
        )
      cdm <- bind(outcome_cohort, outcome_match, outcome_sampled, cdm[[tabName]], name = tabName)
    }
  }
  cdm[[name]] <- cdm[[tabName]] |>
    compute(name = name, temporary = FALSE, overwrite = TRUE) |>
    newCohortTable()
  dropSourceTable(cdm = cdm, starts_with(tmp))
  return(cdm[[name]])
}

cohortCodeUseFromCohort <- function(cohort) {
  cdm <- cdmReference(cohort)
  name <- tableName(cohort)
  summaryCodeUse <- NULL
  codelist <- attr(cohort, "cohort_codelist") |> collect()
  for (id in settings(cohort)$cohort_definition_id) {
    codelist.id <- codelist |>
      filter(cohort_definition_id == id)
    if (nrow(codelist.id) > 0) {
      codelist.id <- split(as.integer(codelist.id$concept_id), codelist.id$codelist_name)
      summaryCodeUse <- bind(
        summaryCodeUse,
        summariseCohortCodeUse(codelist.id, cdm, name, cohortId = id, timing = "entry")
      ) 
    }
  }
  return(summaryCodeUse)
}
