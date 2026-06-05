# Run this block after your original code has created df_teacher_new.
# It adds the requested variables without overwriting your original first_day_* columns.

library(dplyr)
library(lubridate)
library(bigrquery)

make_analysis_continuous <- function(data, first_col, before_col, analysis_col) {
  if (!all(c(first_col, before_col, "Sepsis_first_day") %in% names(data))) return(data)

  data[[analysis_col]] <- ifelse(
    data$Sepsis_first_day == 1,
    data[[before_col]],
    data[[first_col]]
  )
  data
}

make_analysis_binary <- function(data, first_col, before_col, analysis_col) {
  if (!all(c(first_col, before_col, "Sepsis_first_day") %in% names(data))) return(data)

  data[[analysis_col]] <- ifelse(
    data$Sepsis_first_day == 1,
    coalesce(data[[before_col]], 0),
    data[[first_col]]
  )
  data
}

first_nonmissing_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

lab_names_for_analysis <- c(
  "wbc", "hemoglobin", "platelet",
  "sodium", "potassium", "chloride", "bicarbonate", "bun", "creatinine", "glucose",
  "alt", "ast", "bilirubin_total",
  "pt", "inr", "ptt", "fibrinogen", "d_dimer"
)

# Remove only columns created by previous versions of this add-on script, then
# recreate them cleanly. Original columns from your main script are not touched.
old_added_cols <- c(
  "sepsis_time",
  "No_GCS_Measured",
  paste0(lab_names_for_analysis, "_before_sepsis"),
  paste0("analysis_", lab_names_for_analysis),
  "gcs_before_sepsis",
  "analysis_gcs",
  "cam_icu_before_sepsis",
  "analysis_cam_icu",
  "drug_used_before_sepsis",
  "analysis_drug_used",
  "delirium_drug_before_sepsis_v2",
  "analysis_delirium_drug",
  "invasive_vent_before_sepsis",
  "analysis_invasive_vent",
  "noninvasive_vent_before_sepsis",
  "analysis_noninvasive_vent",
  "hfnc_before_sepsis",
  "analysis_hfnc",
  "tracheostomy_before_sepsis",
  "analysis_tracheostomy",
  "norepinephrine_before_sepsis",
  "analysis_norepinephrine",
  "epinephrine_before_sepsis",
  "analysis_epinephrine",
  "vasopressin_before_sepsis",
  "analysis_vasopressin",
  "phenylephrine_before_sepsis",
  "analysis_phenylephrine",
  "dopamine_before_sepsis",
  "analysis_dopamine",
  "n_gcs_before_sepsis",
  "gcs_min_before_sepsis",
  "gcs_min_after_sepsis_24h",
  "GCS_dropping",
  "sofa_total_at_sepsis",
  "sofa_respiration_at_sepsis",
  "sofa_coagulation_at_sepsis",
  "sofa_liver_at_sepsis",
  "sofa_cardiovascular_at_sepsis",
  "sofa_cns_at_sepsis",
  "sofa_renal_at_sepsis",
  "sofa_other_at_sepsis",
  "SOFA_CNS_DROP",
  "SOFA_OTHER_DROP",
  "SAE_CAM_ICU",
  "SAE_CAM_ICU_time",
  "SAE_delirium_drug",
  "SAE_GCS_drop",
  "SAE",
  "SAE_time_v2"
)

df_teacher_new <- df_teacher_new %>%
  select(-any_of(old_added_cols))

# 1) Sepsis_first_day:
# Binary variable for whether sepsis occurred during the first ICU day.
df_teacher_new <- df_teacher_new %>%
  mutate(
    .intime_tmp = as.POSIXct(intime),
    .culture_time_tmp = as.POSIXct(culture_time),
    .sofa_time_tmp = as.POSIXct(sofa_time),
    sepsis_time = case_when(
      !is.na(.culture_time_tmp) & !is.na(.sofa_time_tmp) ~ pmax(.culture_time_tmp, .sofa_time_tmp),
      !is.na(.culture_time_tmp) ~ .culture_time_tmp,
      !is.na(.sofa_time_tmp) ~ .sofa_time_tmp,
      TRUE ~ as.POSIXct(NA)
    ),
    Sepsis_first_day = as.integer(!is.na(sepsis_time) & sepsis_time <= .intime_tmp + hours(24))
  ) %>%
  select(-any_of(c(".intime_tmp", ".culture_time_tmp", ".sofa_time_tmp")))

stay_sepsis_map <- df_teacher_new %>%
  select(stay_id, hadm_id, intime, sepsis_time, Sepsis_first_day) %>%
  mutate(
    intime = as.POSIXct(intime),
    sepsis_time = as.POSIXct(sepsis_time)
  )

# 2) No_GCS_Measured:
# 1 = no valid GCS measurement before sepsis_time; 0 = at least one GCS before sepsis_time.
gcs_ts2 <- bq_table_download(bq_project_query(projectid, sprintf("
  SELECT stay_id, charttime, gcs
  FROM `physionet-data.mimiciv_3_1_derived.gcs`
  WHERE stay_id IN (%s)
", safe_sql(s))), page_size = 1e5) %>%
  mutate(charttime = as.POSIXct(charttime))

gcs_before_sepsis <- gcs_ts2 %>%
  left_join(stay_sepsis_map %>% select(stay_id, sepsis_time), by = "stay_id") %>%
  filter(!is.na(sepsis_time), charttime < sepsis_time, !is.na(gcs)) %>%
  group_by(stay_id) %>%
  summarise(
    n_gcs_before_sepsis = n(),
    gcs_before_sepsis = min(gcs, na.rm = TRUE),
    .groups = "drop"
  )

df_teacher_new <- df_teacher_new %>%
  left_join(gcs_before_sepsis, by = "stay_id") %>%
  mutate(
    n_gcs_before_sepsis = coalesce(n_gcs_before_sepsis, 0L),
    No_GCS_Measured = if_else(is.na(sepsis_time), NA_integer_, as.integer(n_gcs_before_sepsis == 0))
  )

if ("first_day_gcs" %in% names(df_teacher_new)) {
  df_teacher_new <- make_analysis_continuous(
    df_teacher_new,
    "first_day_gcs",
    "gcs_before_sepsis",
    "analysis_gcs"
  )
}

# 3) For first_day_* analysis features:
# If sepsis occurred in the first ICU day, use the closest available value
# before sepsis_time; otherwise use the original first_day_* value.

# 3a) Laboratory first_day_* variables.
if (exists("all_labs")) {
  available_labs <- intersect(lab_names_for_analysis, names(all_labs))

  all_labs_sepsis <- all_labs %>%
    left_join(stay_sepsis_map %>% select(hadm_id, sepsis_time), by = "hadm_id") %>%
    mutate(
      charttime = as.POSIXct(charttime),
      sepsis_time = as.POSIXct(sepsis_time)
    )

  for (v in available_labs) {
    first_col <- paste0("first_day_", v)
    before_col <- paste0(v, "_before_sepsis")
    analysis_col <- paste0("analysis_", v)

    if (!first_col %in% names(df_teacher_new)) next

    labs_before_sepsis <- all_labs_sepsis %>%
      filter(!is.na(sepsis_time), charttime < sepsis_time, !is.na(.data[[v]])) %>%
      arrange(hadm_id, desc(charttime)) %>%
      distinct(hadm_id, .keep_all = TRUE) %>%
      select(hadm_id, all_of(v)) %>%
      rename(!!before_col := all_of(v))

    df_teacher_new <- df_teacher_new %>%
      left_join(labs_before_sepsis, by = "hadm_id")

    df_teacher_new <- make_analysis_continuous(
      df_teacher_new,
      first_col,
      before_col,
      analysis_col
    )
  }
}

# 3b) CAM-ICU first_day variable.
if (exists("cam_icu") && "first_day_cam_icu" %in% names(df_teacher_new)) {
  cam_before_sepsis <- cam_icu %>%
    left_join(stay_sepsis_map %>% select(stay_id, sepsis_time), by = "stay_id") %>%
    mutate(charttime = as.POSIXct(charttime)) %>%
    filter(!is.na(sepsis_time), charttime < sepsis_time) %>%
    arrange(stay_id, charttime) %>%
    group_by(stay_id) %>%
    summarise(
      cam_icu_before_sepsis = first_nonmissing_or_na(case_when(
        cam_icu_derived == "Positive" ~ 1,
        cam_icu_derived == "Negative" ~ 0,
        TRUE ~ NA_real_
      )),
      .groups = "drop"
    )

  df_teacher_new <- df_teacher_new %>%
    left_join(cam_before_sepsis, by = "stay_id")

  df_teacher_new <- make_analysis_continuous(
    df_teacher_new,
    "first_day_cam_icu",
    "cam_icu_before_sepsis",
    "analysis_cam_icu"
  )
}

# 3c) Ventilation first_day variables.
if (exists("vent")) {
  vent_before_sepsis <- vent %>%
    left_join(stay_sepsis_map %>% select(stay_id, sepsis_time), by = "stay_id") %>%
    mutate(starttime = as.POSIXct(starttime)) %>%
    filter(!is.na(sepsis_time), starttime < sepsis_time) %>%
    group_by(stay_id) %>%
    summarise(
      invasive_vent_before_sepsis = as.integer(any(ventilation_status == "InvasiveVent", na.rm = TRUE)),
      noninvasive_vent_before_sepsis = as.integer(any(ventilation_status == "NonInvasiveVent", na.rm = TRUE)),
      hfnc_before_sepsis = as.integer(any(ventilation_status == "HFNC", na.rm = TRUE)),
      tracheostomy_before_sepsis = as.integer(any(ventilation_status == "Tracheostomy", na.rm = TRUE)),
      .groups = "drop"
    )

  df_teacher_new <- df_teacher_new %>%
    left_join(vent_before_sepsis, by = "stay_id")

  df_teacher_new <- make_analysis_binary(df_teacher_new, "first_day_invasive_vent", "invasive_vent_before_sepsis", "analysis_invasive_vent")
  df_teacher_new <- make_analysis_binary(df_teacher_new, "first_day_noninvasive_vent", "noninvasive_vent_before_sepsis", "analysis_noninvasive_vent")
  df_teacher_new <- make_analysis_binary(df_teacher_new, "first_day_hfnc", "hfnc_before_sepsis", "analysis_hfnc")
  df_teacher_new <- make_analysis_binary(df_teacher_new, "first_day_tracheostomy", "tracheostomy_before_sepsis", "analysis_tracheostomy")
}

# 3d) Vasoactive first_day variables.
if (exists("vaso")) {
  vaso_agents <- intersect(
    c("norepinephrine", "epinephrine", "vasopressin", "phenylephrine", "dopamine"),
    names(vaso)
  )

  if (length(vaso_agents) > 0) {
    vaso_before_sepsis <- vaso %>%
      left_join(stay_sepsis_map %>% select(stay_id, sepsis_time), by = "stay_id") %>%
      mutate(starttime = as.POSIXct(starttime)) %>%
      filter(!is.na(sepsis_time), starttime < sepsis_time) %>%
      group_by(stay_id) %>%
      summarise(
        across(all_of(vaso_agents), ~ as.integer(any(. > 0, na.rm = TRUE)), .names = "{.col}_before_sepsis"),
        .groups = "drop"
      )

    df_teacher_new <- df_teacher_new %>%
      left_join(vaso_before_sepsis, by = "stay_id")

    for (agent in vaso_agents) {
      df_teacher_new <- make_analysis_binary(
        df_teacher_new,
        paste0("first_day_", agent),
        paste0(agent, "_before_sepsis"),
        paste0("analysis_", agent)
      )
    }
  }
}

# 3e) Delirium drug first_day variables, using prescription starttime before sepsis.
if (any(c("first_day_delirium_drug", "first_day_drug_used") %in% names(df_teacher_new))) {
  delirium_rx_before_sepsis <- bq_table_download(bq_project_query(projectid, sprintf("
    SELECT hadm_id, starttime, drug
    FROM `physionet-data.mimiciv_3_1_hosp.prescriptions`
    WHERE hadm_id IN (%s)
      AND REGEXP_CONTAINS(LOWER(drug), 'haloperidol|olanzapine|quetiapine|risperidone|lorazepam|midazolam|propofol')
  ", safe_sql(h))), page_size = 1e5) %>%
    left_join(stay_sepsis_map %>% select(hadm_id, sepsis_time), by = "hadm_id") %>%
    mutate(starttime = as.POSIXct(starttime)) %>%
    filter(!is.na(sepsis_time), starttime < sepsis_time) %>%
    distinct(hadm_id) %>%
    mutate(
      delirium_drug_before_sepsis_v2 = 1L,
      drug_used_before_sepsis = 1L
    )

  df_teacher_new <- df_teacher_new %>%
    left_join(delirium_rx_before_sepsis, by = "hadm_id")

  df_teacher_new <- make_analysis_binary(
    df_teacher_new,
    "first_day_delirium_drug",
    "delirium_drug_before_sepsis_v2",
    "analysis_delirium_drug"
  )

  df_teacher_new <- make_analysis_binary(
    df_teacher_new,
    "first_day_drug_used",
    "drug_used_before_sepsis",
    "analysis_drug_used"
  )
}

write.csv(
  df_teacher_new,
  "~/Desktop/df_teacher_PERFECT_with_required_firstday_features.csv",
  row.names = FALSE,
  na = ""
)

length(names(df_teacher_new))
names(df_teacher_new)
