library(data.table)
library(dplyr)
library(grid)

input_path <- "/Users/haoyunniu/Desktop/df_eicu_sepsis3_clean_with_base_first_day_vars_with_complete_death_outcomes_initial.csv"
mimic_table1_path <- "/Users/haoyunniu/Desktop/sepsis3_presepsis_exclusion_outputs/table1_patient_characteristics_after_exclusion.csv"
patient_extra_cache <- "/Users/haoyunniu/Documents/Codex/2026-06-25/files-mentioned-by-the-user-df/eicu_outputs/table1_mimic_eicu_compare/eicu_patient_extra_for_table1.csv"
desktop_dir <- "/Users/haoyunniu/Desktop"

filtered_csv <- file.path(desktop_dir, "df_eicu_sepsis3_clean_with_base_first_day_vars_with_complete_death_outcomes_initial_去高missing.csv")
hospital_missing_csv <- file.path(desktop_dir, "eicu_hospital_missing_summary_去高missing筛选.csv")
hospital_missing_variable_csv <- file.path(desktop_dir, "eicu_hospital_missing_by_variable_去高missing筛选.csv")
hospital_missing_png <- file.path(desktop_dir, "eicu_hospital_missing_summary_去高missing筛选.png")
removed_hospitals_csv <- file.path(desktop_dir, "eicu_removed_high_missing_hospitals.csv")

eicu_table1_csv <- file.path(desktop_dir, "table1_patient_characteristics_eicu_去高missing.csv")
eicu_table1_png <- file.path(desktop_dir, "table1_patient_characteristics_eicu_去高missing.png")
eicu_table1_pdf <- file.path(desktop_dir, "table1_patient_characteristics_eicu_去高missing.pdf")
side_by_side_csv <- file.path(desktop_dir, "table1_mimic_eicu_side_by_side_去高missing.csv")
side_by_side_png <- file.path(desktop_dir, "table1_mimic_eicu_side_by_side_去高missing.png")
side_by_side_pdf <- file.path(desktop_dir, "table1_mimic_eicu_side_by_side_去高missing.pdf")
status_csv <- file.path(desktop_dir, "eicu_去高missing_data_status.csv")

if (!file.exists(input_path)) stop("Missing input CSV: ", input_path)
if (!file.exists(mimic_table1_path)) stop("Missing MIMIC Table 1 CSV: ", mimic_table1_path)
if (!file.exists(patient_extra_cache)) stop("Missing cached eICU patient metadata: ", patient_extra_cache)

eicu_original <- fread(input_path, check.names = FALSE)
mimic_table1 <- fread(mimic_table1_path, check.names = FALSE)
patient_extra <- fread(patient_extra_cache, check.names = FALSE)

if (!"hospital_id" %in% names(eicu_original)) stop("Input CSV must contain hospital_id.")

is_missing <- function(x) is.na(x) | trimws(as.character(x)) == ""

core_missing_vars <- c(
  "age", "gender", "sofa_score", "Sepsis_first_day",
  "first_day_gcs", "No_GCS_Measured", "first_day_cam_icu",
  "first_day_delirium_drug", "first_day_wbc", "first_day_hemoglobin",
  "first_day_platelet", "first_day_sodium", "first_day_potassium",
  "first_day_chloride", "first_day_bicarbonate", "first_day_bun",
  "first_day_creatinine", "first_day_glucose", "first_day_alt",
  "first_day_ast", "first_day_bilirubin_total", "first_day_pt",
  "first_day_inr", "first_day_ptt", "first_day_fibrinogen",
  "first_day_invasive_vent", "first_day_noninvasive_vent",
  "first_day_hfnc", "first_day_tracheostomy", "first_day_norepinephrine",
  "first_day_epinephrine", "first_day_vasopressin",
  "first_day_phenylephrine", "first_day_dopamine"
)
core_missing_vars <- intersect(core_missing_vars, names(eicu_original))

basic_lab_vars <- c(
  "first_day_wbc", "first_day_hemoglobin", "first_day_platelet",
  "first_day_sodium", "first_day_potassium", "first_day_chloride",
  "first_day_bicarbonate", "first_day_bun", "first_day_creatinine",
  "first_day_glucose"
)
basic_lab_vars <- intersect(basic_lab_vars, names(eicu_original))

lab_vars <- c(
  "first_day_wbc", "first_day_hemoglobin", "first_day_platelet",
  "first_day_sodium", "first_day_potassium", "first_day_chloride",
  "first_day_bicarbonate", "first_day_bun", "first_day_creatinine",
  "first_day_glucose", "first_day_alt", "first_day_ast",
  "first_day_bilirubin_total", "first_day_pt", "first_day_inr",
  "first_day_ptt", "first_day_fibrinogen"
)
lab_vars <- intersect(lab_vars, names(eicu_original))

missing_rate <- function(dt, vars) {
  if (length(vars) == 0 || nrow(dt) == 0) return(NA_real_)
  sum(unlist(lapply(dt[, ..vars], is_missing))) / (nrow(dt) * length(vars)) * 100
}

hospital_missing <- eicu_original[, {
  n_h <- .N
  core_pct <- missing_rate(.SD, core_missing_vars)
  basic_lab_pct <- missing_rate(.SD, basic_lab_vars)
  lab_pct <- missing_rate(.SD, lab_vars)
  gcs_pct <- if ("first_day_gcs" %in% names(.SD)) mean(is_missing(first_day_gcs)) * 100 else NA_real_
  cam_pct <- if ("first_day_cam_icu" %in% names(.SD)) mean(is_missing(first_day_cam_icu)) * 100 else NA_real_
  coag_pct <- missing_rate(.SD, intersect(c("first_day_pt", "first_day_inr", "first_day_ptt", "first_day_fibrinogen"), names(.SD)))

  variable_missing_pct <- sapply(core_missing_vars, function(v) mean(is_missing(.SD[[v]])) * 100)
  top_vars <- names(sort(variable_missing_pct, decreasing = TRUE))[seq_len(min(5, length(variable_missing_pct)))]
  top_text <- paste(sprintf("%s %.1f%%", top_vars, variable_missing_pct[top_vars]), collapse = "; ")

  .(
    n_patients = n_h,
    core_missing_pct = round(core_pct, 1),
    basic_lab_missing_pct = round(basic_lab_pct, 1),
    all_lab_missing_pct = round(lab_pct, 1),
    first_day_gcs_missing_pct = round(gcs_pct, 1),
    first_day_cam_icu_missing_pct = round(cam_pct, 1),
    coag_missing_pct = round(coag_pct, 1),
    top_missing_variables = top_text
  )
}, by = hospital_id]

hospital_missing[, high_missing_reason := fifelse(
  core_missing_pct >= 30,
  "core_missing_pct>=30",
  fifelse(
    first_day_gcs_missing_pct > 70,
    "first_day_gcs_missing_pct>70",
    fifelse(
      n_patients >= 10 & basic_lab_missing_pct >= 25,
      "n>=10 & basic_lab_missing_pct>=25",
      ""
    )
  )
)]
hospital_missing[, remove_high_missing := high_missing_reason != ""]
setorder(hospital_missing, -remove_high_missing, -core_missing_pct, -first_day_gcs_missing_pct, -n_patients)
fwrite(hospital_missing, hospital_missing_csv)
fwrite(hospital_missing[remove_high_missing == TRUE], removed_hospitals_csv)

hospital_variable_missing <- rbindlist(lapply(core_missing_vars, function(v) {
  eicu_original[, .(
    variable = v,
    missing_n = sum(is_missing(get(v))),
    missing_pct = round(mean(is_missing(get(v))) * 100, 1)
  ), by = hospital_id]
}))
setorder(hospital_variable_missing, hospital_id, variable)
fwrite(hospital_variable_missing, hospital_missing_variable_csv)

removed_ids <- hospital_missing[remove_high_missing == TRUE, hospital_id]
eicu_filtered <- eicu_original[!hospital_id %in% removed_ids]
fwrite(eicu_filtered, filtered_csv, quote = TRUE, na = "")

fmt_n_pct <- function(n, denom) sprintf("%d (%.1f%%)", n, 100 * n / denom)

fmt_missing <- function(n, denom) {
  if (is.na(n) || n == 0) return("")
  sprintf("%d (%.1f%%)", n, 100 * n / denom)
}

fmt_cont <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x_nonmiss <- x[!is.na(x)]
  if (length(x_nonmiss) == 0) return("")
  sprintf(
    "%.1f [%.1f, %.1f]",
    median(x_nonmiss),
    quantile(x_nonmiss, 0.25, names = FALSE),
    quantile(x_nonmiss, 0.75, names = FALSE)
  )
}

trim_upper <- function(x) toupper(trimws(as.character(x)))

add_cont <- function(rows, data, section, var, label) {
  if (!var %in% names(data)) return(rows)
  x <- suppressWarnings(as.numeric(data[[var]]))
  rows[[length(rows) + 1]] <- data.frame(
    Section = section, Variable = label, Level = "Median [IQR]",
    Overall = fmt_cont(x), Missing = sum(is.na(x)),
    Missing_pct = round(mean(is.na(x)) * 100, 1), stringsAsFactors = FALSE
  )
  rows
}

add_binary <- function(rows, data, section, var, label, yes_label = "Yes") {
  if (!var %in% names(data)) return(rows)
  x <- suppressWarnings(as.numeric(data[[var]]))
  denom <- nrow(data)
  rows[[length(rows) + 1]] <- data.frame(
    Section = section, Variable = label, Level = yes_label,
    Overall = fmt_n_pct(sum(x == 1, na.rm = TRUE), denom),
    Missing = sum(is.na(x)), Missing_pct = round(mean(is.na(x)) * 100, 1),
    stringsAsFactors = FALSE
  )
  rows
}

add_cat <- function(rows, data, section, var, label, levels_order = NULL) {
  if (!var %in% names(data)) return(rows)
  x <- as.character(data[[var]])
  x[is_missing(x)] <- NA_character_
  denom <- nrow(data)
  levels_x <- if (!is.null(levels_order)) {
    intersect(levels_order, unique(x[!is.na(x)]))
  } else {
    sort(unique(x[!is.na(x)]))
  }
  for (lv in levels_x) {
    rows[[length(rows) + 1]] <- data.frame(
      Section = section, Variable = label, Level = lv,
      Overall = fmt_n_pct(sum(x == lv, na.rm = TRUE), denom),
      Missing = sum(is.na(x)), Missing_pct = round(mean(is.na(x)) * 100, 1),
      stringsAsFactors = FALSE
    )
  }
  rows
}

add_unavailable <- function(rows, data, section, label, text = "Not available in eICU") {
  rows[[length(rows) + 1]] <- data.frame(
    Section = section, Variable = label, Level = text, Overall = "",
    Missing = nrow(data), Missing_pct = 100, stringsAsFactors = FALSE
  )
  rows
}

prepare_eicu_table_data <- function(data) {
  data <- as.data.table(data)
  extra <- patient_extra[, .(stay_id, ethnicity, hospitaladmitsource, unitadmitsource, unitstaytype, unittype)]
  data <- merge(data, extra, by = "stay_id", all.x = TRUE, sort = FALSE)

  data[, race_group := fcase(
    trim_upper(ethnicity) == "ASIAN", "Asian",
    trim_upper(ethnicity) == "CAUCASIAN", "White",
    trim_upper(ethnicity) == "AFRICAN AMERICAN", "Black",
    trim_upper(ethnicity) == "HISPANIC", "Hispanic/Latino",
    is.na(ethnicity) | trimws(ethnicity) == "", NA_character_,
    default = "Other"
  )]

  data[, admission_type_group := fcase(
    trim_upper(unitadmitsource) %in% c("EMERGENCY DEPARTMENT"), "EW EMER.",
    trim_upper(unitadmitsource) %in% c("CHEST PAIN CENTER"), "AMBULATORY OBSERVATION",
    trim_upper(unitadmitsource) %in% c("DIRECT ADMIT"), "DIRECT EMER.",
    trim_upper(unitadmitsource) %in% c("OBSERVATION"), "OBSERVATION ADMIT",
    trim_upper(unitadmitsource) %in% c("OPERATING ROOM", "RECOVERY ROOM", "PACU"), "SURGICAL SAME DAY ADMISSION",
    trim_upper(unitadmitsource) %in% c("FLOOR", "ACUTE CARE/FLOOR", "STEP-DOWN UNIT (SDU)", "OTHER HOSPITAL", "OTHER ICU", "ICU", "ICU TO SDU"), "URGENT",
    is.na(unitadmitsource) | trimws(unitadmitsource) == "", NA_character_,
    default = "OTHER"
  )]

  data[, first_day_any_vent := as.integer(
    fifelse(is.na(first_day_invasive_vent), 0, first_day_invasive_vent) == 1 |
      fifelse(is.na(first_day_noninvasive_vent), 0, first_day_noninvasive_vent) == 1 |
      fifelse(is.na(first_day_hfnc), 0, first_day_hfnc) == 1 |
      fifelse(is.na(first_day_tracheostomy), 0, first_day_tracheostomy) == 1
  )]
  data[, first_day_any_vasopressor := as.integer(
    fifelse(is.na(first_day_norepinephrine), 0, first_day_norepinephrine) == 1 |
      fifelse(is.na(first_day_epinephrine), 0, first_day_epinephrine) == 1 |
      fifelse(is.na(first_day_vasopressin), 0, first_day_vasopressin) == 1 |
      fifelse(is.na(first_day_phenylephrine), 0, first_day_phenylephrine) == 1 |
      fifelse(is.na(first_day_dopamine), 0, first_day_dopamine) == 1
  )]
  data
}

build_table1 <- function(data, include_unavailable = TRUE) {
  rows <- list()
  admission_levels <- c(
    "AMBULATORY OBSERVATION", "DIRECT EMER.", "DIRECT OBSERVATION", "ELECTIVE",
    "EW EMER.", "OBSERVATION ADMIT", "SURGICAL SAME DAY ADMISSION", "URGENT", "OTHER"
  )

  rows <- add_cont(rows, data, "Demographics", "age", "Age, years")
  rows <- add_cat(rows, data, "Demographics", "gender", "Sex", levels_order = c("F", "M"))
  rows <- add_cat(rows, data, "Demographics", "race_group", "Race", levels_order = c("Asian", "Black", "Hispanic/Latino", "Other", "White"))
  if (include_unavailable) {
    rows <- add_unavailable(rows, data, "Demographics", "Insurance")
    rows <- add_unavailable(rows, data, "Demographics", "Language")
    rows <- add_unavailable(rows, data, "Demographics", "Marital status")
  }
  rows <- add_cat(rows, data, "Demographics", "admission_type_group", "Admission type", levels_order = admission_levels)

  rows <- add_cont(rows, data, "ICU and sepsis", "icu_length_hours", "ICU length, hours")
  rows <- add_cont(rows, data, "ICU and sepsis", "sofa_score", "SOFA score")
  rows <- add_binary(rows, data, "ICU and sepsis", "Sepsis_first_day", "Sepsis on first ICU day")
  rows <- add_cont(rows, data, "ICU and sepsis", "first_day_gcs", "First-day GCS")
  rows <- add_binary(rows, data, "ICU and sepsis", "No_GCS_Measured", "No GCS measured before sepsis")

  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_cam_icu", "CAM-ICU positive")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_delirium_drug", "Delirium-related drug")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_any_vent", "Any respiratory support")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_invasive_vent", "Invasive ventilation")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_noninvasive_vent", "Noninvasive ventilation")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_hfnc", "HFNC")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_tracheostomy", "Tracheostomy")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_any_vasopressor", "Any vasopressor")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_norepinephrine", "Norepinephrine")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_epinephrine", "Epinephrine")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_vasopressin", "Vasopressin")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_phenylephrine", "Phenylephrine")
  rows <- add_binary(rows, data, "First-day neurologic/treatment", "first_day_dopamine", "Dopamine")

  rows <- add_cont(rows, data, "First-day labs", "first_day_wbc", "WBC")
  rows <- add_cont(rows, data, "First-day labs", "first_day_hemoglobin", "Hemoglobin")
  rows <- add_cont(rows, data, "First-day labs", "first_day_platelet", "Platelet")
  rows <- add_cont(rows, data, "First-day labs", "first_day_sodium", "Sodium")
  rows <- add_cont(rows, data, "First-day labs", "first_day_potassium", "Potassium")
  rows <- add_cont(rows, data, "First-day labs", "first_day_chloride", "Chloride")
  rows <- add_cont(rows, data, "First-day labs", "first_day_bicarbonate", "Bicarbonate")
  rows <- add_cont(rows, data, "First-day labs", "first_day_bun", "BUN")
  rows <- add_cont(rows, data, "First-day labs", "first_day_creatinine", "Creatinine")
  rows <- add_cont(rows, data, "First-day labs", "first_day_glucose", "Glucose")
  rows <- add_cont(rows, data, "First-day labs", "first_day_alt", "ALT")
  rows <- add_cont(rows, data, "First-day labs", "first_day_ast", "AST")
  rows <- add_cont(rows, data, "First-day labs", "first_day_bilirubin_total", "Total bilirubin")
  rows <- add_cont(rows, data, "First-day labs", "first_day_pt", "PT")
  rows <- add_cont(rows, data, "First-day labs", "first_day_inr", "INR")
  rows <- add_cont(rows, data, "First-day labs", "first_day_ptt", "PTT")
  rows <- add_cont(rows, data, "First-day labs", "first_day_fibrinogen", "Fibrinogen")
  bind_rows(rows)
}

make_display_table <- function(table1_data, n_total, dataset_label = "Overall Cohort") {
  display_rows <- list()
  for (sec in unique(table1_data$Section)) {
    sec_data <- table1_data %>% filter(Section == sec)
    display_rows[[length(display_rows) + 1]] <- data.frame(
      row_type = "section", Section = sec, Characteristic = sec,
      Overall = "", Missing = "", stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(sec_data))) {
      row <- sec_data[i, ]
      characteristic <- if (row$Level == "Median [IQR]") {
        paste0(row$Variable, ", median [IQR]")
      } else if (row$Level == "Yes") {
        paste0(row$Variable, ", No. (%)")
      } else {
        paste0(row$Variable, ": ", row$Level)
      }
      display_rows[[length(display_rows) + 1]] <- data.frame(
        row_type = "data", Section = sec, Characteristic = characteristic,
        Overall = row$Overall, Missing = fmt_missing(as.numeric(row$Missing), n_total),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- bind_rows(display_rows)
  names(out)[4:5] <- c(paste0(dataset_label, "\n(n=", format(n_total, big.mark = ","), ")"), "Missing")
  out
}

make_display_from_export <- function(table1_export, n_total, dataset_label) {
  table1_data <- table1_export %>%
    filter(Section != "Title") %>%
    mutate(Missing = suppressWarnings(as.numeric(Missing)), Missing_pct = suppressWarnings(as.numeric(Missing_pct)))
  make_display_table(table1_data, n_total, dataset_label)
}

wrap_text <- function(x, width = 38) {
  vapply(as.character(x), function(z) {
    z <- ifelse(is.na(z), "", z)
    wrapped <- strwrap(z, width = width)
    if (length(wrapped) == 0) "" else paste(wrapped, collapse = "\n")
  }, character(1))
}

line_count <- function(x) {
  vapply(as.character(x), function(z) {
    if (is.na(z) || z == "") return(1L)
    max(1L, length(strsplit(z, "\n", fixed = TRUE)[[1]]))
  }, integer(1))
}

wrap_widths_from_columns <- function(col_widths, n_cols) {
  if (n_cols <= 3) {
    pmax(10L, round(c(86, 58, 30) * col_widths / max(col_widths)))
  } else if (n_cols == 5) {
    pmax(10L, round(c(76, 40, 28, 40, 28) * col_widths / max(col_widths)))
  } else {
    pmax(6L, round(82 * col_widths / max(col_widths)))
  }
}

draw_table_image <- function(display_table, file, title, col_widths, device = c("png", "pdf")) {
  device <- match.arg(device)
  n_cols <- ncol(display_table) - 2
  wrap_widths <- wrap_widths_from_columns(col_widths, n_cols)
  draw_table <- display_table
  for (j in seq_len(n_cols)) {
    draw_table[[j + 2]] <- wrap_text(draw_table[[j + 2]], width = wrap_widths[j])
  }
  headers <- names(draw_table)[3:ncol(draw_table)]
  headers <- mapply(wrap_text, headers, wrap_widths, USE.NAMES = FALSE)
  n_rows <- nrow(draw_table)
  body_line_counts <- apply(draw_table[, 3:ncol(draw_table), drop = FALSE], 1, function(row) max(line_count(row)))
  row_units <- pmax(1.0, body_line_counts * 0.92)
  row_units[draw_table$row_type == "section"] <- pmax(row_units[draw_table$row_type == "section"], 1.12)
  header_units <- max(1.7, max(line_count(headers)) * 0.90 + 0.75)
  width_px <- if (n_cols <= 3) 2200 else 3400
  row_height_px <- if (n_cols <= 3) 58 else 56
  header_height_px <- 76
  title_height_px <- 115
  bottom_margin_px <- 45
  height_px <- title_height_px + header_height_px * header_units + row_height_px * sum(row_units) + bottom_margin_px
  if (device == "png") png(file, width = width_px, height = height_px, res = 220) else pdf(file, width = width_px / 220, height = max(8, height_px / 220))
  on.exit(dev.off(), add = TRUE)
  grid.newpage()
  left <- 0.035; right <- 0.965; top <- 0.965
  table_top <- 0.895; table_bottom <- 0.035
  table_width <- right - left; table_height <- table_top - table_bottom
  col_x <- c(left, left + table_width * cumsum(col_widths))
  total_units <- header_units + sum(row_units)
  header_h <- table_height * header_units / total_units
  grid.text(title, x = unit(left, "npc"), y = unit(top, "npc"), just = c("left", "top"),
            gp = gpar(fontsize = 17, fontface = "bold", fontfamily = "Times"))
  y <- table_top
  grid.rect(x = unit(left + table_width / 2, "npc"), y = unit(y - header_h / 2, "npc"),
            width = unit(table_width, "npc"), height = unit(header_h, "npc"),
            gp = gpar(fill = "#FFFFFF", col = "#BFBFBF", lwd = 1.2))
  for (j in seq_along(headers)) {
    grid.text(headers[j], x = unit((col_x[j] + col_x[j + 1]) / 2, "npc"),
              y = unit(y - header_h / 2, "npc"), just = "center",
              gp = gpar(fontsize = if (n_cols <= 3) 12.5 else 10.5, fontface = "bold", fontfamily = "Times"))
  }
  y <- y - header_h
  for (i in seq_len(n_rows)) {
    row_h <- table_height * row_units[i] / total_units
    row_type <- draw_table$row_type[i]
    fill <- if (row_type == "section") "#F1F1F1" else if (i %% 2 == 0) "#FAFAFA" else "#FFFFFF"
    fontface <- if (row_type == "section") "bold" else "plain"
    grid.rect(x = unit(left + table_width / 2, "npc"), y = unit(y - row_h / 2, "npc"),
              width = unit(table_width, "npc"), height = unit(row_h, "npc"),
              gp = gpar(fill = fill, col = "#D0D0D0", lwd = 0.8))
    cell_values <- as.character(draw_table[i, 3:ncol(draw_table)])
    for (j in seq_along(cell_values)) {
      x_mid <- (col_x[j] + col_x[j + 1]) / 2
      just <- if (j == 1) c("left", "center") else "center"
      x_text <- if (j == 1) col_x[j] + 0.010 else x_mid
      grid.text(cell_values[j], x = unit(x_text, "npc"), y = unit(y - row_h / 2, "npc"),
                just = just, gp = gpar(fontsize = if (n_cols <= 3) 10.5 else 8.7, lineheight = 0.88, fontface = fontface, fontfamily = "Times"))
    }
    y <- y - row_h
  }
  for (x_line in col_x) {
    grid.lines(x = unit(c(x_line, x_line), "npc"), y = unit(c(table_bottom, table_top), "npc"),
               gp = gpar(col = "#BFBFBF", lwd = 1))
  }
}

draw_hospital_missing_image <- function(hospital_missing, file) {
  display <- hospital_missing[, .(
    hospital_id, n_patients, core_missing_pct, basic_lab_missing_pct,
    first_day_gcs_missing_pct, first_day_cam_icu_missing_pct,
    remove_high_missing, high_missing_reason
  )]
  setorder(display, -remove_high_missing, -core_missing_pct, -first_day_gcs_missing_pct)
  display[, remove_high_missing := ifelse(remove_high_missing, "Yes", "No")]
  setnames(display, c("hospital_id", "n_patients", "core_missing_pct", "basic_lab_missing_pct", "first_day_gcs_missing_pct", "first_day_cam_icu_missing_pct", "remove_high_missing", "high_missing_reason"),
           c("Hospital ID", "N", "Core missing %", "Basic lab missing %", "GCS missing %", "CAM-ICU missing %", "Remove", "Reason"))
  display <- as.data.frame(display)
  display$row_type <- "data"
  display$Section <- ""
  display <- display[, c("row_type", "Section", names(display)[1:(ncol(display)-2)])]
  draw_table_image(
    display,
    file,
    "eICU hospital-level missing summary and high-missing removal flag",
    col_widths = c(0.12, 0.07, 0.12, 0.13, 0.12, 0.13, 0.08, 0.23),
    device = "png"
  )
}

eicu_table_data <- prepare_eicu_table_data(eicu_filtered)
eicu_table1 <- build_table1(eicu_table_data, include_unavailable = TRUE)
eicu_table1_export <- bind_rows(
  data.frame(
    Section = "Title",
    Variable = "Table 1. Patient Characteristics of eICU Sepsis Patients after high-missing hospital exclusion",
    Level = "", Overall = paste0("N = ", nrow(eicu_table_data)),
    Missing = NA_real_, Missing_pct = NA_real_, stringsAsFactors = FALSE
  ),
  eicu_table1
) %>%
  mutate(Missing = ifelse(is.na(Missing), "", as.character(Missing)),
         Missing_pct = ifelse(is.na(Missing_pct), "", as.character(Missing_pct)))
fwrite(eicu_table1_export, eicu_table1_csv)

eicu_display <- make_display_table(eicu_table1, nrow(eicu_table_data), "eICU")
draw_table_image(
  eicu_display %>% select(row_type, Section, Characteristic, 4, 5),
  eicu_table1_png,
  "Table 1. Patient Characteristics of eICU Sepsis Patients after high-missing hospital exclusion",
  col_widths = c(0.49, 0.34, 0.17),
  device = "png"
)
draw_table_image(
  eicu_display %>% select(row_type, Section, Characteristic, 4, 5),
  eicu_table1_pdf,
  "Table 1. Patient Characteristics of eICU Sepsis Patients after high-missing hospital exclusion",
  col_widths = c(0.49, 0.34, 0.17),
  device = "pdf"
)

mimic_display <- make_display_from_export(mimic_table1, 11373, "MIMIC-IV")
mimic_display <- mimic_display %>% mutate(row_order = row_number(), key = paste(Section, Characteristic, sep = "||"))
eicu_display <- eicu_display %>% mutate(eicu_order = row_number(), key = paste(Section, Characteristic, sep = "||"))
section_max <- mimic_display %>% group_by(Section) %>% summarise(section_max_order = max(row_order), .groups = "drop")
eicu_only <- eicu_display %>%
  filter(!key %in% mimic_display$key) %>%
  left_join(section_max, by = "Section") %>%
  mutate(row_order = coalesce(section_max_order, max(mimic_display$row_order) + 1) + eicu_order / 1000) %>%
  select(row_type, Section, Characteristic, row_order, key)
combined_index <- bind_rows(
  mimic_display %>% select(row_type, Section, Characteristic, row_order, key),
  eicu_only
) %>% arrange(row_order)
mimic_values <- mimic_display %>% select(key, mimic_overall = 4, mimic_missing = 5)
eicu_values <- eicu_display %>% select(key, eicu_overall = 4, eicu_missing = 5)
side_by_side <- combined_index %>%
  left_join(mimic_values, by = "key") %>%
  left_join(eicu_values, by = "key") %>%
  mutate(
    mimic_overall = coalesce(mimic_overall, ""),
    mimic_missing = coalesce(mimic_missing, ""),
    eicu_overall = coalesce(eicu_overall, ""),
    eicu_missing = coalesce(eicu_missing, "")
  ) %>%
  select(
    row_type, Section, Characteristic,
    `MIMIC-IV Cohort (n=11,373)` = mimic_overall,
    `MIMIC-IV Missing` = mimic_missing,
    `eICU Cohort` = eicu_overall,
    `eICU Missing` = eicu_missing
  )
names(side_by_side)[6] <- paste0("eICU Cohort (n=", format(nrow(eicu_table_data), big.mark = ","), ")")
fwrite(side_by_side %>% select(-row_type, -Section), side_by_side_csv)
draw_table_image(
  side_by_side,
  side_by_side_png,
  "Table 1. Patient Characteristics after exclusion: MIMIC-IV vs eICU high-missing removed",
  col_widths = c(0.36, 0.20, 0.12, 0.20, 0.12),
  device = "png"
)
draw_table_image(
  side_by_side,
  side_by_side_pdf,
  "Table 1. Patient Characteristics after exclusion: MIMIC-IV vs eICU high-missing removed",
  col_widths = c(0.36, 0.20, 0.12, 0.20, 0.12),
  device = "pdf"
)
draw_hospital_missing_image(hospital_missing, hospital_missing_png)

missing_pct <- function(data, var) {
  if (!var %in% names(data)) return(NA_real_)
  round(mean(is_missing(data[[var]])) * 100, 1)
}

status <- data.frame(
  item = c(
    "input_rows",
    "input_hospitals",
    "removed_hospitals",
    "removed_rows",
    "remaining_rows",
    "remaining_hospitals",
    "high_missing_rule",
    "first_day_gcs_missing_before_after",
    "first_day_cam_icu_missing_before_after",
    "basic_lab_missing_before_after",
    "admission_source_grouping",
    "output_csv"
  ),
  value = c(
    nrow(eicu_original),
    uniqueN(eicu_original$hospital_id),
    length(removed_ids),
    nrow(eicu_original) - nrow(eicu_filtered),
    nrow(eicu_filtered),
    uniqueN(eicu_filtered$hospital_id),
    "Remove hospital if core_missing_pct>=30 OR first_day_gcs_missing_pct>70 OR (n>=10 & basic_lab_missing_pct>=25)",
    paste0(missing_pct(eicu_original, "first_day_gcs"), "% -> ", missing_pct(eicu_filtered, "first_day_gcs"), "%"),
    paste0(missing_pct(eicu_original, "first_day_cam_icu"), "% -> ", missing_pct(eicu_filtered, "first_day_cam_icu"), "%"),
    paste0(round(missing_rate(eicu_original, basic_lab_vars), 1), "% -> ", round(missing_rate(eicu_filtered, basic_lab_vars), 1), "%"),
    "eICU unitadmitsource grouped into MIMIC-like Admission type categories: EW EMER., DIRECT EMER., OBSERVATION ADMIT, SURGICAL SAME DAY ADMISSION, URGENT, AMBULATORY OBSERVATION, OTHER.",
    filtered_csv
  ),
  stringsAsFactors = FALSE
)
fwrite(status, status_csv)

cat("Done.\n")
cat("Hospital missing summary: ", hospital_missing_csv, "\n", sep = "")
cat("Filtered CSV: ", filtered_csv, "\n", sep = "")
cat("Removed hospitals: ", length(removed_ids), "\n", sep = "")
cat("Removed rows: ", nrow(eicu_original) - nrow(eicu_filtered), "\n", sep = "")
cat("Remaining rows: ", nrow(eicu_filtered), "\n", sep = "")
cat("Side-by-side PNG: ", side_by_side_png, "\n", sep = "")
