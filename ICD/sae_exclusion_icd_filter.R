# =============================================================================
# SAE (Sepsis-Associated Encephalopathy) Exclusion-Criteria ICD Code Filter
# =============================================================================
#
# PURPOSE
#   SAE is a DIAGNOSIS OF EXCLUSION: it can only be attributed to sepsis once
#   every other cause of altered mental status has been ruled out. When building
#   a study cohort from EHR data (here, the MIMIC-IV ICD dictionary), we need to
#   identify the diagnosis codes that flag those competing causes, so patients
#   carrying them can be excluded.
#
#   This script reads the MIMIC-IV ICD dictionary (ICD-9 + ICD-10 mixed) and
#   tags each code against the SAE exclusion categories compiled from the
#   literature. Output: a long table of matched codes (one row per
#   code-category match) plus a per-code wide summary.
#
# WHY BASE R
#   This version uses ONLY base R (no dplyr/readr/tidyr/stringr), so it runs in
#   a clean R install with no package downloads. Every step is heavily commented.
#
# INPUT  : MIMIC-IV_d_ICD.csv with columns
#            icd_code     - code WITHOUT a decimal point (e.g. "I639" not "I63.9")
#            icd_version  - integer, 9 or 10
#            long_title   - human-readable description
#
# MATCHING STRATEGY
#   Two complementary signals; keep a row if EITHER fires:
#     (1) long_title keyword regex  -- robust across ICD-9/10, catches the
#                                       clinical concept regardless of code.
#     (2) icd_code prefix regex     -- anchors on known code families to catch
#                                       items whose titles use unexpected wording.
#   MIMIC strips the decimal, so all code-prefix patterns target the dot-free
#   form (e.g. "^S06" matches the S06.x TBI family).
#
# IMPORTANT CAVEAT
#   Keyword filtering is a STARTING POINT, not a validated code set. It produces
#   false positives ("personal history of ...", negations like "without ...")
#   and can miss atypically-worded titles. We FLAG review-sensitive matches
#   (history/screening/sequela/etc.) rather than dropping them, and the final
#   set must be checked by a clinician/coder before cohort use.
#
# CONDITIONS WITH NO ICD CODE
#   Several SAE exclusion items are not diagnoses (procedure timing, sedation,
#   demographics, scores). They have no usable ICD code and are intentionally
#   SKIPPED here, then listed explicitly at the end with where to find them.
# =============================================================================


# ----------------------------------------------------------------------------
# 0. Config
# ----------------------------------------------------------------------------
INPUT_PATH  <- "MIMIC-IV_d_ICD.csv"
OUTPUT_LONG <- "sae_exclusion_codes_long.csv"    # one row per (code, category)
OUTPUT_WIDE <- "sae_exclusion_codes_by_code.csv" # one row per code


# ----------------------------------------------------------------------------
# 1. Read the dictionary
# ----------------------------------------------------------------------------
# colClasses forces icd_code to character: ICD-10 codes contain letters and
# ICD-9 codes can have meaningful leading zeros that numeric parsing would drop.
icd <- read.csv(
  INPUT_PATH,
  colClasses    = c(icd_code = "character",
                    icd_version = "integer",
                    long_title = "character"),
  stringsAsFactors = FALSE
)

# Pre-compute helper columns once:
#   title_lc  - lower-cased title so keyword regexes can ignore case cheaply.
#   code_norm - dot/space-stripped code, so code-prefix patterns stay reliable
#               even if a future export reintroduces decimals.
icd$title_lc  <- tolower(icd$long_title)
icd$code_norm <- gsub("[.[:space:]]", "", icd$icd_code)

cat(sprintf("Loaded %d codes (ICD-9: %d, ICD-10: %d).\n",
            nrow(icd), sum(icd$icd_version == 9), sum(icd$icd_version == 10)))


# ----------------------------------------------------------------------------
# 2. Define exclusion categories
# ----------------------------------------------------------------------------
# Each entry: label, title regex (or NA), code-prefix regex (or NA).
# Regexes use word fragments ("encephalit" -> encephalitis/-tic) and synonym
# alternation. Slightly broad on purpose; the review flags in step 4 triage
# over-capture. All regexes are evaluated with perl = TRUE.
#
# CODE-FAMILY REFERENCE (ICD-10 unless noted):
#   S06 traumatic intracranial injury        I60 nontraumatic SAH
#   G00-G06 meningitis/encephalitis          I61 intracerebral hemorrhage
#   A39/A87/B00-B02 specific CNS infections   I63 cerebral infarction (stroke)
#   G40 epilepsy / G41 status epilepticus     I60-I69 cerebrovascular (incl. sequelae)
#   C70-C72,D32-D33,D42-D43 CNS tumors        I674 hypertensive encephalopathy
#   G30/F00 Alzheimer; G20 Parkinson; G35 MS  G92 toxic / G93.x other encephalopathy
#   K72 hepatic failure/encephalopathy        E512 Wernicke
#   F10 alcohol; F11-F19 other substances
categories <- list(
  list(label="Traumatic brain injury",
       title="traumatic brain|intracranial injur|cerebral laceration|cerebral contusion|diffuse axonal|traumatic subdural|traumatic subarachnoid|concussion",
       code ="^S06"),

  list(label="Meningitis / encephalitis / CNS infection",
       title="meningit|encephalit|meningoencephalit|encephalomyelit|cerebral abscess|intracranial abscess|intraspinal abscess|ventriculitis|cerebritis",
       code ="^G0[0-6]|^A39|^A87|^B00[0-2]|^A170|^A321"),

  list(label="Stroke / cerebrovascular disease",
       title="cerebral infarction|cerebral isch|ischa?emic stroke|intracerebral h(a)?emorrhage|nontraumatic.*h(a)?emorrhage|subarachnoid h(a)?emorrhage|cerebral embolism|cerebral thrombosis|occlusion.*cerebral arter|cerebrovascular",
       code ="^I6[0-9]"),

  list(label="Status epilepticus / epilepsy / seizure",
       title="status epilepticus|epilep|seizure|convuls",
       code ="^G40|^G41"),

  list(label="Brain tumor / CNS neoplasm",
       title="brain.*neoplasm|neoplasm.*brain|cerebral.*neoplasm|malignant neoplasm of brain|benign neoplasm of brain|glioma|glioblastoma|meningioma|astrocytoma|neoplasm.*meninges",
       code ="^C7[0-2]|^D32|^D33|^D42|^D43"),

  list(label="Hypertensive encephalopathy",
       title="hypertensive encephalopathy",
       code ="^I674"),

  list(label="Fat embolism",
       title="fat embolism",
       code =NA),

  list(label="Dementia / Alzheimer / neurodegenerative",
       title="dementia|alzheimer|parkinson|multiple sclerosis|huntington|lewy bod|frontotemporal|amyotrophic lateral",
       code ="^G30|^G31|^G20|^G35|^F0[0-3]"),

  list(label="Pre-existing psychiatric illness",
       title="schizophren|schizoaffective|bipolar|psychotic disorder|psychosis",
       code ="^F20|^F25|^F31|^F23|^F2[89]"),

  list(label="Hepatic encephalopathy / failure",
       title="hepatic encephalopathy|hepatic failure|hepatic coma|liver failure.*coma",
       code ="^K72"),

  list(label="Metabolic / toxic / other encephalopathy",
       title="toxic encephalopathy|metabolic encephalopathy|encephalopathy, unspecified|other encephalopathy|anoxic brain|hypoxic.*brain|uremic encephalopathy|uraemic encephalopathy|hyperammon",
       code ="^G92|^G93[014]"),

  list(label="Wernicke encephalopathy",
       title="wernicke",
       code ="^E512"),

  list(label="Alcohol use / intoxication / withdrawal",
       title="alcohol.*(depend|abuse|intoxicat|withdrawal|psychos|dementia)|alcoholic|delirium tremens",
       code ="^F10"),

  list(label="Other substance use / drug intoxication",
       title="(opioid|cocaine|cannabis|sedative|hypnotic|amphetamine|hallucinogen|psychoactive).*(depend|abuse|intoxicat|withdrawal|psychos)|drug.*intoxicat|poisoning by.*(opioid|sedative|narcotic)",
       code ="^F1[1-9]")
)


# ----------------------------------------------------------------------------
# 3. Apply each category -> long table
# ----------------------------------------------------------------------------
# For each category: hit = title-match OR code-match. Keep matching rows tagged
# with the category and HOW they matched (title / code / title+code). Binding
# all categories yields a LONG table; a code may appear under >1 category.
match_one <- function(cat, data) {
  hit_title <- if (!is.na(cat$title))
    grepl(cat$title, data$title_lc, perl = TRUE, ignore.case = TRUE)
  else rep(FALSE, nrow(data))

  hit_code  <- if (!is.na(cat$code))
    grepl(cat$code, data$code_norm, perl = TRUE, ignore.case = TRUE)
  else rep(FALSE, nrow(data))

  keep <- hit_title | hit_code
  if (!any(keep)) return(NULL)

  matched_by <- ifelse(hit_title[keep] & hit_code[keep], "title+code",
                ifelse(hit_title[keep], "title", "code"))

  data.frame(
    icd_code    = data$icd_code[keep],
    icd_version = data$icd_version[keep],
    long_title  = data$long_title[keep],
    category    = cat$label,
    matched_by  = matched_by,
    stringsAsFactors = FALSE
  )
}

# do.call(rbind, ...) stacks the per-category data frames into one long table.
matches_long <- do.call(rbind, lapply(categories, match_one, data = icd))
rownames(matches_long) <- NULL


# ----------------------------------------------------------------------------
# 4. Flag likely false positives for manual review (kept, not dropped)
# ----------------------------------------------------------------------------
# "personal history of stroke", "family history of dementia", "screening",
# "sequela/aftercare", negations, and "encounter for ..." usually should NOT
# exclude an active SAE candidate. Flag them so a reviewer decides.
matches_long$review_flag <- grepl(
  "history of|family history|screening|status post|sequela|aftercare|without mention|encounter for",
  tolower(matches_long$long_title)
)


# ----------------------------------------------------------------------------
# 5. Per-code wide summary
# ----------------------------------------------------------------------------
# Collapse to one row per code: which categories it triggered, how many, and
# whether ANY of its matches was review-flagged.
agg_cats <- tapply(matches_long$category,
                   matches_long$icd_code,
                   function(x) paste(sort(unique(x)), collapse = " | "))
agg_n    <- tapply(matches_long$category,
                   matches_long$icd_code,
                   function(x) length(unique(x)))
agg_flag <- tapply(matches_long$review_flag,
                   matches_long$icd_code, any)

# Look up version + title once per unique code (first occurrence is fine).
first_idx <- !duplicated(matches_long$icd_code)
lut <- matches_long[first_idx, c("icd_code", "icd_version", "long_title")]

matches_wide <- data.frame(icd_code = names(agg_cats), stringsAsFactors = FALSE)
matches_wide$icd_version     <- lut$icd_version[match(matches_wide$icd_code, lut$icd_code)]
matches_wide$long_title      <- lut$long_title[match(matches_wide$icd_code, lut$icd_code)]
matches_wide$n_categories    <- as.integer(agg_n[matches_wide$icd_code])
matches_wide$categories      <- agg_cats[matches_wide$icd_code]
matches_wide$any_review_flag <- agg_flag[matches_wide$icd_code]
# Sort: most cross-category codes first, then by code.
matches_wide <- matches_wide[order(-matches_wide$n_categories, matches_wide$icd_code), ]
rownames(matches_wide) <- NULL


# ----------------------------------------------------------------------------
# 6. Criteria with NO usable ICD code -> SKIPPED, reported
# ----------------------------------------------------------------------------
# These are not diagnoses; they live in other tables. We do not filter them.
non_icd_criteria <- data.frame(
  criterion = c(
    "Recent neurosurgery (<30 days)",
    "Recent cardiac/bypass surgery (<3 months)",
    "Sedative interference at assessment",
    "Inability to complete neuro/delirium exam",
    "Hearing or vision impairment (as a barrier)",
    "Pregnancy / lactation",
    "Coagulopathy / active bleeding (if LP planned)",
    "Age outside study range",
    "Expected stay <24h / non-first ICU admission",
    "Multi-organ failure (if used in definition)"
  ),
  why_no_icd_code = c(
    "Timing of a procedure, not a diagnosis",
    "Timing of a procedure, not a diagnosis",
    "Active medication, not a coded condition",
    "Assessment feasibility, not a diagnosis",
    "Used as an assessment barrier; ICD capture unreliable",
    "Status better taken from a dedicated flag",
    "Protocol contraindication; context-dependent",
    "Demographic, not a diagnosis",
    "Administrative, not a diagnosis",
    "Better derived from organ-dysfunction scoring"
  ),
  where_to_find_it = c(
    "Procedures table + admit/procedure datetimes",
    "Procedures table + datetimes",
    "Medication/infusion records (propofol, benzodiazepines)",
    "CAM-ICU / RASS / GCS charting; missing-data logic",
    "Nursing assessments (ICD codes exist but inconsistent)",
    "Demographics / OB flags (Z3x codes inconsistent)",
    "Labs (INR/platelets) + active-bleeding flags",
    "Patients/admissions demographics",
    "ICU stay records / admission sequence",
    "SOFA components from labs/vitals, not one ICD code"
  ),
  stringsAsFactors = FALSE
)


# ----------------------------------------------------------------------------
# 7. Write outputs + console summary
# ----------------------------------------------------------------------------
write.csv(matches_long, OUTPUT_LONG, row.names = FALSE)
write.csv(matches_wide, OUTPUT_WIDE, row.names = FALSE)

cat("\n=====================================================================\n")
cat("SAE EXCLUSION CODE FILTER -- SUMMARY\n")
cat("=====================================================================\n")

# Per-category counts split by ICD version.
tab <- as.data.frame(table(matches_long$category, matches_long$icd_version),
                     stringsAsFactors = FALSE)
names(tab) <- c("category", "icd_version", "n")
wide_summary <- reshape(tab, idvar = "category", timevar = "icd_version",
                        direction = "wide")
wide_summary[is.na(wide_summary)] <- 0
names(wide_summary) <- gsub("^n\\.", "icd", names(wide_summary))
num_cols <- setdiff(names(wide_summary), "category")
wide_summary$total <- rowSums(wide_summary[, num_cols, drop = FALSE])
wide_summary <- wide_summary[order(-wide_summary$total), ]
print(wide_summary, row.names = FALSE)

cat(sprintf("\nTotal matched (code, category) rows : %d\n", nrow(matches_long)))
cat(sprintf("Distinct codes matched              : %d\n",
            length(unique(matches_long$icd_code))))
cat(sprintf("Rows flagged for manual review      : %d\n",
            sum(matches_long$review_flag)))

cat("\n--- CRITERIA SKIPPED (no usable ICD code) ---------------------------\n")
cat("These SAE exclusion criteria are NOT diagnoses and were NOT filtered\n")
cat("from the ICD dictionary. Handle them with the data noted:\n\n")
print(non_icd_criteria, row.names = FALSE, right = FALSE)

cat(sprintf("\nOutputs written:\n  - %s\n  - %s\n", OUTPUT_LONG, OUTPUT_WIDE))
cat("\nREMINDER: keyword/code-prefix matching is a first pass. Review the\n")
cat("flagged rows and validate the final set with a clinician/coder before\n")
cat("using it to build a cohort.\n")
cat("=====================================================================\n")
