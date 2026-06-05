#轨迹图
library(flexmix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

set.seed(123)

# ====================== 1. User settings ======================
sample_n_subjects <- 1000
k_list <- c(2, 3)
nrep <- 20
save_plots <- TRUE
plot_dir <- "gcs_trajectory_plots"
trajectory_formula <- gcs_min ~ hour_offset + I(hour_offset^2)
group_var <- "stay_id"
# ===============================================================

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

safe_zscore <- function(x) {
  x <- as.numeric(x)
  sx <- sd(x, na.rm = TRUE)
  if (is.na(sx) || sx == 0) {
    rep(0, length(x))
  } else {
    as.numeric(scale(x))
  }
}

# ====================== 2. Clean data ======================
ids <- unique(trajectory_slim$subject_id)
id_keep <- sample(ids, min(sample_n_subjects, length(ids)), replace = FALSE)

df <- trajectory_slim %>%
  filter(subject_id %in% id_keep) %>%
  mutate(
    intime = as.POSIXct(intime),
    outtime = as.POSIXct(outtime)
  ) %>%
  group_by(stay_id) %>%
  mutate(
    icu_length = as.numeric(difftime(first(outtime), first(intime), units = "hours")),
    sofa = first_non_missing(sofa_score),
    age = first_non_missing(age),
    gender = first_non_missing(gender),
    sepsis3 = first_non_missing(sepsis3)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(icu_length),
    !is.na(gcs_min),
    hour_offset >= 0,
    hour_offset <= icu_length
  ) %>%
  mutate(
    stay_id = factor(stay_id),
    gender = factor(gender),
    sepsis3 = factor(sepsis3),
    sofa_z = safe_zscore(sofa),
    age_z = safe_zscore(age)
  ) %>%
  drop_na(gcs_min, hour_offset, sofa_z, age_z, gender, sepsis3) %>%
  mutate(
    stay_id = droplevels(stay_id),
    gender = droplevels(gender),
    sepsis3 = droplevels(sepsis3)
  ) %>%
  arrange(stay_id, hour_offset)

if (nrow(df) == 0) {
  stop("No rows remained after ICU-time trimming and complete-case filtering.")
}

if (save_plots && !dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# ====================== 3. Model definitions ======================
model_specs <- list(
  base = list(
    name = "Empty model",
    cov_terms = character(0)
  ),
  sofa = list(
    name = "SOFA-adjusted",
    cov_terms = c("sofa_z")
  ),
  full = list(
    name = "Fully-adjusted",
    cov_terms = c("sofa_z", "age_z", "gender", "sepsis3")
  )
)

# ====================== 4. Helper functions ======================
make_grouped_formula <- function(formula, group_var) {
  as.formula(
    paste(
      paste(deparse(formula, width.cutoff = 500), collapse = " "),
      "|",
      group_var
    )
  )
}

term_is_usable <- function(data, term) {
  if (!term %in% names(data)) {
    return(FALSE)
  }

  x <- data[[term]]
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(FALSE)
  }

  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return(length(unique(as.character(x))) >= 2)
  }

  length(unique(x)) >= 2
}

validate_covariates <- function(data, cov_terms) {
  if (is.null(cov_terms) || length(cov_terms) == 0) {
    return(list(used = character(0), dropped = character(0)))
  }

  used <- cov_terms[vapply(cov_terms, term_is_usable, data = data, logical(1))]
  dropped <- setdiff(cov_terms, used)

  list(used = used, dropped = dropped)
}

make_cov_formula <- function(cov_terms) {
  if (length(cov_terms) == 0) NULL else reformulate(cov_terms)
}

candidate_covariate_sets <- function(data, cov_terms) {
  checked <- validate_covariates(data, cov_terms)
  used <- checked$used

  candidates <- list(used)

  numeric_used <- used[vapply(data[used], is.numeric, logical(1))]
  if (length(numeric_used) > 0 && length(numeric_used) < length(used)) {
    candidates <- c(candidates, list(numeric_used))
  }

  if ("sofa_z" %in% used) {
    candidates <- c(candidates, list("sofa_z"))
  }

  candidates <- c(candidates, list(character(0)))

  keys <- vapply(candidates, function(x) paste(x, collapse = "+"), character(1))
  list(
    candidates = candidates[!duplicated(keys)],
    dropped = checked$dropped
  )
}

fit_lc <- function(data, k, formula, group_var, cov_terms = character(0), nrep = 20) {
  model_form <- make_grouped_formula(formula, group_var)
  cov_candidates <- candidate_covariate_sets(data, cov_terms)

  last_error <- "no convergence to a suitable mixture"

  for (terms in cov_candidates$candidates) {
    cov_formula <- make_cov_formula(terms)

    fit_try <- tryCatch(
      {
        if (is.null(cov_formula)) {
          stepFlexmix(
            model_form,
            data = data,
            k = k,
            nrep = nrep,
            control = list(iter.max = 500)
          )
        } else {
          stepFlexmix(
            model_form,
            data = data,
            k = k,
            concomitant = FLXPmultinom(cov_formula),
            nrep = nrep,
            control = list(iter.max = 500)
          )
        }
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(fit_try)) {
      return(list(
        fit = fit_try,
        cov_used = terms,
        cov_dropped = cov_candidates$dropped,
        fallback_used = !identical(terms, cov_candidates$candidates[[1]])
      ))
    }
  }

  stop(last_error)
}

add_class_to_data <- function(data, fit, group_var, class_col = "group") {
  cls <- clusters(fit)
  class_labels <- paste0("Class ", cls)

  if (length(cls) == nrow(data)) {
    out <- data
    out[[class_col]] <- factor(class_labels)
    return(out)
  }

  stay_values <- levels(data[[group_var]])
  if (is.null(stay_values)) {
    stay_values <- unique(data[[group_var]])
  }

  if (!is.null(names(cls)) && all(names(cls) %in% as.character(stay_values))) {
    df_class <- data.frame(
      stay_id_tmp = factor(names(cls), levels = as.character(stay_values)),
      group_tmp = factor(paste0("Class ", as.integer(cls))),
      stringsAsFactors = FALSE
    )
  } else if (length(cls) == length(stay_values)) {
    df_class <- data.frame(
      stay_id_tmp = factor(stay_values, levels = as.character(stay_values)),
      group_tmp = factor(paste0("Class ", as.integer(cls))),
      stringsAsFactors = FALSE
    )
  } else {
    stop(
      "Cannot align clusters with data: length(clusters) = ",
      length(cls),
      ", nrow(data) = ",
      nrow(data),
      ", number of stays = ",
      length(stay_values),
      "."
    )
  }

  out <- data %>%
    mutate(stay_id_tmp = factor(.data[[group_var]], levels = as.character(stay_values))) %>%
    left_join(df_class, by = "stay_id_tmp") %>%
    select(-stay_id_tmp)

  out[[class_col]] <- out$group_tmp
  out$group_tmp <- NULL
  out
}

make_pred <- function(fit, time_grid, group_var, data, k) {
  pred_grid <- time_grid
  pred_grid[[group_var]] <- factor(
    levels(data[[group_var]])[1],
    levels = levels(data[[group_var]])
  )

  pred <- predict(fit, newdata = pred_grid)

  if (is.list(pred)) {
    pred <- do.call(cbind, pred)
  } else {
    pred <- as.matrix(pred)
  }

  if (nrow(pred) != nrow(pred_grid) && ncol(pred) == nrow(pred_grid)) {
    pred <- t(pred)
  }

  pred <- pred[, seq_len(min(k, ncol(pred))), drop = FALSE]
  colnames(pred) <- paste0("Class ", seq_len(ncol(pred)))

  data.frame(hour_offset = pred_grid$hour_offset, pred, check.names = FALSE) %>%
    pivot_longer(
      cols = starts_with("Class "),
      names_to = "trajectory",
      values_to = "gcs_pred"
    )
}

plot_trajectory <- function(pred_df, data, class_col, title) {
  ggplot() +
    geom_line(
      data = pred_df,
      aes(x = hour_offset, y = gcs_pred, color = trajectory),
      linewidth = 1.2
    ) +
    geom_smooth(
      data = data,
      aes(x = hour_offset, y = gcs_min, color = .data[[class_col]]),
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.9
    ) +
    scale_y_continuous(breaks = 3:15) +
    coord_cartesian(ylim = c(3, 15)) +
    labs(
      x = "Hours after ICU admission",
      y = "Minimum GCS",
      title = title,
      color = "Class"
    ) +
    theme_bw(base_size = 12)
}

plot_smooth_avg <- function(data, class_col, title) {
  ggplot(data, aes(x = hour_offset, y = gcs_min, color = .data[[class_col]])) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = TRUE,
      linewidth = 1.2
    ) +
    scale_y_continuous(breaks = 3:15) +
    coord_cartesian(ylim = c(3, 15)) +
    labs(
      x = "Hours after ICU admission",
      y = "Minimum GCS",
      title = title,
      color = "Class"
    ) +
    theme_bw(base_size = 12)
}

# ====================== 5. Batch run ======================
time_grid <- data.frame(
  hour_offset = seq(
    min(df$hour_offset, na.rm = TRUE),
    max(df$hour_offset, na.rm = TRUE),
    length.out = 150
  )
)

results <- list()
run_status <- list()

for (k in k_list) {
  for (m in names(model_specs)) {
    spec <- model_specs[[m]]
    key <- paste0(m, "_k", k)

    message("Fitting: k = ", k, " | Model: ", spec$name)

    fit_result <- tryCatch(
      fit_lc(
        data = df,
        k = k,
        formula = trajectory_formula,
        group_var = group_var,
        cov_terms = spec$cov_terms,
        nrep = nrep
      ),
      error = function(e) {
        run_status[[key]] <<- data.frame(
          model = key,
          fit_ok = FALSE,
          cov_used = "",
          cov_dropped = paste(validate_covariates(df, spec$cov_terms)$dropped, collapse = " + "),
          fallback_used = NA,
          error = conditionMessage(e),
          stringsAsFactors = FALSE
        )
        NULL
      }
    )

    if (is.null(fit_result)) {
      warning("Skipped ", key, " because model fitting failed.")
      next
    }

    fit <- fit_result$fit
    df_plot <- add_class_to_data(df, fit, group_var = group_var, class_col = "group")
    pred_df <- make_pred(fit, time_grid, group_var = group_var, data = df, k = k)

    p1 <- plot_trajectory(
      pred_df,
      df_plot,
      "group",
      paste0(spec$name, " (k=", k, ")")
    )

    p2 <- plot_smooth_avg(
      df_plot,
      "group",
      paste0("Smoothed average | ", spec$name, " (k=", k, ")")
    )

    if (save_plots) {
      ggsave(
        filename = file.path(plot_dir, paste0("traj_", m, "_k", k, ".png")),
        plot = p1,
        width = 9,
        height = 5,
        dpi = 300
      )

      ggsave(
        filename = file.path(plot_dir, paste0("smooth_", m, "_k", k, ".png")),
        plot = p2,
        width = 9,
        height = 5,
        dpi = 300
      )
    }

    results[[key]] <- list(
      fit = fit,
      data = df_plot,
      pred = pred_df,
      plot = p1,
      smooth = p2,
      cov_used = fit_result$cov_used,
      cov_dropped = fit_result$cov_dropped,
      fallback_used = fit_result$fallback_used
    )

    run_status[[key]] <- data.frame(
      model = key,
      fit_ok = TRUE,
      cov_used = paste(fit_result$cov_used, collapse = " + "),
      cov_dropped = paste(fit_result$cov_dropped, collapse = " + "),
      fallback_used = fit_result$fallback_used,
      error = "",
      stringsAsFactors = FALSE
    )
  }
}

run_status <- bind_rows(run_status)
print(run_status)

if (save_plots) {
  message("Saved plots to: ", normalizePath(plot_dir, mustWork = FALSE))
}

# Print plots in RStudio.
if (interactive()) {
  for (key in names(results)) {
    print(results[[key]]$plot)
    print(results[[key]]$smooth)
  }
}
