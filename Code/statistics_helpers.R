# ============================================================
# STATISTICAL SUMMARY UTILITIES
# ============================================================


bootstrap_median_ci <- function(
  x,
  bootstrap_reps = 1000,
  confidence_level = 0.95
) {

  # Keep only finite observations
  x <- x[
    is.finite(x)
  ]

  n <- length(x)

  if (n == 0) {

    return(
      c(
        lower = NA_real_,
        upper = NA_real_
      )
    )
  }

  if (n == 1) {

    return(
      c(
        lower = x[1],
        upper = x[1]
      )
    )
  }

  bootstrap_medians <- numeric(
    bootstrap_reps
  )

  # Nonparametric bootstrap
  for (b in seq_len(bootstrap_reps)) {

    bootstrap_sample <- sample(
      x,
      size = n,
      replace = TRUE
    )

    bootstrap_medians[b] <- median(
      bootstrap_sample
    )
  }

  alpha <- 1 - confidence_level

  limits <- quantile(
    bootstrap_medians,
    probs = c(
      alpha / 2,
      1 - alpha / 2
    ),
    names = FALSE,
    type = 7
  )

  return(
    c(
      lower = limits[1],
      upper = limits[2]
    )
  )
}


summarize_experiment <- function(
  data,
  group_columns,
  metric_columns,
  bootstrap_reps = 1000
) {

  # Build one grouping factor from all grouping columns
  group_factor <- do.call(
    interaction,
    c(
      data[group_columns],
      list(
        drop = TRUE,
        lex.order = TRUE
      )
    )
  )

  groups <- split(
    data,
    group_factor
  )

  summary_rows <- lapply(
    groups,
    function(group_data) {

      # Keep the grouping variables in the output
      result <- group_data[
        1,
        group_columns,
        drop = FALSE
      ]

      for (metric in metric_columns) {

        x <- group_data[[metric]]

        # Remove non-finite observations
        x <- x[
          is.finite(x)
        ]

        confidence_interval <-
          bootstrap_median_ci(
            x,
            bootstrap_reps = bootstrap_reps
          )

        # Number of valid observations
        result[[paste0(
          metric,
          "_n"
        )]] <- length(x)

        # Arithmetic mean
        result[[paste0(
          metric,
          "_mean"
        )]] <- mean(x)

        # Standard deviation
        result[[paste0(
          metric,
          "_sd"
        )]] <- if (length(x) > 1) {
          sd(x)
        } else {
          0
        }

        # Median
        result[[paste0(
          metric,
          "_median"
        )]] <- median(x)

        # First quartile
        result[[paste0(
          metric,
          "_q1"
        )]] <- as.numeric(
          quantile(
            x,
            probs = 0.25,
            names = FALSE,
            type = 7
          )
        )

        # Third quartile
        result[[paste0(
          metric,
          "_q3"
        )]] <- as.numeric(
          quantile(
            x,
            probs = 0.75,
            names = FALSE,
            type = 7
          )
        )

        # Minimum
        result[[paste0(
          metric,
          "_min"
        )]] <- min(x)

        # Maximum
        result[[paste0(
          metric,
          "_max"
        )]] <- max(x)

        # Bootstrap 95% confidence interval for the median
        result[[paste0(
          metric,
          "_median_ci_low"
        )]] <- unname(
          confidence_interval["lower"]
        )

        result[[paste0(
          metric,
          "_median_ci_high"
        )]] <- unname(
          confidence_interval["upper"]
        )
      }

      return(result)
    }
  )

  summary_table <- do.call(
    rbind,
    summary_rows
  )

  rownames(summary_table) <- NULL

  return(summary_table)
}