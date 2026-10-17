# ============================================================
# PLOT UTILITIES
# ============================================================


positive_log_range <- function(values) {

  positive_values <- values[
    is.finite(values) & values > 0
  ]

  if (length(positive_values) == 0) {

    return(
      c(
        1e-18,
        1
      )
    )
  }

  lower_limit <- min(positive_values) / 2
  upper_limit <- max(positive_values) * 2

  if (lower_limit == upper_limit) {

    lower_limit <- lower_limit / 10
    upper_limit <- upper_limit * 10
  }

  return(
    c(
      lower_limit,
      upper_limit
    )
  )
}


# ============================================================
# Plot accuracy metrics versus matrix size
# ============================================================

plot_summary_by_size <- function(
  summary_data,
  metric,
  output_file,
  plot_title,
  y_label
) {

  methods <- c(
    "Householder",
    "Classical Gram-Schmidt",
    "R built-in QR"
  )

  symbols <- c(
    16,
    17,
    15
  )

  line_types <- c(
    1,
    2,
    3
  )

  reference_data <- summary_data[
    summary_data$method == "Householder",
    ,
    drop = FALSE
  ]

  reference_data <- reference_data[
    order(reference_data$rows),
    ,
    drop = FALSE
  ]

  x_positions <- seq_len(
    nrow(reference_data)
  )

  matrix_labels <- paste0(
    reference_data$rows,
    "x",
    reference_data$cols
  )

  median_column <- paste0(
    metric,
    "_median"
  )

  q1_column <- paste0(
    metric,
    "_q1"
  )

  q3_column <- paste0(
    metric,
    "_q3"
  )

  y_range <- positive_log_range(
    c(
      summary_data[[q1_column]],
      summary_data[[q3_column]]
    )
  )

  png(
    filename = output_file,
    width = 1200,
    height = 800,
    res = 130
  )

  plot(
    x = x_positions,
    y = reference_data[[median_column]],
    type = "n",
    log = "y",
    xaxt = "n",
    ylim = y_range,
    xlab = "Matrix size",
    ylab = y_label,
    main = plot_title
  )

  axis(
    side = 1,
    at = x_positions,
    labels = matrix_labels
  )

  grid()

  for (method_index in seq_along(methods)) {

    method_name <- methods[method_index]

    method_data <- summary_data[
      summary_data$method == method_name,
      ,
      drop = FALSE
    ]

    method_data <- method_data[
      order(method_data$rows),
      ,
      drop = FALSE
    ]

    median_values <- method_data[[median_column]]
    q1_values <- method_data[[q1_column]]
    q3_values <- method_data[[q3_column]]

    lines(
      x = x_positions,
      y = median_values,
      type = "b",
      pch = symbols[method_index],
      lty = line_types[method_index]
    )

    arrows(
      x0 = x_positions,
      y0 = q1_values,
      x1 = x_positions,
      y1 = q3_values,
      angle = 90,
      code = 3,
      length = 0.035
    )
  }

  legend(
    "topleft",
    legend = methods,
    pch = symbols,
    lty = line_types
  )

  dev.off()
}


# ============================================================
# Plot orthogonality error versus condition number
# ============================================================

plot_condition_orthogonality <- function(
  summary_data,
  output_file
) {

  methods <- c(
    "Householder",
    "Classical Gram-Schmidt",
    "R built-in QR"
  )

  symbols <- c(
    16,
    17,
    15
  )

  line_types <- c(
    1,
    2,
    3
  )

  x_range <- range(
    summary_data$target_kappa,
    finite = TRUE
  )

  y_range <- positive_log_range(
    c(
      summary_data$e_orth_q1,
      summary_data$e_orth_q3
    )
  )

  png(
    filename = output_file,
    width = 1200,
    height = 800,
    res = 130
  )

  plot(
    NA_real_,
    NA_real_,
    xlim = x_range,
    ylim = y_range,
    log = "xy",
    xlab = expression(kappa[2](A)),
    ylab = "Orthogonality error",
    main = "Loss of Orthogonality versus Matrix Condition Number"
  )

  grid()

  for (method_index in seq_along(methods)) {

    method_name <- methods[method_index]

    method_data <- summary_data[
      summary_data$method == method_name,
      ,
      drop = FALSE
    ]

    method_data <- method_data[
      order(method_data$target_kappa),
      ,
      drop = FALSE
    ]

    x_values <- method_data$target_kappa
    median_values <- method_data$e_orth_median
    q1_values <- method_data$e_orth_q1
    q3_values <- method_data$e_orth_q3

    lines(
      x = x_values,
      y = median_values,
      type = "b",
      pch = symbols[method_index],
      lty = line_types[method_index]
    )

    arrows(
      x0 = x_values,
      y0 = q1_values,
      x1 = x_values,
      y1 = q3_values,
      angle = 90,
      code = 3,
      length = 0.035
    )
  }

  legend(
    "topleft",
    legend = methods,
    pch = symbols,
    lty = line_types
  )

  dev.off()
}


# ============================================================
# Plot runtime versus matrix size
# ============================================================

plot_runtime_summary <- function(
  summary_data,
  output_file
) {

  methods <- c(
    "Householder",
    "Classical Gram-Schmidt",
    "R built-in QR"
  )

  symbols <- c(
    16,
    17,
    15
  )

  line_types <- c(
    1,
    2,
    3
  )

  reference_data <- summary_data[
    summary_data$method == "Householder",
    ,
    drop = FALSE
  ]

  reference_data <- reference_data[
    order(reference_data$rows),
    ,
    drop = FALSE
  ]

  x_positions <- seq_len(
    nrow(reference_data)
  )

  matrix_labels <- paste0(
    reference_data$rows,
    "x",
    reference_data$cols
  )

  y_range <- positive_log_range(
    c(
      summary_data$runtime_q1,
      summary_data$runtime_q3
    )
  )

  png(
    filename = output_file,
    width = 1200,
    height = 800,
    res = 130
  )

  plot(
    x = x_positions,
    y = reference_data$runtime_median,
    type = "n",
    log = "y",
    xaxt = "n",
    ylim = y_range,
    xlab = "Matrix size",
    ylab = "Runtime per call (seconds)",
    main = "Runtime Scaling"
  )

  axis(
    side = 1,
    at = x_positions,
    labels = matrix_labels
  )

  grid()

  for (method_index in seq_along(methods)) {

    method_name <- methods[method_index]

    method_data <- summary_data[
      summary_data$method == method_name,
      ,
      drop = FALSE
    ]

    method_data <- method_data[
      order(method_data$rows),
      ,
      drop = FALSE
    ]

    median_values <- method_data$runtime_median
    q1_values <- method_data$runtime_q1
    q3_values <- method_data$runtime_q3

    lines(
      x = x_positions,
      y = median_values,
      type = "b",
      pch = symbols[method_index],
      lty = line_types[method_index]
    )

    arrows(
      x0 = x_positions,
      y0 = q1_values,
      x1 = x_positions,
      y1 = q3_values,
      angle = 90,
      code = 3,
      length = 0.035
    )
  }

  legend(
    "topleft",
    legend = methods,
    pch = symbols,
    lty = line_types
  )

  dev.off()
}