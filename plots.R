plot_random_results <- function(random_results) {

  householder_data <- random_results[
    random_results$method == "Householder",
  ]

  cgs_data <- random_results[
    random_results$method == "Classical Gram-Schmidt",
  ]

  builtin_data <- random_results[
    random_results$method == "R built-in QR",
  ]

  matrix_labels <- paste0(
    householder_data$rows,
    "x",
    householder_data$cols
  )

  x_positions <- seq_along(matrix_labels)

  # ========================================================
  # Orthogonality error
  # ========================================================

  png(
    "random_orthogonality_error.png",
    width = 1200,
    height = 800,
    res = 130
  )

  plot(
    x_positions,
    householder_data$e_orth,
    type = "b",
    log = "y",
    pch = 16,
    xaxt = "n",
    xlab = "Matrix size",
    ylab = "Orthogonality error",
    main = "Orthogonality Error on Random Matrices"
  )

  axis(
    1,
    at = x_positions,
    labels = matrix_labels
  )

  lines(
    x_positions,
    cgs_data$e_orth,
    type = "b",
    pch = 17
  )

  lines(
    x_positions,
    builtin_data$e_orth,
    type = "b",
    pch = 15
  )

  legend(
    "topleft",
    legend = c(
      "Householder",
      "Classical Gram-Schmidt",
      "R built-in QR"
    ),
    pch = c(16, 17, 15),
    lty = 1
  )

  grid()

  dev.off()


  # ========================================================
  # Reconstruction error
  # ========================================================

  png(
    "random_reconstruction_error.png",
    width = 1200,
    height = 800,
    res = 130
  )

  plot(
    x_positions,
    householder_data$e_rec,
    type = "b",
    log = "y",
    pch = 16,
    xaxt = "n",
    xlab = "Matrix size",
    ylab = "Relative reconstruction error",
    main = "Reconstruction Error on Random Matrices"
  )

  axis(
    1,
    at = x_positions,
    labels = matrix_labels
  )

  lines(
    x_positions,
    cgs_data$e_rec,
    type = "b",
    pch = 17
  )

  lines(
    x_positions,
    builtin_data$e_rec,
    type = "b",
    pch = 15
  )

  legend(
    "topleft",
    legend = c(
      "Householder",
      "Classical Gram-Schmidt",
      "R built-in QR"
    ),
    pch = c(16, 17, 15),
    lty = 1
  )

  grid()

  dev.off()


  # ========================================================
  # Runtime
  # ========================================================

  png(
    "random_runtime.png",
    width = 1200,
    height = 800,
    res = 130
  )

  plot(
    x_positions,
    householder_data$runtime,
    type = "b",
    pch = 16,
    xaxt = "n",
    xlab = "Matrix size",
    ylab = "Average runtime (seconds)",
    main = "Runtime Comparison on Random Matrices"
  )

  axis(
    1,
    at = x_positions,
    labels = matrix_labels
  )

  lines(
    x_positions,
    cgs_data$runtime,
    type = "b",
    pch = 17
  )

  lines(
    x_positions,
    builtin_data$runtime,
    type = "b",
    pch = 15
  )

  legend(
    "topleft",
    legend = c(
      "Householder",
      "Classical Gram-Schmidt",
      "R built-in QR"
    ),
    pch = c(16, 17, 15),
    lty = 1
  )

  grid()

  dev.off()
}


plot_near_dependent_results <- function(near_results) {

  householder_data <- near_results[
    near_results$method == "Householder",
  ]

  cgs_data <- near_results[
    near_results$method == "Classical Gram-Schmidt",
  ]

  builtin_data <- near_results[
    near_results$method == "R built-in QR",
  ]

  epsilon_labels <- c(
    "1e-4",
    "1e-8",
    "1e-12"
  )

  x_positions <- seq_along(epsilon_labels)

  png(
    "near_dependent_orthogonality_error.png",
    width = 1200,
    height = 800,
    res = 130
  )

  plot(
    x_positions,
    householder_data$e_orth,
    type = "b",
    log = "y",
    pch = 16,
    xaxt = "n",
    xlab = "Epsilon",
    ylab = "Orthogonality error",
    main = "Orthogonality Error for Nearly Dependent Columns"
  )

  axis(
    1,
    at = x_positions,
    labels = epsilon_labels
  )

  lines(
    x_positions,
    cgs_data$e_orth,
    type = "b",
    pch = 17
  )

  lines(
    x_positions,
    builtin_data$e_orth,
    type = "b",
    pch = 15
  )

  legend(
    "topleft",
    legend = c(
      "Householder",
      "Classical Gram-Schmidt",
      "R built-in QR"
    ),
    pch = c(16, 17, 15),
    lty = 1
  )

  grid()

  dev.off()
}