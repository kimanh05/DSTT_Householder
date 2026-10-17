measure_runtime <- function(function_to_measure, repeats = 10) {

  # Run the function once before timing to reduce warm-up effects
  invisible(function_to_measure())

  start_time <- proc.time()[["elapsed"]]

  for (i in seq_len(repeats)) {
    invisible(function_to_measure())
  }

  end_time <- proc.time()[["elapsed"]]

  average_time <- (end_time - start_time) / repeats

  return(average_time)
}


run_random_experiments <- function() {

  set.seed(2026)

  matrix_sizes <- matrix(
    c(
      10, 5,
      50, 20,
      100, 50,
      200, 100
    ),
    ncol = 2,
    byrow = TRUE
  )

  results <- data.frame()

  for (test_index in seq_len(nrow(matrix_sizes))) {

    rows <- matrix_sizes[test_index, 1]
    cols <- matrix_sizes[test_index, 2]

    A <- matrix(
      rnorm(rows * cols),
      nrow = rows,
      ncol = cols
    )

    # ========================================================
    # Householder QR
    # ========================================================

    householder_time <- measure_runtime(
      function() householder_qr(A)
    )

    householder_result <- householder_qr(A)

    Q_householder <- householder_result$Q[
      ,
      seq_len(cols),
      drop = FALSE
    ]

    R_householder <- householder_result$R[
      seq_len(cols),
      ,
      drop = FALSE
    ]

    householder_error <- verify_qr(
      A,
      Q_householder,
      R_householder
    )

    # ========================================================
    # Classical Gram-Schmidt
    # ========================================================

    cgs_time <- measure_runtime(
      function() classical_gram_schmidt_qr(A)
    )

    cgs_result <- classical_gram_schmidt_qr(A)

    cgs_error <- verify_qr(
      A,
      cgs_result$Q,
      cgs_result$R
    )

    # ========================================================
    # Built-in QR
    # ========================================================

    builtin_time <- measure_runtime(
      function() builtin_qr(A)
    )

    builtin_result <- builtin_qr(A)

    builtin_error <- verify_qr(
      A,
      builtin_result$Q,
      builtin_result$R
    )

    # ========================================================
    # Store results
    # ========================================================

    results <- rbind(
      results,

      data.frame(
        rows = rows,
        cols = cols,
        method = "Householder",
        e_orth = householder_error$e_orth,
        e_rec = householder_error$e_rec,
        runtime = householder_time
      ),

      data.frame(
        rows = rows,
        cols = cols,
        method = "Classical Gram-Schmidt",
        e_orth = cgs_error$e_orth,
        e_rec = cgs_error$e_rec,
        runtime = cgs_time
      ),

      data.frame(
        rows = rows,
        cols = cols,
        method = "R built-in QR",
        e_orth = builtin_error$e_orth,
        e_rec = builtin_error$e_rec,
        runtime = builtin_time
      )
    )
  }

  write.csv(
    results,
    "random_results.csv",
    row.names = FALSE
  )

  return(results)
}


run_near_dependent_experiments <- function() {

  epsilon_values <- c(
    1e-4,
    1e-8,
    1e-12
  )

  results <- data.frame()

  for (epsilon in epsilon_values) {

    # Construct two nearly linearly dependent columns
    A <- matrix(
      c(
        1, 1, 1,
        1, 1, 1 + epsilon
      ),
      nrow = 3,
      ncol = 2
    )

    # ========================================================
    # Householder QR
    # ========================================================

    householder_result <- householder_qr(A)

    Q_householder <- householder_result$Q[
      ,
      1:2,
      drop = FALSE
    ]

    R_householder <- householder_result$R[
      1:2,
      ,
      drop = FALSE
    ]

    householder_error <- verify_qr(
      A,
      Q_householder,
      R_householder
    )

    # ========================================================
    # Classical Gram-Schmidt
    # ========================================================

    cgs_result <- classical_gram_schmidt_qr(A)

    cgs_error <- verify_qr(
      A,
      cgs_result$Q,
      cgs_result$R
    )

    # ========================================================
    # Built-in QR
    # ========================================================

    builtin_result <- builtin_qr(A)

    builtin_error <- verify_qr(
      A,
      builtin_result$Q,
      builtin_result$R
    )

    # ========================================================
    # Store results
    # ========================================================

    results <- rbind(
      results,

      data.frame(
        epsilon = epsilon,
        method = "Householder",
        e_orth = householder_error$e_orth,
        e_rec = householder_error$e_rec
      ),

      data.frame(
        epsilon = epsilon,
        method = "Classical Gram-Schmidt",
        e_orth = cgs_error$e_orth,
        e_rec = cgs_error$e_rec
      ),

      data.frame(
        epsilon = epsilon,
        method = "R built-in QR",
        e_orth = builtin_error$e_orth,
        e_rec = builtin_error$e_rec
      )
    )
  }

  write.csv(
    results,
    "near_dependent_results.csv",
    row.names = FALSE
  )

  return(results)
}