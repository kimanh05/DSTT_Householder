# ============================================================
# NUMERICAL EXPERIMENTS
# ============================================================


evaluate_methods <- function(A) {

  n <- ncol(A)

  # ==========================================================
  # Householder QR
  # ==========================================================

  householder_result <- householder_qr(A)

  Q_householder <- householder_result$Q[
    ,
    seq_len(n),
    drop = FALSE
  ]

  R_householder <- householder_result$R[
    seq_len(n),
    ,
    drop = FALSE
  ]

  householder_error <- verify_qr(
    A,
    Q_householder,
    R_householder
  )


  # ==========================================================
  # Classical Gram-Schmidt
  # ==========================================================

  cgs_result <- classical_gram_schmidt_qr(A)

  cgs_error <- verify_qr(
    A,
    cgs_result$Q,
    cgs_result$R
  )


  # ==========================================================
  # R built-in QR
  # ==========================================================

  builtin_result <- builtin_qr(A)

  builtin_error <- verify_qr(
    A,
    builtin_result$Q,
    builtin_result$R
  )


  return(
    data.frame(
      method = c(
        "Householder",
        "Classical Gram-Schmidt",
        "R built-in QR"
      ),
      e_orth = c(
        householder_error$e_orth,
        cgs_error$e_orth,
        builtin_error$e_orth
      ),
      e_rec = c(
        householder_error$e_rec,
        cgs_error$e_rec,
        builtin_error$e_rec
      )
    )
  )
}


# ============================================================
# Experiment 1:
# Accuracy on independent Gaussian random matrices
# ============================================================

run_random_accuracy_experiment <- function(
  matrix_sizes,
  trials_per_size = 50
) {

  results <- data.frame()

  for (size_index in seq_len(nrow(matrix_sizes))) {

    m <- matrix_sizes[
      size_index,
      1
    ]

    n <- matrix_sizes[
      size_index,
      2
    ]

    cat(
      sprintf(
        "\nRandom accuracy: %dx%d\n",
        m,
        n
      )
    )

    for (trial in seq_len(trials_per_size)) {

      A <- generate_gaussian_matrix(
        m,
        n
      )

      actual_kappa <- condition_number_2(A)

      method_results <- evaluate_methods(A)

      method_results$rows <- m
      method_results$cols <- n
      method_results$trial <- trial
      method_results$condition_number <- actual_kappa

      results <- rbind(
        results,
        method_results
      )

      if (
        trial %% 10 == 0 ||
        trial == trials_per_size
      ) {

        cat(
          sprintf(
            "  Completed %d/%d matrices\n",
            trial,
            trials_per_size
          )
        )
      }
    }
  }

  results <- results[
    ,
    c(
      "rows",
      "cols",
      "trial",
      "condition_number",
      "method",
      "e_orth",
      "e_rec"
    )
  ]

  return(results)
}


# ============================================================
# Experiment 2:
# Stability under controlled condition numbers
# ============================================================

run_condition_experiment <- function(
  m = 100,
  n = 50,
  condition_levels,
  trials_per_level = 50
) {

  results <- data.frame()

  for (
    target_kappa in condition_levels
  ) {

    cat(
      sprintf(
        "\nControlled condition number: %.0e\n",
        target_kappa
      )
    )

    for (
      trial in seq_len(
        trials_per_level
      )
    ) {

      generated <-
        generate_controlled_condition_matrix(
          m = m,
          n = n,
          target_kappa = target_kappa
        )

      A <- generated$A

      method_results <-
        evaluate_methods(A)

      method_results$rows <- m
      method_results$cols <- n
      method_results$trial <- trial

      method_results$target_kappa <-
        target_kappa

      method_results$actual_kappa <-
        generated$actual_kappa

      results <- rbind(
        results,
        method_results
      )

      if (
        trial %% 10 == 0 ||
        trial == trials_per_level
      ) {

        cat(
          sprintf(
            "  Completed %d/%d matrices\n",
            trial,
            trials_per_level
          )
        )
      }
    }
  }

  results <- results[
    ,
    c(
      "rows",
      "cols",
      "target_kappa",
      "actual_kappa",
      "trial",
      "method",
      "e_orth",
      "e_rec"
    )
  ]

  return(results)
}


# ============================================================
# Stable runtime measurement
# ============================================================

measure_runtime_stable <- function(
  function_to_measure,
  minimum_batch_time = 0.15,
  timing_batches = 5,
  maximum_repeats = 10000
) {

  # Warm-up run
  invisible(
    function_to_measure()
  )

  repeats <- 1

  repeat {

    start_time <-
      proc.time()[["elapsed"]]

    for (i in seq_len(repeats)) {

      invisible(
        function_to_measure()
      )
    }

    elapsed_time <-
      proc.time()[["elapsed"]] -
      start_time

    if (
      elapsed_time >= minimum_batch_time ||
      repeats >= maximum_repeats
    ) {
      break
    }

    repeats <- min(
      repeats * 2,
      maximum_repeats
    )
  }

  per_call_times <- numeric(
    timing_batches
  )

  for (
    batch in seq_len(timing_batches)
  ) {

    start_time <-
      proc.time()[["elapsed"]]

    for (i in seq_len(repeats)) {

      invisible(
        function_to_measure()
      )
    }

    elapsed_time <-
      proc.time()[["elapsed"]] -
      start_time

    per_call_times[batch] <-
      elapsed_time / repeats
  }

  return(
    list(
      runtime = median(
        per_call_times
      ),
      within_q1 = as.numeric(
        quantile(
          per_call_times,
          0.25,
          names = FALSE
        )
      ),
      within_q3 = as.numeric(
        quantile(
          per_call_times,
          0.75,
          names = FALSE
        )
      ),
      repeats_per_batch = repeats
    )
  )
}


# ============================================================
# Experiment 3:
# Runtime scaling
# ============================================================

run_runtime_experiment <- function(
  matrix_sizes,
  input_trials = 20,
  timing_batches = 5
) {

  results <- data.frame()

  for (
    size_index in seq_len(
      nrow(matrix_sizes)
    )
  ) {

    m <- matrix_sizes[
      size_index,
      1
    ]

    n <- matrix_sizes[
      size_index,
      2
    ]

    cat(
      sprintf(
        "\nRuntime experiment: %dx%d\n",
        m,
        n
      )
    )

    for (
      trial in seq_len(
        input_trials
      )
    ) {

      A <- generate_gaussian_matrix(
        m,
        n
      )

      # Householder
      householder_timing <-
        measure_runtime_stable(
          function() householder_qr(A),
          timing_batches = timing_batches
        )

      # Classical Gram-Schmidt
      cgs_timing <-
        measure_runtime_stable(
          function() classical_gram_schmidt_qr(A),
          timing_batches = timing_batches
        )

      # R built-in QR
      builtin_timing <-
        measure_runtime_stable(
          function() builtin_qr(A),
          timing_batches = timing_batches
        )

      trial_result <- data.frame(

        rows = rep(
          m,
          3
        ),

        cols = rep(
          n,
          3
        ),

        trial = rep(
          trial,
          3
        ),

        method = c(
          "Householder",
          "Classical Gram-Schmidt",
          "R built-in QR"
        ),

        runtime = c(
          householder_timing$runtime,
          cgs_timing$runtime,
          builtin_timing$runtime
        ),

        within_q1 = c(
          householder_timing$within_q1,
          cgs_timing$within_q1,
          builtin_timing$within_q1
        ),

        within_q3 = c(
          householder_timing$within_q3,
          cgs_timing$within_q3,
          builtin_timing$within_q3
        ),

        repeats_per_batch = c(
          householder_timing$repeats_per_batch,
          cgs_timing$repeats_per_batch,
          builtin_timing$repeats_per_batch
        )
      )

      results <- rbind(
        results,
        trial_result
      )

      cat(
        sprintf(
          "  Completed %d/%d matrices\n",
          trial,
          input_trials
        )
      )
    }
  }

  return(results)
}