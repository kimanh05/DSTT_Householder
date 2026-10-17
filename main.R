# ============================================================
# QR DECOMPOSITION USING HOUSEHOLDER REFLECTIONS
# Linear Algebra Final Project
# ============================================================

source("householder_qr.R")
source("classical_gram_schmidt_qr.R")
source("verify_qr.R")
source("experiments.R")
source("plots.R")


run_project <- function() {

  cat("============================================================\n")
  cat("QR DECOMPOSITION USING HOUSEHOLDER REFLECTIONS\n")
  cat("============================================================\n")

  cat("\nR version:\n")
  cat(R.version.string, "\n")


  # ==========================================================
  # 1. Hand-calculated example
  # ==========================================================

  cat("\n============================================================\n")
  cat("1. HAND-CALCULATED EXAMPLE\n")
  cat("============================================================\n")

  A <- matrix(
    c(
      4, 3, 0,
      1, 2, 2
    ),
    nrow = 3,
    ncol = 2
  )

  cat("\nInput matrix A:\n")
  print(A)


  # ==========================================================
  # Householder QR
  # ==========================================================

  householder_result <- householder_qr(A)

  Q_full <- householder_result$Q
  R_full <- householder_result$R

  number_of_columns <- ncol(A)

  Q_householder <- Q_full[
    ,
    seq_len(number_of_columns),
    drop = FALSE
  ]

  R_householder <- R_full[
    seq_len(number_of_columns),
    ,
    drop = FALSE
  ]

  cat("\nHouseholder Q (full form):\n")
  print(Q_full)

  cat("\nHouseholder R (full form):\n")
  print(zapsmall(R_full))

  cat("\nHouseholder Q * R:\n")
  print(zapsmall(Q_full %*% R_full))

  cat("\nHouseholder Q^T * Q:\n")
  print(zapsmall(t(Q_full) %*% Q_full))

  householder_error <- verify_qr(
    A,
    Q_householder,
    R_householder
  )

  cat(
    "\nHouseholder orthogonality error:",
    format(
      householder_error$e_orth,
      scientific = TRUE,
      digits = 8
    ),
    "\n"
  )

  cat(
    "Householder reconstruction error:",
    format(
      householder_error$e_rec,
      scientific = TRUE,
      digits = 8
    ),
    "\n"
  )


  # ==========================================================
  # 2. R built-in QR
  # ==========================================================

  cat("\n============================================================\n")
  cat("2. R BUILT-IN QR\n")
  cat("============================================================\n")

  builtin_result <- builtin_qr(A)

  cat("\nBuilt-in Q:\n")
  print(builtin_result$Q)

  cat("\nBuilt-in R:\n")
  print(zapsmall(builtin_result$R))

  builtin_error <- verify_qr(
    A,
    builtin_result$Q,
    builtin_result$R
  )

  cat(
    "\nBuilt-in QR orthogonality error:",
    format(
      builtin_error$e_orth,
      scientific = TRUE,
      digits = 8
    ),
    "\n"
  )

  cat(
    "Built-in QR reconstruction error:",
    format(
      builtin_error$e_rec,
      scientific = TRUE,
      digits = 8
    ),
    "\n"
  )


  # ==========================================================
  # 3. Classical Gram-Schmidt
  # ==========================================================

  cat("\n============================================================\n")
  cat("3. CLASSICAL GRAM-SCHMIDT\n")
  cat("============================================================\n")

  cgs_result <- classical_gram_schmidt_qr(A)

  cat("\nCGS Q:\n")
  print(cgs_result$Q)

  cat("\nCGS R:\n")
  print(zapsmall(cgs_result$R))

  cgs_error <- verify_qr(
    A,
    cgs_result$Q,
    cgs_result$R
  )

  cat(
    "\nCGS orthogonality error:",
    format(
      cgs_error$e_orth,
      scientific = TRUE,
      digits = 8
    ),
    "\n"
  )

  cat(
    "CGS reconstruction error:",
    format(
      cgs_error$e_rec,
      scientific = TRUE,
      digits = 8
    ),
    "\n"
  )


  # ==========================================================
  # Save small-example errors
  # ==========================================================

  small_example_results <- data.frame(
    method = c(
      "Householder",
      "R built-in QR",
      "Classical Gram-Schmidt"
    ),
    e_orth = c(
      householder_error$e_orth,
      builtin_error$e_orth,
      cgs_error$e_orth
    ),
    e_rec = c(
      householder_error$e_rec,
      builtin_error$e_rec,
      cgs_error$e_rec
    )
  )

  write.csv(
    small_example_results,
    "small_example_results.csv",
    row.names = FALSE
  )


  # ==========================================================
  # 4. Random matrix experiments
  # ==========================================================

  cat("\n============================================================\n")
  cat("4. RANDOM MATRIX EXPERIMENTS\n")
  cat("============================================================\n")

  random_results <- run_random_experiments()

  print(
    random_results,
    row.names = FALSE
  )


  # ==========================================================
  # 5. Nearly dependent matrix experiments
  # ==========================================================

  cat("\n============================================================\n")
  cat("5. NEAR-DEPENDENT MATRIX EXPERIMENTS\n")
  cat("============================================================\n")

  near_results <- run_near_dependent_experiments()

  print(
    near_results,
    row.names = FALSE
  )


  # ==========================================================
  # 6. Generate plots
  # ==========================================================

  cat("\n============================================================\n")
  cat("6. GENERATING PLOTS\n")
  cat("============================================================\n")

  plot_random_results(random_results)
  plot_near_dependent_results(near_results)

  cat("\nPlots generated successfully.\n")


  # ==========================================================
  # Final summary
  # ==========================================================

  cat("\n============================================================\n")
  cat("PROJECT RUN COMPLETED SUCCESSFULLY\n")
  cat("============================================================\n")

  cat("\nGenerated result files:\n")
  cat("- small_example_results.csv\n")
  cat("- random_results.csv\n")
  cat("- near_dependent_results.csv\n")
  cat("- random_orthogonality_error.png\n")
  cat("- random_reconstruction_error.png\n")
  cat("- random_runtime.png\n")
  cat("- near_dependent_orthogonality_error.png\n")
}


# Capture the complete textual output into a log file
log_output <- capture.output(
  run_project()
)

writeLines(
  log_output,
  "experiment_log.txt"
)

# Also display the saved log in the R console
cat(
  paste(
    log_output,
    collapse = "\n"
  )
)

cat("\n")


# Save the software/environment information
writeLines(
  capture.output(sessionInfo()),
  "session_info.txt"
)