# ============================================================
# HOUSEHOLDER QR NUMERICAL STUDY
# ============================================================


# ============================================================
# Create project directories
# ============================================================

required_directories <- c(
  "Results",
  "Plots",
  "Logs",
  "Report",
  "Archive"
)

for (directory_name in required_directories) {

  if (!dir.exists(directory_name)) {

    dir.create(
      directory_name,
      recursive = TRUE
    )
  }
}


# ============================================================
# Load source files
# ============================================================

source(
  file.path(
    "Code",
    "householder_qr.R"
  )
)

source(
  file.path(
    "Code",
    "classical_gram_schmidt_qr.R"
  )
)

source(
  file.path(
    "Code",
    "verify_qr.R"
  )
)

source(
  file.path(
    "Code",
    "matrix_generators.R"
  )
)

source(
  file.path(
    "Code",
    "statistics_helpers.R"
  )
)

source(
  file.path(
    "Code",
    "experiments.R"
  )
)

source(
  file.path(
    "Code",
    "plots.R"
  )
)


# ============================================================
# Archive generated outputs from the previous run
# ============================================================

archive_previous_outputs <- function() {

  output_folders <- c(
    "Results",
    "Plots",
    "Logs"
  )

  previous_files <- unlist(
    lapply(
      output_folders,
      function(folder_name) {

        if (!dir.exists(folder_name)) {
          return(character(0))
        }

        list.files(
          folder_name,
          full.names = TRUE,
          recursive = FALSE
        )
      }
    )
  )

  previous_files <- previous_files[
    file.info(previous_files)$isdir == FALSE
  ]

  if (length(previous_files) == 0) {
    return(invisible(NULL))
  }

  timestamp <- format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  archive_root <- file.path(
    "Archive",
    paste0(
      "run_",
      timestamp
    )
  )

  counter <- 1

  while (dir.exists(archive_root)) {

    archive_root <- file.path(
      "Archive",
      paste0(
        "run_",
        timestamp,
        "_",
        counter
      )
    )

    counter <- counter + 1
  }

  dir.create(
    archive_root,
    recursive = TRUE
  )

  for (folder_name in output_folders) {

    source_folder <- folder_name

    source_files <- list.files(
      source_folder,
      full.names = TRUE,
      recursive = FALSE
    )

    source_files <- source_files[
      file.info(source_files)$isdir == FALSE
    ]

    if (length(source_files) == 0) {
      next
    }

    destination_folder <- file.path(
      archive_root,
      folder_name
    )

    dir.create(
      destination_folder,
      recursive = TRUE
    )

    for (source_file in source_files) {

      destination_file <- file.path(
        destination_folder,
        basename(source_file)
      )

      moved <- file.rename(
        source_file,
        destination_file
      )

      # Fallback for systems where rename cannot move the file
      if (!moved) {

        copied <- file.copy(
          source_file,
          destination_file,
          overwrite = TRUE
        )

        if (copied) {
          unlink(source_file)
        } else {
          warning(
            paste(
              "Could not archive:",
              source_file
            )
          )
        }
      }
    }
  }

  cat(
    "Previous generated outputs archived to:",
    archive_root,
    "\n"
  )

  invisible(
    archive_root
  )
}


archive_previous_outputs()


# ============================================================
# Experimental configuration
# ============================================================

MASTER_SEED <- 20261018

RANDOM_TRIALS_PER_SIZE <- 50

CONDITION_TRIALS_PER_LEVEL <- 50

RUNTIME_INPUT_TRIALS <- 20

TIMING_BATCHES <- 5

BOOTSTRAP_REPS <- 1000


MATRIX_SIZES <- matrix(
  c(
    10, 5,
    20, 10,
    50, 25,
    100, 50,
    200, 100,
    300, 150
  ),
  ncol = 2,
  byrow = TRUE
)


CONDITION_LEVELS <- c(
  1,
  1e2,
  1e4,
  1e6,
  1e8,
  1e10,
  1e12
)


CONDITION_ROWS <- 100

CONDITION_COLS <- 50


# ============================================================
# Save experimental configuration
# ============================================================

configuration <- data.frame(
  parameter = c(
    "Master seed",
    "Random trials per size",
    "Condition trials per level",
    "Runtime input trials",
    "Timing batches",
    "Bootstrap repetitions",
    "Condition experiment rows",
    "Condition experiment columns"
  ),
  value = c(
    MASTER_SEED,
    RANDOM_TRIALS_PER_SIZE,
    CONDITION_TRIALS_PER_LEVEL,
    RUNTIME_INPUT_TRIALS,
    TIMING_BATCHES,
    BOOTSTRAP_REPS,
    CONDITION_ROWS,
    CONDITION_COLS
  )
)

write.csv(
  configuration,
  file.path(
    "Results",
    "experiment_configuration.csv"
  ),
  row.names = FALSE
)


# ============================================================
# Complete numerical study
# ============================================================

run_project <- function() {

  study_start_time <- Sys.time()

  cat(
    "============================================================\n"
  )

  cat(
    "HOUSEHOLDER QR NUMERICAL STUDY\n"
  )

  cat(
    "============================================================\n"
  )

  cat(
    "\nStudy start time:",
    format(
      study_start_time,
      "%Y-%m-%d %H:%M:%S"
    ),
    "\n"
  )

  cat(
    "\nR version:\n"
  )

  cat(
    R.version.string,
    "\n"
  )


  # ==========================================================
  # Reproducibility
  # ==========================================================

  set.seed(
    MASTER_SEED
  )

  cat(
    "\nMaster random seed:",
    MASTER_SEED,
    "\n"
  )


  # ==========================================================
  # 1. Small verification example
  # ==========================================================

  cat(
    "\n============================================================\n"
  )

  cat(
    "1. SMALL VERIFICATION EXAMPLE\n"
  )

  cat(
    "============================================================\n"
  )

  A_example <- matrix(
    c(
      4, 3, 0,
      1, 2, 2
    ),
    nrow = 3,
    ncol = 2
  )

  example_results <- evaluate_methods(
    A_example
  )

  print(
    example_results,
    row.names = FALSE
  )

  write.csv(
    example_results,
    file.path(
      "Results",
      "small_example_results.csv"
    ),
    row.names = FALSE
  )


  # ==========================================================
  # 2. Accuracy on random Gaussian matrices
  # ==========================================================

  cat(
    "\n============================================================\n"
  )

  cat(
    "2. RANDOM MATRIX ACCURACY EXPERIMENT\n"
  )

  cat(
    "============================================================\n"
  )

  random_raw <- run_random_accuracy_experiment(
    matrix_sizes = MATRIX_SIZES,
    trials_per_size = RANDOM_TRIALS_PER_SIZE
  )

  write.csv(
    random_raw,
    file.path(
      "Results",
      "random_accuracy_raw.csv"
    ),
    row.names = FALSE
  )


  random_summary <- summarize_experiment(
    data = random_raw,
    group_columns = c(
      "rows",
      "cols",
      "method"
    ),
    metric_columns = c(
      "e_orth",
      "e_rec"
    ),
    bootstrap_reps = BOOTSTRAP_REPS
  )

  random_summary <- random_summary[
    order(
      random_summary$rows,
      random_summary$method
    ),
  ]

  write.csv(
    random_summary,
    file.path(
      "Results",
      "random_accuracy_summary.csv"
    ),
    row.names = FALSE
  )


  # ==========================================================
  # 3. Controlled-condition stability experiment
  # ==========================================================

  cat(
    "\n============================================================\n"
  )

  cat(
    "3. CONTROLLED-CONDITION STABILITY EXPERIMENT\n"
  )

  cat(
    "============================================================\n"
  )

  condition_raw <- run_condition_experiment(
    m = CONDITION_ROWS,
    n = CONDITION_COLS,
    condition_levels = CONDITION_LEVELS,
    trials_per_level = CONDITION_TRIALS_PER_LEVEL
  )

  write.csv(
    condition_raw,
    file.path(
      "Results",
      "condition_stability_raw.csv"
    ),
    row.names = FALSE
  )


  condition_summary <- summarize_experiment(
    data = condition_raw,
    group_columns = c(
      "target_kappa",
      "method"
    ),
    metric_columns = c(
      "actual_kappa",
      "e_orth",
      "e_rec"
    ),
    bootstrap_reps = BOOTSTRAP_REPS
  )

  condition_summary <- condition_summary[
    order(
      condition_summary$target_kappa,
      condition_summary$method
    ),
  ]

  write.csv(
    condition_summary,
    file.path(
      "Results",
      "condition_stability_summary.csv"
    ),
    row.names = FALSE
  )


  # ==========================================================
  # 4. Runtime scaling experiment
  # ==========================================================

  cat(
    "\n============================================================\n"
  )

  cat(
    "4. RUNTIME SCALING EXPERIMENT\n"
  )

  cat(
    "============================================================\n"
  )

  runtime_raw <- run_runtime_experiment(
    matrix_sizes = MATRIX_SIZES,
    input_trials = RUNTIME_INPUT_TRIALS,
    timing_batches = TIMING_BATCHES
  )

  write.csv(
    runtime_raw,
    file.path(
      "Results",
      "runtime_raw.csv"
    ),
    row.names = FALSE
  )


  runtime_summary <- summarize_experiment(
    data = runtime_raw,
    group_columns = c(
      "rows",
      "cols",
      "method"
    ),
    metric_columns = c(
      "runtime"
    ),
    bootstrap_reps = BOOTSTRAP_REPS
  )

  runtime_summary <- runtime_summary[
    order(
      runtime_summary$rows,
      runtime_summary$method
    ),
  ]

  write.csv(
    runtime_summary,
    file.path(
      "Results",
      "runtime_summary.csv"
    ),
    row.names = FALSE
  )


  # ==========================================================
  # 5. Generate figures
  # ==========================================================

  cat(
    "\n============================================================\n"
  )

  cat(
    "5. GENERATING FIGURES\n"
  )

  cat(
    "============================================================\n"
  )


  plot_summary_by_size(
    random_summary,
    metric = "e_orth",
    output_file = file.path(
      "Plots",
      "random_orthogonality_summary.png"
    ),
    plot_title =
      "Orthogonality Error on Random Gaussian Matrices",
    y_label =
      "Orthogonality error"
  )


  plot_summary_by_size(
    random_summary,
    metric = "e_rec",
    output_file = file.path(
      "Plots",
      "random_reconstruction_summary.png"
    ),
    plot_title =
      "Reconstruction Error on Random Gaussian Matrices",
    y_label =
      "Relative reconstruction error"
  )


  plot_condition_orthogonality(
    condition_summary,
    output_file = file.path(
      "Plots",
      "condition_orthogonality_summary.png"
    )
  )


  plot_runtime_summary(
    runtime_summary,
    output_file = file.path(
      "Plots",
      "runtime_summary.png"
    )
  )


  # ==========================================================
  # Final summary
  # ==========================================================

  study_end_time <- Sys.time()

  cat(
    "\n============================================================\n"
  )

  cat(
    "NUMERICAL STUDY COMPLETED SUCCESSFULLY\n"
  )

  cat(
    "============================================================\n"
  )

  cat(
    "\nRandom matrices per size:",
    RANDOM_TRIALS_PER_SIZE,
    "\n"
  )

  cat(
    "Condition-number matrices per level:",
    CONDITION_TRIALS_PER_LEVEL,
    "\n"
  )

  cat(
    "Runtime input matrices per size:",
    RUNTIME_INPUT_TRIALS,
    "\n"
  )

  cat(
    "Timing batches per runtime measurement:",
    TIMING_BATCHES,
    "\n"
  )

  cat(
    "Bootstrap repetitions:",
    BOOTSTRAP_REPS,
    "\n"
  )

  cat(
    "\nStudy end time:",
    format(
      study_end_time,
      "%Y-%m-%d %H:%M:%S"
    ),
    "\n"
  )

  cat(
    "Total elapsed study time (minutes):",
    round(
      as.numeric(
        difftime(
          study_end_time,
          study_start_time,
          units = "mins"
        )
      ),
      2
    ),
    "\n"
  )

  cat(
    "\nResults and figures were saved successfully.\n"
  )
}


# ============================================================
# Run project and save console log
# ============================================================

log_connection <- file(
  file.path(
    "Logs",
    "experiment_log.txt"
  ),
  open = "wt"
)

sink(
  log_connection,
  split = TRUE
)

tryCatch(

  {

    run_project()

  },

  finally = {

    sink()

    close(
      log_connection
    )
  }
)


# ============================================================
# Save software environment
# ============================================================

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    "Logs",
    "session_info.txt"
  )
)
