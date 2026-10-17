# ============================================================
# MATRIX GENERATORS FOR NUMERICAL EXPERIMENTS
# ============================================================


condition_number_2 <- function(A) {

  singular_values <- svd(
    A,
    nu = 0,
    nv = 0
  )$d

  return(
    max(singular_values) /
    min(singular_values)
  )
}


generate_gaussian_matrix <- function(m, n) {

  return(
    matrix(
      rnorm(m * n),
      nrow = m,
      ncol = n
    )
  )
}


generate_controlled_condition_matrix <- function(
  m,
  n,
  target_kappa
) {

  if (m < n) {
    stop("The matrix must satisfy m >= n.")
  }

  if (target_kappa < 1) {
    stop("The target condition number must be at least 1.")
  }

  # Generate a random orthonormal basis for the column space
  random_left <- matrix(
    rnorm(m * n),
    nrow = m,
    ncol = n
  )

  U <- svd(
    random_left,
    nu = n,
    nv = 0
  )$u

  # Generate a random orthogonal right factor
  random_right <- matrix(
    rnorm(n * n),
    nrow = n,
    ncol = n
  )

  V <- svd(
    random_right,
    nu = n,
    nv = 0
  )$u

  # Construct singular values on a logarithmic scale
  if (target_kappa == 1) {

    singular_values <- rep(
      1,
      n
    )

  } else {

    singular_values <- exp(
      seq(
        from = 0,
        to = -log(target_kappa),
        length.out = n
      )
    )
  }

  Sigma <- diag(
    singular_values,
    nrow = n,
    ncol = n
  )

  A <- U %*% Sigma %*% t(V)

  actual_kappa <- condition_number_2(A)

  return(
    list(
      A = A,
      target_kappa = target_kappa,
      actual_kappa = actual_kappa
    )
  )
}