verify_qr <- function(A, Q, R) {

  A <- as.matrix(A)
  Q <- as.matrix(Q)
  R <- as.matrix(R)

  identity_matrix <- diag(ncol(Q))

  # Frobenius norm of the orthogonality error
  orthogonality_error <- norm(
    t(Q) %*% Q - identity_matrix,
    type = "F"
  )

  A_norm <- norm(A, type = "F")

  if (A_norm == 0) {
    stop("The input matrix must not be the zero matrix.")
  }

  # Relative Frobenius norm of the reconstruction error
  reconstruction_error <- norm(
    A - Q %*% R,
    type = "F"
  ) / A_norm

  return(
    list(
      e_orth = orthogonality_error,
      e_rec = reconstruction_error
    )
  )
}


builtin_qr <- function(A) {

  A <- as.matrix(A)

  qr_object <- qr(
    A,
    tol = 1e-15,
    LAPACK = FALSE
  )

  Q <- qr.Q(
    qr_object,
    complete = FALSE
  )

  R <- qr.R(
    qr_object,
    complete = FALSE
  )

  return(
    list(
      Q = Q,
      R = R
    )
  )
}