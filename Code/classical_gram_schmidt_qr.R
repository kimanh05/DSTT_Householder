classical_gram_schmidt_qr <- function(A, tol = NULL) {

  A <- as.matrix(A)
  storage.mode(A) <- "double"

  m <- nrow(A)
  n <- ncol(A)

  if (m < n) {
    stop("Classical Gram-Schmidt requires m >= n for this project.")
  }

  if (is.null(tol)) {
    tol <- .Machine$double.eps *
      max(m, n) *
      max(1, norm(A, type = "2"))
  }

  Q <- matrix(0, nrow = m, ncol = n)
  R <- matrix(0, nrow = n, ncol = n)

  for (j in seq_len(n)) {

    v <- A[, j]

    if (j > 1) {

      for (i in seq_len(j - 1)) {

        # Compute the projection coefficient
        R[i, j] <- sum(Q[, i] * A[, j])

        # Remove the component along q_i
        v <- v - R[i, j] * Q[, i]
      }
    }

    R[j, j] <- sqrt(sum(v^2))

    if (R[j, j] <= tol) {
      stop(
        "The matrix is rank-deficient or numerically too close to rank-deficient."
      )
    }

    Q[, j] <- v / R[j, j]
  }

  return(
    list(
      Q = Q,
      R = R
    )
  )
}