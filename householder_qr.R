householder_qr <- function(A, tol = NULL) {

  A <- as.matrix(A)
  storage.mode(A) <- "double"

  m <- nrow(A)
  n <- ncol(A)

  if (m < n) {
    stop("Householder QR requires m >= n for this project.")
  }

  if (is.null(tol)) {
    tol <- .Machine$double.eps *
      max(m, n) *
      max(1, norm(A, type = "2"))
  }

  R <- A
  Q <- diag(m)

  number_of_steps <- min(m - 1, n)

  if (number_of_steps >= 1) {

    for (k in seq_len(number_of_steps)) {

      # Extract the active part of column k
      x <- R[k:m, k]

      x_norm <- sqrt(sum(x^2))

      # Skip the step if the active vector is numerically zero
      if (x_norm <= tol) {
        next
      }

      # Choose the sign to reduce numerical cancellation
      sign_x <- if (x[1] >= 0) 1 else -1
      alpha <- -sign_x * x_norm

      # Construct the Householder vector
      v <- x
      v[1] <- v[1] - alpha

      v_norm <- sqrt(sum(v^2))

      if (v_norm <= tol) {
        next
      }

      # Normalize the Householder vector
      v <- v / v_norm

      # Apply the reflector directly to the active block of R
      active_R <- R[k:m, k:n, drop = FALSE]

      R[k:m, k:n] <-
        active_R -
        2 * v %*% (t(v) %*% active_R)

      # Accumulate the orthogonal matrix Q
      active_Q <- Q[, k:m, drop = FALSE]

      Q[, k:m] <-
        active_Q -
        2 * (active_Q %*% v) %*% t(v)
    }
  }

  return(
    list(
      Q = Q,
      R = R
    )
  )
}