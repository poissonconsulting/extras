dev_res <- function(x, mu, dev) {
  sign(x - mu) * sqrt(dev)
}

dev_res_extended <- function(x, mu, dev) {
  mat <- is.matrix(x) | is.matrix(mu)
  any_na <- any(is.na(x)) | any(is.na(mu))
  sign <- sign(x - mu)
  if (mat & !any_na) {
    mean_sign <- rowMeans(sign)
  } else {
    mean_sign <- mean(sign)
  }
  sign <- sign(mean_sign)
  sign * sqrt(dev)
}
