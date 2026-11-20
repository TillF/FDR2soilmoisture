test_that("counts2eps and eps2counts are approximate inverses", {
  counts <- c(5000, 9000, 13000)
  alpha <- -0.12
  beta <- 0.30
  gamma <- 1.90

  eps <- counts2eps(counts, alpha = alpha, beta = beta, gamma = gamma)
  reconstructed_counts <- eps2counts(eps, alpha = alpha, beta = beta, gamma = gamma)

  expect_equal(reconstructed_counts, counts, tolerance = 1e-8)
})

test_that("default counts2eps calibration warns", {
  expect_warning(
    counts2eps(10000),
    "Default calibration parameters were used"
  )
})
