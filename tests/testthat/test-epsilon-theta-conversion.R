test_that("eps2theta ToppEtal1980 follows published polynomial", {
  epsdata <- data.frame(epsilon = c(1, 5, 20))

  expected <- -0.053 +
    2.92e-2 * epsdata$epsilon -
    5.5e-4 * epsdata$epsilon^2 +
    4.3e-6 * epsdata$epsilon^3

  theta <- eps2theta(epsdata, equation = "ToppEtal1980")
  expect_equal(theta, expected)
})

test_that("theta2eps inverts ToppEtal1980 approximately", {
  theta_values <- c(0.10, 0.25, 0.40)
  eps <- theta2eps(data.frame(theta = theta_values), equation = "ToppEtal1980")
  theta_roundtrip <- eps2theta(data.frame(epsilon = eps), equation = "ToppEtal1980")

  expect_equal(theta_roundtrip, theta_values, tolerance = 2e-3)
})

test_that("deprecated equation names are mapped with warning", {
  epsdata <- data.frame(epsilon = c(10, 20), n = c(0.45, 0.50))

  expected <- eps2theta(epsdata, equation = "FerschEtal2018")
  expect_warning(
    deprecated <- eps2theta(epsdata, equation = "FerschEtal2017"),
    "Deprecated equation name"
  )

  expect_equal(deprecated, expected)
})
