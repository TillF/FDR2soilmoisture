test_that("compare_eps2theta_equations handles missing nlmrt gracefully", {
  set.seed(1)
  common_set <- data.frame(
    epsilon = runif(30, min = 3, max = 40),
    clay_perc = runif(30, min = 5, max = 35)
  )
  common_set$theta <- eps2theta(common_set, equation = "SinghEtal2019")
  common_set$theta <- pmin(pmax(common_set$theta, 0.01), 0.99)

  run_compare <- function() {
    withCallingHandlers(
      compare_eps2theta_equations(common_set, eq_subset = "SinghEtal2019", plot_extern = FALSE),
      warning = function(w) {
        if (grepl("row names were found from a short variable", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  if (requireNamespace("nlmrt", quietly = TRUE)) {
    expect_no_warning(result <- run_compare())
  } else {
    expect_warning(result <- run_compare(), "nlmrt")
  }

  expect_true(is.list(result))
  expect_true("r2_train" %in% names(result))
  expect_true(any(names(result$r2_train) == "theta_SinghEtal2019"))
})

test_that("eps2theta requires exactly one equation", {
  expect_error(
    eps2theta(data.frame(epsilon = 10), equation = c("ToppEtal1980", "SchaapEtal1997")),
    "single character string"
  )
})

test_that("eps_water reports unknown equation names clearly", {
  expect_error(
    eps_water(20, equ = "foo"),
    "Unknown equation 'foo'"
  )
})
