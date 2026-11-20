test_that("V2eps requires temp for picoSMS variants", {
  expect_error(
    V2eps(0.5, type = "picoSMS_linear"),
    "'temp' must be provided"
  )

  expect_error(
    V2eps(0.5, type = "picoSMS_poly"),
    "'temp' must be provided"
  )
})

test_that("V2eps rejects unknown probe type", {
  expect_error(
    V2eps(0.5, type = "unknown"),
    "type must be"
  )
})
