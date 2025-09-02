test_that("validate() works", {
  dat <- read.csv2(
    pkg_file("testdata", "transactions.csv")
  )
  expect_no_error(validate(dat))
})
