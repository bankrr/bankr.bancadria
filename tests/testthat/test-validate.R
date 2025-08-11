test_that("validate() works", {
  dat <- read.csv2(
    pkg_file("testdata", "lista_mov.csv")
  )
  expect_no_error(validate(dat))
})
