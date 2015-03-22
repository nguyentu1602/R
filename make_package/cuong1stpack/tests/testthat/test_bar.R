#' test bar()
library(cuong1stpack)

context('Sum correctness')

test_that("bar adds two numbers", {
  expect_equal(bar(1, 3), 4)
  expect_equal(bar(2, 1.5), 3.5)
  expect_equal(bar(-1, 1), 0)
})