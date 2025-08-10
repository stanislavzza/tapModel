library(tidyverse)
library(tapModel)

test_that("exact accuracy", {
  expect_equal(sum(exact_accuracy_coefs(5,.5)), 2)
  expect_equal(sum(exact_accuracy_coefs(2,.5)), 1)

  distro <- exact_count_probabilites(2,.5,.2,.5)
  coefs  <- exact_accuracy_coefs(2,.5)
  sqrt(sum(distro*coefs))

})
