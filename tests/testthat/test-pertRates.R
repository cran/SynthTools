context("Perturbation Rates")
library(SynthTools)
library(magrittr)

test_that("Perturbation rates are properly output into list",{

  pert_test <- pertRates(PPA, PPAps2, c("age17plus", "marriage", "vet"))
  expect_equal(pert_test$overall %>% class, "numeric")
  expect_equal(names(pert_test$variable), c("age17plus", "marriage", "vet"))
  expect_equal(length(pert_test$variable), 3)
})

test_that("Variables are put in descending order of perturbation rate if requested", {
  pert_test <- pertRates(PPA, PPAps2, c("age17plus", "marriage", "vet"), desc=TRUE)
  expect_equal(names(pert_test$variable), c("marriage", "age17plus", "vet"))
})
