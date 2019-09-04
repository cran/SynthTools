context("Logical Consistency Checks")
library(SynthTools)
library(magrittr)

test_that("The logical consistency of the cross-tabulation of two variables from two different data sets is checked",{

  ps1_test <- logicCheck(PPA, PPAps1, c("race", "sex"))
  expect_equal(ps1_test$consistent, TRUE)

  PPA_diff_dim <- PPA[,2:length(PPA)]
  diff_dim <- dataComp(PPA, PPA_diff_dim)
  expect_equal(diff_dim$same.dim, FALSE)
})

test_that("The function is stopped if the variables from data sets have different factor levels", {

  PPA_diff_sex <- PPA
  SEX <- sample(c(1,2,3), 1000, replace=T) %>% as.factor
  PPA_diff_sex$sex <- SEX
  expect_error(logicCheck(PPA, PPA_diff_sex, c("race", "sex")))
})
