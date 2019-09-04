context("One categorical variable confidence interval")
library(SynthTools)
library(magrittr)

test_that("Function outputs a data set with desired information",{
  occi <- oneCatCI(obs_data=PPA, imp_data_list=PPAm5, type="synthetic", var="sex", sig=3)
  expect_equal(names(occi), c("Response", "Obs", "Lower", "Upper", "SE", "In.CI."))
  expect_equal(nrow(occi), length(levels(PPA$sex)))
})

test_that("Outputs correct number of significant digits", {
  siglen <- 3
  occi <- oneCatCI(obs_data=PPA, imp_data_list=PPAm5, type="synthetic", var="sex", sig=siglen)
  expect_equal(nchar(occi[1,2]), siglen+2)
  expect_equal(nchar(occi[1,3]), siglen+2)
  expect_equal(nchar(occi[1,4]), siglen+2)
  expect_equal(nchar(occi[1,5]), siglen+2)
})

test_that("Specified alpha is used", {
  expect_equal(
    substr(
      capture.output(oneCatCI(obs_data=PPA, imp_data_list=PPAm5, type="synthetic", var="sex", alpha=0.10)),
      1, 3)[1],
      "90%")
})
