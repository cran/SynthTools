context("Continuous variable confidence interval")
library(SynthTools)
library(magrittr)

test_that("Function outputs data set with desired information",{
  cont <- ContCI(PPA, PPAm5, "age", sig=3)
  expect_equal(names(cont), c("Variable", "Observed.Mean", "Lower", "Upper", "SE", "In.CI."))
})

test_that("Outputs correct number of significant digits", {
  siglen <- 5
  cont <- ContCI(PPA, PPAm5, "age", sig=siglen)
  expect_equal(nchar(cont$SE), siglen+2)
})

