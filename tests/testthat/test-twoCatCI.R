context("Two categorical variable confidence interval")
library(SynthTools)
library(magrittr)

test_that("Function outputs data sets with desired information",{
  occi <- twoCatCI(obs_data=PPA, imp_data_list=PPAm5, type="synthetic", vars=c("sex", "race"), sig=3)
  expect_equal(names(occi), c("Observed", "Lower", "Upper", "SEs", "CI_Indicator"))
  expect_equal(dim(occi$Observed)[1], length(levels(PPA$sex)))
  expect_equal(dim(occi$Observed)[2], length(levels(PPA$race)))
})

test_that("Outputs correct number of significant digits", {
  siglen <- 3
  occi <- twoCatCI(obs_data=PPA, imp_data_list=PPAm5, type="synthetic", vars=c("sex", "race"), sig=siglen)
  expect_equal(nchar(occi$Lower[1,2]), siglen+2)
  expect_equal(nchar(occi$Upper[1,3]), siglen+2)
  expect_equal(nchar(occi$SEs[1,4]), siglen+2)
  expect_equal(nchar(occi$Observed[1,2]), siglen+2)
})

