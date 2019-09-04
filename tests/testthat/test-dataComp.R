context("Data attribute comparisons")
library(SynthTools)
library(magrittr)

test_that("The dimensions of two different datasets are compared",{

  ps1_test <- dataComp(PPA, PPAps1)
  expect_equal(ps1_test$same.dim, TRUE)

  PPA_diff_dim <- PPA[,2:length(PPA)]
  diff_dim <- dataComp(PPA, PPA_diff_dim)
  expect_equal(diff_dim$same.dim, FALSE)
})

test_that("The order of variables of two different datasets are compared",{

  ps1_test <- dataComp(PPA, PPAps1)
  expect_equal(ps1_test$same.order, TRUE)

  PPA_diff_order <- PPA[,c(length(PPA), 1:(length(PPA)-1))]
  diff_order <- dataComp(PPA, PPA_diff_order)
  expect_equal(diff_order$same.order, FALSE)
})

test_that("The classification of variables in two datasets with the same name are identical", {

  ps2_test <- dataComp(PPA, PPAps2)
  expect_equal(ps2_test$class.identical, TRUE)

  PPA_diff_class <- PPA
  PPA_diff_class$age <- as.factor(PPA_diff_class$age)
  diff_class <- dataComp(PPA, PPA_diff_class)
  expect_equal(diff_class$class.identical, FALSE)
})

test_that("The table of classifications is output for datasets with the same variable classification", {

  classes <- rep(NA, length(PPAps3))
  for(i in 1:length(PPAps3)){classes[i] <- class(PPAps3[[i]])}
  class_table <- table(classes)
  ct <- dataComp(PPA, PPAps3)
  expect_equal(ct$class.table %>% names, class_table %>% names)
  expect_equal(ct$class.table %>% as.vector, class_table %>% as.vector)
})

test_that("The number of factor levels of two different datasets are compared", {
  ps4_test <- dataComp(PPA, PPAps4)
  expect_equal(ps4_test$fac.num.same, TRUE)

  PPA_diff_sex <- PPA
  SEX <- sample(c(1,2,3), 1000, replace=T) %>% as.factor
  PPA_diff_sex$sex <- SEX
  diff_fac_levs <- dataComp(PPA, PPA_diff_sex)
  expect_equal(diff_fac_levs$fac.num.same, FALSE)
})

test_that("The levels of the same variable from two different datasets are compared", {
  ps5_test <- dataComp(PPA, PPAps5)
  expect_equal(ps5_test$fac.lev.same, TRUE)

  PPA_diff_sex <- PPA
  SEX <- sample(c(1,3), 1000, replace=T) %>% as.factor
  PPA_diff_sex$sex <- SEX
  diff_fac_levs <- dataComp(PPA, PPA_diff_sex)
  expect_equal(diff_fac_levs$fac.lev.same, FALSE)
})
