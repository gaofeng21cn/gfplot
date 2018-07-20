context("test-my-test.R")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("test plot KM works", {
  library(gfplot)
  library(survival)
  library(survminer)
  extrafont::loadfonts()
  data(myeloma)
  clin <- Surv(myeloma$time, myeloma$event)
  labs <- factor(myeloma$molecular_group)
  plot_KMCurve(clin, labs, palette = "jama_classic")
})


test_that("test plot ROC works", {
  library(gfplot)
  library(survminer)
  data(myeloma)
  extrafont::loadfonts()

  plot_ROC(myeloma$CCND1, myeloma$event)

  plot_ROC(myeloma[, 7:10], myeloma$event)
})
