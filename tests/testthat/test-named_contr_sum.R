context("named_contr_sum")

uf <- factor(rep(c("b", "c", "a", NA), 2), levels = c("b", "c", "a"))
uf <- addNA(uf)
contr_uf_ncs <- stats::contr.sum(3)
dimnames(contr_uf_ncs) <- list(c("a", "b", "c"), c("a", "b"))
uf_ncs <- factor(uf, ordered = FALSE, levels = c("a", "b", "c"))
contrasts(uf_ncs) <- contr_uf_ncs

of <- factor(rep(c("b", "c", "a", NA), 2), levels = c("b", "c", "a"),
  ordered = TRUE)
of_ncs <- uf_ncs
contr_of_ncs <- contr_uf_ncs

lf <- rep(c(NA, FALSE, TRUE), 3)
lf_ncs <- factor(lf, ordered = FALSE, levels = c("TRUE", "FALSE"))
contr_lf_ncs <- stats::contr.sum(2)
dimnames(contr_lf_ncs) <- list(c("TRUE", "FALSE"), "TRUE")
contrasts(lf_ncs) <- contr_lf_ncs

nlf <- c(0, 1)
nlf_ncs <- factor(nlf, ordered = FALSE, levels = c("1", "0"))
contr_nlf_ncs <- contr_lf_ncs
dimnames(contr_nlf_ncs) <- list(c("1", "0"), "1")
contrasts(nlf_ncs) <- contr_nlf_ncs

nf <- c(0, 2, 1)
nf_ncs <- factor(nf, ordered = FALSE, levels = c("0", "1", "2"))
contr_nf_ncs <- contr_uf_ncs
dimnames(contr_nf_ncs) <- list(c("0", "1", "2"), c("0", "1"))
contrasts(nf_ncs) <- contr_nf_ncs

cf <- rep(c("b", "c", "a", NA), 2)
contr_cf_ncs <- contr_uf_ncs
cf_ncs <- factor(cf)
contrasts(cf_ncs) <- contr_cf_ncs

test_that("contrast method works", {
  expect_equal(named_contr_sum(uf), contr_uf_ncs)
  expect_equal(named_contr_sum(of), contr_of_ncs)
  expect_equal(named_contr_sum(lf), contr_lf_ncs)
  expect_equal(named_contr_sum(nlf), contr_nlf_ncs)
  expect_equal(named_contr_sum(nf), contr_nf_ncs)
  expect_equal(named_contr_sum(cf), contr_cf_ncs)
})

test_that("factor method and fac_and_contr work", {
  expect_equal(named_contr_sum(uf, return_contr = FALSE), uf_ncs)
  expect_equal(named_contr_sum(of, return_contr = FALSE), of_ncs)
  expect_equal(named_contr_sum(lf, return_contr = FALSE), lf_ncs)
  expect_equal(named_contr_sum(nlf, return_contr = FALSE), nlf_ncs)
  expect_equal(named_contr_sum(nf, return_contr = FALSE), nf_ncs)
  expect_equal(named_contr_sum(cf, return_contr = FALSE), cf_ncs)
})

test_that("scale works", {
  expect_equal(named_contr_sum(uf, 0.5), 0.5 * contr_uf_ncs)
})

test_that("errors thrown correctly", {
  expect_error(named_contr_sum(uf, 0))
  expect_error(named_contr_sum(uf, c(1, 2)))
  expect_error(named_contr_sum(1))
  expect_error(named_contr_sum(rep(c("a", NA), 3)))
  expect_error(named_contr_sum(factor(rep(c("a", NA), 3))))
  expect_error(named_contr_sum(data.frame(factor(rep(c("a", "b"), 3)))))
})

rm(list = ls())
