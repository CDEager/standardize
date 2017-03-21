context("scaled_contr_poly")

uf <- factor(rep(c("b", "c", "a", NA), 2), levels = c("b", "c", "a"))
uf <- addNA(uf)
contr_uf_scp <- scale(stats::contr.poly(3))[, 1:2]
dimnames(contr_uf_scp) <- list(c("b", "c", "a"), c(".L", ".Q"))
uf_scp <- factor(uf, ordered = TRUE, levels = c("b", "c", "a"))
contrasts(uf_scp) <- contr_uf_scp

of <- factor(rep(c("b", "c", "a", NA), 2), levels = c("b", "c", "a"),
  ordered = TRUE)
of_scp <- uf_scp
contr_of_scp <- contr_uf_scp

nf <- c(0, 2, 1)
nf_scp <- factor(nf, ordered = TRUE, levels = c("0", "2", "1"))
contr_nf_scp <- contr_uf_scp
dimnames(contr_nf_scp) <- list(c("0", "2", "1"), c(".L", ".Q"))
contrasts(nf_scp) <- contr_nf_scp

cf <- rep(c("b", "c", "a", NA), 2)
contr_cf_scp <- contr_uf_scp
cf_scp <- factor(cf, ordered = TRUE, levels = c("b", "c", "a"))
contrasts(cf_scp) <- contr_cf_scp

sf <- 4
contr_sf_scp <- scale(stats::contr.poly(4))[, 1:3]
sf_scp <- factor(sf, ordered = TRUE, levels = paste(1:4))
contrasts(sf_scp) <- contr_sf_scp


test_that("contrast method works", {
  expect_equal(scaled_contr_poly(uf), contr_uf_scp)
  expect_equal(scaled_contr_poly(of), contr_of_scp)
  expect_equal(scaled_contr_poly(nf), contr_nf_scp)
  expect_equal(scaled_contr_poly(cf), contr_cf_scp)
  expect_equal(scaled_contr_poly(sf), contr_sf_scp)
})

test_that("factor method and fac_and_contr work", {
  expect_equal(scaled_contr_poly(uf, return_contr = FALSE), uf_scp)
  expect_equal(scaled_contr_poly(of, return_contr = FALSE), of_scp)
  expect_equal(scaled_contr_poly(nf, return_contr = FALSE), nf_scp)
  expect_equal(scaled_contr_poly(cf, return_contr = FALSE), cf_scp)
  expect_equal(scaled_contr_poly(sf, return_contr = FALSE), sf_scp)
})

test_that("scale works", {
  expect_equal(scaled_contr_poly(uf, 0.5), 0.5 * contr_uf_scp)
})

test_that("errors thrown correctly", {
  expect_error(scaled_contr_poly(uf, 0))
  expect_error(scaled_contr_poly(uf, c(1, 2)))
  expect_error(scaled_contr_poly(2))
  expect_error(scaled_contr_poly(rep(c("a", NA), 3)))
  expect_error(scaled_contr_poly(factor(rep(c("a", NA), 3))))
  expect_error(scaled_contr_poly(data.frame(factor(rep(c("a", "b"), 3)))))
})

rm(list = ls())
