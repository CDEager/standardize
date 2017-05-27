context("standardize")

set.seed(1)

d <- expand.grid(
  f1 = c("a", "b", "c"),
  f2 = c(TRUE, FALSE),
  f3 = c(5, 6),
  o1 = c(1, 2, 3))
d <- as.data.frame(lapply(d, function(n) rep(n, 50)))
d$o1 <- factor(d$o1, ordered = TRUE)
d$g1 <- rep(1:50, each = nrow(d) / 50)
d$x1 <- stats::rnorm(nrow(d), 2, 3)
d$x2 <- stats::rnorm(nrow(d), -3, 5)
d$x3 <- stats::rnorm(nrow(d))
d$y <- stats::rnorm(nrow(d), 10, 2)

formula <- y ~ f1 + f2 * f3 + o1 + poly(x2, 2) + log(I(x3^2)) +
  (1 + f1 + scale_by(x1 ~ f2) | g1)

sf <- standardize(formula = formula, data = d)
lmod <- lme4::lFormula(formula = sf$formula, data = sf$data)
mf <- lmod$fr
mt <- terms(mf)

d2 <- d
d2[["y"]] <- scale(d$y)
d2[["f1"]] <- named_contr_sum(d$f1, return_contr = FALSE)
d2[["f2"]] <- named_contr_sum(d$f2, return_contr = FALSE)
d2[["f3"]] <- named_contr_sum(d$f3, return_contr = FALSE)
d2[["o1"]] <- scaled_contr_poly(d$o1, return_contr = FALSE)
d2[["poly_x2"]] <- scale(poly(d$x2, 2))
d2[["log_I_x3.pow.2"]] <- scale(log(d$x3^2))
d2[["x1_scaled_by_f2"]] <- scale_by(x1 ~ f2, d)
d2[["g1"]] <- factor(d$g1, ordered = FALSE)
d2 <- standardize:::strip_attr(d2[, colnames(mf)])
mf2 <- mf
attributes(mf2) <- attributes(mf2)[c("names", "class", "row.names")]

sf2 <- standardize(y ~ x1 + f1, d, scale = 0.5)


test_that("basic method works", {
  expect_equal(d2, mf2)
  expect_equal(is.standardized(sf), TRUE)
  expect_equal(standardize(formula, d, family = binomial)$data$y, d$y)
  expect_equal(sd(sf2$data$y), 1)
  expect_equal(sd(sf2$data$x1), 0.5)
  expect_equal(sf2$data$f1, named_contr_sum(d$f1, 0.5, FALSE))
  expect_equal(lmod$reTrms$cnms, list(g1 = c("(Intercept)", "f1a", "f1b",
    "x1_scaled_by_f2")))
  expect_equal(colnames(lmod$X), c("(Intercept)", "f1a", "f1b", "f2TRUE", "f35",
    "o1.L", "o1.Q", "poly_x21", "poly_x22", "log_I_x3.pow.2", "f2TRUE:f35"))
})


nd <- predict(sf, d)
w <- getOption("warn")
options(warn = -1)
mod <- lme4::lmer(sf$formula, sf$data)
options(warn = w)
sf$data <- standardize:::strip_attr(sf$data)
preds <- suppressWarnings(predict(mod, nd))

test_that("predict and lmer work", {
  expect_equal(predict(sf, d, response = TRUE), sf$data)
  expect_equal(predict(sf, d), sf$data[, -1])
  expect_equal(predict(sf, d, fixed = FALSE), sf$data[, c(2, 8:9)])
  expect_equal(predict(sf, d, random = FALSE), sf$data[, 2:7])
  expect_equal(preds, fitted(mod))
  expect_error(predict(sf, d, fixed = FALSE, random = FALSE))
})


set.seed(1)

d <- data.frame(y = rnorm(10), x = 1:10, o1 = rep(1, 10), o2 = rep(1:2, 5),
  f1 = rep(1:2, each = 5))
s1 <- standardize(y ~ offset(o1) + x + offset(o2^2), d, offset = d$o2)
s2 <- standardize(scale_by(y ~ f1) ~ offset(o1) + x + offset(o2^2), d, offset = d$o2)

sd1 <- sd(d$y)
sd2 <- rep(c(sd(d$y[1:5]), sd(d$y[6:10])), each = 5)

s1o1 <- d$o1 / sd1
s1o2 <- d$o2^2 / sd1
s1oo <- d$o2 / sd1

s2o1 <- d$o1 / sd2
s2o2 <- d$o2^2 / sd2
s2oo <- d$o2 / sd2

test_that("offset works", {
  expect_equal(s1$data$o1, s1o1)
  expect_equal(s1$data$o2.pow.2, s1o2)
  expect_equal(s1$data$o1, s1o1)
  expect_equal(s1$data$o2.pow.2, s1o2)
  expect_equal(s1$offset, s1oo)
  expect_equal(s2$offset, s2oo)
})


rm(list = ls())
