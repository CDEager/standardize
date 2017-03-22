context("standardize_terms")

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

sf <- standardize_terms(formula = formula, data = d)
lmod <- lme4::lFormula(formula = sf, data = d)
mf <- lmod$fr
mt <- terms(mf)

d2 <- d
d2[["y"]] <- scale(d$y)
d2[["f1"]] <- named_contr_sum(d$f1, return_contr = FALSE)
d2[["f2"]] <- named_contr_sum(d$f2, return_contr = FALSE)
d2[["f3"]] <- named_contr_sum(d$f3, return_contr = FALSE)
d2[["o1"]] <- scaled_contr_poly(d$o1, return_contr = FALSE)
d2[["poly(x2, 2)"]] <- scale(poly(d$x2, 2))
d2[["log(I(x3^2))"]] <- scale(log(d$x3^2))
d2[["scale_by(x1 ~ f2)"]] <- scale_by(x1 ~ f2, d)
d2[["g1"]] <- factor(d$g1, ordered = FALSE)
d2 <- d2[, colnames(mf)]
mf2 <- mf
attributes(mf2) <- attributes(mf2)[c("names", "class", "row.names")]

# from lme4:::terms.merMod
fe <- re <- attr(mf, "formula")
fe[[length(fe)]] <- lme4::nobars(fe[[length(fe)]])
fe <- terms.formula(fe)

re <- reformulate(paste0("(", vapply(lme4::findbars(re),
  function(x) paste(deparse(x, 500L), collapse = " "), ""), ")"),
  response = re[[2]])
re <- terms.formula(lme4::subbars(re))

## these would result in errors
# mf.fe <- model.frame(formula = fe, data = mf)
# mf.re <- model.frame(formula = re, data = mf)

attributes(re) <- attributes(fe) <- attributes(mt)
fe <- standardize:::condense_terms(fe)
re <- standardize:::condense_terms(re)

test_that("works with lFormula", {
  expect_equal(d2, mf2)
  expect_equal(attr(sf, "predvars"), attr(mt, "predvars"))
  expect_equal(attr(fe, "predvars"), attr(mt, "predvars.fixed"))
  expect_equal(attr(re, "predvars"), attr(mt, "predvars.random"))
  expect_equal(lmod$reTrms$cnms, list(g1 = c("(Intercept)", "f1a", "f1b",
    "scale_by(x1 ~ f2)")))
  expect_equal(colnames(lmod$X), c("(Intercept)", "f1a", "f1b", "f2TRUE",
    "f35", "o1.L", "o1.Q", "poly(x2, 2)1", "poly(x2, 2)2",
    "log(I(x3^2))", "f2TRUE:f35"))
})


xp <- lmod$X
zp <- Matrix::t(lmod$reTrms$Zt)

b <- stats::rnorm(ncol(xp))
o <- matrix(c(1, -0.79693, -0.39091, 0.49957, -0.79693, 1, 0.02749,
  -0.83037, -0.39091, 0.02749, 1, -0.11733, 0.49957, -0.83037, -0.11733, 1),
  4, 4)
s <- c(1.2, .7, .5, .5)
g <- t(chol(o)) %*% matrix(stats::rnorm(200), 4, 50)
for (i in 1:4) g[i, ] <- g[i, ] * s[i]
g <- as.vector(g)

yraw <- as.vector(xp %*% b) + as.vector(zp %*% g) + stats::rnorm(nrow(d), 0, .25)
d$y <- scale(yraw)[, 1] * 35 + 150

sf <- standardize_terms(formula, d)
mod <- lme4::lmer(formula = sf, data = d)

test_that("works with lmer", {
  expect_warning(expect_equal(fitted(mod), predict(mod, newdata = d)))
})


test_that("standardized_scale works", {
  expect_equal(standardized_scale(sf), 1)
  expect_equal(standardized_scale(mod), 1)
})


x <- model.matrix(lme4::nobars(sf), d)
x2 <- lmod$X
attributes(x) <- attributes(x2)
test_that("model.matrix works", {
  expect_equal(x, x2)
})


sf <- standardize_terms(formula, d, family = binomial)
mf <- model.frame(sf, d)
test_that("non-gaussian works", {
  expect_equal(mf$y, d$y)
})

