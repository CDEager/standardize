context("scale_by")

set.seed(1)

d <- data.frame(
  f1 = rep(c("a", "b", "c"), c(5, 10, 20)),
  x1 = stats::rnorm(35, rep(c(1, 2, 3), c(5, 10, 20)),
    rep(c(.5, 1.5, 3), c(5, 10, 20))))

formula <- x1 ~ f1
centers <- matrix(with(d, tapply(x1, f1, mean)))
scales <- matrix(with(d, tapply(x1, f1, sd)))
new_center <- mean(centers)
new_scale <- mean(scales)
rownames(centers) <- rownames(scales) <- letters[1:3]
pred <- list(formula = formula, centers = centers, scales = scales,
  new_center = new_center, new_scale = new_scale)
class(pred) <- c("scaledby_pred", "list")
sb <- d$x1
sb[d$f1 == "a"] <- scale(d$x1[d$f1 == "a"])[, 1]
sb[d$f1 == "b"] <- scale(d$x1[d$f1 == "b"])[, 1]
sb[d$f1 == "c"] <- scale(d$x1[d$f1 == "c"])[, 1]
attr(sb, "scaledby") <- pred
class(sb) <- c("scaledby", "numeric")

newdata <- data.frame(
  f1 = c("a", "b", "c", "d"),
  x1 = rep(1, 4))
sbpred <- as.vector((1 - centers) / scales)
sbpred[4] <- (1 - new_center) / new_scale
pred$centers <- rbind(pred$centers, new_center)
pred$scales <- rbind(pred$scales, new_scale)
rownames(pred$centers) <- rownames(pred$scales) <- letters[1:4]
attr(sbpred, "scaledby") <- pred
class(sbpred) <- class(sb)
names(sbpred) <- NULL

test_that("basic method works", {
  expect_equal(scale_by(x1 ~ f1, d), sb)
  expect_equal(scale_by(data = newdata, pred = sb), sbpred)
})


d$x2 <- poly(d$x1, 2)
formula <- x2 ~ f1
centers <- scales <- matrix(nrow = 3, ncol = 2)
rownames(centers) <- rownames(scales) <- letters[1:3]
sb <- d$x2
for (j in letters[1:3]) {
  centers[j, ] <- colMeans(d$x2[d$f1 == j, ])
  scales[j, ] <- apply(d$x2[d$f1 == j, ], 2, sd)
  sb[d$f1 == j, ] <- scale(d$x2[d$f1 == j, ])[, 1:2]
}
scales[, 2] <- scales[, 2] * 2
sb[, 2] <- sb[, 2] / 2
new_center <- colMeans(centers)
new_scale <- colMeans(scales)
pred <- list(formula = formula, centers = centers, scales = scales,
  new_center = new_center, new_scale = new_scale)
class(pred) <- c("scaledby_pred", "list")
attributes(sb) <- attributes(d$x2)
attr(sb, "scaledby") <- pred
class(sb) <- c("scaledby", "poly", "matrix")

newdata$x2 <- poly(newdata$x1, degree = 2, coefs = attr(d$x2, "coefs"))
sbpred <- (newdata$x2 - rbind(centers, new_center)) / rbind(scales, new_scale)
pred$centers <- rbind(pred$centers, new_center)
pred$scales <- rbind(pred$scales, new_scale)
rownames(pred$centers) <- rownames(pred$scales) <- letters[1:4]
attributes(sbpred) <- attributes(newdata$x2)
attr(sbpred, "scaledby") <- pred
class(sbpred) <- class(sb)
rownames(sbpred) <- NULL

test_that("matrix and scale work", {
  expect_equal(scale_by(x2 ~ f1, d, c(1, 0.5)), sb)
  expect_equal(scale_by(data = newdata, pred = attr(sb, "scaledby")), sbpred)
})


d <- data.frame(
  f1 = rep(letters[1:3], each = 2),
  x1 = c(1, 1, 3, NA, 5, 10))
formula <- x1 ~ f1
new_center <- mean(c(5, 10))
new_scale <- sd(c(5, 10))
centers <- matrix(new_center, 3, 1)
scales <- matrix(new_scale, 3, 1)
rownames(centers) <- rownames(scales) <- letters[1:3]
pred <- list(formula = formula, centers = centers, scales = scales,
  new_center = new_center, new_scale = new_scale)
class(pred) <- c("scaledby_pred", "list")
sb <- (d$x1 - new_center) / new_scale
attr(sb, "scaledby") <- pred
class(sb) <- c("scaledby", "numeric")

test_that("min of two unique non-NA values and nval work", {
  expect_equal(scale_by(x1 ~ f1, d), sb)
})


d <- data.frame(
  f1 = rep(c("a", "b", "c"), c(5, 10, 20)),
  x1 = stats::rnorm(35, rep(c(1, 2, 3), c(5, 10, 20)),
    rep(c(.5, 1.5, 3), c(5, 10, 20))))

formula <- x1 ~ f1
centers <- matrix(with(d, tapply(x1, f1, mean)))
scales <- 0
new_center <- mean(centers)
new_scale <- 0
rownames(centers) <- letters[1:3]
pred <- list(formula = formula, centers = centers, scales = scales,
  new_center = new_center, new_scale = new_scale)
class(pred) <- c("scaledby_pred", "list")
sb <- d$x1
sb[d$f1 == "a"] <- scale(d$x1[d$f1 == "a"], scale = FALSE)[, 1]
sb[d$f1 == "b"] <- scale(d$x1[d$f1 == "b"], scale = FALSE)[, 1]
sb[d$f1 == "c"] <- scale(d$x1[d$f1 == "c"], scale = FALSE)[, 1]
attr(sb, "scaledby") <- pred
class(sb) <- c("scaledby", "numeric")

newdata <- data.frame(
  f1 = c("a", "b", "c", "d"),
  x1 = rep(1, 4))
sbpred <- as.vector(1 - centers)
sbpred[4] <- 1 - new_center
pred$centers <- rbind(pred$centers, new_center)
rownames(pred$centers) <- letters[1:4]
attr(sbpred, "scaledby") <- pred
class(sbpred) <- class(sb)
names(sbpred) <- NULL

test_that("centering only works", {
  expect_equal(scale_by(x1 ~ f1, d, 0), sb)
  expect_equal(scale_by(data = newdata, pred = sb), sbpred)
})


d <- expand.grid(f1 = c("a", "b"), f2 = c("d", "e"))
d$f1f2 <- interaction(d[, 1:2])
d <- rbind(d, d, d)
d$x1 <- stats::rnorm(nrow(d))
sbi1 <- sbi2 <- sbi3 <- scale_by(x1 ~ f1f2, d)
attr(sbi1, "scaledby")$formula <- x1 ~ f1 + f2
attr(sbi2, "scaledby")$formula <- x1 ~ f1 * f2
attr(sbi3, "scaledby")$formula <- x1 ~ f1 : f2

test_that("muliple factors work", {
  expect_equal(scale_by(x1 ~ f1 + f2, d), sbi1)
  expect_equal(scale_by(x1 ~ f1 * f2, d), sbi2)
  expect_equal(scale_by(x1 ~ f1 : f2, d), sbi3)
})


d$x2 <- log(abs(d$x1))
sb <- scale_by(x2 ~ f1f2, d)
attr(sb, "scaledby")$formula <- log(abs(x1)) ~ f1f2

test_that("transformed response works", {
  expect_equal(scale_by(log(abs(x1)) ~ f1f2, d), sb)
})


d2 <- data.frame(
  f1 = rep(letters[1:3], each = 2),
  x1 = c(1, 1, 3, NA, NA, 10))

test_that("errors are thrown correctly", {
  expect_error(scale_by(x1 ~ f1f2, d, 2, list()))
  expect_error(scale_by(x1 ~ f1f2, d, c(1, 2)))
  expect_error(scale_by(f1f2 ~ x2, d))
  expect_error(scale_by(x1 ~ f1f2, d, -1))
  expect_error(scale_by(x1 ~ f1, d2))
})

rm(list = ls())
