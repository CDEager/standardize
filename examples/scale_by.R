dat <- data.frame(
  f1 = rep(c("a", "b", "c"), c(5, 10, 20)),
  x1 = rnorm(35, rep(c(1, 2, 3), c(5, 10, 20)),
    rep(c(.5, 1.5, 3), c(5, 10, 20))))

dat$x1_scaled <- scale(dat$x1)
dat$x1_scaled_by_f1 <- scale_by(x1 ~ f1, dat)

mean(dat$x1)
sd(dat$x1)
with(dat, tapply(x1, f1, mean))
with(dat, tapply(x1, f1, sd))

mean(dat$x1_scaled)
sd(dat$x1_scaled)
with(dat, tapply(x1_scaled, f1, mean))
with(dat, tapply(x1_scaled, f1, sd))

mean(dat$x1_scaled_by_f1)
sd(dat$x1_scaled_by_f1)
with(dat, tapply(x1_scaled_by_f1, f1, mean))
with(dat, tapply(x1_scaled_by_f1, f1, sd))

newdata <- data.frame(
  f1 = c("a", "b", "c", "d"),
  x1 = rep(1, 4))

newdata$x1_pred_scaledby <- scale_by(dat$x1_scaled_by_f1, newdata)

newdata
