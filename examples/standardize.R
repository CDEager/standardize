dat <- expand.grid(ufac = letters[1:3], ofac = 1:3)
dat <- as.data.frame(lapply(dat, function(n) rep(n, 60)))
dat$ofac <- factor(dat$ofac, ordered = TRUE)
dat$x <- rpois(nrow(dat), 5)
dat$z <- rnorm(nrow(dat), rep(rnorm(30), each = 18), rep(runif(30), each = 18))
dat$subj <- rep(1:30, each = 18)
dat$y <- rnorm(nrow(dat), -2, 5)

sobj <- standardize(y ~ log(x + 1) + scale_by(z ~ subj) + ufac + ofac +
  (1 | subj), dat)

sobj
sobj$formula
head(dat)
head(sobj$data)
sobj$contrasts
sobj$groups
mean(sobj$data$y)
sd(sobj$data$y)
mean(sobj$data$log_x.p.1)
sd(sobj$data$log_x.p.1)
with(sobj$data, tapply(z_scaled_by_subj, subj, mean))
with(sobj$data, tapply(z_scaled_by_subj, subj, sd))

sobj <- standardize(y ~ log(x + 1) + scale_by(z ~ subj) + ufac + ofac +
  (1 | subj), dat, scale = 0.5)

sobj
sobj$formula
head(dat)
head(sobj$data)
sobj$contrasts
sobj$groups
mean(sobj$data$y)
sd(sobj$data$y)
mean(sobj$data$log_x.p.1)
sd(sobj$data$log_x.p.1)
with(sobj$data, tapply(z_scaled_by_subj, subj, mean))
with(sobj$data, tapply(z_scaled_by_subj, subj, sd))

\dontrun{
mod <- lmer(sobj$formula, sobj$data)
# this next line causes warnings about contrasts being dropped, but
# these warnings can be ignored (i.e. the statement still evaluates to TRUE)
all.equal(predict(mod, newdata = predict(sobj, dat)), fitted(mod))
}
