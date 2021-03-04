pkgname <- "standardize"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('standardize')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("named_contr_sum")
### * named_contr_sum

flush(stderr()); flush(stdout())

### Name: named_contr_sum
### Title: Create named sum contrasts for an unordered factor.
### Aliases: named_contr_sum

### ** Examples

f <- factor(rep(c("a", "b", "c", NA), 2), levels = c("b", "c", "a"))
f <- addNA(f)
levels(f)  # NA listed as factor level
contrasts(f)  # NA included in contrast matrix
named_contr_sum(f)  # named sum contrasts (NA dropped; levels alphabetized)
named_contr_sum(levels(f))  # same output
named_contr_sum(f, return_contr = FALSE)  # factor with named sum contrasts
named_contr_sum(f, 0.5)  # deviations of magniude 0.5

f <- c(TRUE, FALSE, FALSE, TRUE)
class(f)  # logical
named_contr_sum(f)  # TRUE gets the dummy variable
f <- named_contr_sum(f, return_contr = FALSE)
class(f)  # factor

named_contr_sum(letters[1:5])  # character argument
named_contr_sum(rep(letters[1:5], 2), return_contr = FALSE)  # creates factor

# ordered factors are converted to unordered factors, so use with caution
f <- factor(rep(1:3, 2), ordered = TRUE)
is.ordered(f)  # TRUE
f
f <- named_contr_sum(f, return_contr = FALSE)
is.ordered(f)  # FALSE
f

## Not run: 
##D # error from stats::contr.sum because only one unique non-NA value
##D named_contr_sum(5)
##D named_contr_sum(rep(c("a", NA), 3))
## End(Not run)



cleanEx()
nameEx("predict.standardized")
### * predict.standardized

flush(stderr()); flush(stdout())

### Name: predict.standardized
### Title: Place new data into an already existing standardized space.
### Aliases: predict.standardized

### ** Examples

## Not run: 
##D train <- subset(mydata, train)
##D test <- subset(mydata, !train)
##D train.s <- standardize(y ~ x1 + f1 + (1 | g1), train)
##D mod <- lmer(train.s$formula, train.s$data)
##D test.s <- predict(train.s, test, response = TRUE)
##D preds <- predict(mod, newdata = test.s)  # can ignore warning about dropped contrasts
##D res <- test.s$y - preds
## End(Not run)



cleanEx()
nameEx("scale_by")
### * scale_by

flush(stderr()); flush(stdout())

### Name: scale_by
### Title: Center and scale a continuous variable conditioning on factors.
### Aliases: scale_by

### ** Examples

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



cleanEx()
nameEx("scaled_contr_poly")
### * scaled_contr_poly

flush(stderr()); flush(stdout())

### Name: scaled_contr_poly
### Title: Create scaled orthogonal polynomial contrasts for an ordered
###   factor.
### Aliases: scaled_contr_poly

### ** Examples

f <- factor(rep(c("a", "b", "c"), 5), ordered = TRUE)
contrasts(f) <- contr.poly(3)

# difference in contrasts
contrasts(f)
scaled_contr_poly(f)
scaled_contr_poly(f, scale = 0.5)

# different options for 'x'
scaled_contr_poly(levels(f))
scaled_contr_poly(3)
scaled_contr_poly(c(2, 5, 6))

# return factor
f2 <- scaled_contr_poly(f, return_contr = FALSE)
f2



cleanEx()
nameEx("standardize")
### * standardize

flush(stderr()); flush(stdout())

### Name: standardize
### Title: Standardize a formula and data frame for regression.
### Aliases: standardize

### ** Examples

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

## Not run: 
##D mod <- lmer(sobj$formula, sobj$data)
##D # this next line causes warnings about contrasts being dropped, but
##D # these warnings can be ignored (i.e. the statement still evaluates to TRUE)
##D all.equal(predict(mod, newdata = predict(sobj, dat)), fitted(mod))
## End(Not run)



cleanEx()
nameEx("standardize.news")
### * standardize.news

flush(stderr()); flush(stdout())

### Name: standardize.news
### Title: Print the version history of the 'standardize' package.
### Aliases: standardize.news

### ** Examples

standardize.news()




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
