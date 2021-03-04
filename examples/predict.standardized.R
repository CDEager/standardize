\dontrun{
train <- subset(mydata, train)
test <- subset(mydata, !train)
train.s <- standardize(y ~ x1 + f1 + (1 | g1), train)
mod <- lmer(train.s$formula, train.s$data)
test.s <- predict(train.s, test, response = TRUE)
preds <- predict(mod, newdata = test.s)  # can ignore warning about dropped contrasts
res <- test.s$y - preds
}
