## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(standardize)

summary(ptk)

## -----------------------------------------------------------------------------
mean(ptk$cdur)
sd(ptk$cdur)

mean(ptk$speechrate)
sd(ptk$speechrate)

summary(lm(cdur ~ speechrate, ptk))

## -----------------------------------------------------------------------------
ptk$cdur_scaled <- scale(ptk$cdur)[, 1]
ptk$sr_scaled <- scale(ptk$speechrate)[, 1]

mean(ptk$cdur_scaled)
sd(ptk$cdur_scaled)

mean(ptk$sr_scaled)
sd(ptk$sr_scaled)

summary(lm(cdur_scaled ~ sr_scaled, ptk))

## -----------------------------------------------------------------------------
ptk$sr_scaled_by_speaker <- scale_by(speechrate ~ speaker, ptk)

mean(ptk$sr_scaled_by_speaker)
sd(ptk$sr_scaled_by_speaker)

with(ptk, tapply(speechrate, speaker, mean))
with(ptk, tapply(speechrate, speaker, sd))

with(ptk, tapply(sr_scaled, speaker, mean))
with(ptk, tapply(sr_scaled, speaker, sd))

with(ptk, tapply(sr_scaled_by_speaker, speaker, mean))
with(ptk, tapply(sr_scaled_by_speaker, speaker, sd))

## -----------------------------------------------------------------------------
ptk$sr_scaled_0.5 <- scale(ptk$speechrate) * 0.5
ptk$sr_scaled_by_speaker_0.5 <- scale_by(speechrate ~ speaker, ptk, scale = 0.5)

mean(ptk$sr_scaled_0.5)
sd(ptk$sr_scaled_0.5)

with(ptk, tapply(sr_scaled_by_speaker_0.5, speaker, mean))
with(ptk, tapply(sr_scaled_by_speaker_0.5, speaker, sd))

## -----------------------------------------------------------------------------
options(contrasts = c("contr.treatment", "contr.poly"))

contrasts(ptk$prevowel)

summary(lm(cdur_scaled ~ prevowel, ptk))

## -----------------------------------------------------------------------------
options(contrasts = c("contr.sum", "contr.poly"))

contrasts(ptk$prevowel)

summary(lm(cdur_scaled ~ prevowel, ptk))

## -----------------------------------------------------------------------------
contrasts(ptk$prevowel) <- named_contr_sum(ptk$prevowel)

contrasts(ptk$prevowel)

summary(lm(cdur_scaled ~ prevowel, ptk))

## -----------------------------------------------------------------------------
contrasts(ptk$prevowel) <- named_contr_sum(ptk$prevowel, scale = 0.5)

contrasts(ptk$prevowel)

## -----------------------------------------------------------------------------
ptk$prehigh <- ptk$prevowel %in% c("i", "u")

class(ptk$prehigh)

unique(ptk$prehigh)

ptk$prehigh <- named_contr_sum(ptk$prehigh, return_contr = FALSE)

class(ptk$prehigh)

levels(ptk$prehigh)

contrasts(ptk$prehigh)

## -----------------------------------------------------------------------------
ptk$preheight <- "Mid"
ptk$preheight[ptk$prevowel == "a"] <- "Low"
ptk$preheight[ptk$prevowel %in% c("i", "u")] <- "High"
ptk$preheight <- factor(ptk$preheight, ordered = TRUE, levels = c("Low",
  "Mid", "High"))

head(ptk$preheight)

contrasts(ptk$preheight)

## -----------------------------------------------------------------------------
contr3 <- contr.poly(3)
contr5 <- contr.poly(5)

apply(contr3, 2, mean)
apply(contr5, 2, mean)

apply(contr3, 2, sd)
apply(contr5, 2, sd)

## -----------------------------------------------------------------------------
sc_1_contr3 <- scaled_contr_poly(3)
sc_0.5_contr3 <- scaled_contr_poly(3, scale = 0.5)

sc_1_contr3

apply(sc_1_contr3, 2, sd)

sc_0.5_contr3

apply(sc_0.5_contr3, 2, sd)

## -----------------------------------------------------------------------------
contrasts(ptk$preheight)

summary(lm(cdur_scaled ~ preheight, ptk))

contrasts(ptk$preheight) <- scaled_contr_poly(ptk$preheight)

contrasts(ptk$preheight)

summary(lm(cdur_scaled ~ preheight, ptk))

## -----------------------------------------------------------------------------
ptk$preheight <- "Mid"
ptk$preheight[ptk$prevowel == "a"] <- "Low"
ptk$preheight[ptk$prevowel %in% c("i", "u")] <- "High"
ptk$preheight <- factor(ptk$preheight, ordered = TRUE, levels = c("Low",
  "Mid", "High"))

sobj <- standardize(cdur ~ place + stress + preheight + log(wordfreq) +
  scale_by(speechrate ~ speaker) + (1 | speaker), ptk)

## -----------------------------------------------------------------------------
is.standardized(sobj)

sobj

names(sobj)

head(sobj$data)

mean(sobj$data$cdur)
sd(sobj$data$cdur)

mean(sobj$data$log_wordfreq)
sd(sobj$data$log_wordfreq)
all.equal(scale(log(ptk$wordfreq))[, 1], sobj$data$log_wordfreq[, 1])

with(sobj$data, tapply(speechrate_scaled_by_speaker, speaker, mean))
with(sobj$data, tapply(speechrate_scaled_by_speaker, speaker, sd))

sobj$contrasts

sobj$groups

## -----------------------------------------------------------------------------
sobj <- standardize(cdur ~ place + stress + preheight + log(wordfreq) +
  scale_by(speechrate ~ speaker) + (1 | speaker), ptk, scale = 0.5)

sobj

names(sobj)

head(sobj$data)

mean(sobj$data$cdur)
sd(sobj$data$cdur)

mean(sobj$data$log_wordfreq)
sd(sobj$data$log_wordfreq)
all.equal(0.5 * scale(log(ptk$wordfreq))[, 1], sobj$data$log_wordfreq[, 1])

with(sobj$data, tapply(speechrate_scaled_by_speaker, speaker, mean))
with(sobj$data, tapply(speechrate_scaled_by_speaker, speaker, sd))

sobj$contrasts

sobj$groups

## -----------------------------------------------------------------------------
library(lme4)

mod <- lmer(sobj$formula, sobj$data)

summary(mod)

## -----------------------------------------------------------------------------
newdata <- predict(sobj, ptk)
newdata_fe <- predict(sobj, ptk, random = FALSE)
newdata_re <- predict(sobj, ptk, fixed = FALSE)

head(newdata)

head(newdata_fe)

head(newdata_re)

## -----------------------------------------------------------------------------
# predictions using both the fixed and random effects
preds <- predict(mod, newdata = newdata)
all.equal(preds, fitted(mod))

# predictions using only the fixed effects
preds_fe <- predict(mod, newdata = newdata_fe, re.form = NA)

head(preds)

head(preds_fe)

## -----------------------------------------------------------------------------
library(afex)

pvals <- mixed(mod, data = sobj$data, check_contrasts = FALSE)

pvals

## -----------------------------------------------------------------------------
library(emmeans)

stress_comparisons <- emmeans(mod, pairwise ~ stress)

stress_comparisons

