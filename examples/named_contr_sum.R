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

\dontrun{
# error from stats::contr.sum because only one unique non-NA value
named_contr_sum(5)
named_contr_sum(rep(c("a", NA), 3))
}
