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
