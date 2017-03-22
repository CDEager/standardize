

# what is the first element in lst which matches x exactly
in_list <- function(x, lst) {
  m <- which(sapply(lst, function(n) isTRUE(all.equal(n, x))))
  if (length(m)) return(m[1])
  return(0)
}


# reorder false/true interpretation types
reorder_ft <- function(x) {
  if (in_list(tolower(x), list(c("0", "1"), c("false", "true"), c("f", "t"),
  c("no", "yes"), c("n", "y")))) {
    x <- x[2:1]
  }
  return(x)
}


# is x a single number whose sign is in sgn
is.scalar <- function(x, sgn = c(-1, 0, 1)) {
  return(is.numeric(x) && is.vector(x) && length(x) == 1 && sign(x) %in% sgn)
}


# return family information
#' @importFrom MASS negative.binomial
get_family <- function(family) {
  if (inherits(family, "family")) {
    if (!is.null(family$family) && !is.null(family$link)) {
      return(family)
    }
    stop("'family' not recognized")
  }
  
  if (isTRUE(all.equal(family, MASS::negative.binomial)) ||
  (is.character(family) && family %in% c("negbin", "nb", "negative.binomial",
  "negative binomial"))) {
    return("negbin")
  }
  
  if (is.character(family)) {
    tryfunc <- tryCatch(family <- get(family, mode = "function",
      envir = parent.frame()), error = function(e) e)
    if (inherits(tryfunc, "error")) stop("'family' not recognized")
  }
  
  if (is.function(family)) {
    family <- family()
    if (!is.null(family$family) && !is.null(family$link)) {
      return(family)
    }
  }
  
  stop("'family' not recognized")
}


# is x character, logical, or anything with only two unique
# non-NA values but that isn't already an unordered factor
is.charlogbin <- function(x) {
  return(NCOL(x) == 1 && (is.character(x) || is.logical(x) ||
    (!is.uf(x) && nval(x) == 2)))
}


# convert things that aren't factors but should be (according to charlogbin)
# to unordered factors.  x can be a data.frame or a vector
charlogbin_to_uf <- function(x) {
  if (is.data.frame(x)) {
    uf <- which(sapply(x, is.charlogbin))
    for (j in uf) x[[j]] <- factor(x[[j]], ordered = FALSE)
  } else if (is.charlogbin(x)) {
    x <- factor(x, ordered = FALSE)
  }
  return(x)
}


# how many unique values are in x (x a factor, vector, matrix, or data.frame)
nval <- function(x, rm.na = TRUE) {
  x <- unique(x)
  if ((L <- length(dim(x))) > 2) stop("'x' must have at most two dims")
  if (!L) x <- matrix(x)
  if (rm.na) x <- x[!rowna(x), , drop = FALSE]
  return(nrow(x))
}


# is x an unordered factor
is.uf <- function(x) {
  return(is.factor(x) && !is.ordered(x))
}


# are any/all of the observations in each row of 'x' NA?
rowna <- function(x, f = any) {
  if (length(dim(x)) != 2) stop("'x' must have two dims")
  return(apply(is.na(x), 1, f))
}


# are any/all of the observations in each column of 'x' NA?
colna <- function(x, f = any) {
  if (length(dim(x)) != 2) stop("'x' must have two dims")
  if (is.data.frame(x)) {
    return(sapply(x, function(n) f(is.na(n))))
  }
  return(apply(is.na(x), 2, f))
}


#' @importFrom lme4 findbars
get_ranef_groups <- function(formula) {
  bars <- lme4::findbars(formula)
  if (length(bars)) {
    g <- sapply(bars, function(x) {
      x <- as.character(stats::as.formula(substitute(~foo, list(foo = x[[3]]))))
      x[length(x)]
    })
    g <- stats::as.formula(paste("~", paste(g, collapse = "+")))
    return(attr(terms(g), "factors"))
  }
  return(NULL)
}


strip_terms <- function(formula) {
  env <- environment(formula)
  attributes(formula) <- NULL
  class(formula) <- "formula"
  environment(formula) <- env
  return(formula)
}


condense_terms <- function(formula) {
  a <- attributes(formula)
  formula <- terms(strip_terms(formula))
  newa <- attributes(formula)
  if (is.matrix(newa$factors)) {
    keep <- c(1, which(rownames(a$factors) %in% rownames(newa$factors)) + 1)
  } else {
    keep <- 1:2
  }
  newa$variables <- a$variables[keep]
  newa$predvars <- a$predvars[keep]
  newa$standardized.scale <- a$standardized.scale
  attributes(formula) <- newa
  return(formula)
}

