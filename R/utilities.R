

# The functions in this file are not exported.  When a function is an internal
# function from another package (i.e. one that would need to be accessed using
# the ':::' operator), the ':::' is replaced with '__', and the function
# definitions are taken directly from the other package with minimal alteration.
# Such functions are taken from the following package versions:
#   lme4 1.1-12


# remove attributes from columns of data frame x which cause predict issues
strip_attr <- function(x) {
  a <- attributes(x)
  a <- a[names(a) %in% c("names", "row.names", "class")]
  attributes(x) <- a
  rm_attr <- c("scaled:center", "scaled:scale", "pred", "coefs")
  for (j in 1:ncol(x)) {
    cl <- class(x[[j]])
    cl <- cl[!(cl %in% c("poly", "scaledby"))]
    class(x[[j]]) <- cl
    a <- attributes(x[[j]])
    a <- a[!(names(a) %in% rm_attr)]
    attributes(x[[j]]) <- a
  }
  return(x)
}


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
  
  if (is.character(family) && family %in% c("ordinal", "ordered")) {
    return("ordinal")
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
  if ((L <- length(dim(x))) > 2) stop("'x' must have at most two dims")
  x <- unique(x)
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


strip_terms <- function(terms) {
  formula <- terms
  attributes(formula) <- NULL
  class(formula) <- "formula"
  environment(formula) <- environment(terms)
  return(formula)
}


condense_terms <- function(terms) {
  a <- attributes(terms)
  terms <- terms(strip_terms(terms))
  newa <- attributes(terms)
  keep <- which(names(a$rename) %in% rownames(newa$factors))
  newa$variables <- a$variables[c(1, keep + 1)]
  newa$predvars <- a$predvars[c(1, keep + 1)]
  newa$dataClasses <- a$dataClasses[keep]
  newa$rename <- a$rename[keep]
  attributes(terms) <- newa
  return(terms)
}


#' @importFrom stringr str_replace_all
make_new_names <- function(nms) {
  nms <- vapply(nms, function(x) simplify_fcall(x, "poly"), "")
  nms <- vapply(nms, function(x) simplify_fcall(x, "scale_by"), "")
  nms <- stringr::str_replace_all(nms, "scale_by\\(", "scale(")
  
  torep <- c(" ", "\\(", "\\)", "~", "%", ">=", "<=", "==", "=", ">", "<",
    "\\^", "\\+", "-", "@", "&&", "&", "\\|\\|", "\\|", "/", ":", "\\*", ",")
  repwth <- c("", "_", "", "_by_", ".m.", ".ge.", ".le.", ".ee.", ".e.", ".g.",
    ".l.", ".pow.", ".p.", ".m.", ".at.", ".a.", ".a.", ".o.", ".o.", ".d.",
    ".", ".t.", ".")
  for (j in 1:length(torep)) {
    nms <- stringr::str_replace_all(nms, torep[j], repwth[j])
  }
  
  return(make.names(nms, unique = TRUE))
}


make_new_formula <- function(mt, nms) {
  f <- strip_terms(stats::delete.response(stats::terms(strip_terms(mt))))
  names(nms) <- rownames(attr(mt, "factors"))
  resp <- nms[1]
  fe <- terms(lme4::nobars(f))
  bars <- lme4::findbars(f)
  
  b0 <- attr(fe, "intercept")
  fe <- factor_formula(fe, nms)
  if (is.null(fe)) {
    fe <- "1"
  } else if (!b0) {
    fe <- paste0("0+", fe)
  }
  form <- paste0(resp, "~", fe)
  
  if (length(bars)) {
    for (b in 1:length(bars)) {
      grp <- stats::terms(eval(substitute(~foo, list(foo = bars[[b]][[3]]))))
      eff <- stats::terms(eval(substitute(~foo, list(foo = bars[[b]][[2]]))))
      b0 <- attr(eff, "intercept")
      grp <- factor_formula(grp, nms)
      eff <- factor_formula(eff, nms)
      if (is.null(eff)) {
        eff <- "1"
      } else if (b0) {
        eff <- paste0("1+", eff)
      } else {
        eff <- paste0("0+", eff)
      }
      form <- paste0(form, "+(", eff, "|", grp, ")")
    }
  }
  return(formula(form))
}


factor_formula <- function(trms, nms = NULL) {
  fmat <- attr(trms, "factors")
  if (!is.matrix(fmat)) return(NULL)
  if (is.null(nms)) {
    nms <- rownames(fmat)
  } else {
    nms <- nms[rownames(fmat)]
  }
  keep <- rep(TRUE, ncol(fmat))
  if (ncol(fmat) > 2) {
    for (j in ncol(fmat):2) {
      if (keep[j] && all(fmat[, j] < 2)) {
        involved <- fmat[, j] > 0
        lower <- which(apply(fmat[, 1:(j - 1), drop = FALSE], 2, function(u) {
          !any(u > 0 & !involved) & !any(u > 1)
        }))
        keep[lower] <- FALSE
      }
    }
  }
  fmat <- fmat[, keep, drop = FALSE]
  for (j in 1:ncol(fmat)) {
    if (all(fmat[, j] < 2)) {
      colnames(fmat)[j] <- paste(nms[fmat[, j] > 0], collapse = "*")
    } else {
      colnames(fmat)[j] <- paste(nms[fmat[, j] > 0], collapse = ":")
    }
  }
  return(paste(colnames(fmat), collapse = " + "))
}


#' @importFrom stringr str_locate_all
simplify_fcall <- function(u, f) {
  f <- paste0(f, "\\(")
  loc <- stringr::str_locate_all(u, f)[[1]]
  if (!(J <- nrow(loc))) return(u)
  for (j in J:1) {
    loc <- stringr::str_locate_all(u, f)[[1]]
    first <- unname(loc[j, 1])
    
    op <- stringr::str_locate_all(u, "\\(")[[1]][, 2]
    cp <- stringr::str_locate_all(u, "\\)")[[1]][, 2]
    names(op) <- rep("o", length(op))
    names(cp) <- rep("c", length(cp))
    p <- sort(c(op, cp))
    p <- p[p > first]
    op <- names(p) == "o"
    np <- length(op)

    pos <- 1
    opens <- 1 * op[1]
    while (opens && pos < np) {
      pos <- pos + 1
      if (op[pos]) {
        opens <- opens + 1
      } else {
        opens <- opens - 1
      }
    }
    if (opens) stop("Invalid expression (parentheses don't match up)")
    last <- unname(p[pos])

    if (first > 1) {
      pre <- substr(u, 1, first - 1)
    } else {
      pre <- character(0)
    }

    if (last < nchar(u)) {
      pos <- substr(u, last + 1, nchar(u))
    } else {
      pos <- character(0)
    }

    txt <- deparse(parse(text = substr(u, first, last))[[1]][1:2])

    u <- paste0(pre, txt, pos)
  }
  
  return(u)
}


lme4__safeDeparse <- function(x, collapse = " ") {
  return(paste(deparse(x, 500L), collapse = collapse))
}


lme4__reOnly <- function(f, response = FALSE) {
  if (response && length(f) == 3) {
    response <- f[[2]]
  } else {
    response <- NULL
  }
  
  return(reformulate(paste0("(", vapply(lme4::findbars(f), lme4__safeDeparse, 
    ""), ")"), response = response))
}

