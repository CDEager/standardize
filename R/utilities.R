

# for warnings and errors
add_quotes <- function(x, collapse = " ") {
  return(paste0("'", x, "'", collapse = collapse))
}


check_dots_standardize <- function(...) {
  dots <- list(...)
  
  if (!is.null(dots$na.action) && !is.na.pass(dots$na.action)) {
    warning("'na.action' must be 'na.pass'; other na.action's can be passed in",
      " model-fitting functions. Ignoring specified 'na.action'.",
      call. = FALSE)
  }
  
  dots["na.action"] <- NULL
  
  if (length(dots)) {
    warning("Ignoring arguments ", add_quotes(names(dots)), " passed in '...'")
  }
  
  invisible(NULL)
}


check_dots_predict.standardized <- function(...) {
  dots <- list(...)
  
  if (!is.null(dots$na.action) && !is.na.pass(dots$na.action)) {
    warning("'na.action' must be 'na.pass'; other na.action's can be passed in",
      " model-fitting functions. Ignoring specified 'na.action'.",
      call. = FALSE)
  }
  
  dots["na.action"] <- NULL
  
  if (length(dots)) {
    warning("Ignoring arguments ", add_quotes(names(dots)), " passed in '...'")
  }
  
  invisible(NULL)
}


# split a matrix (based on split.data.frame but with col option)
msplit <- function(x, f, byrow = TRUE, drop = FALSE, ...) {
  if (byrow) {
    return(lapply(split(x = seq_len(nrow(x)), f = f, drop = drop, ...),
      function(ind) x[ind, , drop = FALSE]))
  }
  return(lapply(split(x = seq_len(ncol(x)), f = f, drop = drop, ...),
    function(ind) x[, ind, drop = FALSE]))
}


is.linear <- function(family) {
  if (!inherits(family, "family")) family <- get_family(family)
  return(inherits(family, "family") && family$family == "gaussian" &&
    family$link == "identity")
}


is.na.pass <- function(x) {
  if (is.character(x)) return(x == "na.pass")
  return(isTRUE(all.equal(x, na.pass)))
}


get_data_classes <- function(mf, gfacs, o) {
  d <- sapply(mf, function(x) {
    if (is.uf(x)) return("factor")
    if (is.ordered(x)) return("ordered")
    if (inherits(x, "scaledby")) {
      if (inherits(x, "poly")) return("scaledby.poly")
      return("scaledby")
    }
    if (inherits(x, "poly")) return("poly")
    if (is.numeric(x)) return("numeric")
    return(class(x)[1])
  })
  
  d[gfacs] <- "group"
  d[o] <- "offset"
  d[1] <- paste0("response.", d[1])
  
  return(d)
}


check_offset <- function(o, y) {
  if (!is.list(o)) o <- list(`(offset)` = o)
  
  nr <- sapply(o, NROW)
  nc <- sapply(o, NCOL)
  num <- sapply(o, is.numeric)
  
  if (any(nc != NCOL(y)) || any(nr != NROW(y)) || !all(num)) {
    stop("offset should be numeric and have same dimensions as response")
  }
  
  invisible(o)
}


# names of variables
varnms <- function(x) {
  if (!inherits(x, "terms")) x <- stats::terms(x)
  x <- attr(x, "factors")
  if (!is.matrix(x)) return(NULL)
  return(rownames(x))
}


# convert effects or group from ranef bar to formula
barform <- function(x, n) {
  return(eval(substitute(~ foo, list(foo = x[[n]]))))
}


scale_offset <- function(pred, offset, data) {
  if (is.integer(offset)) offset[] <- as.numeric(offset[])
  
  if (!inherits(pred, "scaledby_pred")) return(offset / pred)
  
  data[[ncol(data) + 1]] <- offset
  pred$formula[[2]] <- as.name(colnames(data)[ncol(data)])
  offset <- scale_by(pred, data)
  class(offset) <- class(offset)[-1]
  attr(offset, "pred") <- NULL
  
  return(offset)
}


# remove attributes from columns of data frame x which cause predict issues
strip_attr <- function(x) {
  a <- attributes(x)
  a <- a[intersect(names(a), c("names", "row.names", "class"))]
  attributes(x) <- a
  
  rm_classes <- c("poly", "scaledby")
  rm_attr <- c("scaled:center", "scaled:scale", "pred", "coefs")
  
  # subscripting all of them keeps it a data.frame
  x[1:ncol(x)] <- lapply(x, function(j) {
    a <- attributes(j)
    a <- a[setdiff(names(a), rm_attr)]
    a$class <- setdiff(a$class, rm_classes)
    attributes(j) <- a
    return(j)
  })
  
  return(x)
}


# what is the first element in lst which matches x exactly
in_list <- function(x, lst) {
  if (!is.list(x)) x <- list(x)
  return(setNames(match(x, lst, nomatch = 0L), names(x)))
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
    if (length(uf <- which(sapply(x, is.charlogbin)))) {
      x[uf] <- lapply(x[uf], factor, ordered = FALSE)
    }
  } else if (is.charlogbin(x)) {
    x <- factor(x, ordered = FALSE)
  }
  return(x)
}


# how many unique values are in x (x a factor, vector, matrix, or data.frame)
nval <- function(x, rm.na = TRUE) {
  if ((L <- length(dim(x))) > 2) stop("'x' must have at most two dims")
  x <- unique(x)
  if (!L) {
    if (rm.na) return(length(setdiff(x, NA)))
    return(length(x))
  }
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
  if (is.data.frame(x)) return(sapply(x, function(n) f(is.na(n))))
  return(apply(is.na(x), 2, f))
}


#' @importFrom lme4 findbars
ranef_groups <- function(formula) {
  if (length(bars <- lme4::findbars(formula))) {
    return(rownames(attr(terms((formula(paste("~", paste(sapply(bars,
      function(x) deparse(x[[3]])), collapse = "+"))))), "factors")))
  }
  return(character())
}


#' @importFrom stringr str_replace_all
make_new_names <- function(nms) {
  nms <- vapply(nms, simplify_fcall, "", f = "poly")
  nms <- vapply(nms, simplify_fcall, "", f = "scale_by")
  
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


simplify_fcall <- function(u, f) {
  fchar <- paste0(f, "\\(")
  if (!grepl(fchar, u)) return(u)
  u <- parse(text = u)[[1]]
  return(deparse(.simplify_fcall(u, fchar, f)))
}


#' @importFrom stringr str_replace_all
.simplify_fcall <- function(u, fchar, fstart) {
  if (deparse(u[[1]]) == fstart) {
    u <- u[1:2]
    if (fstart == "scale_by") return(simp_sb_f(u))
  }
  
  if (length(contains <- which(grepl(fchar, sapply(u, deparse)) &
  sapply(u, is.call)))) {
    u[contains] <- lapply(u[contains], .simplify_fcall, fchar = fchar,
      fstart = fstart)
  }
  
  return(u)
}


simp_sb_f <- function(x) {
  form <- x[[2]]
  form[2] <- NULL
  v <- vapply(rownames(attr(terms(formula(form)), "factors")), make_new_names,
    "")
  rhsf <- paste(v, collapse = ".")
  lhsf <- make_new_names(deparse(x[[2]][[2]]))
  return(as.name(paste(lhsf, "scaled_by", rhsf, sep = "_")))
}


replace_variables <- function(call, old_names, new_names) {
  stopifnot(is.call(call), is.character(old_names), is.character(new_names))
  new_names <- lapply(new_names, function(x) parse(text = x)[[1]])
  return(.replace_variables(call, old_names, new_names))
}


.replace_variables <- function(f, o, n) {
  m <- match(sapply(f, deparse), o)
  
  i <- which(!is.na(m))
  r <- which(sapply(f, is.call) & is.na(m))
  
  if (length(i)) f[i] <- n[m[i]]
  if (length(r)) f[r] <- lapply(f[r], .replace_variables, o = o, n = n)
  
  return(f)
}


lme4_safeDeparse <- function(x, collapse = " ") {
  return(paste(deparse(x, 500L), collapse = collapse))
}


lme4_reOnly <- function(f, response = FALSE) {
  if (response && length(f) == 3) {
    response <- f[[2]]
  } else {
    response <- NULL
  }
  
  return(reformulate(paste0("(", vapply(lme4::findbars(f), lme4_safeDeparse, 
    ""), ")"), response = response))
}


mpc_offset <- function(pv, po) {
  if (is.null(po)) return(pv[[2]])
  
  if (!inherits(po, "scaledby_pred")) {
    return(substitute(a / b, list(a = pv[[2]], b = po)))
  }
  
  po$formula[[2]] <- pv[[2]]
  return(substitute(standardize::scale_by(object = a), list(a = po)))
}


mpc_group <- function(pv, v) {
  v <- factor(v, ordered = FALSE)
  return(substitute(factor(x = X, ordered = FALSE, levels = L), list(X = pv,
    L = levels(v))))
}


mpc_factor <- function(pv, v, scale) {
  v <- named_contr_sum(v, scale, FALSE)
  return(substitute(standardize::fac_and_contr(x = X, levels = L, contrasts = C,
    ordered = FALSE), list(X = pv, L = levels(v), C = contrasts(v))))
}


mpc_ordered <- function(pv, v, scale) {
  v <- scaled_contr_poly(v, scale, FALSE)
  return(substitute(standardize::fac_and_contr(x = X, levels = L, contrasts= C,
    ordered = TRUE), list(X = pv, L = levels(v), C = contrasts(v))))
}


mpc_numeric <- function(pv, v, scale) {
  a <- attributes(scale(v))
  return(substitute(scale(x = X, center = C, scale = S), list(X = pv,
    C = a[["scaled:center"]], S = a[["scaled:scale"]] / scale)))
}


mpc_scaledby <- function(pv, data, scale) {
  pv$object <- attr(scale_by(pv$object$formula, data, scale), "pred")
  return(pv)
}

