

#' Standardize a formula for regression.
#' 
#' Create a terms object (of class \code{standardized.terms}) which places all
#' variables in \code{data} on the same scale, making regression output more
#' easy to interpret. For mixed effects regressions, this also offers
#' computational benefits, and for Bayesian regressions, it also makes
#' determining reasonable priors easier.
#'
#' First \code{\link[stats]{model.frame}} is called. Then,
#' if \code{family = gaussian}, the response is checked to ensure that it is
#' numeric and has more than two unique values.  If \code{\link{scale_by}} is
#' used on the response in \code{formula}, then the \code{scale} argument to
#' \code{scale_by} is ignored and forced to \code{1}.  If \code{\link{scale_by}}
#' is not called, then \code{\link[base]{scale}} is used with default arguments.
#' For all other values for \code{family}, the response is not checked.
#'
#' For the predictors in the formula, first any random effects grouping factor
#' in the formula is coerced to factor and unused levels are dropped.  The
#' levels of the resulting factor are then recorded in the \code{predvars}
#' attribute.  Then for the remaining predictors, regardless of their original
#' class, if they have only two unique non-\code{NA} values, they are coerced
#' to unordered factors.  Then, \code{\link{named_contr_sum}} and
#' \code{\link{scaled_contr_poly}} are called for unordered and ordered factors,
#' respectively, using the \code{scale} argument provided in the call
#' to \code{standardize_terms} as the \code{scale} argument to the contrast
#' functions.  For numeric variables, if the variable contains a call to
#' \code{\link{scale_by}}, then, regardless of whether the call to
#' \code{\link{scale_by}} specifies \code{scale}, the value of \code{scale}
#' in the call to \code{standardize_terms} is used.  If the numeric variable
#' does not contain a call to \code{\link{scale_by}}, then
#' \code{\link[base]{scale}} is called, ensuring that the result has
#' standard deviation \code{scale}.
#'
#' With the default value of \code{scale = 1}, the result is a terms object
#' with a \code{predvars} attribute which ensures that a model frame created
#' using the terms object and the same data frame with which it was created
#' has numeric variables on unit scale, unordered factors with named sum
#' sum contrasts, and ordered factors with orthogonal polynomial contrasts
#' on unit scale.  For gaussian regressions, the response is also placed on
#' unit scale.  If \code{scale = 0.5}, then gaussian responses would still
#' be placed on unit scale, but unordered factors' named sum contrasts would
#' take on values {-0.5, 0, 0.5} rather than {-1, 0, 1}, the standard deviation
#' of each column in the contrast matrices for ordered factors would be
#' \code{0.5} rather than \code{1}, and the standard deviation of numeric
#' variables would be \code{0.5} rather than \code{1} (within-factor-level
#' in the case of \code{\link{scale_by}} calls).  The returned object
#' can be used as the \code{formula} argument to regression fitting functions
#' including those in the \code{lme4} and \code{rstanarm} packages, etc.
#' 
#' @param formula A regression \code{\link[stats]{formula}}.
#' @param data A data.frame containing the variables in \code{formula}.
#' @param family A regression \code{\link[stats]{family}} (default gaussian).
#' @param scale The desired scale for the regression frame. Must be a single
#'   positive number. See 'Details'.
#' @param na.action See \code{\link[stats]{model.frame}}.
#' 
#' @return A terms object of class \code{standardized.terms}. The
#'   \code{predvars} attribute is altered to ensure that proper predictions
#'   are made when the object is used to create model frames and matrices.
#'   It also has a \code{standardized.scale} attribute which contains the
#'   \code{scale} argument passed to \code{standardize_terms}.
#' 
#' @section Note: Offsets are not currently supported.  When
#'   a \code{merMod} object (i.e. the model object
#'   returned by \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, or
#'   \code{\link[lme4]{glmer.nb}}) fit with a \code{standardized.terms} object
#'   as the formula is used with the function \code{predict}, warnings will be
#'   issued saying that contrasts were dropped from factors. 
#'   These warnings don't affect the predictions made.
#'
#' @seealso \code{\link[base]{scale}}, \code{\link{scale_by}},
#'   \code{\link{named_contr_sum}}, and \code{\link{scaled_contr_poly}}.
#' 
#' @examples
#' # NEED TO ADD EXAMPLES
#' 
#' @importFrom lme4 subbars
#'
#' @export
standardize <- function(formula, data, family = gaussian, scale = 1,
                        na.action = "na.pass") {
  mc <- match.call()
  formula <- stats::formula(formula)
  if (!inherits(formula, "formula")) {
    stop("'formula' must be coercible to formula")
  }
  formula <- strip_terms(formula)
  gfacs <- rownames(get_ranef_groups(formula))
  
  if (!is.data.frame(data)) stop("'data' must be a data.frame")
  attr(data, "terms") <- NULL
  
  family <- get_family(family)
  gau <- !is.character(family) && family$family == "gaussian" &&
    family$link == "identity"
  
  if (!is.scalar(scale, 1)) {
    stop("'scale' must be a single positive number")
  }
  
  mf <- stats::model.frame(lme4::subbars(formula), data,
    drop.unused.levels = TRUE, na.action = na.action)
  if (is.null(stats::model.response(mf))) {
    stop("no response in formula")
  }
  if (!is.null(stats::model.offset(mf))) {
    stop("offsets not currently supported")
  }
  
  if (NCOL(mf[[1]]) > 1) {
    stop("response variable must be a vector")
  }

  a <- attributes(terms(mf))
  p <- a$predvars
  
  mf <- charlogbin_to_uf(mf)
  
  if (gau) {
    if (!is.numeric(mf[[1]])) {
      stop("'family' is gaussian but response is not numeric")
    }
    if (nval(mf[[1]]) < 3) {
      stop("'family' is gaussian but response has fewer than 3 unique values")
    }
    if (!inherits(mf[[1]], "scaledby")) {
      mfj <- scale(mf[[1]])
      p2 <- p[[2]]
      p[[2]] <- call("scale")
      p[[2]]$x <- p2
      p[[2]]$center = attr(mfj, "scaled:center")
      p[[2]]$scale = attr(mfj, "scaled:scale")
    } else {
      # to ignore scales other than 1 which were specified
      p[[2]]$object <- attr(scale_by(p[[2]]$object$formula, data), "pred")
    }
  }
  
  d <- sapply(colnames(mf), function(n) {
    if (n %in% gfacs) return("group")
    if (is.uf(mf[[n]])) return("factor")
    if (is.ordered(mf[[n]])) return("ordered")
    if (inherits(mf[[n]], "scaledby")) {
      if (inherits(mf[[n]], "poly")) return("scaledby.poly")
      return("scaledby")
    }
    if (inherits(mf[[n]], "poly")) return("poly")
    return("numeric")
  })
  
  vtype <- function(type) {
    return(which(d[-1] %in% type) + 1)
  }
  
  lvs <- vector("list", ncol(mf))
  names(lvs) <- colnames(mf)
  
  for (j in vtype("group")) {
    mf[[j]] <- factor(mf[[j]], ordered = FALSE)
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("factor", ordered = FALSE, levels = levels(mf[[j]]))
    p[[j + 1]]$x <- pj
  }
  
  for (j in vtype("factor")) {
    mfj <- named_contr_sum(mf[[j]], scale, FALSE)
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("fac_and_contr", levels = levels(mfj),
      contrasts = contrasts(mfj), ordered = FALSE)
    p[[j + 1]]$x <- pj
    p[[j + 1]][[1]] <- quote(standardize::fac_and_contr)
    lvs[[j]] <- levels(mfj)
  }
  
  for (j in vtype("ordered")) {
    mfj <- scaled_contr_poly(mf[[j]], scale, FALSE)
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("fac_and_contr", levels = levels(mfj),
      contrasts = contrasts(mfj), ordered = TRUE)
    p[[j + 1]]$x <- pj
    p[[j + 1]][[1]] <- quote(standardize::fac_and_contr)
    lvs[[j]] <- levels(mfj)
  }
  
  for (j in vtype(c("numeric", "poly"))) {
    mfj <- scale(mf[[j]])
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("scale", center = attr(mfj, "scaled:center"),
      scale = attr(mfj, "scaled:scale") / scale)
    p[[j + 1]]$x <- pj
    if (d[j] == "numeric") {
      lvs[[j]] <- 0
    } else {
      coefs <- attr(mf[[j]], "coefs")
      lvs[[j]] <- (poly(coefs$alpha[1], degree = length(coefs$alpha),
        coefs = coefs, simple = TRUE) - p[[j + 1]]$center) / p[[j + 1]]$scale
    }
  }
  
  for (j in vtype(c("scaledby", "scaledby.poly"))) {
    p[[j + 1]]$object <- attr(scale_by(p[[j + 1]]$object$formula, data,
      scale), "pred")
    if (d[j] == "scaledby") {
      lvs[[j]] <- 0
    } else {
      coefs <- attr(mf[[j]], "coefs")
      lvs[[j]] <- (poly(coefs$alpha[1], degree = length(coefs$alpha),
        coefs = coefs, simple = TRUE) - p[[j + 1]]$object$new_center) /
        p[[j + 1]]$object$new_scale
    }
  }
  
  a$predvars <- p
  a$dataClasses <- d
  a$standardized.scale <- scale
      
  terms <- formula
  attributes(terms) <- a
  
  frame <- stats::model.frame(lme4::subbars(terms), data, na.action = na.action)
  attributes(frame) <- attributes(frame)[c("names", "row.names", "class")]
  
  nms <- make_new_names(colnames(frame))
  names(nms) <- colnames(frame)
  attr(terms, "rename") <- nms
  names(lvs) <- colnames(frame) <- unname(nms)
  formula <- make_new_formula(terms, nms)
  lvs <- lvs[rownames(attr(terms(stats::delete.response(
    lme4::nobars(formula))), "factors"))]

  facs <- mats <- list()
  nums <- NULL
  for (j in names(lvs)) {
    if (is.factor(frame[[j]])) {
      facs[[j]] <- list(levels = levels(frame[[j]]),
        contrasts = contrasts(frame[[j]]), ordered = is.ordered(frame[[j]]))
    } else if (NCOL(frame[[j]]) > 1) {
      mats[[j]] <- as.vector(lvs[[j]])
    } else {
      nums <- c(nums, j)
    }
  }

  sf <- standardized(call = mc, scale = scale, formula = formula,
    frame = frame, pred.terms = terms, grid.levels = list(facs = facs,
    nums = nums, mats = mats))

  return(sf)
}

