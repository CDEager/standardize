

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
standardize_terms <- function(formula, data, family = gaussian, scale = 1,
                              na.action = "na.pass") {

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
  
  if (!is.vector(mf[[1]])) {
    stop("response variable must be a vector")
  }
  
  terms <- terms(mf)
  a <- attributes(terms)
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
      p[[2]]$pred <- attr(scale_by(p[[2]]$pred$formula, data), "scaledby")
      p[[2]][[1]] <- quote(standardize::scale_by)
    }
  }
  
  for (g in gfacs) {
    j <- which(colnames(mf) == g)
    mf[[j]] <- factor(mf[[j]], ordered = FALSE)
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("factor", ordered = FALSE)
    p[[j + 1]]$x <- pj
  }
  
  d <- character(ncol(mf))
  names(d) <- colnames(mf)
  
  uf <- which(sapply(mf, is.uf))
  of <- which(sapply(mf, is.ordered))
  num <- which(sapply(mf, function(x) is.numeric(x) && !inherits(x,
    "scaledby")))
  sb <- which(sapply(mf, function(x) inherits(x, "scaledby")))
  
  d[uf] <- "factor"
  d[of] <- "ordered"
  d[sb] <- "numeric"
  d[num] <- "numeric"
  
  uf <- uf[!(uf %in% c(1, which(colnames(mf) %in% gfacs)))]
  for (j in uf) {
    mfj <- named_contr_sum(mf[[j]], scale, FALSE)
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("fac_and_contr", levels = levels(mfj),
      contrasts = contrasts(mfj), ordered = FALSE)
    p[[j + 1]]$x <- pj
    p[[j + 1]][[1]] <- quote(standardize::fac_and_contr)
  }
  
  of <- of[of != 1]
  for (j in of) {
    mfj <- scaled_contr_poly(mf[[j]], scale, FALSE)
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("fac_and_contr", levels = levels(mfj),
      contrasts = contrasts(mfj), ordered = TRUE)
    p[[j + 1]]$x <- pj
    p[[j + 1]][[1]] <- quote(standardize::fac_and_contr)
  }
  
  num <- num[num != 1]
  for (j in num) {
    mfj <- scale(mf[[j]])
    pj <- p[[j + 1]]
    p[[j + 1]] <- call("scale", center = attr(mfj, "scaled:center"),
      scale = attr(mfj, "scaled:scale") / scale)
    p[[j + 1]]$x <- pj
  }
  
  sb <- sb[sb != 1]
  for (j in sb) {
    p[[j + 1]]$pred <- attr(scale_by(p[[j + 1]]$pred$formula, data,
      scale), "scaledby")
    p[[j + 1]][[1]] <- quote(standardize::scale_by)
  }
  
  a$predvars <- p
  a$dataClasses <- d
  a$standardized.scale <- scale
  attributes(formula) <- a
  class(formula) <- c("standardized.terms", "terms", "formula")
  
  return(formula)
}


#' Get the \code{scale} argument passed to \code{standardize}.
#'
#' Returns the \code{scale} argument from a \code{standardized.terms} object.
#'
#' @param object Any object for which a \code{\link[stats]{terms}} method
#'   exists.
#'
#' @return A postive scalar corresponding to the \code{scale} argument passed
#'   to \code{\link{standardize_terms}} which created the
#'   \code{standardized.terms} object used in creating \code{object}.  If
#'   \code{standardize_terms} was not used, then \code{NULL} is returned.
#'
#' @export
standardized_scale <- function(object) {
  if (!inherits(object, "standardized.terms")) {
    if (inherits(object, "merMod")) {
      object <- attr(object@frame, "formula")
    } else {
      object <- terms(object)
    }
  }
  return(attr(object, "standardized.scale"))
}


#' S3 method for \code{\link[stats]{model.frame}} for class \code{standardized.terms}.
#'
#' @param formula,data,... See \code{\link[stats]{model.frame}}.
#'
#' @export
model.frame.standardized.terms <- function(formula, data = NULL, ...) {
  mc <- match.call()
  mc[[1]] <- quote(stats::model.frame)
  mc$formula <- condense_terms(formula)
  mc["xlev"] <- NULL
  mc["contrasts"] <- NULL
  mf <- eval(mc, parent.frame())
  class(attr(mf, "terms")) <- c("standardized.terms", "terms", "formula")
  return(mf)
}


#' S3 method for \code{\link[stats]{model.matrix}} for class \code{standardized.terms}.
#'
#' @param object,data,... See \code{\link[stats]{model.matrix}}.
#'
#' @export
model.matrix.standardized.terms <- function(object, data = environment(object),
                                            ...) {
  return(stats::model.matrix(object = condense_terms(object), data = data, ...))
}


#' S3 method for \code{\link[stats]{formula}} for class \code{standardized.terms}.
#'
#' Calls \code{\link[stats]{formula}} and then ensures that the result has
#' the proper terms attributes.
#'
#' @param x,... See \code{\link[stats]{formula}}.
#'
#' @export
formula.standardized.terms <- function(x, ...) {
  a <- attributes(x)
  formula <- stats::formula(strip_terms(x), ...)
  a[[".Environment"]] <- environment(formula)
  attributes(formula) <- a
  return(condense_terms(formula))
}

