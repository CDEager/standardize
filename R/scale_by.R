

#' Center and scale a continuous variable conditioning on factors.
#'
#' \code{scale_by} centers and scales a numeric variable within each level
#' of a factor (or the interaction of several factors).
#'
#' First, the behavior when \code{object} is a formula and \code{scale = 1}
#' is described.
#' The left hand side of the formula must indicate a numeric variable
#' to be scaled.  The full interaction of the variables on the right hand side
#' of the formula is taken as the factor to condition scaling on (i.e.
#' it doesn't matter whether they are separated with \code{+}, \code{:}, or
#' \code{*}).  For the remainder of this section, the numeric variable will
#' be referred to as \code{x} and the full factor interaction term will be
#' referred to as \code{facs}.
#'
#' First, if \code{facs} has more than one element, then a new factor is
#' created as their full interaction term.  When a factor has \code{NA} values,
#' \code{NA} is treated as a level.  For each level of the factor which has
#' at least two unique non-\code{NA} \code{x} values, the mean of \code{x}
#' is recorded as the level's center and the standard deviation of \code{x}
#' is recorded as the level's scale. The mean of these
#' centers is recorded as \code{new_center} and the mean of these scales
#' is recorded as \code{new_scale}, and \code{new_center} and
#' \code{new_scale} are used as the center and scale for factor levels with
#' fewer than two unique non-\code{NA} \code{x} values. Then for each level of
#' the factor, the level's center is subtracted from its \code{x} values, and
#' the result is divided by the level's scale.
#' The result is that any level with more than two unique non-\code{NA} \code{x}
#' values now has mean \code{0} and standard deviation \code{1}, and levels
#' with fewer than two are placed on a similar scale (though their standard
#' deviation is undefined).  Note that the overall standard deviation of the
#' resulting variable (or standard deviations if \code{x} is a matrix) will not
#' be exactly \code{1} (but will be close).  The interpretation of the
#' variable is how far an observation is from its level's average value for
#' \code{x} in terms of within-level standard deviations.
#'
#' If \code{scale = 0}, then only centering (but not scaling) is performed.
#' If \code{scale} is neither \code{0} nor \code{1}, then \code{x} is scaled
#' such that the standard deviation within-level is \code{scale}.  Note that
#' this is different than the \code{scale} argument to \code{\link[base]{scale}}
#' which specifies the number the centered variable is divided by (which is
#' the inverse of the use here).  If \code{x} is a matrix with more than
#' one column, then \code{scale} must either be a vector with an element for
#' each column of \code{x} or a single number which will be used for all
#' columns.  If any element of \code{scale} is \code{0}, then all elements are
#' treated as \code{0}.  No element in \code{scale} can be negative.
#'
#' If \code{object} is not a formula, it must be a numeric variable which
#' resulted from a previous \code{scale_by} call, or the \code{pred} attribute
#' of such a numeric variable. In this case, \code{scale}
#' is ignored, and \code{x} in \code{data} is scaled
#' using the \code{formula}, \code{centers} and \code{scales} in \code{object}
#' (with new levels treated using \code{new_center} and \code{new_scale}).
#'
#' @param object A \code{\link[stats]{formula}} whose left hand side indicates
#'   a numeric variable to be scaled and whose right hand side indicates
#'   factors to condition this scaling on; or the result of a previous call
#'   to \code{scale_by} or the \code{pred} attribute of a previous call.
#'   See 'Details'.
#'
#' @param data A data.frame containing the numeric variable to be scaled and
#'   the factors to condition on.
#'
#' @param scale Numeric (default 1).  The desired standard deviation for the
#'   numeric variable within-factor-level.  If the numeric variable is a matrix,
#'   then \code{scale} must have either one element (used for all columns),
#'   or as many elements as there are columns in the numeric variable. To center
#'   the numeric variable without scaling, set \code{scale} to \code{0}.
#'   See 'Details'.
#'
#' @return A numeric variable which is conditionally scaled within each level
#'   of the conditioning factor(s), with standard deviation \code{scale}.  It has
#'   an additional class \code{scaledby}, as well as an attribute
#'   \code{pred} with class \code{scaledby_pred}, which is a list containing
#'   the formula, the centers and scales for known factor levels, and the
#'   center and scale to be applied to new factor levels.  The variable returned
#'   can be used as the \code{object} argument in future calls to
#'   \code{scale_by}, as can its \code{pred} attribute.
#'
#' @seealso \code{\link[base]{scale}}.
#'
#' @example examples/scale_by.R
#'
#' @author Christopher D. Eager <eager.stats@gmail.com>
#'
#' @export
scale_by <- function(object = NULL, data = NULL, scale = 1) {

  if (inherits(object, "scaledby")) {
    pred <- attr(object, "pred")
  } else if (inherits(object, "scaledby_pred")) {
    pred <- object
  } else if (inherits(object, "formula")) {
    pred <- NULL
    formula <- object
  } else {
    stop("Invalid 'object'; should be a formula or result of previous",
      " scale_by call")
  }

  if (!is.null(pred)) {
    if (!inherits(pred, "scaledby_pred")) {
      stop("'object', if not a formula, must be the 'pred' attribute of a\n",
        " previous scale_by call, or the numeric variable returned\n",
        " by a previous scale_by call")
    }
    formula <- pred$formula
  }

  if (is.null(data)) {
    environment(formula) <- parent.frame()
    data <- stats::model.frame(formula = formula, na.action = "na.pass")
  } else {
    data <- stats::model.frame(formula = formula, data = data,
      na.action = "na.pass")
  }

  mt <- attr(data, "terms")
  fmat <- attr(mt, "factors")
  if (!is.matrix(fmat) || sum(rowSums(fmat) == 0) != 1 ||
  all(rowSums(fmat) == 0)) {
    stop("'formula' must be a two-sided formula")
  }

  x <- data[[1]]
  if (!is.numeric(x)) stop("left hand side of 'formula' must be numeric")
  if (is.integer(x)) x[] <- as.numeric(x[])
  a <- attributes(x)
  a$class <- class(x)
  if (!is.matrix(x)) x <- matrix(x)
  d <- ncol(x)

  facs <- rownames(fmat)[rowSums(fmat) > 0]
  data[facs] <- lapply(data[facs], function(f) addNA(factor(f, ordered = FALSE)))
  fi <- interaction(data[facs], drop = TRUE)
  levels(fi)[is.na(levels(fi))] <- "NA"

  if (!is.null(pred)) {
    centers <- pred$centers
    scales <- pred$scales
    new_center <- pred$new_center
    new_scale <- pred$new_scale
    scale <- 1 * is.matrix(scales)
    if (ncol(centers) != d || (scale && ncol(scales) != d)) {
      stop("'pred' and 'data' imply different dimensions for numeric variable")
    }

    if (length(newlvs <- setdiff(levels(fi), rownames(centers)))) {
      newc <- matrix(new_center, length(newlvs), d, byrow = TRUE)
      rownames(newc) <- newlvs
      centers <- rbind(centers, newc)
      if (scale) {
        news <- matrix(new_scale, length(newlvs), d, byrow = TRUE)
        rownames(news) <- newlvs
        scales <- rbind(scales, news)
      }
    }

    fi <- factor(fi, levels = rownames(centers))

  } else {
    if (!is.numeric(scale) || !is.vector(scale) || any(scale < 0)) {
      stop("'scale' must be a positive numeric vector or '0' to indicate ",
        "centering only")
    }
    if (any(scale == 0)) scale <- 0
    if (length(scale) == 1) scale <- rep(scale, d)
    if (length(scale) != d) {
      stop("'scale' must have either a single element, or as many elements ",
        "as there are columns in the numeric variable")
    }

    xi <- msplit(x, fi)
    r2 <- sapply(xi, nval) >= 2
    xi <- xi[r2]

    centers <- matrix(nrow = nlevels(fi), ncol = d)
    rownames(centers) <- levels(fi)
    if (d > 1) {
      centers[r2, ] <- t(sapply(xi, colMeans))
    } else {
      centers[r2, ] <- matrix(sapply(xi, colMeans))
    }
    if (all(is.na(centers))) {
      stop("at least one group must have more than one observation with no NAs")
    }
    new_center <- unname(colMeans(centers, na.rm = TRUE))
    if (any(rna <- rowna(centers))) {
      centers[rna, ] <- matrix(new_center, sum(rna), d, byrow = TRUE)
    }

    if (scale[1]) {
      scales <- matrix(nrow = nlevels(fi), ncol = d)
      rownames(scales) <- levels(fi)
      if (d > 1) {
        scales[r2, ] <- t(sapply(xi,
          function(i) apply(i, 2, sd, na.rm = TRUE) / scale))
      } else {
        scales[r2, ] <- matrix(sapply(xi,
          function(i) apply(i, 2, sd, na.rm = TRUE) / scale))
      }
      new_scale <- unname(colMeans(scales, na.rm = TRUE))
      if (any(rna)) {
        scales[rna, ] <- matrix(new_scale, sum(rna), d, byrow = TRUE)
      }
    } else {
      scales <- 0
      new_scale <- 0
    }
  }

  fi <- as.numeric(fi)
  if (scale[1]) {
    x <- (x - centers[fi, , drop = FALSE]) / scales[fi, , drop = FALSE]
  } else {
    x <- x - centers[fi, , drop = FALSE]
  }

  if (ncol(x) == 1) {
    x <- as.vector(x)
  }

  a$pred <- structure(list(formula = formula, centers = centers, scales = scales,
    new_center = new_center, new_scale = new_scale), class = c("scaledby_pred",
    "list"))
  a$class <- unique(c("scaledby", a$class))
  attributes(x) <- a

  return(x)
}


#' S3 \code{\link[stats]{makepredictcall}} method for class \code{scaledby}.
#'
#' Allows \code{\link{scale_by}} to be used within a regression
#' \code{\link[stats]{formula}} and ensures that the \code{predvars} attribute
#' makes the correct call to \code{scale_by}.
#'
#' @param var,call See \code{\link[stats]{makepredictcall}}.
#'
#' @author Christopher D. Eager <eager.stats@gmail.com>
#'
#' @export
makepredictcall.scaledby <- function(var, call) {
  return(substitute(standardize::scale_by(object = X),
    list(X = attr(var, "pred"))))
}

