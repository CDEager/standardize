

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
#' be exactly \code{1} (but will be close).  The intepretation of the
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
#' If \code{object} is not a formula, it must be the numeric variable which
#' resulted from a previous \code{scale_by} call, or the \code{pred} attribute
#' of such a numeric variable. In this case, \code{scale}
#' is ignored, and \code{x} in \code{data} is scaled
#' using the formula, centers and scales from
#' a previous call will be used (with new levels treated using \code{new_center}
#' and \code{new_scale}).
#'
#' @param object A \code{\link[stats]{formula}} whose left hand side indicates
#'   a numeric variable to be scaled and whose right hand side indicates
#'   factors to condition this scaling on; or the result of a previous call
#'   to \code{scale_by} or the \code{pred} attribute of a previous call.
#'   See 'Details'.
#' @param data A data.frame containing the numeric variable to be scaled and
#'   the factors to condition on.
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
#' @examples
#' dat <- data.frame(
#'   f1 = rep(c("a", "b", "c"), c(5, 10, 20)),
#'   x1 = rnorm(35, rep(c(1, 2, 3), c(5, 10, 20)),
#'     rep(c(.5, 1.5, 3), c(5, 10, 20))))
#' 
#' dat$x1_scaled <- scale(dat$x1)
#' dat$x1_scaled_by_f1 <- scale_by(x1 ~ f1, dat)
#'
#' mean(dat$x1)
#' sd(dat$x1)
#' with(dat, tapply(x1, f1, mean))
#' with(dat, tapply(x1, f1, sd))
#' 
#' mean(dat$x1_scaled)
#' sd(dat$x1_scaled)
#' with(dat, tapply(x1_scaled, f1, mean))
#' with(dat, tapply(x1_scaled, f1, sd))
#' 
#' mean(dat$x1_scaled_by_f1)
#' sd(dat$x1_scaled_by_f1)
#' with(dat, tapply(x1_scaled_by_f1, f1, mean))
#' with(dat, tapply(x1_scaled_by_f1, f1, sd))
#'
#' newdata <- data.frame(
#'   f1 = c("a", "b", "c", "d"),
#'   x1 = rep(1, 4))
#'
#' newdata$x1_pred_scaledby <- scale_by(dat$x1_scaled_by_f1, newdata)
#'
#' newdata
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
    stop()
  }
  
  if (!is.null(pred)) {
    if (!inherits(pred, "scaledby_pred")) {
      stop("'pred', if specified, must be the 'pred' attribute of a\n",
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
  
  x <- data[[rownames(fmat)[rowSums(fmat) == 0]]]
  if (!is.numeric(x)) stop("left hand side of 'formula' must be numeric")
  a <- attributes(x)
  if (!is.matrix(x)) x <- matrix(x)
  d <- ncol(x)
  
  facs <- rownames(fmat)[rowSums(fmat) > 0]
  for (f in facs) {
    data[[f]] <- factor(data[[f]], ordered = FALSE)
    if (anyNA(data[[f]])) data[[f]] <- addNA(data[[f]])
  }
  fi <- interaction(data[facs])
  
  if (!is.null(pred)) {
    centers <- pred$centers
    scales <- pred$scales
    new_center <- pred$new_center
    new_scale <- pred$new_scale
    scale <- 1 * is.matrix(scales)
    if (ncol(centers) != d || (scale && ncol(scales) != d)) {
      stop("'pred' and 'data' imply different dimensions for numeric variable")
    }
    
    newlvs <- levels(fi)[!(levels(fi) %in% rownames(centers))]
    newc <- matrix(new_center, length(newlvs), d, byrow = TRUE)
    rownames(newc) <- newlvs
    centers <- rbind(centers, newc)
    if (scale) {
      news <- matrix(new_scale, length(newlvs), d, byrow = TRUE)
      rownames(news) <- newlvs
      scales <- rbind(scales, news)
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
    
    xi <- lapply(levels(fi), function(i) x[fi %in% i, , drop = FALSE])
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
    rna <- rowna(centers)
    if (any(rna)) {
      centers[rna, ] <- matrix(new_center, sum(rna), d, byrow = TRUE)
    }
    
    if (scale[1]) {
      scales <- matrix(nrow = nlevels(fi), ncol = d)
      rownames(scales) <- levels(fi)
      if (d > 1) {
        scales[r2, ] <- t(sapply(xi, function(i) apply(i, 2, function(u)
          sd(u, na.rm = TRUE)) / scale))
      } else {
        scales[r2, ] <- matrix(sapply(xi, function(i) apply(i, 2, function(u)
          sd(u, na.rm = TRUE)) / scale))
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
  
  if (scale[1]) {
    x <- (x - centers[as.numeric(fi), , drop = FALSE]) / 
      scales[as.numeric(fi), , drop = FALSE]
  } else {
    x <- x - centers[as.numeric(fi), , drop = FALSE]
  }

  if (ncol(x) == 1) {
    x <- as.vector(x)
  }

  pred <- list(formula = formula, centers = centers, scales = scales,
    new_center = new_center, new_scale = new_scale)
  class(pred) <- c("scaledby_pred", "list")
  a$pred <- pred
  attributes(x) <- a
  class(x) <- c("scaledby", class(x))
  
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
#' @export
makepredictcall.scaledby <- function(var, call) {
  call <- call("scale_by")
  call[[1]] <- quote(standardize::scale_by)
  call$object <- attr(var, "pred")
  return(call)
}

