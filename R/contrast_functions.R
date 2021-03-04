

#' Create a factor and specify contrasts.
#'
#' The \code{fac_and_contr} function is a convenience function which coerces
#' \code{x} to a factor with specified \code{levels} and \code{contrasts}.
#'
#' @param x An object coercible to \code{\link[base]{factor}}.
#'
#' @param levels A character vector of levels for the factor.
#'
#' @param contrasts A matrix of \code{\link[stats]{contrasts}} for the factor.
#'
#' @param ordered A logical indicating whether or not the factor is ordered
#'   (default \code{FALSE}).
#'
#' @seealso \code{\link{named_contr_sum}} (unordered factors) and
#'   \code{\link{scaled_contr_poly}} (ordered factors).
#'
#' @author Christopher D. Eager <eager.stats@gmail.com>
#'
#' @export
fac_and_contr <- function(x, levels, contrasts, ordered = FALSE) {
  x <- factor(x, levels = levels, ordered = ordered)
  contrasts(x) <- contrasts
  return(x)
}


#' Create named sum contrasts for an unordered factor.
#'
#' \code{named_contr_sum} creates sum contrasts for a factor which are named
#' with the levels of the factor rather than with numbers (e.g. if a factor
#' \code{f1} has levels \code{A}, \code{B}, and \code{C}, then rather than
#' creating contrast columns \code{f11} and \code{f12}, it creates columns
#' \code{f1A} and \code{f1B}).  The absolute value of the non-zero elements
#' of the matrix can also be specified.
#'
#' First, \code{x} is coerced to factor, and its levels (excluding \code{NA})
#' are sorted alphabetically.  If there are two unique values, and they are
#' equal to (ignoring case) "F" and "T", "FALSE" and "TRUE", "N" and "Y",
#' "NO" and "YES", or "0" and "1", then their order is reversed (this makes it
#' so the positive level gets the dummy coefficient rather than the negative
#' level, yielding a more intuitive interpretation for coefficients).  Then
#' \code{\link[stats]{contr.sum}} is called, and the column names of the
#' resulting contrast matrix are set using the character vector of unique values
#' (excluding the final element that gets coded as \code{-1} for all dummy
#' variables).  This entire matrix is then multiplied by \code{scale}; with
#' the default value of \code{1}, this does not change the matrix; if, for
#' example, \code{scale = 0.5}, then rather than each column containing values
#' in \code{-1, 0, 1}, each column would contain values in \code{-0.5, 0, 0.5}.
#' If \code{return_contr = TRUE}, then this contrast matrix is
#' returned.  If \code{return_contr = FALSE}, then \code{x} is converted to an
#' unordered factor with the named sum contrats and returned. \code{NA} is never
#' assigned as a level in the contrast matrix or in the factor returned by the
#' function, but \code{NA} values in \code{x} are not removed in the factor
#' returned when \code{return_contr = FALSE}. See the examples.
#'
#' @param x An object coercible to factor or a numeric or character vector of
#'   levels.
#'
#' @param scale A positive number by which the entire contrast
#'   matrix returned by \code{\link[stats]{contr.sum}} is multiplied.  See
#'   'Details'.
#'
#' @param return_contr A logical. If \code{TRUE} (the default), a contrast
#'   matrix is returned. If \code{FALSE}, \code{x} is converted to an unordered
#'   factor with the contrast matrix applied, and the factor is returned.
#'
#' @return If \code{return_contr = TRUE}, a contrast matrix obtained from
#'   \code{\link[stats]{contr.sum}} with named columns rather than numbered
#'   columns and deviations with magnitude \code{scale}.
#'   If \code{return_contr = FALSE}, then \code{x} is returned
#'   as an unordered factor with the named sum contrasts applied.
#'
#' @seealso \code{\link{scaled_contr_poly}} for ordered factors.
#'
#' @example examples/named_contr_sum.R
#'
#' @author Christopher D. Eager <eager.stats@gmail.com>
#'
#' @export
named_contr_sum <- function(x, scale = 1, return_contr = TRUE) {
  x <- factor(x, ordered = FALSE)
  if (!is.scalar(scale, 1)) {
    stop("'scale' must be a single positive number")
  }

  n <- length(lvs <- reorder_ft(sort(levels(x))))
  contr <- stats::contr.sum(lvs) * scale
  colnames(contr) <- lvs[-n]

  if (return_contr) return(contr)
  return(fac_and_contr(x, lvs, contr, FALSE))
}


#' Create scaled orthogonal polynomial contrasts for an ordered factor.
#'
#' The function \code{\link[stats]{contr.poly}} creates orthogonal polynomial
#' contrasts for an ordered factor, with the standard deviations of the
#' columns in the contrast matrix determined by the number of columns.  The
#' \code{scaled_contr_poly} function takes this contrast matrix and alters
#' the scale so that the standard deviations of the columns all equal
#' \code{scale}.
#'
#' If \code{x} is a factor, then the non-\code{NA} levels of \code{x} are used
#' as the levels for the contrast matrix.  If \code{x} is a vector,
#' then the unique non-\code{NA} values in \code{x} in the order in which
#' they appear in \code{x} are used as the levels for the contrast matrix.
#' If \code{x} is a single integer greater than or equal to \code{3}, then
#' the numbers \code{1:x} are used as the levels for the contrast matrix.  Any
#' other value for \code{x} results in an error (if \code{x = 2}, then
#' polynomial contrasts are technically possible, but all binary predictors
#' should be treated as unordered factors and coded with sum contrasts).
#' \code{\link[stats]{contr.poly}} is then called to obtain an orthogonal
#' polynomial contrast matrix of the appropriate degree. The contrast matrix is
#' is put on unit scale and then multiplied by the \code{scale} argument,
#' resulting in an orthogonal polynomial contrast matrix where
#' each column has standard deviation \code{scale}.  If
#' \code{return_contr = TRUE}, the contrast matrix is returned.  If
#' \code{return_contr = FALSE}, then \code{x} is coerced to
#' an ordered factor with the contrast matrix applied, and \code{x} is returned.
#' \code{NA} is never
#' assigned as a level in the contrast matrix or in the factor returned by the
#' function, but \code{NA} values in \code{x} are not removed in the factor
#' returned when \code{return_contr = FALSE}.
#'
#' @param x A factor, a numeric or character vector of levels ordered least to
#'   greatest, or a single integer greater than or equal to \code{3}.
#'   See 'Details'.
#'
#' @param scale A single positive number indicating the standard deviation
#'   for the columns of the contrast matrix. Default is 1.
#'
#' @param return_contr A logical indicating whether the contrast matrix should
#'   be returned, or \code{x} as an ordered factor with the contrasts applied.
#'   See 'Details'.
#'
#' @return If \code{return_contr = TRUE} a scaled orthogonal polynomial contrast
#'   matrix is returned.  If \code{return_contr = FALSE}, then a factor with the
#'   scaled orthogonal polynomial contrasts is returned.
#'
#' @seealso \code{\link{named_contr_sum}} for unordered factors.
#'
#' @example examples/scaled_contr_poly.R
#'
#' @author Christopher D. Eager <eager.stats@gmail.com>
#'
#' @export
scaled_contr_poly <- function(x, scale = 1, return_contr = TRUE) {
  if (sc <- is.scalar(x, 1)) {
    lvs <- paste(1:x)
  } else if (is.factor(x)) {
    lvs <- setdiff(levels(x), NA)
  } else {
    lvs <- setdiff(x, NA)
  }

  if (!is.scalar(scale, 1)) {
    stop("'scale' must be a single positive number")
  }
  if ((n <- length(lvs)) < 3) {
    stop("Factors with fewer than 3 levels should be coded as unordered ",
      "with sum contrasts")
  }

  contr <- stats::contr.poly(n)
  contr <- scale(contr)[, 1:(n - 1)] * scale
  if (!sc) rownames(contr) <- lvs

  if (return_contr) return(contr)
  return(fac_and_contr(x, lvs, contr, TRUE))
}

