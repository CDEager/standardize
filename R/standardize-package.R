

#' standardize: An R package providing tools for standardizing model frames.
#'
#' When all of the predictors in a regression are on a similar scale, it makes
#' the interpretation of their effect sizes more comparable. In the case of
#' gaussian regression, placing the response on unit scale also eases
#' interpretation.  Standardizing regression frames also has computational
#' benefits in the case of mixed effects regressions, and makes determining
#' reasonable priors in Bayesian regressions simpler.
#'
#' The \code{\link{named_contr_sum}} function gives named sum contrasts to
#' unordered factors, and allows the absolute value of the non-zero cells in
#' contrast matrix to be specified through its \code{scale} argument. The
#' \code{\link{scaled_contr_poly}} function gives orthogonal polynomial
#' contrasts to ordered factors, and allows the standard deviation of the
#' columns in the contrast matrix to be specified through its \code{scale}
#' argument. The \code{\link{scale_by}} function allows numeric variables
#' to be scaled conditioning on factors, such that the numeric variable has
#' the same mean and standard deviation within each level of a factor (or the
#' interaction of several factors), with the standard deviation specified
#' through its \code{scale} argument.
#'
#' The \code{\link{standardize_terms}} function creates a terms object of class
#' \code{standardized.terms} which can be used in regression fitting functions
#' to ensure that all of the predictors in the regression are placed on the
#' same scale.  This is done by passing the function's \code{scale} argument
#' to \code{\link{named_contr_sum}} for all unordered factors (and also
#' any predictor with only two unique values regardless of its original class),
#' to \code{\link{scaled_contr_poly}} for all ordered factors, and to
#' \code{\link{scale_by}} for numeric variables which contain calls to the
#' function.  For numeric predictors not contained in a \code{\link{scale_by}}
#' call, \code{\link[base]{scale}} is called, ensuring that the result has
#' standard deviation equal to the \code{scale} argument to
#' \code{\link{standardize_terms}}.  Gaussian responses are always placed on
#' unit scale, using \code{\link[base]{scale}} (or \code{\link{scale_by}} if
#' the function was used on the left hand side of the regression formula).
#'
#' @docType package
#' @name standardize-package
#'
#' @import stats
#' @import methods
NULL

