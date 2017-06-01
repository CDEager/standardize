

#' Class \code{standardized} containing regression variables in a standardized space.
#'
#' The \code{\link{standardize}} function returns a list of class
#' \code{standardized}, which has a \code{print} method,
#' and which can additionally be used to place new data into the same
#' standardized space as the data passed in the call to \code{\link{standardize}}
#' using the \code{\link[=predict.standardized]{predict}} function.
#' The \code{standardized} list contains the following elements.
#' \describe{
#'   \item{call}{The call to \code{\link{standardize}} which created the
#'     object.}
#'   \item{scale}{The \code{scale} argument to \code{\link{standardize}}.}
#'   \item{formula}{The regression formula in standardized space (with new
#'     names) which can be used along with the \code{data} element to fit
#'     regressions.  It has an attribute \code{standardized.scale} which is the
#'     same as the \code{scale} element of the object (this allows users and
#'     package devleopers to write regression-fitting functions which can tell
#'     if the input is from a \code{standardized} object).}
#'   \item{family}{The regression family.}
#'   \item{data}{A data frame containing the regression variables in a
#'     standardized space (renamed to have valid variable names corresponding
#'     to those in the \code{formula} element).}
#'   \item{offset}{The offset passed through the \code{offset} argument to 
#'     \code{\link{standardize}} (scaled if \code{family = gaussian}), or 
#'     \code{NULL} if the \code{offset} argument was not used.}
#'   \item{pred}{A list containing unevaluated calls which allow the 
#'     \code{\link[=predict.standardized]{predict}} method to work.}
#'   \item{variables}{A data frame with the name of the original variable,
#'     the corresponding name in the standardized data frame and formula,
#'     and the class of the variable in the standardized data frame.}
#'   \item{contrasts}{A named list of contrasts for all factors included as
#'     predictors, or \code{NULL} if no predicors are factors.}
#'   \item{groups}{A named list of levels for random effects grouping factors,
#'     or \code{NULL} if there are no random effects.}
#' }
#'
#' In the \code{variables} data frame, the \code{Variable} column contains the
#' name of the variable in the original formula passed to \code{\link{standardize}}.
#' The \code{Standardized Name} column contains the name of the variable in the standardized
#' formula and data frame. The original variable name is altered such that the
#' original name is still recoverable but is also a valid variable name for
#' regressions run using the \code{formula} and \code{data} elements of the
#' \code{standardized} object.  For example, \code{exp(x)} would become
#' \code{exp_x} and \code{log(x + 1)} would become \code{log_x.p.1}.  If
#' the indicator function is used, this can lead to a long and possibly
#' difficult to interpret name; e.g. \code{I(x1 > 0 & x2 < 0)} would become
#' \code{I_x1.g.0.a.x2.l.0}. In such cases, it is better to create the variable
#' explicitly in the data frame and give it a meaningful name; in this case,
#' something like \code{mydata$x1Pos_x2Neg <- mydata$x1 > 0 & mydata$x2 < 0},
#' and then use \code{x1Pos_x2Neg} in the call to \code{\link{standardize}}.
#' The \code{Class} column in the \code{variables} data frame takes the
#' following values (except for non-gaussian responses, which are left
#' unaltered, and so may have a different class; the class for the response is 
#' always preceded by \code{response.}).
#' \describe{
#'   \item{numeric}{A numeric vector.}
#'   \item{poly}{A numeric matrix resulting from a call to
#'     \code{\link[stats]{poly}}.}
#'   \item{scaledby}{A numeric vector resulting from a call to
#'     \code{\link{scale_by}}.}
#'   \item{scaledby.poly}{A numeric matrix resulting from a call to
#'     \code{\link[stats]{poly}} nested within a call to
#'     \code{\link{scale_by}}.}
#'   \item{factor}{An unordered factor.}
#'   \item{ordered}{An ordered factor.}
#'   \item{group}{A random effects grouping factor.}
#'   \item{offset}{If the offset function was used within the formula passed to 
#'     \code{\link{standardize}}, then the variable is numeric and labeled as 
#'     \code{offset}.  The \code{formula} element of the \code{standardize} 
#'     object contains offset calls to ensure regression fitting functions use 
#'     them properly.  If the \code{offset} argument was used in the call to 
#'     \code{\link{standardize}} (rather than putting offset calls in the 
#'     formula), then the offset is not in the \code{variables} data frame (it 
#'     is in the \code{offset} element of the \code{standardized} object).}
#' }
#'
#' The \code{standardized} object has a printing method which displays the call,
#' formula, and variable frame along with an explanation of the
#' standardization.  The \code{\link{is.standardized}} function returns
#' \code{TRUE} if an object is the result of a call to \code{\link{standardize}}
#' and \code{FALSE} otherwise.  The \code{\link[=predict.standardized]{predict}}
#' method places new data into the same standardied space as the data
#' passed to the original \code{\link{standardize}} call.
#'
#' @name standardized-class
NULL


#' Place new data into an already existing standardized space.
#'
#' To put new data into the same standardized space as the data in the
#' \code{\link[=standardized-class]{standardized}} object, 
#' \code{predict} can be used with the \code{standardized} object as the first
#' argument.  The \code{predict} method also allows logicals \code{response},
#' \code{fixed}, and \code{random} to be used to specify which elements of the
#' original data frame are present in \code{newdata}.  A regression model
#' fit with the \code{formula} and \code{data} elements of a
#' \code{\link[=standardized-class]{standardized}} object cannot be used to
#' directly predict the response variable for new data.  The new data must
#' first be placed into the standardized space. If offsets were included
#' in the \code{formula} argument used to create the \code{standardized} object,
#' then when \code{fixed = TRUE} the offset variables must be in \code{newdata}.
#' If an offset was passed to the \code{offset} argument in the call to
#' \code{\link{standardize}}, then the offset cannot be passed to \code{predict}.
#'
#' @section Note: You may see a warning "contrasts dropped from factor <x>" for
#'   each factor when predicting new data with a fitted model object, but this
#'   warning can be ignored (the actual predictions will still be correct).
#'
#' @param object An object of class \code{standardized}.
#' @param newdata Data to be placed into the same standardized space as the
#'   data in the call to \code{\link{standardize}} which produced the
#'   \code{\link[=standardized-class]{standardized}} object.
#' @param response A logical (default \code{FALSE}) indicating whether
#'   \code{newdata} contains the response variable.
#' @param fixed A logical (default \code{TRUE}) indicating whether
#'   \code{newdata} contains variables pertaining to the fixed effects.
#' @param random A logical (default \code{TRUE}) indicating whether
#'   \code{newdata} contains variables pertaining to the random effects.
#' @param ... Ignored with a warning.
#'
#' @return A data.frame with the \code{newdata} standardized using the
#'   \code{pred} element of the \code{\link[=standardized-class]{standardized}}
#'   object.
#'
#' @examples
#' \dontrun{
#' train <- subset(mydata, train)
#' test <- subset(mydata, !train)
#' train.s <- standardize(y ~ x1 + f1 + (1 | g1), train)
#' mod <- lmer(train.s$formula, train.s$data)
#' test.s <- predict(train.s, test, response = TRUE)
#' preds <- predict(mod, newdata = test.s)  # can ignore warning about dropped contrasts
#' res <- test.s$y - preds
#' }
#'
#' @export
predict.standardized <- function(object, newdata = NULL, response = FALSE,
                                 fixed = TRUE, random = TRUE, ...) {
  stopifnot(is.standardized(object), is.data.frame(newdata))
  
  check_dots_predict.standardized(...)
  
  p <- object$pred
  if (fixed && !random) {
    p <- p$fixed
  } else if (!fixed && random) {
    p <- p$random
  } else if (fixed && random) {
    p <- p$all
  } else {
    stop("'fixed' and 'random' cannot both be FALSE")
  }
  if (!response) {
    p <- p[-2]
  }
  if (length(p) < 2) {
    stop("No variables when response = ", response, ", fixed = ", fixed,
      ", and random = ", random, ".")
  }
  
  fr <- setNames(data.frame(matrix(nrow = nrow(newdata), ncol = length(p) - 1)),
    names(p)[-1])
  fr[names(p)[-1]] <- eval(p, envir = newdata)
  fr <- strip_attr(fr)
  
  return(fr)
}


#' Determine if an object has class \code{\link[=standardized-class]{standardized}}.
#'
#' @param object Any R object.
#'
#' @return \code{TRUE} if \code{object} is the result of a \code{\link{standardize}}
#'   call and \code{FALSE} otherwise.
#'
#' @export
is.standardized <- function(object) {
  return(inherits(object, "standardized"))
}


#' S3 \code{print} method for class \code{\link[=standardized-class]{standardized}}.
#'
#' @param x An object of class \code{standardized}.
#' @param ... Not used.
#'
#' @export
print.standardized <- function(x, ...) {
  sc <- round(x$scale, 3)
  
  cat("\nCall:\n")
  print(x$call)
  
  cat("\nStandardized Formula:\n")
  f <- x$formula
  attr(f, "standardized.scale") <- NULL
  print(f, showEnv = FALSE)
  
  cat("\nVariables:\n")
  print(x$variables, row.names = FALSE, right = FALSE)
  
  o <- any(x$variables$Class == "offset") | !is.null(x$offset)
  
  if (is.linear(f <- x$family)) {
    if (x$variables$Class[1] %in% c("response.scaledby", "response.scaledby.poly")) {
      cat("\nResponse has mean 0 and standard deviation 1 ",
        "within each factor level.\n", sep = "")
      if (o) {
        cat("Offsets are divided by the standard deviation of the raw response",
          "within each factor level.\n")
      }
    } else {
      cat("\nResponse has mean 0 and standard deviation 1.\n")
      if (o) {
        cat("Offsets are divided by the standard deviation of the raw response.\n")
      }
    }
  } else {
    if (!is.character(f)) f <- paste0(f$family, "(", f$link, ")")
    cat("\nResponse not altered because family = ", f, "\n", sep = "")
    if (o) {
      cat("Offsets are unaltered for the same reason.\n")
    }
  }
  
  cat(
    "\nContinuous variables have mean 0 and standard deviation ", sc, "\n",
    "  (within-factor-level if scale_by was used)\n",
    "Unordered factors have sum contrasts with deviation ", sc, "\n",
    "Ordered factors have orthogonal polynomial contrasts whose\n",
    "  columns have standard deviation ", sc, "\n",
    "Grouping factors are coded as unordered factors with default contrasts\n",
    "\n", sep = "")
}

