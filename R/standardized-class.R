

standardized <- setClass("standardized",
  slots = list(
    call = "call",
    scale = "numeric",
    formula = "formula",
    frame = "data.frame",
    pred.terms = "ANY",
    grid.levels = "list"
  )
)


#' @export
`$.standardized` <- function(x, name) {
  if (name %in% slotNames(x)) {
    return(slot(x, name))
  }
  return(NULL)
}


#' @export
is.standardized <- function(object) {
  return(inherits(object, "standardized"))
}


#' @export
predict.standardized <- function(object, newdata, response = FALSE,
                                 fixed = TRUE, random = TRUE,
                                 na.action = "na.pass") {
  mt <- object@pred.terms
  a <- attributes(mt)
  if (fixed && !random) {
    mt <- lme4::nobars(mt)
  } else if (!fixed && random) {
    mt <- lme4::subbars(lme4__reOnly(mt, TRUE))
    attributes(mt) <- a
  } else if (fixed && random) {
    mt <- lme4::subbars(mt)
  } else {
    stop("'fixed' and 'random' cannot both be FALSE")
  }
  if (!response) {
    mt <- stats::delete.response(mt)
    attributes(mt) <- a
  }
  mt <- condense_terms(mt)
  
  mf <- model.frame(mt, newdata, na.action = na.action)
  a <- attributes(mf)
  a <- a[names(a) %in% c("names", "row.names", "class")]
  attributes(mf) <- a
  colnames(mf) <- unname(attr(mt, "rename"))
  
  return(mf)
}
setMethod("predict", "standardized", predict.standardized)


#' @export
summary.standardized <- function(object, ...) {
  summ <- list(call = object@call, scale = object@scale,
    formula = object@formula)
  a <- attributes(object@pred.terms)
  
  summ$variables <- data.frame(
    Variable = names(a$rename),
    Name = unname(a$rename),
    Class = unname(a$dataClasses))
    
  facs <- a$dataClasses %in% c("factor", "ordered")
  summ$contrasts <- lapply(object@frame[, facs, drop = FALSE], contrasts)
  
  groups <- a$dataClasses == "group"
  summ$groups <- lapply(object@frame[, groups, drop = FALSE], levels)
  
  class(summ) <- c("summ.standardized", "list")
  return(summ)
}
setMethod("summary", "standardized", summary.standardized)


#' @export
print.summ.standardized <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  
  cat("\nNew Formula:\n")
  print(x$formula, showEnv = FALSE)
  
  cat("\nVariables:\n")
  print(x$variables, row.names = FALSE, right = FALSE)
  
  cat("\nStandardized Scale: ", x$scale, "\n",
      "Continuous variables have mean zero and standard deviation 'scale'\n",
      "Unordered factors are coded with sum contrasts with deviation 'scale'\n",
      "Ordered factors are coded with orthogonal polynomial contrasts\n",
      "  with column standard deviations of 'scale'\n",
      "Groups are coded as unordered factors with default contrasts\n\n",
      sep = "")
}


#' @export
print.standardized <- function(x, ...) {
  print(summary(x))
}
setMethod("print", "standardized", print.standardized)


#' @export
setMethod("show", signature(object = "standardized"), function(object) {
  print(object)
})

