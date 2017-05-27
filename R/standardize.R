

#' Standardize a formula and data frame for regression.
#' 
#' Create a \code{\link[=standardized-class]{standardized}} object which places
#' all variables in \code{data} on the same scale based on \code{formula},
#' making regression output easier to interpret.
#' For mixed effects regressions, this also offers computational benefits, and
#' for Bayesian regressions, it also makes determining reasonable priors easier.
#'
#' First \code{\link[stats]{model.frame}} is called. Then,
#' if \code{family = gaussian}, the response is checked to ensure that it is
#' numeric and has more than two unique values.  If \code{\link{scale_by}} is
#' used on the response in \code{formula}, then the \code{scale} argument to
#' \code{scale_by} is ignored and forced to \code{1}.  If \code{\link{scale_by}}
#' is not called, then \code{\link[base]{scale}} is used with default arguments.
#' The result is that gaussian responses are on unit scale (i.e. have mean
#' \code{0} and standard deviation \code{1}), or, if \code{\link{scale_by}} is
#' used on the left hand side of \code{formula}, unit scale within each
#' level of the specified conditioning factor.
#' Offsets in gaussian models are divided by the standard deviation of the
#' the response prior to scaling (within-factor-level if \code{\link{scale_by}}
#' is used on the response).  In this way, if the transformed offset is added
#' to the transformed response, and then placed back on the response's original
#' scale, the result would be the same as if the un-transformed offset had
#' been added to the un-transformed response.
#' For all other values for \code{family}, the response and offsets are not checked.
#' If offsets are used within the \code{formula}, then they will be in the
#' \code{formula} and \code{data} elements of the \code{\linkS4class{standardized}}
#' object.  If the \code{offset} argument to the \code{standardize} function is
#' used, then the offset provided in the argument will be
#' in the \code{offset} element of the \code{\linkS4class{standardized}} object
#' (scaled if \code{family = gaussian}).
#'
#' For the other predictors in the formula, first any random effects grouping factors
#' in the formula are coerced to factor and unused levels are dropped.  The
#' levels of the resulting factor are then recorded in the \code{groups} element.
#' Then for the remaining predictors, regardless of their original
#' class, if they have only two unique non-\code{NA} values, they are coerced
#' to unordered factors.  Then, \code{\link{named_contr_sum}} and
#' \code{\link{scaled_contr_poly}} are called for unordered and ordered factors,
#' respectively, using the \code{scale} argument provided in the call
#' to \code{standardize} as the \code{scale} argument to the contrast
#' functions.  For numeric variables, if the variable contains a call to
#' \code{\link{scale_by}}, then, regardless of whether the call to
#' \code{\link{scale_by}} specifies \code{scale}, the value of \code{scale}
#' in the call to \code{standardize} is used.  If the numeric variable
#' does not contain a call to \code{\link{scale_by}}, then
#' \code{\link[base]{scale}} is called, ensuring that the result has
#' standard deviation \code{scale}.
#'
#' With the default value of \code{scale = 1}, the result is a
#' \code{\linkS4class{standardized}} object which contains a formula and data
#' frame (and offset vector if the \code{offset} argument to the 
#' \code{standardize} function was used) which can be used to fit regressions 
#' where the predictors are all on a similar scale.  Its data frame
#' has numeric variables on unit scale, unordered factors with named sum
#' sum contrasts, and ordered factors with orthogonal polynomial contrasts
#' on unit scale.  For gaussian regressions, the response is also placed on
#' unit scale.  If \code{scale = 0.5} (for example),
#' then gaussian responses would still
#' be placed on unit scale, but unordered factors' named sum contrasts would
#' take on values {-0.5, 0, 0.5} rather than {-1, 0, 1}, the standard deviation
#' of each column in the contrast matrices for ordered factors would be
#' \code{0.5} rather than \code{1}, and the standard deviation of numeric
#' variables would be \code{0.5} rather than \code{1} (within-factor-level
#' in the case of \code{\link{scale_by}} calls).
#'
#' @param formula A regression \code{\link[stats]{formula}}.
#' @param data A data.frame containing the variables in \code{formula}.
#' @param family A regression \code{\link[stats]{family}} (default gaussian).
#' @param scale The desired scale for the regression frame. Must be a single
#'   positive number. See 'Details'.
#' @param na.action See \code{\link[stats]{model.frame}}.
#' @param offset An optional \code{\link[stats]{offset}} vector. Offsets can
#'   also be included in the \code{formula} (e.g. \code{y ~ x + offset(o)}), but
#'   if this is done, then the column \code{o} (in this example) must be in any 
#'   data frame passed as the \code{newdata} argument to 
#'   \code{\link[=predict.standardized]{predict}}. 
#' 
#' @return A \code{\link[=standardized-class]{standardized}} object. The
#'   \code{formula}, \code{data}, and \code{offset} elements of the object can 
#'   be used in calls to regression functions.
#' 
#' @section Note: The \code{\link{scale_by}}
#'   function is supported so long as it is not nested within other function
#'   calls.  The \code{\link[stats]{poly}} function is supported so long as
#'   it is either not nested within other function calls, or is nested as the
#'   transformation of the numeric variable in a \code{\link{scale_by}} call.
#'   If \code{\link[stats]{poly}} is used, then the \code{lsmeans} function
#'   will yield misleading results (as would normally be the case).
#'
#' @seealso For scaling and contrasts, see \code{\link[base]{scale}},
#'   \code{\link{scale_by}}, \code{\link{named_contr_sum}}, and
#'   \code{\link{scaled_contr_poly}}. For putting new data into the same space
#'   as the standardized data, see \code{\link[=predict.standardized]{predict}}.
#'   For the elements in the returned object, see
#'   \code{\linkS4class{standardized}}.
#' 
#' @examples
#' dat <- expand.grid(ufac = letters[1:3], ofac = 1:3)
#' dat <- as.data.frame(lapply(dat, function(n) rep(n, 60)))
#' dat$ofac <- factor(dat$ofac, ordered = TRUE)
#' dat$x <- rpois(nrow(dat), 5)
#' dat$z <- rnorm(nrow(dat), rep(rnorm(30), each = 18), rep(runif(30), each = 18))
#' dat$subj <- rep(1:30, each = 18)
#' dat$y <- rnorm(nrow(dat), -2, 5)
#' 
#' sdat <- standardize(y ~ log(x + 1) + scale_by(z ~ subj) + ufac + ofac +
#'   (1 | subj), dat)
#' 
#' sdat
#' sdat$formula
#' head(dat)
#' head(sdat$data)
#' sdat$contrasts
#' sdat$groups
#' mean(sdat$data$y)
#' sd(sdat$data$y)
#' mean(sdat$data$log_x.p.1)
#' sd(sdat$data$log_x.p.1)
#' with(sdat$data, tapply(z_scaled_by_subj, subj, mean))
#' with(sdat$data, tapply(z_scaled_by_subj, subj, sd))
#' 
#' sdat <- standardize(y ~ log(x + 1) + scale_by(z ~ subj) + ufac + ofac +
#'   (1 | subj), dat, scale = 0.5)
#' 
#' sdat
#' sdat$formula
#' head(dat)
#' head(sdat$data)
#' sdat$contrasts
#' sdat$groups
#' mean(sdat$data$y)
#' sd(sdat$data$y)
#' mean(sdat$data$log_x.p.1)
#' sd(sdat$data$log_x.p.1)
#' with(sdat$data, tapply(z_scaled_by_subj, subj, mean))
#' with(sdat$data, tapply(z_scaled_by_subj, subj, sd))
#'
#' \dontrun{
#' mod <- lmer(sdat$formula, sdat$data)
#' # this next line causes warnings about contrasts being dropped, but
#' # these warnings can be ignored (i.e. the statement still evaluates to TRUE)
#' all.equal(predict(mod, newdata = predict(sdat, dat)), fitted(mod))
#' }
#' 
#' @importFrom lme4 subbars
#'
#' @export
standardize <- function(formula, data, family = gaussian, scale = 1,
                        na.action = "na.pass", offset) {
  mc <- match.call()
  
  formula <- stats::formula(formula)
  if (!inherits(formula, "formula")) {
    stop("'formula' must be coercible to formula")
  }
  tt <- terms(formula)
  if (!attr(tt, "response")) stop("no response in formula")
  if (!length(attr(tt, "term.labels"))) stop("no variables in formula")
  gfacs <- ranef_groups(formula)
  
  if (!is.data.frame(data)) stop("'data' must be a data.frame")
  attr(data, "terms") <- NULL
  
  family <- get_family(family)
  gau <- isTRUE(all.equal(family, gaussian()))
  
  if (!is.scalar(scale, 1)) {
    stop("'scale' must be a single positive number")
  }
  
  mf <- mc
  mf[[1]] <- quote(stats::model.frame)
  mf$formula <- lme4::subbars(formula)
  mf$drop.unused.levels = TRUE
  mf$na.action <- na.action
  mf[c("family", "scale", "offset")] <- NULL
  mf <- eval(mf, parent.frame())
  
  p <- attr(attr(mf, "terms"), "predvars")
  
  if (gau) {
    if (!is.numeric(mf[[1]])) {
      stop("'family' is gaussian but response is not numeric")
    }
    if (nval(mf[[1]]) < 3) {
      stop("'family' is gaussian but response has fewer than 3 unique values")
    }
    if (inherits(mf[[1]], "scaledby")) {
      p[[2]] <- mpc_scaledby(p[[2]], data, 1)
      pred_offset <- p[[2]]$object
      pred_offset$new_center[] <- 0
      pred_offset$centers[] <- 0
    } else {
      p[[2]] <- mpc_numeric(p[[2]], mf[[1]], 1)
      pred_offset <- p[[2]]$scale
    }
  } else {
    pred_offset <- NULL
  }
    
  # there won't be an "(offset)" column since we set the argument to NULL
  # the call to stats::model.frame and are handling it separately.
  # The goal is to produce the equivalent of the 'data' arg to a reg func,
  # not a model frame (which is why weights, etastart, etc aren't args to
  # standardize)
  if (length(o <- grep("^offset\\(", colnames(mf)))) {
    check_offset(mf[o], mf[[1]])
    colnames(mf)[o] <- substr(colnames(mf)[o], 8, nchar(colnames(mf)[o]) - 1)
    p[o + 1] <- mapply(mpc_offset, pv = p[o + 1],
      MoreArgs = list(po = pred_offset), SIMPLIFY = FALSE)
  }
  
  if (missing(offset)) {
    offset <- NULL
  } else {
    check_offset(offset, mf[[1]])
    if (gau) offset <- scale_offset(pred_offset, offset, data)
  }
  
  check_uf <- setdiff(2:ncol(mf), o)
  mf[check_uf] <- charlogbin_to_uf(mf[check_uf])
  
  d <- get_data_classes(mf, gfacs, o)
  vt <- split(2:ncol(mf), d[-1])
  
  p[vt$group + 1] <- mapply(mpc_group, pv = p[vt$group + 1], v = mf[vt$group],
    SIMPLIFY = FALSE)
  
  p[vt$factor + 1] <- mapply(mpc_factor, pv = p[vt$factor + 1],
    v = mf[vt$factor], MoreArgs = list(scale = scale), SIMPLIFY = FALSE)
  
  p[vt$ordered + 1] <- mapply(mpc_ordered, pv = p[vt$ordered + 1],
    v = mf[vt$ordered], MoreArgs = list(scale = scale), SIMPLIFY = FALSE)
  
  if (length(w <- c(vt$numeric, vt$poly))) {
    w <- sort(w)
    p[w + 1] <- mapply(mpc_numeric, pv = p[w + 1], v = mf[w],
      MoreArgs = list(scale = scale), SIMPLIFY = FALSE)
  }
  
  if (length(w <- c(vt$scaledby, vt$scaledby.poly))) {
    w <- sort(w)
    p[w + 1] <- mapply(mpc_scaledby, pv = p[w + 1], MoreArgs = list(data = data,
      scale = scale), SIMPLIFY = FALSE)
  }
  
  old_names <- colnames(mf)
  new_names <- make_new_names(old_names)
  
  vars <- data.frame(Variable = old_names, `Standardized Name` = new_names,
    Class = d, check.names = FALSE)
  names(p)[2:length(p)] <- new_names
  fr <- setNames(data.frame(matrix(nrow = nrow(data), ncol = ncol(mf))),
    new_names)
  fr[new_names] <- eval(p, envir = data)
  fr <- strip_attr(fr)
  
  old_names[o] <- paste0("offset(", old_names[o], ")")
  new_names[o] <- paste0("offset(", new_names[o], ")")
  
  form <- replace_variables(formula, old_names, new_names)
  attr(form, "standardized.scale") <- scale
  
  if (length(facs <- d %in% c("factor", "ordered"))) {
    contr <- lapply(fr[facs], contrasts)
  } else {
    contr <- NULL
  }
  if (length(groups <- d == "group")) {
    groups <- lapply(fr[groups], levels)
  } else {
    groups <- NULL
  }
  
  if (length(pfe <- varnms(delete.response(terms(lme4::nobars(form)))))) {
    pfe <- p[sort(c(1, 2, match(pfe, names(p))))]
  } else {
    pfe <- NULL
  }
  if (length(groups)) {
    pre <- varnms(lme4::subbars(lme4_reOnly(form)))
    pre <- p[sort(c(1, 2, match(pre, names(p))))]
  } else {
    pre <- NULL
  }
  pred <- list(all = p, fixed = pfe, random = pre, offset = pred_offset)
  
  return(structure(list(call = mc, scale = scale, formula = form,
    family = family, data = fr, offset = offset, pred = pred, variables = vars,
    contrasts = contr, groups = groups), class = "standardized"))
}

