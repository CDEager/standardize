

lme4__safeDeparse <- function(x, collapse = " ") {
  return(paste(deparse(x, 500L), collapse = collapse))
}


lme4__reOnly <- function(f, response = FALSE) {
  if (response && length(f) == 3) {
    response <- f[[2]]
  } else {
    response <- NULL
  }
  
  return(reformulate(paste0("(", vapply(lme4::findbars(f), lme4__safeDeparse, 
    ""), ")"), response = response))
}


