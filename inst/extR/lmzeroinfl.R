

#' @title lmzeroinfl
#' 
#' @description ..
#' 
#' @param formula ..
#' 
#' @param data ..
#' 
#' @param ... ..
#' 
#' @details 
#' [lmzeroinfl] determines whether zero-inflated negative binomial regression
#' should be used, instead of zero-inflated Poisson regression,
#' by the significance of the inverse of the dispersion parameter \eqn{\theta = 1/\alpha}.
#' The workhorse in this function is \link[pscl]{zeroinfl}.
#' See some examples in \url{https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/}.
#' 
#' @importFrom pscl zeroinfl
#' 
#' @seealso `backward_maxp.zeroinfl`
#' 
#' @examples 
#' 
#' if (FALSE) {
#' # potential error from ?devtools::check
#' cibeta(lmzeroinfl(Days ~ Age, data = MASS::quine))
#' 
#' data(bioChemists, package = 'pscl')
#' cibeta(lmzeroinfl(art ~ ., data = bioChemists))
#' cibeta(lmzeroinfl(art ~ .|ment, data = bioChemists))
#' cibeta(lmzeroinfl(art ~ fem + phd, data = bioChemists))
#' cibeta(lmzeroinfl(art ~ fem + phd | ment, data = bioChemists))
#' }
#' 
#' @export
lmzeroinfl <- function(formula, data, ...) {
  
  orig_cl <- match.call()
  yval <- eval(formula[[2L]], envir = data)
  if (min(yval) > 0) stop('minimum response is greater than 0; zero-inflated model not appropriate')
  
  cl <- orig_cl
  cl[[1L]] <- quote(zeroinfl)
  cl$dist <- 'poisson'
  model_pois <- eval(cl)
  model_pois <- suppressWarnings(backward_maxp.zeroinfl(model_pois, data = data, alpha = .05))
  
  cl$dist <- 'negbin'
  model_nb <- tryCatch(eval(cl), error = identity, warning = identity)
  if (inherits(model_nb, what = c('error', 'warning'))) return(model_pois)
  model_nb <- suppressWarnings(backward_maxp.zeroinfl(model_nb, data = data, alpha = .05))
  p_theta <- summary(model_nb)[[1L]]$count['Log(theta)', 'Pr(>|z|)']
  if (!is.na(p_theta) && p_theta < .05) return(model_nb)
  return(model_pois)

}  
  


# Generally, \link[MASS]{stepAIC} and \link[stats]{update} does NOT work for 
# zero-inflated models.



#' @title vuong_zeroinfl
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param ... ..
#' 
#' @details
#' Function [vuong_zeroinfl] determines whether zero-inflated or non-zero-inflated model 
#' should be used, using \link[pscl]{vuong}'s closeness test.
#' 
#' 
#' 
#' @examples 
#' library(pscl) # "2024-01-14"
#' ?pscl::zeroinfl
#' 
#' zeroinfl(art ~ . | ., data = bioChemists) |>
#'  vuong_zeroinfl()
#' 
#' zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin") |>
#'  vuong_zeroinfl()
#'
#' @importFrom MASS glm.nb
#' @importFrom pscl vuong
#' @export
vuong_zeroinfl <- function(object, ...) {
  if (!inherits(object, what = 'zeroinfl')) stop('input must be zeroinfl object')
  cl0 <- object$call
  fom0 <- object$formula
  fom <- eval(call(name = '~', fom0[[2L]], fom0[[3L]][[2L]]))
    
  cl <- cl0
  cl$dist <- NULL
  cl$formula <- fom
  
  switch(object$dist, poisson = {
    cl[[1L]] <- quote(glm)
    cl$family <- quote(poisson(link = 'log'))
  }, negbin = {
    cl[[1L]] <- quote(glm.nb)
    cl$link <- quote(log)
  })
  
  no_zeroinfl <- eval(cl)
  
  message('model1 (zeroinfl); model2 (non_zeroinfl)')
  vuong(m1 = object, m2 = no_zeroinfl)

}







