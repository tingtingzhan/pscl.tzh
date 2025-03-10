

#' @title Additional S3 methods for \link[pscl]{zeroinfl}
#' 
#' @param x \link[pscl]{zeroinfl}
#' 
#' @name S3_zeroinfl
#' @export
coef_.zeroinfl <- function(x) x$coefficients # see ?pscl:::coef.zeroinfl

#' @rdname S3_zeroinfl
#' @importFrom stats family formula
#' @export
endpoint.zeroinfl <- function(x) {
  edp <- formula(x)[[2L]] # [tzh::endpoint.default]
  return(list(
    count = paste(edp, '(Count Model)'), # |> as.expression(),
    zero = paste0('I(', edp, ' == 0)')# |> as.expression()
  ))
}


#' @rdname S3_zeroinfl
#' @export
desc_.zeroinfl <- function(x) {
  #paste('zero-inflated', family_text(x$dist), 'regression') # [tzh::family_text]
  paste('zero-inflated', x$dist, 'regression')
}



#' @rdname S3_zeroinfl
#' @export
.pval.summary.zeroinfl <- function(x) {
  # returned value from ?pscl:::summary.zeroinfl
  lapply(x$coefficients, FUN = function(i) {
    ret <- i[, 'Pr(>|z|)']
    nm <- rownames(i)
    if (any(id <- ('Log(theta)' == nm))) {
      # `x$coefficients` does not contain 'Log(theta)'
      ret <- ret[!id]
      nm <- nm[!id]
    }
    names(ret) <- nm # if nrow-1, rownames will not be retained
    return(ret)
  })
}




# (necessary) clash ?MuMIn:::family.zeroinfl
#' @importFrom MASS negative.binomial 
#' @importFrom stats binomial poisson
#' @export
family.zeroinfl <- function(object, ...) { # see ?pscl::zeroinfl
  if (length(count_dist <- object$dist) != 1L) stop('will not happen')
  ret <- list(
    count = switch(count_dist, 
                   poisson = poisson(link = 'log'),
                   negbin = negative.binomial(link = 'log', theta = object$theta), # ?MASS::negative.binomial
                   geometric = stop('then what?')),
    # parameter 'dist': (a log link is always used)
    # theta is needed for negbin dist
    zero = binomial(link = object$link) # this is ?MuMIn:::family.zeroinfl
    # parameter 'link': (a binomial family is always used).
  )
  class(ret) <- 'listof'
  return(ret)
}



# @rdname S3_zeroinfl
# @export
#getLink.zeroinfl <- function(x) {
#  # `x$link` is link for zero-model only!!
#  x |>
#    family() |>
#    vapply(FUN = getLink.default, FUN.VALUE = '')
#}




#' @rdname S3_zeroinfl
#' @param level ..
#' @param ... ..
#' @importFrom stats confint.default
#' @export
confint_.zeroinfl <- function(x, level = .95, ...) {
  nm <- x |> coef_.zeroinfl() |> names()
  ci <- confint.default(x, level = level, ...) # no `confint` method for 'zeroinfl' in \pkg{pscl}
  rnm <- dimnames(ci)[[1L]]
  ret <- lapply(paste0(nm, '_'), FUN = function(i) {
    out <- ci[startsWith(rnm, prefix = i), , drop = FALSE]
    rownames(out) <- gsub(i, replacement = '', x = rownames(out))
    attr(out, which = 'conf.level') <- level
    return(out)
  })
  names(ret) <- nm
  return(ret)
}

#' @importFrom stats nobs
#' @export
nobs.zeroinfl <- function(object, ...) object[['n']]



