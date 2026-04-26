

#' @title \link[pscl]{zeroinfl} Objects
#' 
#' @examples
#' library(ecip); list(
#'  '`zeroinfl`' = zeroinfl(art ~ . | 1, data = bioChemists) |>
#'   structure(fig.height = 7)
#' ) |> fastmd::render2html()
#' 
#' @name zeroinfl
NULL


#' @importFrom ecip coef_
#' @export
coef_.zeroinfl <- function(x) x$coefficients

#' @importFrom ecip endpoint
#' @export
endpoint.zeroinfl <- function(x) {
  #edp <- formula(x)[[2L]] # 
  edp <- NextMethod(generic = 'endpoint') # [ecip::endpoint.default]
  return(list(
    count = paste(edp, '(Count Model)'), # |> as.expression(),
    zero = paste0('I(', edp, ' == 0)')# |> as.expression()
  ))
}


#' @importClassesFrom fastmd md_lines
#' @importFrom ecip desc_
#' @export
desc_.zeroinfl <- function(x) {
  
  paste('zero-inflated', switch(
    EXPR = x$dist,
    poisson = 'Poisson', 
    negbin = 'negative binomial', 
    geometric = 'geometric'
  ), 'regression [@Lambert92]') |>
    new(Class = 'md_lines', bibentry = .lambert92(), package = 'pscl')
  
}



#' @importFrom ecip .pval
#' @method .pval summary.zeroinfl
#' @export
.pval.summary.zeroinfl <- function(x) {
  # returned value from ?pscl:::summary.zeroinfl
  lapply(x$coefficients, FUN = \(i) {
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
#    vapply(FUN = getLink, FUN.VALUE = '')
#}




#' @importFrom ecip confint_
#' @export
confint_.zeroinfl <- function(x, level = .95, ...) {
  nm <- x |> coef_.zeroinfl() |> names()
  ci <- confint.default(x, level = level, ...) # no `confint` method for 'zeroinfl' in \pkg{pscl}
  rnm <- dimnames(ci)[[1L]]
  ret <- lapply(paste0(nm, '_'), FUN = \(i) {
    out <- ci[startsWith(rnm, prefix = i), , drop = FALSE]
    rownames(out) <- gsub(i, replacement = '', x = rownames(out))
    attr(out, which = 'conf.level') <- level
    return(out)
  })
  names(ret) <- nm
  return(ret)
}

#' @export
nobs.zeroinfl <- function(object, ...) object[['n']]


# `pscl:::terms.zeroinfl()` return is not desired (not sure to which version tzh was commenting..).
#' @importFrom ecip dataClasses
#' @export
dataClasses.zeroinfl <- function(x) {
  (x$terms$full) |> 
    # do *not* overwrite ?pscl:::terms.zeroinfl; packageDate('pscl') # 2024-01-14
    dataClasses() # ecip:::dataClasses.terms()
}



#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export
md_.zeroinfl <- md_ecip







