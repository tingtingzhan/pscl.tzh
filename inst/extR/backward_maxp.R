

#' @title Naive Backward Elimination of Maximum p-Value
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param ... ..
#' 
#' @details ..
#' 
#' 
#' @references .. 
#' 
#' @export
backward_maxp <- function(object, ...) UseMethod('backward_maxp')

# simplify this function for ?backward_maxp.zeroinfl
replace_row <- function(x, pattern, id = grep(pattern, x = rnm), value) {
  if (is.data.frame(x)) {
    nr <- .row_names_info(x, type = 2L)
    rnm <- row.names.data.frame(x)
  } else if (is.matrix(x)) {
    nr <- dim(x)[1L]
    rnm <- dimnames(x)[[1L]]
  } else stop('not supported yet!')
  
  if (is.logical(id)) {
    if (length(id) != nr) stop('do not allow') # could be 'matrix'
    id <- which(id)
  }
  if (is.character(id)) id <- match(x = id, table = rnm, nomatch = NA_integer_)
  if (!is.integer(id)) stop('id cannot be turned into integer')
  if (anyNA(id)) stop('do not allow missing column id')
  id1 <- min(id)
  id2 <- max(id)
  if (!setequal(id, id1:id2)) stop('id must be consecutive')
  .head <- if (id1 > 1L) x[1:(id1-1L), , drop = FALSE] # else NULL # 'matrix' or 'data.frame'
  .tail <- if (id2 < nr) x[(id2+1L):nr, , drop = FALSE] # else NULL # 'matrix' or 'data.frame'
  if (!length(value)) return(rbind(.head, .tail))
  .center <- if (is.list(value)) do.call(rbind, args = value) else value
  return(rbind(.head, .center, .tail))
}





#' @importFrom pscl zeroinfl
#' @export
backward_maxp.zeroinfl <- function(
    object, alpha = .05, data, 
    ...
) {
  # based on ?mpath::be.zeroinfl, which has serious flaw for categorical predictors!
  dist <- object$dist
  fit <- object
  rhs1 <- attr(fit$terms$count, which = 'term.labels', exact = TRUE)
  rhs2 <- attr(fit$terms$zero, which = 'term.labels', exact = TRUE)
  
  # my new!
  check_dat <- data[unique.default(c(rhs1, rhs2))]
  .check_fac <- names(which(unlist(lapply(check_dat, FUN = \(i) {
    if (is.character(i) && length(unique.default(i) > 2L)) return(TRUE)
    if (is.factor(i) && nlevels(i) > 2L) return(TRUE)
    return(FALSE)
  }))))
  # end of my new!
  
  j <- 1L
  ret_logLik <- ret_BIC <- ret_AIC <- double()
  ret_dropvar <- character()

  repeat {
    coef <- summary(fit)$coefficients
    
    # my new!!
    if (length(.check_fac)) {
      xlevs <- xlevels(fit)
      for (ichk in .check_fac) { # ichk = .check_fac[1]
        ilev <- paste0(ichk, xlevs[[ichk]][-1L]) # -1L removes reference level
        
        id_ct <- match(ilev, table = rownames(coef$count))
        if (!anyNA(id_ct)) {
          ifac_ct <- coef$count[id_ct, , drop = FALSE]
          irepl_ct <- ifac_ct[which.min(ifac_ct[,4L]), , drop = FALSE]
          rownames(irepl_ct) <- ichk
          coef$count <- replace_row(coef$count, id = id_ct, value = irepl_ct)
        }
        
        id_zr <- match(ilev, table = rownames(coef$zero))
        if (!anyNA(id_zr)) {
          ifac_zr <- coef$zero[id_zr, , drop = FALSE]
          irepl_zr <- ifac_zr[which.min(ifac_zr[,4L]), , drop = FALSE]
          rownames(irepl_zr) <- ichk
          coef$zero <- replace_row(coef$zero, id = id_zr, value = irepl_zr)
        }
      }
    }
    # end of my new
    
    d <- dim(coef$count)[1L]
    count_rm <- if (dist != 'negbin') 1L else c(1L, d)
    zero_rm <- 1L
    rhs1 <- attr(fit$terms$count, which = 'term.labels', exact = TRUE)
    rhs2 <- attr(fit$terms$zero, which = 'term.labels', exact = TRUE)
    count.pval <- coef$count[-count_rm, 4L]
    zero.pval <- coef$zero[-zero_rm, 4L]
    count_ord1 <- which.max(count.pval) # returns integer(0) if all NA
    zero_ord1 <- which.max(zero.pval)
    if (!length(count_ord1) && !length(zero_ord1)) break
    count.max <- if (!length(count_ord1)) 0 else count.pval[count_ord1]
    zero.max <- if (!length(zero_ord1)) 0 else zero.pval[zero_ord1]
    
    if (count.max > zero.max) {
      if (count.max <= alpha) break
      # drop one variable with max p-val in count component
      dropvar <- paste0('count_', rhs1[count_ord1])
      rhs1 <- rhs1[-count_ord1]
    } else if (zero.max > alpha) {
      # drop one variable with max p-val in zero component
      dropvar <- paste0('zero_', rhs2[zero_ord1])
      rhs2 <- rhs2[-zero_ord1]
    } else break
    
    rhs1tmp <- if (!length(rhs1)) 1 else paste(rhs1, collapse = ' + ')
    rhs2tmp <- if (!length(rhs2)) 1 else paste(rhs2, collapse = ' + ')
    res <- deparse1(terms(fit$terms$count)[[2L]]) # response
    out <- paste(res, '~', rhs1tmp, '|', rhs2tmp)
    environment(out) <- parent.frame()
    fit <- try(zeroinfl(eval(str2lang(out)), data = data, dist = dist)) # should be very rare
    if (inherits(fit, what = 'try-error')) break
    ret_dropvar[j] <- dropvar
    ret_logLik[j] <- logLik(fit)
    ret_BIC[j] <- AIC(fit, k = log(dim(data)[1]))
    ret_AIC[j] <- AIC(fit)
    j <- j + 1L
  }
  if (FALSE) {
    print.data.frame(data.frame(
      Dropvar = ret_dropvar, logLik = ret_logLik, BIC = ret_BIC, AIC = ret_AIC
    ), row.names = FALSE)
  } 
  return(fit)
}





